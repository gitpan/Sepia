package Devel::Xref;

our $VERSION = '0.01';

=head1 NAME

Devel::Xref - Generates cross reference database for use by Perl programs.

=head1 SYNOPSIS

    use Devel::Xref qw(rebuild defs callers);

    rebuild;
    for (defs 'foo') {
        printf "%s:%d: sub %s\::foo() defined\n", @{$_}[0..2];
    }

    for (callers 'foo') {
        printf "%s:%d: sub foo() called by %s\::%s().\n", @{$_}[0..3];
    }

=head1 DESCRIPTION

L<Devel::Xref> is intended as a programmatic interface to the
information supplied by L<B::Xref>.  It is intended to be a component
for interactive Perl development, with other packages providing a
friendly interface to the raw information it extracts.  L<B::Xref>
could be seen as an example of this sort of user-level tool, if it
weren't for the fact that this module was created later, and stole
most of its code.

=cut

use strict;
use Config;
use Cwd 'abs_path';
use B qw(peekop class comppadlist main_start svref_2object walksymtable
         OPpLVAL_INTRO SVf_POK OPpOUR_INTRO cstring
        );

=head2 Variables

=over

=item C<%call>

A map of subs to call locations and callers

=item C<%callby>

A map of subs to subs called.

=item C<%def>

A map of subs to definitions.

=item C<%package_sub>

A map of packages to subs defined.

=item C<%var_use>

A map of global/package variables to uses.

=item C<%var_def>

A map of global/package variables to definitions (usually empty, since
it only picks up local (...) declarations.

=item C<%module_files>

A map of module names to containing files.

=back

=item C<%file_modules>

A map of files to module names.

=back

=cut

our %call;
our %callby;
our %def;
our %package_sub;
our %var_def;
our %var_use;
our %module_files;
our %file_modules;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK =
    qw(xref_definitions xref_object xref_main
       redefined forget rebuild
       %call %def %callby %package_sub
       callers callees defs);

our %EXPORT_TAGS =
    (all => \@EXPORT_OK,
     most => [qw(redefined forget rebuild callers callees defs)]);

sub UNKNOWN { ["?", "?", "?"] }

my @pad;			# lexicals in current pad
				# as ["(lexical)", type, name]
my %done;			# keyed by $$op: set when each $op is done
my $top = UNKNOWN;		# shadows top element of stack as
				# [pack, type, name] (pack can be "(lexical)")
my $file;			# shadows current filename
my $line;			# shadows current line number
my $subname;			# shadows current sub name
my @todo = ();			# List of CVs that need processing
my %firstline;		# first line numbers seen in a sub (hack)

my %code = (intro => "i", used => "",
	    subdef => "s", subused => "&",
	    formdef => "f", meth => "->");


# Options
my ($debug_op, $debug_top, $nodefs, $raw);

# XXX: it turns out that rooting around trying to figure out the file
# ourselves is more reliable than what we grab from the op.
sub guess_module_file {
    my ($pack, $ofile) = @_;
    my $file;

    if (exists  $module_files{$pack}) {
	my $m = (keys %{$module_files{$pack}})[0];
	return $m if $m =~ /\Q$ofile\E/;
    }

    (my $mod = $pack) =~ s|::|/|g;
    while ($mod && !$file) {
	$file ||= $INC{"$mod.pm"};
	$mod =~ s|/?[^/]+$||;
    }

    $file ||= $ofile;		# okay, we failed.

    if ($file && $file !~ /^\//) {
	$file = abs_path($file);
    }

    if (!$file || !-f $file) {
	undef $file;
    }
    $file;
}

# XXX: should weed through the code below so it only generates decent
# package names, but this will fix it for now.
sub realpack {
    my $p = shift;
    if (!defined $p || $p eq '?') {
	return undef;
    } elsif ($p eq '') {
	return 'main';
    } else {
	return $p;
    }
}

# Turn a possibly-qualified name into a package and basename.
sub split_name {
    local $_ = shift;
    my ($p, $s);
    if (/^(.*)::(.+)$/) {
	($p, $s) = ($1, $2);
    } else {
	($p, $s) = ('main', $_);
    }
    undef $s if $s eq '?';
    ($p, $s);
}

# Add an item to a list of xref locations only if no item with that
# same location already exists (XXX: should use a hash for this?)
sub add_use {
    my ($l, $e, $file, $line) = @_;
    push @{$_[0]}, $e unless grep {
	$_->{line} == $line && $_->{file} eq $file
    } @$l;
}

# Add an item only if no item with the same package and file exists.
sub add_def {
    my ($l, $e, $file, $line, $pack) = @_;
    push @{$_[0]}, $e unless grep {
	$_->{file} eq $file && (!$pack || $pack eq $_->{package})
    } @$l;
}

sub process {
    my ($var, $event) = @_;
    my ($pack, $type, $name) = @$var;
    my $pack = realpack($pack);
    if ($type eq "*") {
	if ($event eq "used") {
	    return;
	} elsif ($event eq "subused") {
	    $type = "&";
	}
    }
    $type =~ s/(.)\*$/$1/g;
    $file = guess_module_file($pack, $file);
    if (defined($file)) {
	if ($pack) {
	    $module_files{$pack}{$file} = 1;
	    $file_modules{$file}{$pack} = 1;
	}
    }

    if (($type eq '&' || $type eq '->') && $subname ne '(definitions)') {
	# Handle caller/callee relations
	my ($spack, $sname) = split_name($subname);

	add_use $call{$name},
	{ file => $file,
	  sub => $sname,
	  sub_package => $spack,
	  package => $pack,
	  line => $line
	},
	    $file, $line;

	push @{$callby{$sname}},
	{ sub => $name,
	  sub_package => $pack,
	  package => $spack
	} unless grep {
	    $_->{sub} eq $name
		&& $_->{sub_package} eq $pack
		&& $_->{package} eq $spack
	    } @{$callby{$sname}};
    } elsif ($type eq 's' || $subname eq '(definitions)') {
	# Handle definition
	my $l = $line;
	if (exists($firstline{$name}{$pack})
	    && $firstline{$name}{$pack} < $l) {
	    $l = $firstline{$name}{$pack};
	}
	my $obj = { file => $file,
		    sub => $name,
		    package => $pack,
		    line => $l };
	add_use $package_sub{$pack}, $obj, $file, $l, $pack;
	add_def $def{$name}, $obj, $file, $l, $pack;
    } elsif ($name !~ /^[\x00-\x1f^] | ^\d+$ | ^[\W_]$
		       | ^(?:ENV|INC|STD(?:IN|OUT|ERR)|SIG)$ /x
	     && realpack($pack)) {
	# Variables, but ignore specials and lexicals
	my ($spack, $sname) = split_name($subname);
	if ($event eq 'intro') {
	    add_def $var_def{$name},
	    { file => $file,
	      package => $spack,
	      line => $line,
	      sub => $sname
	    },
		$file, $line, $pack;
	} elsif ($event eq 'used') {
	    add_use $var_use{$name},
	    { file => $file,
	      package => $spack,
	      line => $line,
	      sub => $sname,
	    },
		$file, $line;
	}
    }
}

# Because the CV's line number points to the end of the sub, we guess
# a line number based on the first pp_nextstate seen in the sub.
sub update_line_number {
    my ($pack, $name) = split_name($subname);
    my $found;
    my $l = $line - 1; # because we usually see "sub foo {\n    first_stmt...}"
    if ($pack && exists $def{$name}) {
	for (@{$def{$name}}) {
	    if ($_->{package} eq $pack) {
		if ($l < $_->{line}) {
		    $_->{line} = $l;
		}
		$found = 1;
		last;
	    }
	}
    }
    if (!exists($firstline{$name}{$pack})
	|| $firstline{$name}{$pack} > $l) {
	$firstline{$name}{$pack} = $l;
    }
}

sub load_pad {
    my $padlist = shift;
    my ($namelistav, $vallistav, @namelist, $ix);
    @pad = ();
    return if class($padlist) eq "SPECIAL";
    ($namelistav,$vallistav) = $padlist->ARRAY;
    @namelist = $namelistav->ARRAY;
    for ($ix = 1; $ix < @namelist; $ix++) {
	my $namesv = $namelist[$ix];
	next if class($namesv) eq "SPECIAL";
	my ($type, $name) = $namesv->PV =~ /^(.)([^\0]*)(\0.*)?$/;
	$pad[$ix] = [undef, $type, $name];
    }
    if ($Config{useithreads}) {
	my (@vallist);
	@vallist = $vallistav->ARRAY;
	for ($ix = 1; $ix < @vallist; $ix++) {
	    my $valsv = $vallist[$ix];
	    next unless class($valsv) eq "GV";
	    # these pad GVs don't have corresponding names, so same @pad
	    # array can be used without collisions
	    $pad[$ix] = [$valsv->STASH->NAME, "*", $valsv->NAME];
	}
    }
}

sub xref {
    my $start = shift;
    my $op;
    for ($op = $start; $$op; $op = $op->next) {
	last if $done{$$op}++;
	warn sprintf("top = [%s, %s, %s]\n", @$top) if $debug_top;
	warn peekop($op), "\n" if $debug_op;
	my $opname = $op->name;
	if ($opname =~ /^(or|and|mapwhile|grepwhile|range|cond_expr)$/) {
	    xref($op->other);
	} elsif ($opname eq "match" || $opname eq "subst") {
	    xref($op->pmreplstart);
	} elsif ($opname eq "substcont") {
	    xref($op->other->pmreplstart);
	    $op = $op->other;
	    redo;
	} elsif ($opname eq "enterloop") {
	    xref($op->redoop);
	    xref($op->nextop);
	    xref($op->lastop);
	} elsif ($opname eq "subst") {
	    xref($op->pmreplstart);
	} else {
	    no strict 'refs';
	    my $ppname = "pp_$opname";
	    &$ppname($op) if defined(&$ppname);
	}
    }
}

sub xref_cv {
    my $cv = shift;
    my $pack = $cv->GV->STASH->NAME;
    $subname = ($pack eq "main" ? "" : "$pack\::") . $cv->GV->NAME;
    load_pad($cv->PADLIST);
    xref($cv->START);
    $subname = "(main)";
}

sub xref_object {
    my $cvref = shift;
    xref_cv(svref_2object($cvref));
}

sub xref_main {
    $subname = "(main)";
    load_pad(comppadlist);
    xref(main_start);
    while (@todo) {
	xref_cv(shift @todo);
    }
}

sub pp_nextstate {
    my $op = shift;
    $file = $op->file;
    die "pp_nextstate: $file" if $file =~ /::/;
    $line = $op->line;
    update_line_number;
    $top = UNKNOWN;
}

sub pp_padsv {
    my $op = shift;
     $top = $pad[$op->targ];
#     process($top, $op->private & OPpLVAL_INTRO ? "intro" : "used");
}

sub pp_padav { pp_padsv(@_) }
sub pp_padhv { pp_padsv(@_) }

sub deref {
    my ($op, $var, $as) = @_;
    $var->[1] = $as . $var->[1];
    process($var, $op->private & OPpOUR_INTRO ? "intro" : "used");
}

sub pp_rv2cv { deref(shift, $top, "&"); }
sub pp_rv2hv { deref(shift, $top, "%"); }
sub pp_rv2sv { deref(shift, $top, "\$"); }
sub pp_rv2av { deref(shift, $top, "\@"); }
sub pp_rv2gv { deref(shift, $top, "*"); }

sub pp_gvsv {
    my $op = shift;
    my $gv;
    if ($Config{useithreads}) {
	$top = $pad[$op->padix];
	$top = UNKNOWN unless $top;
	$top->[1] = '$';
    }
    else {
	$gv = $op->gv;
	$top = [$gv->STASH->NAME, '$', $gv->SAFENAME];
    }
    process($top, $op->private & OPpLVAL_INTRO ||
                  $op->private & OPpOUR_INTRO   ? "intro" : "used");
}

sub pp_gv {
    my $op = shift;
    my $gv;
    if ($Config{useithreads}) {
	$top = $pad[$op->padix];
	$top = UNKNOWN unless $top;
	$top->[1] = '*';
    }
    else {
	$gv = $op->gv;
	$top = [$gv->STASH->NAME, "*", $gv->SAFENAME];
    }
    process($top, $op->private & OPpLVAL_INTRO ? "intro" : "used");
}

sub pp_const {
    my $op = shift;
    my $sv = $op->sv;
    # constant could be in the pad (under useithreads)
    if ($$sv) {
	$top = [undef, "",
		(class($sv) ne "SPECIAL" && $sv->FLAGS & SVf_POK)
		? cstring($sv->PV) : undef];
    }
    else {
	$top = $pad[$op->targ];
	$top = UNKNOWN unless $top;
    }
}

sub pp_method {
    my $op = shift;
    $top = ["(method)", "->".$top->[1], $top->[2]];
}

sub pp_entersub {
    my $op = shift;
    if ($top->[1] eq "m") {
	process($top, "meth");
    } else {
	process($top, "subused");
    }
    $top = UNKNOWN;
}

#
# Stuff for cross referencing definitions of variables and subs
#

sub B::GV::xref {
    my $gv = shift;
    my $cv = $gv->CV;
    $file = $gv->FILE;
    # XXX: sometimes the "file" is a module.  Why?
    $line = $gv->LINE;
    if ($$cv) {
	#return if $done{$$cv}++;
	process([$gv->STASH->NAME, "&", $gv->NAME], "subdef");
	push(@todo, $cv);
    }
    my $form = $gv->FORM;
    if ($$form) {
	return if $done{$$form}++;
	process([$gv->STASH->NAME, "", $gv->NAME], "formdef");
    }
}

sub xref_definitions {
    my ($pack, %exclude);
    return if $nodefs;
    $subname = "(definitions)";
    foreach $pack (qw(B O AutoLoader DynaLoader XSLoader Config DB VMS
		      strict vars FileHandle Exporter Carp PerlIO::Layer
		      attributes utf8 warnings)) {
        $exclude{$pack."::"} = 1;
    }
    no strict qw(vars refs);
    walksymtable(\%{"main::"}, "xref", sub { !defined($exclude{$_[0]}) });
}

=head2 Functions

=over

=item C<rebuild()>

Rebuild the Xref database.

=cut

sub rebuild {
    %call = (); %callby = (); %def = (); %package_sub = ();
    %var_def = (); %var_use = ();
    %module_files = (); %file_modules = ();
    xref_definitions;
    xref_main;
    1;
}

=item C<forget(@funcs)>

Forget that C<@funcs> were defined.  XXX: this B<really> needs to take
packages into account.

=cut

sub unmention(%$$) {
    my ($h, $K, $V) = @_;
    while (my ($k, $v) = each %$h) {
	$h->{$k} = [grep { $_->{$K} ne $V } @$v];
    }
}

sub forget {
    for my $obj (@_) {
	delete $def{$obj};
	delete $callby{$obj};
	unmention %call, 'sub', $obj;
	unmention %package_sub, 'sub', $obj;
	unmention %var_use, 'sub', $obj;
	unmention %var_def, 'sub', $obj;
    }
}

=item C<redefined(@funcs)>

Recompute xref info for each of of C<@funcs>.

=cut

sub redefined {
    for my $obj (@_) {
	forget $obj;
	xref_object($obj);
    }
}

sub _ret_list {
    my $l = shift;
    my @r = map {
	[ @{$_}{qw(file line sub package)} ]
    } @$l;
    # Remove Exporter if we've got more than one answer, since it's
    # bogus.
    if (@r > 1) {
	@r = grep { $_->[0] !~ /Exporter.pm$/ } @r;
    }
    return wantarray ? @r : \@r;
}

=item C<callers($func)>

List callers of C<$func>.

=cut

sub callers {
    my $f = shift;
    return _ret_list $call{$f};
}

=item C<callees($func)>

List callees of C<$func>.

=cut

sub callees {
    my $f = shift;
    my @r = map {
	defs($_->{sub})
    } @{$callby{$f}};
    return wantarray ? @r : \@r;
}

=item C<defs($func)>

Find locations where C<$func> is defined.

=cut

sub defs {
    my $f = shift;
    $f =~ s/.*:://;
    return _ret_list $def{$f};
}

=item C<var_defs($var)>

Find locations where C<$var> is defined.

=cut

sub var_defs {
    my $v = shift;
    $v =~ s/.*:://;
    return _ret_list $var_def{$v};
}

=item C<var_uses($var)>

Find locations where C<$var> is used.

=cut

sub var_uses {
    my $v = shift;
    $v =~ s/.*:://;
    return _ret_list $var_use{$v};
}

=item C<package_subs($pack)>

Find subs in package C<$pack>.

=cut

sub package_subs {
    my $p = shift;
    return _ret_list $package_sub{$p};
}

=item C<mod_file($mod)>

Find file for module C<$mod>.

=cut

sub mod_files {
    my $m = shift;
    return sort keys %{$module_files{$m}}
	if exists $module_files{$m};
    return undef;
}

=item C<file_modules($file)>

List the modules defined in file C<$file>.

=cut

sub file_modules {
    my $f = shift;
    return sort keys %{$file_modules{$f}}
	if exists $file_modules{$f};
    return undef;
}

=item C<apropos($expr)>

Find subs matching C<$expr>.

=cut

sub _apropos {
    my ($h, $re) = @_;
    my @r = do {
	if($re) {
	    $re = qr/$re/;
	    sort grep /$re/, keys %$h;
	} else {
	    sort keys %$h;
	}
    };
    return wantarray ? @r : \@r;
}

sub apropos {
    _apropos \%def, @_;
}

=item C<var_apropos($expr)>

Find variables matching C<$expr>.

=cut

sub var_apropos {
    _apropos \%var_use, @_;
}

=item C<mod_apropos($expr)>

Find modules matching C<$expr>.

=cut

sub mod_apropos {
    _apropos \%module_files, @_;
}

=item C<file_apropos($expr)>

Find modules matching C<$expr>.

=cut

sub file_apropos {
    _apropos \%file_modules, @_;
}

1;

__END__

=back

=head1 EXPORTS

Nothing by default, but all sub and variable described above can be
imported.  L<Devel::Xref> also defines the tags C<:most> for the
above-listed functions, and C<:all> for those and the variables as
well.

=head1 BUGS

See L<B::Xref>.  Also, we currently ignore module names when looking
up a sub by name.  Finally, there is some evil in the way we guess
file and line numbers, both of which should be done more cleanly and
effectively.

=head1 AUTHOR

L<B::Xref> by Malcolm Beattie, m(angl|odifi)ed by Sean O'Rourke
(seano@cpan.org).

=cut

