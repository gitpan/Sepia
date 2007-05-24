package Sepia;

=head1 NAME

Sepia - Simple Emacs-Perl Interface

=head1 SYNOPSIS

From inside Emacs:

   M-x load-library RET sepia RET
   M-x sepia-init RET

At the prompt in the C<*perl-interaction*> buffer:

   main @> ,help

=cut

$VERSION = '0.73';
@ISA = qw(Exporter);

require Exporter;
use strict;
use Cwd 'abs_path';
use Scalar::Util 'looks_like_number';
use Module::Info;
use Text::Abbrev;
use Carp;
use B;

use vars qw($PS1 $dies $STOPDIE $STOPWARN %REPL %RK %REPL_DOC
            $PACKAGE $WANTARRAY $PRINTER $STRICT);

BEGIN {
    eval { require PadWalker; import PadWalker qw(peek_my) };
    if ($@) {
        *peek_my = sub { +{ } };
    }
    eval { require Lexical::Persistence; import Lexical::Persistence };
    if ($@) {
        *repl_strict = sub {
            print STDERR "Strict mode requires Lexical::Persistence.\n";
            0;
        };
    } else {
        *repl_strict = sub {
            my $x = as_boolean(shift, $STRICT);
            if ($x && !$STRICT) {
                $STRICT = new Lexical::Persistence;
            } elsif (!$x) {
                undef $STRICT;
            }
            0;
        };
    }
}

=head1 DESCRIPTION

Sepia is a set of features to make Emacs a better tool for Perl
development.  This package contains the Perl side of the
implementation, including all user-serviceable parts (for the
cross-referencing facility see L<Sepia::Xref>).

Though not intended to be used independent of the Emacs interface, the
Sepia module's functionality can be used through a rough procedural
interface.

=head2 C<@compls = completions($string [, $type])>

Find a list of completions for C<$string> with glob type $type.
Completion operates on word subparts separated by [:_], so
e.g. "S:m_w" completes to "Sepia::my_walksymtable".

=cut

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    return qr/.*/ if $re eq '';
    if (wantarray) {
        map {
            s/(?:^|(?<=[A-Za-z\d]))(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
            qr/^$_/
        } split /:+/, $re, -1;
    } else {
        if ($re !~ /[^\w\d_^:]/) {
            $re =~ s/(?<=[A-Za-z\d])(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$2+/g;
        }
        qr/$re/;
    }
}

sub _completions1
{
    no strict;
    print STDERR "_completions1(@_)\n";
    my $stash = shift;
    my $re = shift || '';
    $re = qr/$re/;
    if (@_ == 0 || !defined $_[0]) {
        map "$stash$_", grep /$re/, keys %$stash;
    } else {
        map {
            _completions1("$stash$_", @_);
        } grep /$re.*::$/, keys %$stash;
    };
}

sub _completions
{
    _completions1 '::', _apropos_re($_[0]);
}

my %sigil;
BEGIN {
    %sigil = qw(ARRAY @ SCALAR $ HASH %);
}

sub completions
{
    no strict;
    my ($str, $type, $infunc) = @_;
    my @ret = map { s/^:://; $_ } ($type ? do {
        (grep { defined *{$_}{$type} } _completions $str),
            (defined $infunc && defined *{$infunc}{CODE}) ? do {
                my ($apre) = _apropos_re($str);
                my $st = $sigil{$type};
                grep {
                    (my $tmp = $_) =~ s/^\Q$st//;
                    $tmp =~ /$apre/;
                } lexicals($infunc);
            } : ();
    } : do {
        grep {
            defined *{$_}{CODE} || defined *{$_}{IO}
                || (/::$/ && defined *{$_}{HASH});
        } _completions $str;
    });
    if (!@ret && $str !~ /[^\w\d]/) {
        ## Complete "simple" sequences as abbreviations, e.g.:
        ##   wtci -> Want_To_Complete_It, NOT
        ##        -> WaTCh_trIpe
        my $broad = join '.*', map "\\b$_", split '', $str;
        @ret = map { s/^:://; $_ } ($type ? do {
            (grep { defined *{$_}{$type} } _completions1 '::', qr/$broad/),
                (defined $infunc && defined *{$infunc}{CODE}) ? do {
                    my ($apre) = _apropos_re($str);
                    my $st = $sigil{$type};
                    grep {
                        (my $tmp = $_) =~ s/^\Q$st//;
                        $tmp =~ /$apre/;
                    } lexicals($infunc);
                } : ();
        } : do {
            grep {
                defined *{$_}{CODE} || defined *{$_}{IO}
                    || (/::$/ && defined *{$_}{HASH});
            } _completions1 '::', qr/$broad/;
        })
    }
    @ret;
}

=head2 C<@locs = location(@names)>

Return a list of [file, line, name] triples, one for each function
name in C<@names>.

=cut

sub location
{
    no strict;
    my @x= map {
        my $str = $_;
        if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
            if ($pfx) {
                warn "Sorry -- can't lookup variables.";
                [];
            } else {
                # XXX: svref_2object only seems to work with a package
                # tacked on, but that should probably be done
                # elsewhere...
                $name = 'main::'.$name unless $name =~ /::/;
                my $cv = B::svref_2object(\&{$name});
                if ($cv && defined($cv = $cv->START) && !$cv->isa('B::NULL')) {
                    my ($file, $line) = ($cv->file, $cv->line);
                    if ($file !~ /^\//) {
                        for (@INC) {
                            if (-f "$_/$file") {
                                $file = "$_/$file";
                                last;
                            }
                        }
                    }
                    my ($shortname) = $name =~ /^(?:.*::)([^:]+)$/;
                    [Cwd::abs_path($file), $line, $shortname || $name]
                } else {
#                    warn "Bad CV for $name: $cv";
                    [];
                }
            }
        } else {
            []
        }
    } @_;
    return @x;
}

=head2 C<@matches = apropos($name [, $is_regex])>

Search for function C<$name>, either in all packages or, if C<$name>
is qualified, only in one package.  If C<$is_regex> is true, the
non-package part of C<$name> is a regular expression.

=cut

sub my_walksymtable(&*)
{
    no strict;
    my ($f, $st) = @_;
    local *_walk = sub {
        local ($stash) = @_;
        &$f for keys %$stash;
        _walk("$stash$_") for grep /(?<!main)::$/, keys %$stash;
    };
    _walk($st);
}

sub apropos
{
    my ($it, $re, @types) = @_;
    my $stashp;
    if (@types) {
        $stashp = grep /STASH/, @types;
        @types = grep !/STASH/, @types;
    } else {
        @types = qw(CODE);
    }
    no strict;
    if ($it =~ /^(.*::)([^:]+)$/) {
        my ($stash, $name) = ($1, $2);
        if (!defined %$stash) {
            return;
        }
        if ($re) {
            my $name = qr/^$name/;
            map {
                "$stash$_"
            }
            grep {
                my $stashnm = "$stash$_";
                /$name/ &&
                    (($stashp && /::$/)
                     || scalar grep { defined *{$stashnm}{$_} } @types)
            } keys %$stash;
        } else {
            defined &$it ? $it : ();
        }
    } else {
        my @ret;
        my $findre = $re ? qr/$it/ : qr/^\Q$it\E$/;
        my_walksymtable {
            push @ret, "$stash$_" if /$findre/;
        } '::';
        map { s/^:*(?:main:+)*//;$_ } @ret;
    }
}

=head2 C<@names = mod_subs($pack)>

Find subs in package C<$pack>.

=cut

sub mod_subs
{
    no strict;
    my $p = shift;
    my $stash = \%{"$p\::"};
    if (defined $stash) {
        grep { defined &{"$p\::$_"} } keys %$stash;
    }
}

=head2 C<@decls = mod_decls($pack)>

Generate a list of declarations for all subroutines in package
C<$pack>.

=cut

sub mod_decls
{
    my $pack = shift;
    no strict 'refs';
    my @ret = map {
	my $sn = $_;
	my $proto = prototype(\&{"$pack\::$sn"});
	$proto = defined($proto) ? "($proto)" : '';
	"sub $sn $proto;\n";
    } mod_subs($pack);
    return wantarray ? @ret : join '', @ret;
}

=head2 C<$info = module_info($module, $type)>

Emacs-called function to get module information.

=cut

sub module_info($$)
{
    my ($m, $func) = @_;
    my $info;
    if (-f $m) {
	$info = Module::Info->new_from_file($m);
    } else {
	(my $file = $m) =~ s|::|/|g;
	$file .= '.pm';
	if (exists $INC{$file}) {
	    $info = Module::Info->new_from_loaded($m);
	} else {
	    $info = Module::Info->new_from_module($m);
	}
    }
    if ($info) {
        return $info->$func;
    }
}

=head2 C<$file = mod_file($mod)>

Find the likely file owner for module C<$mod>.

=cut

sub mod_file
{
    my $m = shift;
    $m =~ s/::/\//g;
    while ($m && !exists $INC{"$m.pm"}) {
        $m =~ s#(?:^|/)[^/]+$##;
    }
    $m ? $INC{"$m.pm"} : undef;
}

=head2 C<@mods = package_list>

Gather a list of all distributions on the system. XXX UNUSED

=cut

our $INST;
sub inst()
{
    unless ($INST) {
        eval 'require ExtUtils::Installed';
        $INST = new ExtUtils::Installed;
    }
    $INST;
}

sub package_list
{
    sort inst->modules;
}

=head2 C<@mods = module_list>

Gather a list of all packages (.pm files, really) installed on the
system, grouped by distribution. XXX UNUSED

=cut

sub module_list
{
    @_ = package_list unless @_;
    my $incre = join '|', map quotemeta, @INC;
    $incre = qr|(?:$incre)/|;
    my $inst = inst;
    map {
        [$_, sort map {
            s/$incre//; s|/|::|g;$_
        } grep /\.pm$/, $inst->files($_)]
    } @_;
}

=head2 C<@mods = doc_list>

Gather a list of all documented packages (.?pm files, really)
installed on the system, grouped by distribution. XXX UNUSED

=cut

sub doc_list
{
    @_ = package_list unless @_;
    my $inst = inst;
    map {
        [$_, sort map {
            s/.*man.\///; s|/|::|g;s/\..?pm//; $_
        } grep /\..pm$/, $inst->files($_)]
    } @_;
}

=head2 C<lexicals($subname)>

Return a list of C<$subname>'s lexical variables.  Note that this
includes all nested scopes -- I don't know if or how Perl
distinguishes inner blocks.

=cut

sub lexicals
{
    my $cv = B::svref_2object(\&{+shift});
    return unless $cv && ($cv = $cv->PADLIST);
    my ($names, $vals) = $cv->ARRAY;
    map {
        my $name = $_->PV; $name =~ s/\0.*$//; $name
    } grep B::class($_) ne 'SPECIAL', $names->ARRAY;
}

=head2 C<$lisp = tolisp($perl)>

Convert a Perl scalar to some ELisp equivalent.

=cut

sub tolisp($)
{
    my $thing = @_ == 1 ? shift : \@_;
    my $t = ref $thing;
    if (!$t) {
        if (!defined $thing) {
            'nil'
        } elsif (looks_like_number $thing) {
            ''.(0+$thing);
        } else {
            qq{"$thing"};
        }
    } elsif ($t eq 'GLOB') {
        (my $name = $$thing) =~ s/\*main:://;
        $name;
    } elsif ($t eq 'ARRAY') {
        '(' . join(' ', map { tolisp($_) } @$thing).')'
    } elsif ($t eq 'HASH') {
        '(' . join(' ', map {
            '(' . tolisp($_) . " . " . tolisp($thing->{$_}) . ')'
        } keys %$thing).')'
    } elsif ($t eq 'Regexp') {
        "'(regexp . \"" . quotemeta($thing) . '")';
#     } elsif ($t eq 'IO') {
    } else {
        qq{"$thing"};
    }
}

=head2 C<printer(\@res [, $iseval])>

Print C<@res> appropriately on the current filehandle.  If C<$iseval>
is true, use terse format.  Otherwise, use human-readable format,
which can use either L<Data::Dumper>, L<YAML>, or L<Data::Dump>.

=cut

sub print_dumper
{
    local $Data::Dumper::Deparse = 1;
    local $Data::Dumper::Indent = 0;
    no strict;
    eval {
        local $_ = Data::Dumper::Dumper(@res > 1 ? \@res : $res[0]);
        s/^\$VAR1 = //;
        s/;$//;
        $_;
    };
}

sub print_plain
{
    no strict;
    $::__ = "@res";
}

sub print_yaml
{
    no strict;
    eval { require YAML };
    if ($@) {
        print_dumper;
    } else {
        YAML::Dump(\@res);
    }
}

sub print_dump
{
    no strict;
    eval { require Data::Dump };
    if ($@) {
        print_dumper;
    } else {
        Data::Dump::dump;
    }
}

sub printer
{
    no strict;
    local *res = shift;
    my ($iseval, $wantarray) = @_;
    @__ = @res;
    my $str;
    if ($iseval) {
        $::__ = "@res";
    } elsif (@res == 1 && (ref $res[0]) =~ /^PDL/) {
        $::__ = "$res[0]";
    } else {
        $::__ = $PRINTER->();
    }
    if ($iseval) {
        print ';;;', length $::__, "\n$::__\n";
    } else {
        print "=> $::__\n";
    }
}

=head2 C<repl(\*FH)>

Execute a command interpreter on FH.  The prompt has a few bells and
whistles, including:

  * Obviously-incomplete lines are treated as multiline input (press
    'return' twice or 'C-c' to discard).

  * C<die> is overridden to enter a recursive interpreter at the point
    C<die> is called.  From within this interpreter, you can examine a
    backtrace by calling "bt", return from C<die> with "r EXPR", or
    go ahead and die by pressing Control-c.

Behavior is controlled in part through the following package-globals:

=over 4

=item C<$PACKAGE> -- evaluation package

=item C<$PRINTER> -- result printer (default: print_dumper)

=item C<$PS1> -- the default prompt

=item C<$STOPDIE> -- true to enter the inspector on C<die()>

=item C<$STOPWARN> -- true to enter the inspector on C<warn()>

=item C<$STRICT> -- whether 'use strict' is applied to input

=item C<$WANTARRAY> -- evaluation context

=item C<%REPL> -- maps shortcut names to handlers

=item C<%REPL_DOC> -- maps shortcut names to documentation

=back

=cut

BEGIN {
    no strict;
    $PS1 = "> ";
    $dies = 0;
    $STOPDIE = 1;
    $STOPWARN = 0;
    $PACKAGE = 'main';
    $WANTARRAY = 1;
    $PRINTER = \&Sepia::print_dumper;
    %REPL = (help => \&Sepia::repl_help,
             cd => \&Sepia::repl_chdir,
             methods => \&Sepia::repl_methods,
             package => \&Sepia::repl_package,
             who => \&Sepia::repl_who,
             wantarray => \&Sepia::repl_wantarray,
             format => \&Sepia::repl_format,
             strict => \&Sepia::repl_strict,
             quit => \&Sepia::repl_quit,
         );
    %REPL_DOC = (
        cd =>
    'cd DIR            Change directory to DIR',
        format =>
    'format [dumper|dump|yaml|plain]
                       Set output formatter (default: dumper)',
        help =>
    'help               Display this message',
        methods =>
    'methods X          List methods for reference or package X',
        package =>
    'package PACKAGE    Set evaluation package to PACKAGE',
        quit =>
    'quit               Quit the REPL',
        strict =>
    'strict [0|1]       Turn \'use strict\' mode on or off',
        wantarray =>
    'wantarray [0|1]    Set or toggle evaluation context',
        who =>
    'who PACKAGE        List variables and subs in PACKAGE',
    );
    %RK = abbrev keys %REPL;
}

sub prompt()
{
    "$PACKAGE ".($WANTARRAY ? '@' : '$').$PS1
}

sub Dump {
    eval {
        Data::Dumper->Dump([$_[0]], [$_[1]]);
    };
}

sub eval_in_env
{
    my ($expr, $env) = @_;
    local $::ENV = $env;
    my $str = '';
    for (keys %$env) {
        next unless /^([\$\@%])(.+)/;
        $str .= "local *$2 = \$::ENV->{'$_'}; ";
    }
    eval "do { no strict; $str $expr }";
}

sub debug_upeval
{
    my ($lev, $exp) = $_[0] =~ /^\s*(\d+)\s+(.*)/;
    print " <= $exp\n";
    (0, eval_in_env($exp, peek_my(0+$lev)));
}

sub debug_inspect
{
    local $_ = shift;
    for my $i (split) {
        my $sub = (caller $i)[3];
        next unless $sub;
        my $h = peek_my($i);
        print "[$i] $sub:\n";
        no strict;
        for (sort keys %$h) {
            local @res = $h->{$_};
            print "\t$_ = ", $PRINTER->(), "\n";
        }
    }
    0;
}

sub repl_help
{
    print "REPL commands (prefixed with ','):\n";
    for (sort keys %REPL) {
        print "    ",
            exists $REPL_DOC{$_} ? "$REPL_DOC{$_}\n": "$_    (undocumented)\n";
    }
    0;
}

sub repl_format
{
    my $t = shift;
    chomp $t;
    $t = 'dumper' if $t eq '';
    my %formats = abbrev qw(dumper dump yaml plain);
    if (exists $formats{$t}) {
        no strict;
        $PRINTER = \&{'print_'.$formats{$t}};
    } else {
        warn "No such format '$t' (dumper, dump, yaml, plain).\n";
    }
    0;
}

sub repl_chdir
{
    chomp(my $dir = shift);
    $dir =~ s/^~\//$ENV{HOME}\//;
    $dir =~ s/\$HOME/$ENV{HOME}/;
    if (-d $dir) {

        chdir $dir;
        my $ecmd = '(cd "'.Cwd::getcwd().'")';
        print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't chdir\n";
    }
    0;
}

sub who
{
    my ($pack, $re) = (shift =~ /^(\S+)(?:\s+(\S.*))?/);
    $re ||= '';
    $re = qr/$re/;
    no strict;
    sort grep /$re/, map {
        (defined %{$pack.'::'.$_} ? '%'.$_ : (),
         defined ${$pack.'::'.$_} ? '$'.$_ : (), # ?
         defined @{$pack.'::'.$_} ? '@'.$_ : (),
         defined &{$pack.'::'.$_} ? $_ : (),
     )
    } grep !/::$/ && !/^(?:_<|[^\w])/, keys %{$pack.'::'};
}


sub columnate
{
    my $len = 0;
    my $width = $ENV{COLUMNS} || 80;
    for (@_) {
        $len = length if $len < length;
    }
    my $nc = int($width / ($len+1)) || 1;
    my $nr = @_ / $nc + (@_ % $nc ? 1 : 0);
    my $fmt = ('%-'.($len+1).'s') x $nc . "\n";
    my @incs = map { $_ * $nr } 0..$nc-1;
    my $str = '';
    for my $r (0..$nr) {
        $str .= sprintf $fmt, map { $_ || '' } @_[map { $r + $_ } @incs];
    }
    $str
}

sub repl_who
{
    print columnate who @_;
    0;
}

sub methods
{
    my $pack = shift;
    no strict;
    (grep(defined &{"$pack\::$_"}, keys %{$pack.'::'}),
     defined @{$pack.'::ISA'} ? (map methods($_), @{$pack.'::ISA'}) : ());
}

sub repl_methods
{
    my $x = shift;
    $x =~ s/^\s+//;
    $x =~ s/\s+$//;
    if ($x =~ /^\$/) {
        $x = eval "ref $x";
        return 1 if $@;
    }
    Sepia::printer [methods $x];
    0;
}

sub as_boolean
{
    my ($val, $cur) = @_;
    $val =~ s/\s+//g;
    length($val) ? $val : !$cur;
}

sub repl_wantarray
{
    $WANTARRAY = as_boolean shift, $WANTARRAY;
    0;
}

sub repl_package
{
    chomp(my $p = shift);
    no strict;
    if (defined %{$p.'::'}) {
        $PACKAGE = $p;
#         my $ecmd = '(setq sepia-eval-package "'.$p.'")';
#         print ";;;###".length($ecmd)."\n$ecmd\n";
    } else {
        warn "Can't go to package $p -- doesn't exist!\n";
    }
    0;
}

sub repl_quit
{
    1;
}

sub debug_help
{
    print <<EOS;
Inspector commands (prefixed with ','):
    \\C-c        Pop one debugger level
    backtrace       show backtrace
    inspect N ...   inspect lexicals in frame(s) N ...
    eval N EXPR     evaluate EXPR in lexical environment of frame N
    return EXPR     return EXPR
    die/warn        keep on dying/warning
EOS
    0;
}

sub debug_backtrace
{
    Carp::cluck;0
}

sub debug_return
{
    (1, repl_eval(@_));
}

sub repl_eval
{
    my ($buf, $wantarray, $pkg) = @_;
    no strict;
    local $PACKAGE = $pkg || $PACKAGE;
    if ($STRICT) {
        if (!$WANTARRAY) {
            $buf = 'scalar($buf)';
        }
        my $ctx = join(',', keys %{$STRICT->get_context('_')});
        $ctx = $ctx ? "my ($ctx);" : '';
        $buf = eval "sub { package $PACKAGE; use strict; $ctx $buf }";
        if ($@) {
            print STDERR "ERROR\n$@\n";
            return;
        }
        $STRICT->call($buf);
    } else {
        $buf = "do { package $PACKAGE; no strict; $buf }";
        if ($WANTARRAY) {
            eval $buf;
        } else {
            scalar eval $buf;
        }
    }
}

sub repl
{
    my ($fh, $level) = @_;
    select((select($fh), $|=1)[0]);
    my $in;
    my $buf = '';
    my $sigged = 0;

    my $nextrepl = sub { $sigged = 1; };

    local *__;
    my $MSG = "('\\C-c' to exit, ',h' for help)";
    my %dhooks = (
                backtrace => \&Sepia::debug_backtrace,
                inspect => \&Sepia::debug_inspect,
                eval => \&Sepia::debug_upeval,
                return => \&Sepia::debug_return,
                help => \&Sepia::debug_help,
            );
    local *CORE::GLOBAL::die = sub {
        my @dieargs = @_;
        if ($STOPDIE) {
            local $dies = $dies+1;
            local $PS1 = "*$dies*> ";
            no strict;
            local %Sepia::REPL = (
                %dhooks, die => sub { local $Sepia::STOPDIE=0; die @dieargs });
            local %Sepia::RK = abbrev keys %Sepia::REPL;
            print "@_\nDied $MSG\n";
            return Sepia::repl($fh, 1);
        }
        CORE::die(@_);
    };

    local *CORE::GLOBAL::warn = sub {
        if ($STOPWARN) {
            local $dies = $dies+1;
            local $PS1 = "*$dies*> ";
            no strict;
            local %Sepia::REPL = (
                %dhooks, warn => sub { local $Sepia::STOPWARN=0; warn @dieargs });
            local %Sepia::RK = abbrev keys %Sepia::REPL;
            print "@_\nWarned $MSG\n";
            return Sepia::repl($fh, 1);
        }
        CORE::warn(@_);
    };
    print <<EOS;
Sepia version $Sepia::VERSION.
Press ",h" for help, or "^D" or ",q" to exit.
EOS
    print prompt;
    my @sigs = qw(INT TERM PIPE ALRM);
    local @SIG{@sigs};
    $SIG{$_} = $nextrepl for @sigs;
 repl: while (my $in = <$fh>) {
            if ($sigged) {
                $buf = '';
                $sigged = 0;
                print "\n", prompt;
                next repl;
            }
            $buf .= $in;
            my $iseval;
            if ($buf =~ /^<<(\d+)\n(.*)/) {
                $iseval = 1;
                my $len = $1;
                my $tmp;
                $buf = $2;
                while ($len && defined($tmp = read $fh, $buf, $len, length $buf)) {
                    $len -= $tmp;
                }
            }
            my (@res, @warn);
            local $SIG{__WARN__} = sub {
                push @warn, shift;
            };
            if ($buf =~ /^,(\S+)\s*(.*)/s) {
                ## Inspector shortcuts
                my $short = $1;
                if (exists $Sepia::RK{$short}) {
                    my $ret;
                    my $arg = $2;
                    chomp $arg;
                    ($ret, @res) = $Sepia::REPL{$Sepia::RK{$short}}->($arg, wantarray);
                    if ($ret) {
                        return wantarray ? @res : $res[0];
                    }
                } else {
                    if (grep /^$short/, keys %Sepia::REPL) {
                        print "Ambiguous shortcut '$short': ",
                            join(', ', sort grep /^$short/, keys %Sepia::REPL),
                                "\n";
                    } else {
                        print "Unrecognized shortcut '$short'\n";
                    }
                    $buf = '';
                    print prompt;
                    next repl;
                }
            } else {
                ## Ordinary eval
                @res = repl_eval $buf, wantarray;

                if ($@) {
                    if ($@ =~ /at EOF$/m) {
                        ## Possibly-incomplete line
                        if ($in eq "\n") {
                            print "Error:\n$@\n*** cancel ***\n", prompt;
                            $buf = '';
                        } else {
                            print ">> ";
                        }
                        next repl;
                    } else {
                        warn $@;
                        $buf = '';
                        Sepia::printer \@res, $iseval, wantarray if $iseval;
                    }
                }
            }
            if ($buf !~ /;$/) {
                ## Be quiet if it ends with a semicolon.
                Sepia::printer \@res, $iseval, wantarray;
            }
            $buf = '';
            if (@warn) {
                if ($iseval) {
                    my $tmp = "@warn";
                    print ';;;'.length($tmp)."\n$tmp\n";
                } else {
                    print "@warn\n";
                }
            }
            print prompt;
        }
}

sub perl_eval
{
    tolisp(repl_eval(shift));
}

1;
__END__

=head1 TODO

See the README file included with the distribution.

=head1 AUTHOR

Sean O'Rourke, E<lt>seano@cpan.orgE<gt>

Bug reports welcome, patches even more welcome.

=head1 COPYRIGHT

Copyright (C) 2005-2007 Sean O'Rourke.  All rights reserved, some
wrongs reversed.  This module is distributed under the same terms as
Perl itself.

=cut