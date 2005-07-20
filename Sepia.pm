package Sepia;
our $VERSION = '0.58';

require Exporter;
our @ISA = qw(Exporter);

use strict;
use Cwd 'abs_path';
# Hack to make Module::Info a soft requirement:
use Module::Info;
use B;

sub _apropos_re($)
{
    # Do that crazy multi-word identifier completion thing:
    my $re = shift;
    if ($re !~ /[^\w\d_^:]/) {
	$re =~ s/(?<=[A-Za-z\d])(([^A-Za-z\d])\2*)/[A-Za-z\\d]*$1+/g;
    }
    qr/$re/;
}

=item C<@compls = completions($string [, $package])>

Find a list of completions for C<$string>; if C<$string> has no
package prefixes, C<$package> may specify a package in which to look.
Completion operates on word subparts separated by [:_], so
e.g. "S:m_w" completes to "Sepia::my_walksymtable".

=cut

sub completions
{
    no strict;
    my ($str, $pack) = @_;
    if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
        my @nameparts = split /:+/, $name, 1000;
        if (@nameparts == 1 && $pack) {
            @nameparts = (split(/:+/, $pack), $name);
        }
        local *_completions = sub {
            no strict;
            my ($stash, $part, @rest) = @_;
            $part = join '[^_]*_', split /_/, $part, 1000;
            $part = _apropos_re($part);
            if (@rest) {
                map {
                    _completions("$stash$_", @rest)
                } grep /^$part.*\::$/, keys %$stash;
            } else {
                map { "$stash$_" } grep /^$part/, keys %$stash;
            }
        };

        my $type = ($pfx eq '$' ? 'SCALAR'
                    : $pfx eq '@' ? 'ARRAY'
                    : $pfx eq '&' ? 'CODE'
                    : $pfx eq '%' ? 'HASH'
                    : undef);
        map {
            s/^(?::*main)?::/$pfx/;$_
        } grep {
            !$type || defined(*{$_}{$type})
        } _completions('::', @nameparts);
    }
}

=item C<@locs = location(@names)>

Return a list of [file, line, name] triples, one for each function
name in C<@names>.

=cut

sub location
{
    no strict;
    map {
        my $str = $_;
        if (my ($pfx, $name) = $str =~ /^([\%\$\@]?)(.+)/) {
            if ($pfx) {
                print STDERR "Sorry -- can't lookup variables.";
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
#                     print STDERR "Bad CV for $name: $cv";
                    [];
                }
            }
        } else {
#             print STDERR "Name `$str' doesn't match.";
            []
        }
    } @_
}

=item C<@matches = apropos($name [, $is_regex])>

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
#         print STDERR "Searching for $findre...";
        my_walksymtable {
            push @ret, "$stash$_" if /$findre/;
        } '::';
        map { s/^:*(?:main:+)*//;$_ } @ret;
    }
}

=item C<@names = mod_subs($pack)>

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

=item C<@decls = mod_decls($pack)>

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

=item C<$info = module_info($module, $type)>

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

=item C<$file = mod_file($mod)>

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

=item C<lexicals($subname)>

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

######################################################################
## XXX: this is the only part that depends on Emacs:

{ package EL;
  use Emacs::Lisp;
  ## XXX: "submit a patch" to use (message ...) correctly -- message's
  ## first argument is actually a format, not a plain string.
  sub Emacs::Minibuffer::WRITE {
      my ($stream, $output, $length, $offset) = @_;
      Emacs::Lisp::message ('%s', substr ($output, $offset, $length));
      return ($length);
  }
}

=item C<$func = emacs_warner($bufname)>

Create a function that will insert its arguments into Emacs buffer
C<$bufname>.  Useful as a C<$SIG{__WARN__}> handler.

=cut

sub emacs_warner
{
    my $buf = shift;
    $buf = EL::get_buffer_create($buf);
    return sub {
        my $msg = "@_";         # can't be inside EL::save_current_buffer
        EL::save_current_buffer {
            EL::set_buffer($buf);
            EL::insert($msg);
        };
    };
}

1;
