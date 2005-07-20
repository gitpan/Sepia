=head1 C<Emacs::Buffer> -- tied buffer interface

This is a very preliminary tied interface to Emacs buffers as perl
scalars or file handles.  There are likely bugs, and definitely
infelicities.  For example, C<$buf =~ s/a/b/g> doesn't do what you
want it to, but instead stringifies C<$buf>, performs the
substitution, and assigns the string result to C<$buf>.  Also, doing
some things causes Emacs and perl to have a failure to communicate,
the only solution to which being to kill one or the other.

=cut

package Emacs::Buffer;
{
    package EL; use Emacs::Lisp;
    sub point;
    sub point_min;
    sub point_max;
    sub erase_buffer;
}
require Tie::Handle;

our @ISA = qw(Tie::Handle);

######################################################################
## Filehandle interface

sub TIEHANDLE
{
    my ($class, $buf) = @_;
    unless (ref $buf) {
        $buf = EL::get_buffer_create($buf);
    }
    EL::save_current_buffer {
        EL::set_buffer($buf);
        EL::goto_char(EL::point_min);
    };
    bless \$buf, $class;
}

sub READ
{
    my $self = shift;
    my $buf = \$_[0];
    my (undef, $len, $offset) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        my $pos = EL::point;
        my $maxpos = EL::point_max;
        if ($len > $maxpos - $pos) {
            $len = $maxpos - $pos;
        }
        my $str = EL::buffer_substring($pos, $pos + $len);
        substr($$buf, $offset) = $str;
    };
    $len;
}

sub READLINE
{
    my $self = shift;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        if (EL::point == EL::point_max) {
            return undef;
        }
        EL::beginning_of_line();
        my $beg = EL::point;
        EL::beginning_of_line(2);
        EL::buffer_substring($beg, EL::point);
    }
}

sub EOF
{
    my $self = shift;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::point == EL::point_max;
    }
}

sub WRITE
{
    my ($self, $buf, $len, $off) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::insert(substr($buf, $off, $len));
    };
    $len;
}

sub PRINT
{
    my $self = shift;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::insert("@_");
    };
}

# NOTE: Seeking is weird in that it doesn't update the cursor
# position.  I think this has something to do with the distinction
# between "point" and "window-point" (see the Elisp info pages).
sub SEEK
{
    my ($self, $n, $whence) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        my $pos;
        if ($whence == 0) {
            $pos = $n;
        } elsif ($whence == 1) {
            $pos = EL::point + $n;
        } else {
            $pos = EL::point_max + $n;
        }
        EL::goto_char(int($pos));
    }
}

sub TELL
{
    my $self = shift;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::point;
    }
}

######################################################################
## Scalar interface

*TIESCALAR = \&TIEHANDLE;

sub FETCH
{
    my $self = shift;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::buffer_substring(EL::point_min, EL::point_max);
    }
}

sub STORE
{
    my ($self, $val) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$self);
        EL::erase_buffer();
        EL::insert("$val");
    }
}

######################################################################
## Overload interface (doesn't work well with e.g. substr...)

sub dot
{
    my ($lhs, $rhs, $type) = @_;
    if (!defined $type) {
        EL::save_current_buffer {
            EL::set_buffer($$lhs);
            EL::goto_char(EL::point_max);
            if (ref($rhs) eq 'Emacs::Buffer') {
                EL::insert_buffer($rhs);
            } else {
                EL::insert("$rhs");
            }
        };
        $lhs;
    } else {
        ($lhs, $rhs) = ($rhs, $lhs) if $type;
        "$lhs$rhs";
    }
}

sub assign
{
    my ($lhs, $rhs) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$lhs);
        EL::erase_buffer();
        EL::insert("$rhs");
    };
    $lhs;
}

sub str
{
    my ($thing) = @_;
    EL::save_current_buffer {
        EL::set_buffer($$thing);
        EL::buffer_substring(EL::point_min, EL::point_max);
    };
}

use overload '.' => \&dot,
    '=' => \&assign,
    '""' => \&str;

sub new
{
    my ($class, $name)= @_;
    if (!ref $name) {
        $name = EL::get_buffer_create($name);
    }
    return bless \$name, $class;
}

1;
