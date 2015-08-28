use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package FFI::Echidna::OO::Role {
    
  # ABSTRACT: Roles for FFI::Echidna
  
  use constant moose_class => 'Moose::Role';
  sub import {
    goto &FFI::Echidna::OO::import;
  }
  
}
  
1;

__END__

=head1 DESCRIPTION

Private class for use by FFI::Platypus.

=head1 SEE ALSO

=over 4

=item L<h2ffi>

=item L<FFI::Platypus>

=back

=cut
