use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package FFI::Echidna::OO {
  
  # ABSTRACT: OO for FFI::Echidna
  
  use Import::Into;
  use constant moose_class => 'Moose';
    
  # in principle, I agree with the idea of Moose turning on
  # warnings.  In principal I agree with warning about
  # experimental features.  I disagree with turning on
  # warnings that I explicitly have turned off.  Since I've
  # decided to use experimental features (postderef
  # is not experimental anymore in 5.24, and I find it
  # unlikely that signatures will removed) we define our
  # own strictures and warnings and override Moose's
  # (usually reasonable) preferences.
    
  # also, add namespace::autoclean.  Wow there is a lot
  # of boiler plate in order to do stuff in moose.  Note
  # that this means that the use FFI::Echidna::OO should
  # be the LAST use statement before the actual guts of
  # the class.
    
  sub import ($class, @modules) {
    my($caller, $caller_file) = caller;
      
    my $pm = "$caller.pm";
    $pm =~ s{::}{/}g;
    $INC{$pm} //= $caller_file;
    no warnings 'uninitialized';
    my $old = ${^WARNING_BITS};
    unshift @modules, $class->moose_class;
    push @modules, 'FFI::Echidna::Type';
    push @modules, 'namespace::autoclean';
    while(@modules) {
      my $module = shift @modules;
      my $pm = "$module.pm";
      $pm =~ s{::}{/}g;
      require $pm;
      my @args = ref $modules[0] eq 'ARRAY' ? (shift @modules)->@* : ();
      $module->import::into($caller, @args);
    }
    ${^WARNING_BITS} = $old;
    
    return;
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
