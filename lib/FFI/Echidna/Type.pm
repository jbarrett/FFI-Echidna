use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package FFI::Echidna::Type {

  # ABSTRACT: Moose types for FFI::Echidna
  
  # note: considered using MooseX::Types for this,
  # but it actually seems to redistribute the
  # boilerplate, rather than reducing it so...
  
  use Moose::Util::TypeConstraints;
  use MooseX::Getopt ();
  use MooseX::Types::Path::Class ();
  use Import::Into;
  use namespace::autoclean;

  my @types = (
    subtype('FFI::Echidna::Type::RegexpRef'  => as 'RegexpRef'),
    subtype('FFI::Echidna::Type::DirList'    => as 'ArrayRef[Path::Class::Dir]'),
    subtype('FFI::Echidna::Type::FileList'   => as 'ArrayRef[Path::Class::File]'),
    subtype('FFI::Echidna::Type::StrList'    => as 'ArrayRef[Str]'),
    subtype('FFI::Echidna::Type::HeaderFile' => as 'Path::Class::File'),
    MooseX::Types::Path::Class::File->__type_constraint,
    MooseX::Types::Path::Class::Dir->__type_constraint,
    map { Moose::Util::TypeConstraints::get_type_constraint_registry->get_type_constraint($_) } qw(
      Int
      Str
      Bool
    ),
  );

  coerce 'FFI::Echidna::Type::RegexpRef'
  => from 'Str'
  => via { qr{$_} };
    
  coerce 'FFI::Echidna::Type::DirList'
  => from 'ArrayRef[Str]'
  => via { [map { Path::Class::Dir->new($_) } $_->@*] };

  coerce 'FFI::Echidna::Type::FileList'
  => from 'ArrayRef[Str]'
  => via { [map { Path::Class::File->new($_) } $_->@*] };
  
  use constant File => MooseX::Types::Path::Class::File->__type_constraint;
  
  coerce 'FFI::Echidna::Type::HeaderFile'
  => from 'Str',
  => via {
    -r File->coerce($_) ? $_ : do {
      my $tmp = FFI::Echidna::FS->tempfile('clang_model_XXXXXX', SUFFIX => '.h');
      $tmp->spew("#include <$_>");
      $tmp
    }
  };
    
  MooseX::Getopt::OptionTypeMap->add_option_type_to_map(
    'FFI::Echidna::Type::RegexpRef'  => '=s',
    'FFI::Echidna::Type::HeaderFile' => '=s',
  );

  sub import
  {
    my $caller = caller;
    foreach my $type (@types)
    {
      constant->import::into($caller, ($type->name =~ s{^.*::}{}r) => $type);
    }
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
