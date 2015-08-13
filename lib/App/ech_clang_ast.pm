use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );
use FFI::Echidna;

package App::ech_clang_ast {

  use YAML::XS qw( Dump );
  use FFI::Echidna::OO qw( MooseX::Types::Path::Class );

  with 'MooseX::Getopt';

  has clang => (
    is      => 'ro',
    isa     => 'Str',
    lazy    => 1,
    default => 'clang',
  );
  
  has _cl => (
    is      => 'ro',
    isa     => 'FFI::Echidna::ClangWrapper',
    lazy    => 1,
    default => sub ($self) {
      FFI::Echidna::ClangWrapper->new( clang_path => $self->clang );
    },
  );

  has _header => (
    is      => 'ro',
    isa     => 'Path::Class::File',
    coerce  => 1,
    lazy    => 1,
    default => sub ($self) {
      if($self->extra_argv->@* == 1) {
        my $header = $self->extra_argv->[0];
        unless(-r $header) {
          my $tmp = FFI::Echidna::FS->tempfile("ech_clang_ast_XXXX", SUFFIX => '.h');
          $tmp->spew("#include <$header>\n");
          $header = $tmp;
        }
        return $header;
      } else {
        die "must provide exactly one header file to translate";
      }
    },
  );
  
  sub main ($class, @args) {
    local @ARGV = @args;
    my $app = App::ech_clang_ast->new_with_options;
    $app->run;
    return 0;
  }
  
  sub run ($self) {
    print Dump($self->_cl->ast_list($self->_header));
  }

  __PACKAGE__->meta->make_immutable;

}

1;

__END__

# ABSTRACT: Dump the Clang AST of a header file

=head1 SYNOPSIS

 ech_clang_ast foo.h

=head1 DESCRIPTION

This program dumps the clang ast into yaml format, which may be useful
in debugging L<h2ffi> and L<FFI::Echidna>.

=head1 OPTIONS

=head2 --clang

Specify the clang executable to use.  Usually this is C<clang> or C<clang++>

=head1 SEE ALSO

=over 4

=item L<h2ffi>

Generate a Perl FFI extension from a C header file.

=item L<FFI::Platypus>

Module for writing Perl extensions using FFI.

=item L<FFI::Echidna>

Platypus' scrappy little brother.  Provides the machinery for this program.

=back

=cut
