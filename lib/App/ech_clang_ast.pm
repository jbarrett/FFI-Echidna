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
    default => sub ($self) {
      $self->cpp ? 'clang++' : 'clang',
    },
  );
  
  has e => (
    is  => 'ro',
    isa => 'Str',
  );
  
  has cpp => (
    is      => 'ro',
    isa     => 'Bool',
    default => 0,
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
      if($self->e) {
        my $header = FFI::Echidna::FS->tempfile("ech_clang_ast_XXXX", SUFFIX => '.h');
        $header->spew($self->e);
        return $header;
      }
    
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

  sub translate {
    my($self, $ast) = @_;
    my @ret;
    foreach my $node (@{ $ast }) {
      if(ref $node eq 'ARRAY') {
        push @ret, $self->translate($node);
      } else {
        my $node2 = FFI::Echidna::ClangAstNode->new($node);
        my $str = $node2->type;
        $str .= " " . $node2->data if $node2->data;
        push @ret, $str;
      }
    }
    \@ret;
  }
  
  sub run ($self) {
    print Dump($self->translate($self->_cl->ast_list($self->_header)));
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

=head2 --cpp

The source header file is C++ instead of C.  This is the same as C<--clang clang++>.

=head2 -e

Parse C / C++ code from the command line instead of from a file.

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
