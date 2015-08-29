use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );
use FFI::Echidna;

package App::h2ffi {

  use FFI::Echidna::OO qw( Moose::Util::TypeConstraints );

  with 'MooseX::Getopt';

  has perl_package_name => (
    is      => 'ro',
    lazy    => 1,
    isa     => Str,
    default => sub ($self) {
      my @package = map { ucfirst $_ } split /\//, $self->_header;
      $package[-1] =~ s/\.(c|cpp|cxx|h|hpp|hxx)$//;
      join '::', @package;
    },
  );
  
  has perl_minimum_version => (
    is      => 'ro',
    isa     => Str,
    default => '5.008001',
  );
  
  has l => (
    is => 'ro',
    isa => Str,
  );
  
  has _header => (
    is      => 'ro',
    isa     => Str,
    lazy    => 1,
    default => sub ($self) {
      unless($self->extra_argv->@* == 1) {
        die "must provide exactly one header file to translate";
      }
      $self->extra_argv->[0];
    },
  );
  
  has _header_file => (
    is      => 'ro',
    isa     => HeaderFile,
    lazy    => 1,
    coerce  => 1,
    default => sub ($self) {
      $self->_header ne 'sys' ? $self->_header : FFI::Echidna::ClangWrapper->_standard_headers_example;
    },
  );
  
  has template_engine => (
    is      => 'ro',
    default => 'TT',
  );
  
  has _tt => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $class = "FFI::Echidna::Template::" . $self->template_engine;
      eval qq{ use $class; 1 }; die $@ if $@;
      $class->new;
    },
  );
  
  has _clang_wrapper_flags => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      { cpp_flags => [
        (map { "-I$_" } $self->I->@*),
        (map { "-D$_" } $self->D->@*),
      ]};
    },
  );
  
  sub _new_clang_model ($self, $header) {
    FFI::Echidna::ClangModel->new($header, clang => $self->_clang_wrapper_flags);
  }
  
  sub _new_clang_wrapper ($self) {
    FFI::Echidna::ClangWrapper->new($self->_clang_wrapper_flags->%*);
  }
  
  has _model => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $clang = $self->_new_clang_model($self->_header);
      my $model = App::h2ffi::Model->new(
        app => $self,
      );
      $clang->append_to_model($model);
      $model;
    },
  );

  foreach my $type (qw( constant typedef function )) {
    has "filter_$type" => (
      is      => 'ro',
      lazy    => 1,
      coerce  => 1,
      isa     => RegexpRef,
      default => sub { qr{} },
    );
  }
  
  has I => (
    is      => 'ro',
    isa     => DirList,
    lazy    => 1,
    coerce  => 1,
    default => sub { [] },
  );
  
  has D => (
    is      => 'ro',
    isa     => StrList,
    lazy    => 1,
    default => sub { [] },
  );
  
  has debug => (
    is      => 'ro',
    isa     => subtype( 'Str', where { /^(h|ast|module)$/n } ),
    lazy    => 1,
    default => 'module',
  );
  
  sub main ($class, @args) {
    local @ARGV = @args;
    my $app = App::h2ffi->new_with_options;
    $app->run;
    return 0;
  }
  
  sub run ($self) {
    if($self->debug eq 'module') {
      $self->_tt->process('default.pm.tt', { model => $self->_model } );
    } elsif($self->debug eq 'h') {
      say $self
        ->_new_clang_wrapper
        ->cpp_out($self->_header_file);
    } elsif($self->debug eq 'ast') {
      say $self
        ->_new_clang_wrapper
        ->ast_out($self->_header_file);
    }
  }

  __PACKAGE__->meta->make_immutable;

  package App::h2ffi::Model {
  
    use FFI::Echidna::OO;
    
    extends 'FFI::Echidna::PerlModuleModel';
    
    has app => (
      is       => 'ro',
      isa      => 'App::h2ffi',
      required => 1,
    );

    foreach my $attr (qw( perl_package_name perl_minimum_version )) {
      has "+$attr" => ( default => sub ($self) { $self->app->$attr } );
    }

    foreach my $attr (qw( constant typedef function )) {
      my $method = "filter_$attr";
      has "+string_filter_$attr" => ( default => sub ($self) { $self->app->$method } );
    }

    has '+system_model' => (
      default => sub ($self) {
        my $model = FFI::Echidna::ModuleModel->new;
        $self
          ->app
          ->_new_clang_model(FFI::Echidna::ClangWrapper->_standard_headers_example)
          ->append_to_model($model);
        $model;
      },
    );
    
    has '+libname' => (
      default => sub ($self) {
        $self->app->l,
      },
    );

    __PACKAGE__->meta->make_immutable;
  }
}

1;

__END__

# ABSTRACT: Convert C header file to a Perl FFI extension

=head1 SYNOPSIS

 h2ffi foo.h

=head1 DESCRIPTION

As with h2xs for XS, This program is intended to streamline the process
of starting a new extension with L<FFI::Platypus>.

=head1 OPTIONS

=head2 --perl_package_name

The name of the Perl package for the generated module.  If not provided
a reasonable guess will be chosen based on the name of the header file.

=head2 --perl_minimum_version

The minimum supported version of Perl for your generated module.  This
should be at least 5.008001, as that is the minimum supported Perl for
L<FFI::Platypus>.  If it is higher, then the template may use more
recent Perl syntax features.

=head2 -l

The name of the library.  This is usually the dynamic library (.so or .dll)
without its "lib" prefix.  So OpenGL would be 'GL' and libarchive would be
'archive' (even though the latter is usually known with its lib prefix).

If this is not provided, then C<undef> will be used, which means that
L<FFI::Platypus> will search the current process for symbols.  This is
frequently useful when creating bindings to functions provided by the
standard C library.

=head2 --template_engine

Specify the template engine to use.  At the moment, only Template Toolkit (TT)
is supported.

=head2 --filter_constant

Specify a regular expression to filter constants.  The name of the
constant must match.

=head2 --filter_typedef

Specify a regular expression to filter typedefs.  The alias portion of the
typedef must match.

=head2 --filter_function

Specify a regular expression to filter functions.  The name of the
function must match.

=head2 -I

C include directories.

=head2 -D

C macro defines

=head1 CAVEATS

Requires clang.  Works fine with a Perl built with gcc, but you will need
clang in your path.

=head1 SEE ALSO

=over 4

=item L<FFI::Platypus>

Module for writing Perl extensions using FFI.

=item L<FFI::Echidna>

Platypus' scrappy little brother.  Provides the machinery for this program.

=back

=cut
