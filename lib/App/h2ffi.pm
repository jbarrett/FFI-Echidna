use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );
use FFI::Echidna;

package App::h2ffi {

  use FFI::Echidna::OO;

  with 'MooseX::Getopt';

  has perl_package_name => (
    is      => 'ro',
    lazy    => 1,
    isa     => 'Str',
    default => sub ($self) {
      my @package = map { ucfirst $_ } split /\//, $self->_header;
      $package[-1] =~ s/\.(c|cpp|cxx|h|hpp|hxx)$//;
      join '::', @package;
    },
  );
  
  has perl_minimum_version => (
    is      => 'ro',
    isa     => 'Str',
    default => '5.008001',
  );
  
  has libname => (
    is => 'ro',
    isa => 'Str',
  );
  
  has _header => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      if($self->extra_argv->@* == 1) {
        return $self->extra_argv->[0];
      } else {
        die "must provide exactly one header file to translate";
      }
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
  
  has _model => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $clang = FFI::Echidna::ClangModel->new($self->_header);
      my $model = App::h2ffi::Model->new(app => $self);
      $clang->append_to_model($model);
      $model;
    },
  );

  foreach my $type (qw( constant typedef function )) {
    has "filter_$type" => (
      is      => 'ro',
      lazy    => 1,
      coerce  => 1,
      isa     => 'FFI::Echidna::Type::RegexpRef',
      default => sub {
        qr{},
      },
    );
  }
  
  sub main ($class, @args) {
    local @ARGV = @args;
    my $app = App::h2ffi->new_with_options;
    $app->run;
    return 0;
  }
  
  sub run ($self) {
    $self->_tt->process('default.pm.tt', { h2ffi => $self, model => $self->_model } );
  }

  __PACKAGE__->meta->make_immutable;

  package App::h2ffi::Model {
  
    use FFI::Echidna::OO;
    
    extends 'FFI::Echidna::ModuleModel';
    
    has app => ( is => 'ro', weak_ref => 1, isa => 'App::h2ffi' );
    
    sub filter_constants ($self, $c) {
      $c->name =~ $self->app->filter_constant ? $c : ();
    }
    sub filter_typedefs ($self, $t) {
      $t->alias =~ $self->app->filter_typedef ? $t : ();
    }
    sub filter_functions ($self, $f) {
      $f->name =~ $self->app->filter_function ? $f : ();
    }

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

=head2 --libname

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
