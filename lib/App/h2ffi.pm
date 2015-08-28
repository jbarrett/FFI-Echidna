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
  
  sub libname ($self) { $self->l }
  
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
  
  sub _new_clang_model ($self, $header) {
    my $clang = FFI::Echidna::ClangModel->new($header, clang => {
      cpp_flags => [
        (map { "-I$_" } $self->I->@*),
        (map { "-D$_" } $self->D->@*),
      ],
    });
  }
  
  has _model => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $clang = $self->_new_clang_model($self->_header);
      my $model = FFI::Echidna::PerlModuleModel->new(
        app => $self,
        string_filter_constant => $self->filter_constant,
        string_filter_typedef  => $self->filter_typedef,
        string_filter_function => $self->filter_function,
        perl_package_name      => $self->perl_package_name,
        system_model           => do {
          my $model = FFI::Echidna::ModuleModel->new;
          $self
            ->_new_clang_model(FFI::Echidna::ClangWrapper->_standard_headers_example)
            ->append_to_model($model);
          $model;
        },
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
  
  sub main ($class, @args) {
    local @ARGV = @args;
    my $app = App::h2ffi->new_with_options;
    $app->run;
    return 0;
  }
  
  sub run ($self) {
    $self->_tt->process('perl.pm.tt', { h2ffi => $self, model => $self->_model } );
  }

  __PACKAGE__->meta->make_immutable;

  package FFI::Echidna::PerlModuleModel {
  
    use FFI::Platypus;
    use FFI::Echidna::OO;
    use constant typedef_class  => 'FFI::Echidna::PerlModuleModel::Typedef';
    use constant constant_class => 'FFI::Echidna::PerlModuleModel::Constant';
    use constant function_class => 'FFI::Echidna::PerlModuleModel::Function';
    
    extends 'FFI::Echidna::ModuleModel';
    
    has system_model => (
      is      => 'ro',
      lazy    => 1,
      default => sub ($self) {
        my $model = FFI::Echidna::ModuleModel->new;
        FFI::Echidna::ClangModel
          ->new(FFI::Echidna::ClangWrapper->_standard_headers_example)
          ->append_to_model($model);
        $model;
      },
    );
  
    has perl_package_name => ( is => 'ro', isa => Str, required => 1 );
    has "string_filter_$_" => ( is => 'ro', isa => RegexpRef, default => sub { qr{} } ) for qw( constant typedef function );
    
    has ffi => ( is => 'ro', isa => 'FFI::Platypus', lazy => 1, default => sub { FFI::Platypus->new } );
    
    has platypus_types => (
      is      => 'ro',
      lazy    => 1,
      default => sub { {} },
    );
    
    sub filter_constants ($self, $c) {
      $c->name =~ $self->string_filter_constant ? $c : ();
    }
    
    sub filter_typedefs ($self, $t) {
      return unless $t->alias =~ $self->string_filter_typedef && !$self->system_model->lookup_typedef($t->alias);
      
      if($t->type eq 'void *') {
        $t->platypus_type('opaque');
      }
        
      if($t->type =~ /^(const\s+)char \*$/) {
        $t->platypus_type('string');
        push $t->todo->@*, "@{[ $t->type ]} is usually a string, but may be a pointer to char @{[ $t->alias ]}";
      }
      
      $t->platypus_type($self->platypus_types->{$t->platypus_type}) while $self->platypus_types->{$t->platypus_type};
    
      # The typedef is already defined as part of Platypus,
      # or by a previous typedef
      return if eval { $self->ffi->type_meta($t->alias); 1 };

      # parse function pointers      
      if($t->platypus_type =~ /^(const\s+)?(?<ret>[A-Za-z_][A-Za-z_0-9]*)\s+\(\*\)\s*\((?<args>.*?)\)$/) {
        
        my $ret  = $+{ret};
        my $args = $+{args};
        
                   # resolve the type, typedefs not supported when defining a closure
        my @args = map { $self->platypus_types->{$_} // $_ }
                   # any non void pointer is not supported natively
                   map { /\*/ ? do { push $t->todo->@*, "'$_' (non opaque pointer) not yet supported for closures";'opaque' } : $_ }
                   # special case, pointer to typedef'd void IS supported as opaque type
                   map { /^(.*?)\s*\*$/ && ($self->platypus_types->{$1}//'') eq 'void' ? 'opaque' : $_ }
                   # we don't care about const 
                   map { s/^const\s+//r }
                   # split on , and ignore the white space
                   split /\s*,\s*/, $args;
        $t->platypus_type('(' . join(', ', @args) . ')->' . $ret);
        
      }
          
      if(eval { $self->ffi->type($t->platypus_type => $t->alias); 1 }) {
        $self->platypus_types->{$t->alias} = $t->platypus_type;
      } else {
        # we aren't (yet?) smart enough to parse this type, so set it to opaque
        # and mark it as a todo
        push $t->todo->@*, "unable to automatically determine Platypus type for '@{[ $t->platypus_type ]}' (@{[ $t->alias ]})";
        $t->platypus_type('opaque');
      }
        
      return $t;
    }
    sub filter_functions ($self, $f) {
      if($f->name =~ $self->string_filter_function && !$self->system_model->lookup_function($f->name)) {
        return $f;
      } else {
        return;
      }
    }

    __PACKAGE__->meta->make_immutable;

    package FFI::Echidna::PerlModuleModel::Todo {
    
      use FFI::Echidna::OO::Role;
      
      has todo => (
        is      => 'ro',
        lazy    => 1,
        default => sub { [] },
      );      
    
    }
    
    package FFI::Echidna::PerlModuleModel::Typedef {
    
      use FFI::Echidna::OO;
      
      extends 'FFI::Echidna::ModuleModel::Typedef';
      with 'FFI::Echidna::PerlModuleModel::Todo';
      
      has platypus_type => (
        is      => 'rw',
        lazy    => 1,
        default => sub ($self) { $self->type },
      );
      
      sub perl_render ($self) {
        sprintf "'%s' => '%s'", $self->platypus_type, $self->alias;
      }
      
      __PACKAGE__->meta->make_immutable;
    
    }

    package FFI::Echidna::PerlModuleModel::Constant {
    
      use Data::Dumper qw( Dumper );
      use FFI::Echidna::OO;
      
      extends 'FFI::Echidna::ModuleModel::Constant';
      with 'FFI::Echidna::PerlModuleModel::Todo';
      
      sub perl_render ($self) {
        my $value = $self->value;
        do { no warnings; eval $value };
        $@ ? do { local $Data::Dumper::Terse = 1; Dumper($value) =~ s/\s*$//r } : $value;
      }
      
      __PACKAGE__->meta->make_immutable;
    
    }
    
    package FFI::Echidna::PerlModuleModel::Function {
    
      use FFI::Echidna::OO;
      
      extends 'FFI::Echidna::ModuleModel::Function';
      with 'FFI::Echidna::PerlModuleModel::Todo';
      
      __PACKAGE__->meta->make_immutable;
    
    }
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
