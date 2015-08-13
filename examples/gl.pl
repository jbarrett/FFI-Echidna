use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );
use FFI::Echidna;

package MyModel {

  use FFI::Echidna::OO;
  
  extends 'FFI::Echidna::ModuleModel';
  
  sub filter_constants ($self, $constant) {
    return unless $constant->name =~ /^GL_/;
    say    'CONSTANT ', $constant->name, ' = ', $constant->value;
    $constant;
  }
  
  sub filter_typedefs ($self, $typedef) {
    return unless $typedef->alias =~ /^GL/;
    say    'TYPEDEF  ', $typedef->alias, ' ', $typedef->ffi_platypus_type($self) || $typedef->real_type;
    $typedef;
  }
  
  sub filter_functions ($self, $function) {
    return unless $function->name =~ /^gl[A-Z]/;
    printf "FUNCTION %s %s(%s)\n", $function->return_type, $function->name, join(', ', map { $_->type } $function->arguments->@*);
    $function;
  }
  
  __PACKAGE__->meta->make_immutable;
}

my $clang;

foreach my $header (qw( GL/gl.h OpenGL/gl.h )) {
  say "trying $header";
  $clang = eval { FFI::Echidna::ClangModel->new($header) };
  last if $@ eq '';
}

die $@ if $@;

my $model = MyModel->new;

$clang->append_to_model($model);
