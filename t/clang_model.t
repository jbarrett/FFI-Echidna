use strict;
use warnings;
use 5.020;
use Test::More tests => 2;
use FFI::Echidna;

my $header = FFI::Echidna::FS->tempfile("test_XXXX", SUFFIX => '.h');

$header->spew(scalar do { local $/; <DATA> });

my $clang = FFI::Echidna::ClangModel->new($header);

subtest 'attributes' => sub {
  plan tests => 4;
  isa_ok $clang, 'FFI::Echidna::ClangModel';
  isa_ok $clang->ast, 'FFI::Echidna::ClangAstNode';
  isa_ok $clang->macros, 'HASH';
  isa_ok $clang->clang, 'FFI::Echidna::ClangWrapper';
};

subtest 'append_to_model' => sub {
  plan tests => 4;

  my $model = FFI::Echidna::ModuleModel->new;
  eval { $clang->append_to_model($model) };
  is $@, '', 'no die';
  
  is $model->_hash->{constants}->{FOO}->value, 42, 'FOO=42';
  is $model->_hash->{typedefs}->{foo_t}->type, 'int', 'foo_t=int';
  is $model->_hash->{functions}->{foo}->name, 'foo', 'foo=foo()';
};

__DATA__
#define FOO 42
typedef int foo_t;
void foo(int a, int b, int c );
