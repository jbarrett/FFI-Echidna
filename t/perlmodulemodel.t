use strict;
use warnings;
use 5.020;
use Test::More tests => 4;
use FFI::Echidna;

my $header = do { local $/; <DATA> };

my $clang = FFI::Echidna::ClangModel->new(\$header);
my $model = FFI::Echidna::PerlModuleModel->new(
  perl_package_name => 'Foo',
);

isa_ok $model, 'FFI::Echidna::ModuleModel';
$clang->append_to_model($model);

my $pm = '';

FFI::Echidna::Template::TT
  ->new
  ->process('default.pm.tt', { model => $model }, \$pm);

note $header;
note '====================';
note $pm;

is eval "return 'compiles';$pm", 'compiles', 'generated code compiles';
diag $@ if $@;

subtest 'secondary meta' => sub {
  plan tests => 3;
  is $model->perl_package_name, 'Foo', 'package Foo';
  is $model->perl_minimum_version, '5.008001', 'requires Perl 5.8.1 by default';
  is $model->libname, undef, 'no libname';
};

subtest 'core model' => sub {
  plan tests => 4;
  
  subtest 'constants' => sub {
    plan tests => 2;
    is $model->lookup_constant('FOO1')->value, 22, 'FOO1 = 22';
    is $model->lookup_constant('FOO1')->value, 22, 'BAR2 = 33';
  };

  subtest 'types' => sub {
    plan tests => 2;
    is $model->lookup_typedef('foo_t')->type, 'int', 'foo_t = int';
    is $model->lookup_typedef('bar_t')->type, 'long', 'foo_t = int';
  };
  
  subtest 'functions' => sub {
    plan tests => 4;
    is $model->lookup_function('baz1')->name, 'baz1', 'have baz1';
    is $model->lookup_function('baz2')->name, 'baz2', 'have baz2';
    is $model->lookup_function('baz3')->name, 'baz3', 'have baz3';
    is $model->lookup_function('baz4')->name, 'baz4', 'have baz4';
  };
  
  my @todos = $model->todos->@*;
  
  is scalar @todos, 0, 'No TODOs';
  diag $_ for @todos;
  
};

__DATA__
#define FOO1 22
#define BAR2 33

typedef int foo_t;
typedef long bar_t;

foo_t baz1(bar_t, bar_t);
void  baz2(int, int);
foo_t baz3(bar_t a, bar_t b);
void  baz4(int a, int b);
