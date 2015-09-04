use strict;
use warnings;
use 5.020;
use Test::More tests => 1;
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

__DATA__
typedef int foo_t;
typedef long bar_t;

foo_t baz1(bar_t, bar_t);
void  baz2(int, int);
foo_t baz3(bar_t a, bar_t b);
void  baz4(int a, int b);
