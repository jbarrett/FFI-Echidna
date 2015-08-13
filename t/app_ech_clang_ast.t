use strict;
use warnings;
use 5.020;
use Test::More tests => 1;
use Test::Script;

script_compiles('bin/ech_clang_ast');
