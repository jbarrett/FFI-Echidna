use strict;
use warnings;
use 5.020;
use Test::More tests => 6;

use_ok 'App::h2ffi';
use_ok 'App::ech_clang_ast';
use_ok 'FFI::Echidna';
use_ok 'FFI::Echidna::Type';

package Foo { Test::More::use_ok 'FFI::Echidna::OO' }
package Bar { Test::More::use_ok 'FFI::Echidna::OO::Role' }
