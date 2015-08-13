use strict;
use warnings;
use 5.020;
use Test::More tests => 2;
use FFI::Echidna;

my $clang = FFI::Echidna::ClangWrapper->new;
isa_ok $clang, 'FFI::Echidna::ClangWrapper';
ok $clang->version, "version = @{[$clang->version]}";
