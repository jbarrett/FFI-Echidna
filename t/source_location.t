use strict;
use warnings;
use 5.020;
use Test::More tests => 5;
use FFI::Echidna;

my $location = FFI::Echidna::SourceLocation->new(
  filename => '/foo/bar/baz.c',
  line     => 400,
  column   => 42,
);

isa_ok $location, 'FFI::Echidna::SourceLocation';
isa_ok $location->filename, 'Path::Class::File';
is  $location->line,   400, 'line   = 400';
is  $location->column,  42, 'column = 42';
is  $location->to_string, '/foo/bar/baz.c:400:42', 'to_string';
