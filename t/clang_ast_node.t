use strict;
use warnings;
use 5.020;
use Test::More tests => 4;
use FFI::Echidna;
use YAML::XS qw( Load Dump );
use experimental qw( signatures postderef );

my $root = FFI::Echidna::ClangAstNode->new(Load(do { local $/; <DATA> }));
isa_ok $root, 'FFI::Echidna::ClangAstNode';

#note Dump($root);

subtest typedecl => sub {
  plan tests => 7;
  my $ast = $root->children->[3];
  isa_ok $ast, 'FFI::Echidna::ClangAstNode';
  is $ast->type, 'TypedefDecl', 'type';
  is $ast->address, '0x46d2f60', 'address';
  is $ast->locations->[0]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:1", 'ast.location.0';
  is $ast->locations->[1]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:13", 'ast.location.1';
  is $ast->locations->[2]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:13", 'ast.location.2';
  is $ast->data, "foo_t 'int'", 'data';
};

subtest functiondecl => sub {
  plan tests => 6;
  my $ast = $root->children->[4];
  is $ast->type, 'FunctionDecl', 'type';
  is $ast->address, '0x46d3180', 'address';
  is $ast->locations->[0]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:19", 'ast.location.0';
  is $ast->locations->[1]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:47", 'ast.location.1';
  is $ast->locations->[2]->to_string, "/tmp/tuF5NslPr8/test_avH5.h:1:24", 'ast.location.2';
  is $ast->data, "foo 'void (int, int, int)'", 'data';
};

subtest paramvardecl => sub {
  plan tests => 3;

  my @children = $root->children->[4]->children->@*;

  foreach my $name (qw( a b c )) {
    subtest $name => sub {
      my $ast = shift @children;
      is $ast->type, 'ParmVarDecl', 'type';
      is $ast->data, "$name 'int'", 'data';
    };
  }
};

__DATA__
---
- TranslationUnitDecl 0x46d2650 <<invalid sloc>> <invalid sloc>
- TypedefDecl 0x46d2b50 <<invalid sloc>> <invalid sloc> implicit __int128_t '__int128' echidna_location()
- TypedefDecl 0x46d2bb0 <<invalid sloc>> <invalid sloc> implicit __uint128_t 'unsigned __int128' echidna_location()
- TypedefDecl 0x46d2f00 <<invalid sloc>> <invalid sloc> implicit __builtin_va_list '__va_list_tag [1]' echidna_location()
- TypedefDecl 0x46d2f60 </tmp/tuF5NslPr8/test_avH5.h:1:1, col:13> col:13 foo_t 'int' echidna_location(/tmp/tuF5NslPr8/test_avH5.h:1:1 /tmp/tuF5NslPr8/test_avH5.h:1:13 /tmp/tuF5NslPr8/test_avH5.h:1:13)
- - FunctionDecl 0x46d3180 <col:19, col:47> col:24 foo 'void (int, int, int)' echidna_location(/tmp/tuF5NslPr8/test_avH5.h:1:19 /tmp/tuF5NslPr8/test_avH5.h:1:47 /tmp/tuF5NslPr8/test_avH5.h:1:24)
  - ParmVarDecl 0x46d2fc0 <col:28, col:32> col:32 a 'int' echidna_location(/tmp/tuF5NslPr8/test_avH5.h:1:28 /tmp/tuF5NslPr8/test_avH5.h:1:32 /tmp/tuF5NslPr8/test_avH5.h:1:32)
  - ParmVarDecl 0x46d3030 <col:35, col:39> col:39 b 'int' echidna_location(/tmp/tuF5NslPr8/test_avH5.h:1:35 /tmp/tuF5NslPr8/test_avH5.h:1:39 /tmp/tuF5NslPr8/test_avH5.h:1:39)
  - ParmVarDecl 0x46d30a0 <col:42, col:46> col:46 c 'int' echidna_location(/tmp/tuF5NslPr8/test_avH5.h:1:42 /tmp/tuF5NslPr8/test_avH5.h:1:46 /tmp/tuF5NslPr8/test_avH5.h:1:46)
