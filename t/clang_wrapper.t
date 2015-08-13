use strict;
use warnings;
use 5.020;
use Test::More tests => 3;
use FFI::Echidna;
use YAML::XS qw( Dump );

my $clang = FFI::Echidna::ClangWrapper->new;

subtest version => sub {
  isa_ok $clang, 'FFI::Echidna::ClangWrapper';
  ok $clang->version, "version = @{[$clang->version]}";
};

subtest macros => sub {
  plan tests => 2;

  my $header = FFI::Echidna::FS->tempfile("test_XXXX", SUFFIX => '.h');
  
  $header->spew("#define FOO 42\n" .
                "#define BAR 47\n" .
                "#define BAZ foo(a,b,c)\n");
  
  
  subtest "all_macros" => sub {
    plan tests => 3;
    my $macros = $clang->all_macros($header);
    is $macros->{FOO}, 42, "FOO=42";
    is $macros->{BAR}, 47, "BAR=47";
    is $macros->{BAZ}, 'foo(a,b,c)', "BAZ=foo(a,b,c)";
  };

  subtest "macros" => sub {
    plan tests => 3;
    my $macros = $clang->macros($header);
    is $macros->{FOO}, 42, "FOO=42";
    is $macros->{BAR}, 47, "BAR=47";
    is $macros->{BAZ}, undef, "BAZ=undef";
  };

};

subtest ast => sub {
  plan tests => 2;

  my $header = FFI::Echidna::FS->tempfile("test_XXXX", SUFFIX => '.h');
  
  $header->spew("typedef int foo_t;" .
                "void foo(int a, int b, int c);");
  
  subtest ast_list => sub {
    plan tests => 1;
    my $ast = $clang->ast_list($header);
    is ref $ast, 'ARRAY', 'returns an array ref';
    note Dump($ast);
  };
  
  subtest ast => sub {
    plan tests => 1;
    my $ast = $clang->ast($header);
    isa_ok $ast, 'FFI::Echidna::ClangAstNode';
    note Dump($ast);
  };

};
