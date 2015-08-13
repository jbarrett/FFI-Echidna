use strict;
use warnings;
use 5.020;
use FFI::Echidna;
use Test::More tests => 4;
use Test::Dir;
use Test::File;

subtest 'tempdir' => sub {
  plan tests => 2;
  my $tempdir = FFI::Echidna::FS->tempdir;
  isa_ok $tempdir, 'Path::Class::Dir';
  dir_exists_ok $tempdir;
};

subtest 'tempfile' => sub {
  plan tests => 7;
  my $tempfile = FFI::Echidna::FS->tempfile("foo_XXXX", SUFFIX => ".c");
  isa_ok $tempfile, 'Path::Class::File';
  file_exists_ok $tempfile;
  file_empty_ok $tempfile;
  $tempfile->spew("mycontent");  
  is scalar($tempfile->slurp), "mycontent", "read and write content";
  file_not_empty_ok $tempfile;  
  is $tempfile->parent, FFI::Echidna::FS->tempdir, "tempfile is in our tempdir";
  like $tempfile->basename, qr{^foo_....\.c$}, "filename pattern matches";
};

subtest 'sharedir' => sub {
  plan tests => 4;
  my $sharedir = FFI::Echidna::FS->sharedir;
  isa_ok $sharedir, 'Path::Class::Dir';
  dir_exists_ok $sharedir;
  
  my $default_template = $sharedir->file(qw( tt default.pm.tt ));
  file_exists_ok $default_template;
  file_not_empty_ok $default_template;
};

subtest 'homedir' => sub {
  plan tests => 2;
  my $homedir = FFI::Echidna::FS->homedir;
  isa_ok $homedir, 'Path::Class::Dir';
  dir_exists_ok $homedir;
};
