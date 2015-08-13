use strict;
use warnings;
use 5.020;
use Test::More tests => 2;
use FFI::Echidna;
use Capture::Tiny qw( capture );

subtest normal => sub {
  plan tests => 9;

  my $r = FFI::Echidna::ProcessCaptureResult->new($^X, -E => 'print "mystdout"; print STDERR "mystderr"');
  
  is $r->command, $^X, "command method";
  is $r->stdout ,  'mystdout', 'stdout';
  is $r->stderr ,  'mystderr', 'stderr';

  is $r->signal,    0, 'no signal';
  is $r->exit,      0, 'no exit';
  is $r->error,    '', 'no error';
  isa_ok $r->die_on_error, 'FFI::Echidna::ProcessCaptureResult';
  
  is $r->errno, '', 'errno';
  is $r->child_error, 0, 'child_error';

};

subtest exitfail => sub {
  plan tests => 9;

  my $r = FFI::Echidna::ProcessCaptureResult->new($^X, -E => 'print "mystdout"; print STDERR "mystderr"; exit 3');

  is $r->command, $^X, "command method";
  is $r->stdout ,  'mystdout', 'stdout';
  is $r->stderr ,  'mystderr', 'stderr';

  is $r->signal,    0, 'no signal';
  is $r->exit,      3, 'exit 3';
  isnt $r->error,  '', "error: @{[$r->error]}";
  
  eval { capture { $r->die_on_error } };
  
  isnt $@, '', "die_on_error $@";
  
  is $r->errno, '', 'errno';
  isnt $r->child_error, 0, 'child_error';
};

