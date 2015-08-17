use strict;
use warnings;
use 5.020;
use Test::More tests => 1;
use FFI::Echidna;

note "INCLUDE=$_" foreach FFI::Echidna::ClangWrapper->new->include_paths->@*;

subtest 'can parse common headers' => sub {

  my @headers = qw( stddef.h stdint.h stdlib.h stdio.h );

  plan tests => scalar @headers;

  foreach my $header (@headers)
  {
    my $model = FFI::Echidna::ClangModel->new($header);
    my $cbc = eval { $model->cbc };
    my $error = $@;
    is $@, '', "parses play: $header";
  }

};
