use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package App::h2ffi {

  use Moose;
  with 'MooseX::Getopt';
  no warnings 'experimental::signatures';
  use namespace::autoclean;

  with 'MooseX::Getopt';

  sub main ($class, @args) {
    local @ARGV = @args;
    
    my $app = App::h2ffi->new_with_options;
    
  }

  __PACKAGE__->meta->make_immutable;
}

1;
