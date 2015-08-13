use strict;
use warnings;
use 5.020;
use Test::More tests => 1;
use lib 'inc';
use Test::NoWarnSubTest;
use FFI::Echidna;

no_warn_subtest_ok {
  plan tests => 4;
  
  eval q{
    use strict;
    use warnings;
    use 5.020;
    use experimental qw( signatures postderef );
    
    package Foo {

      use FFI::Echidna::OO;
      
      has z => (is => 'ro');      
      
      sub foo ($x, $y) {
        $x->@*
      }
      
      __PACKAGE__->meta->make_immutable;
    }
  };
  
  is $@, '', 'no die';  
  can_ok 'Foo', 'foo';
  can_ok 'Foo', 'z';
  is('Foo'->can('has'), undef, 'Foo cannot has');
};
