use strict;
use warnings;
use 5.020;
use experimental qw( signatures postderef );

package Test::NoWarnSubTest {

  use base qw( Test::Builder::Module Exporter );

  our @EXPORT = qw( no_warn_subtest_ok );

  sub no_warn_subtest_ok :prototype(&;$) {
    my($block, $testname) = @_;
    
    my @warnings;
    local $SIG{__WARN__} = sub ($warning) {
      push @warnings, $warning;
    };
    
    my $b = __PACKAGE__->builder;
    
    state $counter = 1;
    $testname //= "no warnings in block " . $counter++;
    
    $b->subtest($testname => sub {
      $b->plan(tests => 2);
      $b->subtest(block => sub {
        $block->();
      });
      $b->ok(@warnings == 0, "no warnings generated");
      $b->diag("warning: $_") for @warnings;
    });
  }
}

1;
