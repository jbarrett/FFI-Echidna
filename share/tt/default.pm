package [% pm.package_name %];

use strict;
use warnings;
use [% pm.minimum_perl %];
use FFI::Platypus ();
use FFI::CheckLib ();

my $ffi = FFI::Platypus->new(
  FFI::CheckLib::find_lib_or_die(
    lib => '[% pm.libname %]'
  )
);

1;
