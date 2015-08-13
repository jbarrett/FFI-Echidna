# FFI::Echidna [![Build Status](https://secure.travis-ci.org/plicease/FFI-Echidna.png)](http://travis-ci.org/plicease/FFI-Echidna)

Developer tools for FFI

# SYNOPSIS

    % h2ffi \
      --perl_package_name OpenGL::FFI::Mesa::GL \
      --libname GL \
      --filter_constant ^GL_ \
      --filter_typedef ^GL \
      --filter_function ^gl \
      GL/gl.h

# DESCRIPTION

The goal of [FFI::Echidna](https://metacpan.org/pod/FFI::Echidna) is to provide tools for writing FFI
modules in Perl using [FFI::Platypus](https://metacpan.org/pod/FFI::Platypus) (and in the future possibly
other FFI tool sets and languages).  I expect the first tangible
and public interface to be [h2ffi](https://metacpan.org/pod/h2ffi), which will generate a Perl
extension to an existing library using its C header files in the
same vein as [h2xs](https://metacpan.org/pod/h2xs).  In the longer term I'd like to add tools
to interface with other languages (both on the DLL side and on the
scripting/VM side).

I intend on using the [FFI::Echidna](https://metacpan.org/pod/FFI::Echidna) API in order to build
[OpenGL::FFI](https://metacpan.org/pod/OpenGL::FFI), which will support a number of different OpenGL
implementations, each with subtle incompatibilities, and hundreds
of functions and constants.  At the moment, you should consider
the API to be quite unstable (not to mention undocumented).  If
that doesn't deter you, you should feel free to try this out.  A
good place to start, is perhaps [OpenGL::FFI](https://metacpan.org/pod/OpenGL::FFI) itself which will
serve as a working example.

# SEE ALSO

- [h2ffi](https://metacpan.org/pod/h2ffi)
- [FFI::Platypus](https://metacpan.org/pod/FFI::Platypus)

# AUTHOR

Graham Ollis <plicease@cpan.org>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2015 by Graham Ollis.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
