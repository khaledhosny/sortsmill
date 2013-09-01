# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 1

# StM_LIB_ZLIB
# ------------
#
# Check for zlib, using AC_LIB_HAVE_LINKFLAGS from Gnulib.
#
# Modifies CPPFLAGS, sets HAVE_LIBZ, LIBZ, and LTLIBZ. See the Gnulib
# manual for more details.
#
AC_DEFUN([StM_LIB_ZLIB],[{ :
   AC_LANG_PUSH([C])
   AC_LIB_HAVE_LINKFLAGS([z],[],[@%:@include <zlib.h>],[
      z_stream strm;
      const char *zv = zlibVersion ();
      int i = deflateInit (&strm, Z_DEFAULT_COMPRESSION);
   ])
   AC_LANG_POP
}])

