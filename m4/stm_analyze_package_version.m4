# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 3

# FIXME: Document this macro.

AC_DEFUN([StM_ANALYZE_PACKAGE_VERSION],[
   AC_REQUIRE([AC_PROG_SED])
   VERSION_MAJOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | \
      ${SED} -e 's/^\([[0-9]][[0-9]]*\).*/\1/'`
   VERSION_MINOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | \
      ${SED} -e 's/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
   VERSION_PATCH=`AS_ECHO(["${PACKAGE_VERSION}"]) | \
      ${SED} -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
   VERSION_EXTRA=`AS_ECHO(["${PACKAGE_VERSION}"]) | \
      ${SED} -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\(.*\)/\1/' \
             -e 's/^_//'`
   VERSION_EXTRA_SHORT=`AS_ECHO(["${VERSION_EXTRA}"]) | \
      ${SED} -e 's/alpha/a/' -e 's/beta/b/'`
   PACKAGE_VERSION_SHORT=`AS_ECHO(["${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_PATCH}${VERSION_EXTRA_SHORT}"])`
   AC_SUBST([VERSION_MAJOR])
   AC_SUBST([VERSION_MINOR])
   AC_SUBST([VERSION_PATCH])
   AC_SUBST([VERSION_EXTRA])
   AC_SUBST([VERSION_EXTRA_SHORT])
   AC_SUBST([PACKAGE_VERSION_SHORT])
])

