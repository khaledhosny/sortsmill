# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
#
# This file is free software; Khaled Hosny and Barry Schwartz give
# unlimited permission to copy and/or distribute it, with or without
# modifications, as long as this notice is preserved.

# serial 1

# StM_PROG_SORTSMILL_TIG([action-if-not-found])
# ---------------------------------------------
AC_DEFUN([StM_PROG_SORTSMILL_TIG],[
   AC_PROG_AWK
   AC_PROG_SED
   AC_PATH_PROGS([SORTSMILL_TIG],[sortsmill-tig],[])
   if test -z "${SORTSMILL_TIG}"; then
      m4_ifval([$1],[$1],[
         AC_MSG_ERROR([sortsmill-tig is needed but was not found in \$PATH
sortsmill-tig may be found at https://bitbucket.org/sortsmill/sortsmill-tig])
      ])
   else
      SORTSMILL_TIG_VERSION=`${SORTSMILL_TIG} --version | ${AWK} -- 'NR==1 {print $NF}'`
      SORTSMILL_TIG_VERSION_MAJOR=`AS_ECHO(["${SORTSMILL_TIG_VERSION}"]) | \
         ${SED} -e 's/^\([[0-9]][[0-9]]*\).*/\1/'`
      SORTSMILL_TIG_VERSION_MINOR=`AS_ECHO(["${SORTSMILL_TIG_VERSION}"]) | \
         ${SED} -e 's/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
      SORTSMILL_TIG_VERSION_PATCH=`AS_ECHO(["${SORTSMILL_TIG_VERSION}"]) | \
         ${SED} -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
      SORTSMILL_TIG_VERSION_EXTRA=`AS_ECHO(["${SORTSMILL_TIG_VERSION}"]) | \
         ${SED} -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\(.*\)/\1/' -e 's/^_//'`
      SORTSMILL_TIG_VERSION_EXTRA_SHORT=`AS_ECHO(["${SORTSMILL_TIG_VERSION_EXTRA}"]) | \
         ${SED} -e 's/alpha/a/' -e 's/beta/b/'`
      SORTSMILL_TIG_SHORT_VERSION="${SORTSMILL_TIG_VERSION_MAJOR}.${SORTSMILL_TIG_VERSION_MINOR}.${SORTSMILL_TIG_VERSION_PATCH}${SORTSMILL_TIG_VERSION_EXTRA_SHORT}"
      AC_SUBST([SORTSMILL_TIG])
      AC_SUBST([SORTSMILL_TIG_VERSION])
      AC_SUBST([SORTSMILL_TIG_VERSION_MAJOR])
      AC_SUBST([SORTSMILL_TIG_VERSION_MINOR])
      AC_SUBST([SORTSMILL_TIG_VERSION_PATCH])
      AC_SUBST([SORTSMILL_TIG_VERSION_EXTRA])
      AC_SUBST([SORTSMILL_TIG_VERSION_EXTRA_SHORT])
      AC_SUBST([SORTSMILL_TIG_SHORT_VERSION])
   fi
])
