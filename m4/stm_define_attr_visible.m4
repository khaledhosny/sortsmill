# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 2

# StM_DEFINE_ATTR_VISIBLE([c_preprocessor_variable = `StM_ATTR_VISIBLE'])
# -----------------------------------------------------------------------
#
# For use with Gnulib. Adds a config.h define for the visibility
# attribute as it is implemented in GCC >= 4.0.
#
# See
# http://www.gnu.org/software/gnulib/manual/gnulib.html#Exported-Symbols-of-Shared-Libraries
# for advice on how to use the visibility attribute.
#
AC_DEFUN([StM_DEFINE_ATTR_VISIBLE],[{ :;
   m4_ifval([$1],[$1],[StM_ATTR_VISIBLE])=''
   test x"${HAVE_VISIBILITY}" = x1 && \
      m4_ifval([$1],[$1],[StM_ATTR_VISIBLE])='__attribute__((__visibility__("default")))'
   AC_DEFINE_UNQUOTED(m4_ifval([$1],[$1],[StM_ATTR_VISIBLE]),
      [${m4_ifval([$1],[$1],[StM_ATTR_VISIBLE])}],
      [Define to the attribute for a visible symbol in a shared library.])
}])
