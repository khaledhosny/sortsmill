# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 6

# StM_LIB_ATOMIC_OPS
# ------------------
#
# Check for atomic_ops.
#
# Users may set either or both of the precious variables
# ATOMIC_OPS_CFLAGS and ATOMIC_OPS_LIBS, or one can let the configure
# script try to set them.
#
# Detection can be overridden by setting the cache variable
# ac_cv_header_atomic_ops_h to `yes' or `no'. A test can be forced by
# `unset ac_cv_header_atomic_ops_h'.
#
# If the header <atomic_ops.h> is found, HAVE_ATOMIC_OPS_H is set to
# 1, otherwise 0. HAVE_ATOMIC_OPS_H is passed to both AC_SUBST and
# AC_DEFINE.
#
# If the support library libatomic_ops.{a,so,...} is found, or if the
# user set ATOMIC_OPS_LIBS, then HAVE_ATOMIC_OPS_LIB is set to `yes',
# otherwise to `no'. HAVE_ATOMIC_OPS_LIB is passed to both AC_SUBST
# and AC_DEFINE.
#
AC_DEFUN([StM_LIB_ATOMIC_OPS],[{ :
   AC_ARG_VAR([ATOMIC_OPS_CFLAGS],
      [C compiler flags for atomic_ops, overriding automatic detection])
   AC_ARG_VAR([ATOMIC_OPS_LIBS],
      [linker flags for atomic_ops, overriding automatic detection])

   StM_LIB_ATOMIC_OPS__save_cflags="${CFLAGS}"
   CFLAGS="${ATOMIC_OPS_CFLAGS} ${CFLAGS}"
   if test -z "${ac_cv_header_atomic_ops_h}"; then
      AC_CHECK_HEADERS([atomic_ops.h])
   fi
   CFLAGS="${StM_LIB_ATOMIC_OPS__save_cflags}"
   unset StM_LIB_ATOMIC_OPS__save_cflags

   HAVE_ATOMIC_OPS_H=0
   test x"${ac_cv_header_atomic_ops_h}" = xyes && HAVE_ATOMIC_OPS_H=1
   AC_SUBST([HAVE_ATOMIC_OPS_H])
   AC_DEFINE_UNQUOTED([HAVE_ATOMIC_OPS_H],[${HAVE_ATOMIC_OPS_H}],
      [Define to 1 if we have the <atomic_ops.h> header file, to 0 otherwise.])

   if test x"${ac_cv_header_atomic_ops_h}" = xyes; then
      HAVE_ATOMIC_OPS_LIB=1
      if test -z "${ATOMIC_OPS_LIBS}"; then
         StM_LIB_ATOMIC_OPS__save_libs="${LIBS}"
         LIBS=
         AC_SEARCH_LIBS([AO_locks],[atomic_ops])
         ATOMIC_OPS_LIBS="${LIBS}"
         test x"${ac_cv_search_AO_locks}" = xno && HAVE_ATOMIC_OPS_LIB=0
         LIBS="${StM_LIB_ATOMIC_OPS__save_libs}"
         unset StM_LIB_ATOMIC_OPS__save_libs
      fi
   else
      # If we do not have the header, assume we do not have the
      # support library, because we cannot use it, anyway.
      HAVE_ATOMIC_OPS_LIB=0

      # Ignore the precious variable settings.
      ATOMIC_OPS_CFLAGS=
      ATOMIC_OPS_LIBS=
   fi

   AC_SUBST([HAVE_ATOMIC_OPS_LIB])
   AC_DEFINE_UNQUOTED([HAVE_ATOMIC_OPS_LIB],[${HAVE_ATOMIC_OPS_LIB}],
      [Define to 1 if we have the libatomic_ops support library for <atomic_ops.h>, to 0 otherwise.])
}])
