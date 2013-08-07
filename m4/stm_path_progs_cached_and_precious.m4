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

# StM_PATH_PROGS_CACHED_AND_PRECIOUS(variable, message, progs-to-check-for, feature-test)
# ---------------------------------------------------------------------------------------
AC_DEFUN([StM_PATH_PROGS_CACHED_AND_PRECIOUS],[
AC_ARG_VAR([$1],[$2])
# Override the cache if the precious variable $1 is set.
test -n "${$1}" && unset ac_cv_path_$1
__ac_cv_path_$1_found=yes
AC_CACHE_CHECK([for $3],[ac_cv_path_$1],
   [AC_PATH_PROGS_FEATURE_CHECK([$1],[$3],[
      if test -n "${$1}"; then
         # Bypass the test if the precious variable $1 is set.
         ac_cv_path_$1="${$1}"
         ac_path_$1_found=:
      else
         # Run the test.
         $4
      fi])
      if test -z "${ac_cv_path_$1}"; then
         ac_cv_path_$1="not found"
         __ac_cv_path_$1_found=no
      fi
   ])
if test x"${__ac_cv_path_$1_found}" = x"yes"; then
   $1="${ac_cv_path_$1}"
else
   $1=""
   # Do not cache the negative result. That way someone can install
   # the program and have it be discovered, even though they did not
   # clear a cache entry.
   unset ac_cv_path_$1
fi
])
