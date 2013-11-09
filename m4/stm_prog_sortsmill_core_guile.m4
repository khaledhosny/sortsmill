# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 2

# StM_PROG_SORTSMILL_CORE_GUILE
# -----------------------------
#
# Set SORTSMILL_CORE_GUILE to the path of the first
# sortsmill-core-guile in the PATH, or to an empty string if
# sortsmill-core-guile is not found. The result is cached in
# ac_cv_path_SORTSMILL_CORE_GUILE. The test may be overridden by
# setting SORTSMILL_CORE_GUILE or the cache variable.
#
AC_DEFUN([StM_PROG_SORTSMILL_CORE_GUILE],[if :; then
   AC_REQUIRE([AC_PROG_AWK])
   StM_PATH_PROGS_CACHED_AND_PRECIOUS([SORTSMILL_CORE_GUILE],
      [sortsmill-core-guile the program],
      [sortsmill-core-guile],[
         if ${ac_path_SORTSMILL_CORE_GUILE} --version | \
               ${AWK} 'NR==1 && /sortsmill-core-guile \(Sorts Mill Core Guile\)/ { exit 0 }; { exit 1 }'; then
            ac_cv_path_SORTSMILL_CORE_GUILE=${ac_path_SORTSMILL_CORE_GUILE}
            ac_path_SORTSMILL_CORE_GUILE_found=:
         fi
      ])
fi])
