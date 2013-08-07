# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 2

# StM_PROG_WGET
# -------------
#
# Set WGET to the path of the first wget in the PATH, or to an empty
# string if wget is not found. The result is cached in
# ac_cv_path_WGET. The test may be overridden by setting WGET or the
# cache variable.
#
AC_DEFUN([StM_PROG_WGET],[
   AC_REQUIRE([AC_PROG_EGREP])
   StM_PATH_PROGS_CACHED_AND_PRECIOUS([WGET],[GNU Wget command],
      [wget],[         
         if LC_ALL=C LANG=C ${ac_path_WGET} --version | \
                 LC_ALL=C LANG=C ${EGREP} -q '^Wgetrc:'; then
            ac_cv_path_WGET=${ac_path_WGET}
            ac_path_WGET_found=:
         fi
      ])
])
