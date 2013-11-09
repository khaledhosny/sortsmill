
# -*- autoconf -*-
#
# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# 
# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.  This file is offered as-is,
# without any warranty.

# serial 1

# StM_PROG_SORTSMILL_APII_TO_GUILE
# --------------------------------
#
# Set SORTSMILL_APII_TO_GUILE to the path of the first
# sortsmill-apii-to-guile in the PATH, or to an empty string if
# sortsmill-apii-to-guile is not found. The result is cached in
# ac_cv_path_SORTSMILL_APII_TO_GUILE. The test may be overridden by
# setting SORTSMILL_APII_TO_GUILE or the cache variable.
#
AC_DEFUN([StM_PROG_SORTSMILL_APII_TO_GUILE],[if :
then
   AC_REQUIRE([AC_PROG_FGREP])
   StM_PATH_PROGS_CACHED_AND_PRECIOUS([SORTSMILL_APII_TO_GUILE],
      [sortsmill-apii-to-guile the program],
      [sortsmill-apii-to-guile],[
         if ${ac_path_SORTSMILL_APII_TO_GUILE} '(phony)' /dev/null | \
               ${FGREP} '(library' 2> /dev/null > /dev/null
         then
            ac_cv_path_SORTSMILL_APII_TO_GUILE=${ac_path_SORTSMILL_APII_TO_GUILE}
            ac_path_SORTSMILL_APII_TO_GUILE_found=:
         fi
      ])
fi])
