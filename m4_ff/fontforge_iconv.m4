dnl -*- autoconf -*-

dnl FONTFORGE_ICONV_SUPPORTS_UTF7
dnl -----------------------------
AC_DEFUN([FONTFORGE_ICONV_SUPPORTS_UTF7],
[
AC_CACHE_CHECK([whether iconv_open(3) supports UTF-7],
               [fontforge_cv_lib_iconv_open_supports_utf7],
[AC_RUN_IFELSE(
        [AC_LANG_PROGRAM(
                [#include <iconv.h>],
                [
if (iconv_open("UTF-8", "UTF-7") == -1) return 1;
if (iconv_open("UTF-7", "UTF-8") == -1) return 1;
return 0;
])],
        [fontforge_cv_lib_iconv_open_supports_utf7=yes],
        [fontforge_cv_lib_iconv_open_supports_utf7=no])])])

