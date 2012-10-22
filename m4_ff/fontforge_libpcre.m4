dnl -*- autoconf -*-

dnl FONTFORGE_LIBPCRE
dnl -----------------
AC_DEFUN([FONTFORGE_LIBPCRE],
[
PKG_CHECK_MODULES([LIBPCRE],[libpcre >= 8.30])
FONTFORGE_LIBPCRE_PROPERTY([PCRE_CONFIG_UTF8],[support for UTF-8])
FONTFORGE_LIBPCRE_PROPERTY([PCRE_CONFIG_UNICODE_PROPERTIES],[support for Unicode properties])
])


dnl FONTFORGE_LIBPCRE_PROPERTY(property, description)
dnl -------------------------------------------------
AC_DEFUN([FONTFORGE_LIBPCRE_PROPERTY],
[
AC_MSG_CHECKING([whether libpcre was compiled with $2])

__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${LIBPCRE_CFLAGS}"
LIBS="${LIBS} ${LIBPCRE_LIBS}"

AC_LANG_PUSH([C])

AC_RUN_IFELSE(
   [AC_LANG_PROGRAM(
[
#include <pcre.h>
],
[
int error;
int value;
error = pcre_config ($1, &value);
return (error == 0 && value) ? 0 : 1;
])],
   [AC_MSG_RESULT([yes])],
   [AC_MSG_RESULT([no])
    AC_MSG_FAILURE([${PACKAGE_NAME} requires that libpcre be compiled with $2.])])

AC_LANG_POP

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
