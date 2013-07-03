dnl -*- autoconf -*-

dnl FONTFORGE_LIBPCRE
dnl -----------------
AC_DEFUN([FONTFORGE_LIBPCRE],
[
# Require at least the first version of libpcre to have the PCRE_UCP
# macro.
PKG_CHECK_MODULES([LIBPCRE],[libpcre >= 8.10])

FONTFORGE_LIBPCRE_PROPERTY([PCRE_CONFIG_UTF8],[support for UTF-8])
FONTFORGE_LIBPCRE_PROPERTY([PCRE_CONFIG_UNICODE_PROPERTIES],[support for Unicode properties])
FONTFORGE_LIBPCRE_CHECK_PCRE_FREE_STUDY
if test x"${fontforge_cv_func_pcre_free_study}" = xyes; then
   AC_DEFINE([HAVE_PCRE_FREE_STUDY],[1],
	     [Define to 1 if libpcre has the `pcre_free_study' function.])
fi
FONTFORGE_LIBPCRE_CHECK_PCRE_STUDY_JIT_COMPILE
if test x"${fontforge_cv_macro_PCRE_STUDY_JIT_COMPILE}" = xyes; then
   AC_DEFINE([HAVE_PCRE_FREE_JIT_COMPILE],[1],
	     [Define to 1 if libpcre has the `PCRE_STUDY_JIT_COMPILE' macro.])
fi
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


dnl FONTFORGE_LIBPCRE_CHECK_PCRE_FREE_STUDY
dnl ---------------------------------------
AC_DEFUN([FONTFORGE_LIBPCRE_CHECK_PCRE_FREE_STUDY],
[
AC_CACHE_CHECK([whether libpcre has the pcre_free_study function],
	       [fontforge_cv_func_pcre_free_study],
	       [
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${LIBPCRE_CFLAGS}"
LIBS="${LIBS} ${LIBPCRE_LIBS}"

AC_LANG_PUSH([C])

AC_LINK_IFELSE(
   [AC_LANG_PROGRAM(
[
#include <pcre.h>
],
[
pcre_free_study (0);
])],
[fontforge_cv_func_pcre_free_study=yes],
[fontforge_cv_func_pcre_free_study=no])

AC_LANG_POP

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
])


dnl FONTFORGE_LIBPCRE_CHECK_PCRE_STUDY_JIT_COMPILE
dnl ----------------------------------------------
AC_DEFUN([FONTFORGE_LIBPCRE_CHECK_PCRE_STUDY_JIT_COMPILE],
[
AC_CACHE_CHECK([whether libpcre has the `PCRE_STUDY_JIT_COMPILE' macro],
	       [fontforge_cv_macro_PCRE_STUDY_JIT_COMPILE],
	       [
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${LIBPCRE_CFLAGS}"
LIBS="${LIBS} ${LIBPCRE_LIBS}"

AC_LANG_PUSH([C])

AC_LINK_IFELSE(
   [AC_LANG_PROGRAM(
[
#include <pcre.h>
],
[
int x = PCRE_STUDY_JIT_COMPILE;
])],
[fontforge_cv_macro_PCRE_STUDY_JIT_COMPILE=yes],
[fontforge_cv_macro_PCRE_STUDY_JIT_COMPILE=no])

AC_LANG_POP

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
])
