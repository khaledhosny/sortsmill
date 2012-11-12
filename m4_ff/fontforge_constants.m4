dnl -*- autoconf -*-

dnl FONTFORGE_C_CONST_FLT_EPSILON
dnl ------------------------------
AC_DEFUN([FONTFORGE_C_CONST_FLT_EPSILON],
[FONTFORGE_C_FLOATING_POINT_CONST(
        [FLT_EPSILON],
        [fontforge_cv_c_const_flt_epsilon],
        ["(%.100g %d %d %d)\n"],
        ["(%.100g %.100g %d %d)\n"]
)])


dnl FONTFORGE_C_CONST_DBL_EPSILON
dnl --------------------------------
AC_DEFUN([FONTFORGE_C_CONST_DBL_EPSILON],
[FONTFORGE_C_FLOATING_POINT_CONST(
        [DBL_EPSILON],
        [fontforge_cv_c_const_dbl_epsilon],
        ["(%.100lg %d %d %d)\n"],
        ["(%.100lg %.100lg %d %d)\n"]
)])


dnl FONTFORGE_C_FLOATING_POINT_CONST
dnl --------------------------------
AC_DEFUN([FONTFORGE_C_FLOATING_POINT_CONST],
[
AC_CACHE_CHECK([the value of $1],
[$2],
[
$2='unknown'

AC_LANG_PUSH([C])
__libs="${LIBS}"
LIBS="${LIBS} ${FREXP_LIBM}"

AC_RUN_IFELSE(
[AC_LANG_PROGRAM(
[
#include <math.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
],
[
int exp;
double mant;
mant = frexp (($1), &exp);
if (mant == 0.5 && DBL_MIN_EXP < exp)
  printf (($3), ($1), 1, 2, exp - 1);
else
  printf (($4), ($1), mant, 2, exp);
])
],
[$2="`./conftest${EXEEXT}`"],
[:])
])

LIBS="${__libs}"
AC_LANG_POP
])

if test x"${$2}" != xunknown; then
   AC_MSG_FAILURE([${PACKAGE_NAME} needs to know the value of $1.])
fi
])
