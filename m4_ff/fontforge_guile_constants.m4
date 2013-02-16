dnl -*- autoconf -*-

AC_DEFUN([FONTFORGE_GUILE_CONSTANTS],
[
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${GUILE_CFLAGS}"
LIBS="${LIBS} ${GUILE_LIBS}"

FONTFORGE_C_CONST([SCM_EOL],[fontforge_cv_c_const_SCM_EOL],["%p"],[#include <libguile.h>])
AC_SUBST([SCM_EOL],["${fontforge_cv_c_const_SCM_EOL}"])

FONTFORGE_C_CONST([SCM_EOF_VAL],[fontforge_cv_c_const_SCM_EOF_VAL],["%p"],[#include <libguile.h>])
AC_SUBST([SCM_EOF_VAL],["${fontforge_cv_c_const_SCM_EOF_VAL}"])

FONTFORGE_C_CONST([SCM_UNSPECIFIED],[fontforge_cv_c_const_SCM_UNSPECIFIED],["%p"],[#include <libguile.h>])
AC_SUBST([SCM_UNSPECIFIED],["${fontforge_cv_c_const_SCM_UNSPECIFIED}"])

FONTFORGE_C_CONST([SCM_UNDEFINED],[fontforge_cv_c_const_SCM_UNDEFINED],["%p"],[#include <libguile.h>])
AC_SUBST([SCM_UNDEFINED],["${fontforge_cv_c_const_SCM_UNDEFINED}"])

FONTFORGE_C_CONST([SCM_F_DYNWIND_REWINDABLE],
                  [fontforge_cv_c_const_SCM_F_DYNWIND_REWINDABLE],["%d"],[#include <libguile.h>])
AC_SUBST([SCM_F_DYNWIND_REWINDABLE],["${fontforge_cv_c_const_SCM_F_DYNWIND_REWINDABLE}"])

FONTFORGE_C_CONST([SCM_F_WIND_EXPLICITLY],
                  [fontforge_cv_c_const_SCM_F_WIND_EXPLICITLY],["%d"],[#include <libguile.h>])
AC_SUBST([SCM_F_WIND_EXPLICITLY],["${fontforge_cv_c_const_SCM_F_WIND_EXPLICITLY}"])

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
