dnl -*- autoconf -*-

AC_DEFUN([FONTFORGE_PYTHON_CONSTANTS],
[
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${PYTHON_CFLAGS}"
LIBS="${LIBS} ${GUILE_LIBS}"

FONTFORGE_C_CONST([PY_MAJOR_VERSION],[fontforge_cv_c_const_PY_MAJOR_VERSION],["%d"],[#include <Python.h>])
AC_SUBST([PY_MAJOR_VERSION],["${fontforge_cv_c_const_PY_MAJOR_VERSION}"])

FONTFORGE_C_CONST([PY_MINOR_VERSION],[fontforge_cv_c_const_PY_MINOR_VERSION],["%d"],[#include <Python.h>])
AC_SUBST([PY_MINOR_VERSION],["${fontforge_cv_c_const_PY_MINOR_VERSION}"])

FONTFORGE_C_CONST([PY_MICRO_VERSION],[fontforge_cv_c_const_PY_MICRO_VERSION],["%d"],[#include <Python.h>])
AC_SUBST([PY_MICRO_VERSION],["${fontforge_cv_c_const_PY_MICRO_VERSION}"])

FONTFORGE_C_CONST([PY_RELEASE_LEVEL],[fontforge_cv_c_const_PY_RELEASE_LEVEL],["%d"],[#include <Python.h>])
AC_SUBST([PY_RELEASE_LEVEL],["${fontforge_cv_c_const_PY_RELEASE_LEVEL}"])

FONTFORGE_C_CONST([PY_RELEASE_SERIAL],[fontforge_cv_c_const_PY_RELEASE_SERIAL],["%d"],[#include <Python.h>])
AC_SUBST([PY_RELEASE_SERIAL],["${fontforge_cv_c_const_PY_RELEASE_SERIAL}"])

FONTFORGE_C_CONST([PY_VERSION],[fontforge_cv_c_const_PY_VERSION],["%s"],[#include <Python.h>])
AC_SUBST([PY_VERSION],["${fontforge_cv_c_const_PY_VERSION}"])
AC_SUBST([PY_VERSION_QUOTED],["\"${fontforge_cv_c_const_PY_VERSION}\""])

FONTFORGE_C_CONST([PY_VERSION_HEX],[fontforge_cv_c_const_PY_VERSION_HEX],["%d"],[#include <Python.h>])
AC_SUBST([PY_VERSION_HEX],["${fontforge_cv_c_const_PY_VERSION_HEX}"])

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
