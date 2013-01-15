dnl -*- autoconf -*-

dnl FONTFORGE_ARG_ENABLE(option, help-message, config-entry)
dnl --------------------------------------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE],
[
AC_ARG_ENABLE([$1],[$2],
        [eval AS_TR_SH(i_do_have_$1)="${enableval}"],
        [eval AS_TR_SH(i_do_have_$1)=no])
if test x"${AS_TR_SH(i_do_have_$1)}" = xyes; then
   AC_DEFINE([$3],1,[Define if enabling feature '$1'.])
fi
])

dnl FONTFORGE_ARG_DISABLE(option, help-message, config-entry)
dnl ---------------------------------------------------------
AC_DEFUN([FONTFORGE_ARG_DISABLE],
[
AC_ARG_ENABLE([$1],[$2],
        [eval AS_TR_SH(i_do_have_$1)="${enableval}"],
        [eval AS_TR_SH(i_do_have_$1)=yes])
if test x"${AS_TR_SH(i_do_have_$1)}" != xyes; then
   AC_DEFINE([$3],1,[Define if disabling feature '$1'.])
fi
])


dnl FONTFORGE_ARG_DISABLE_PYTHON_SCRIPTING
dnl --------------------------------------
AC_DEFUN([FONTFORGE_ARG_DISABLE_PYTHON_SCRIPTING],
[
AC_ARG_ENABLE([python-scripting],
        [AS_HELP_STRING([--disable-python-scripting],[disable Python scripting])],
        [i_do_have_python_scripting="${enableval}"],
        [i_do_have_python_scripting=yes])
if test x"${i_do_have_python_scripting}" = xyes; then
   FONTFORGE_PATH_PROG([CYTHON],[cython],[Cython compiler command])
   AM_PATH_PYTHON([2.3])
   PKG_CHECK_MODULES([PYTHON],[python-"${PYTHON_VERSION}"],,[i_do_have_python_scripting=no])
fi
if test x"${i_do_have_python_scripting}" != xyes; then
   AC_DEFINE([_NO_PYTHON],1,[Define if not using Python.])
fi
AM_CONDITIONAL([PYTHON_SCRIPTING],[test x"${i_do_have_python_scripting}" = xyes])
])


dnl FONTFORGE_ARG_DISABLE_PYTHON_COMPATIBILITY
dnl ------------------------------------------
AC_DEFUN([FONTFORGE_ARG_DISABLE_PYTHON_COMPATIBILITY],
[
AC_ARG_ENABLE([python-compatibility],
        [AS_HELP_STRING([--disable-python-compatibility],
                        [do not install the Python modules `fontforge' and `psMat'
                         that are for compatibility with traditional FontForge;
                         you can still use `sortsmillff.ffcompat' and `sortsmillff.psMat'
                         for the same functionality])],
        [i_do_have_python_compatibility="${enableval}"],
        [i_do_have_python_compatibility=yes])
PYTHON_COMPATIBILITY="${i_do_have_python_compatibility}"
AC_SUBST([PYTHON_COMPATIBILITY])
AM_CONDITIONAL([PYTHON_COMPATIBILITY],[test x"${i_do_have_python_compatibility}" = xyes])
])


dnl FONTFORGE_ARG_ENABLE_FORTRAN_API
dnl --------------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE_FORTRAN_API],
[
AC_ARG_ENABLE([fortran-api],
        [AS_HELP_STRING([--enable-fortran-api],[build the API for Fortran])],
        [i_do_have_fortran_api="${enableval}"],
        [i_do_have_fortran_api=no])
AM_CONDITIONAL([FORTRAN_API],[test x"${i_do_have_fortran_api}" = xyes])
])


dnl FONTFORGE_ARG_ENABLE_PURE_API
dnl -----------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE_PURE_API],
[
AC_ARG_VAR([PURE],[Pure interpreter command])
AC_ARG_VAR([PURE_INCLUDEDIR],[directory for Pure-language included source scripts [DIR=LIBDIR/pure]])
AC_ARG_VAR([PURE_LIBDIR],[directory for Pure-language dynamic libraries [DIR=LIBDIR/pure]])

AC_SUBST([pure_includedir], ['${libdir}/pure'])
test x"$PURE_INCLUDEDIR" = x || AC_SUBST([pure_includedir],['${PURE_INCLUDEDIR}'])

AC_SUBST([pure_libdir], ['${libdir}/pure'])
test x"$PURE_LIBDIR" = x || AC_SUBST([pure_libdir],['${PURE_LIBDIR}'])

AC_ARG_ENABLE([pure-api],
        [AS_HELP_STRING([--enable-pure-api],[build the API for Pure (http://pure-lang.googlecode.com/)])],
        [i_do_have_pure_api="${enableval}"],
        [i_do_have_pure_api=no])
if test x"${i_do_have_pure_api}" = xyes; then
   PKG_CHECK_MODULES([PURE],[pure])
fi
if test x"${i_do_have_pure_api}" = xyes; then
   if test x"${PURE}" = x; then
      AC_MSG_CHECKING([for the `pure' command])
      AC_PATH_PROG([PURE],[pure])
      if test x"${PURE}" = x; then
         AC_MSG_ERROR([could not find `pure'])
      fi
      AC_MSG_RESULT([${PURE}])
   fi
fi
AC_SUBST([i_do_have_pure_api])
AM_CONDITIONAL([PURE_API],[test x"${i_do_have_pure_api}" = xyes])
])


dnl FONTFORGE_ARG_ENABLE_REAL
dnl -------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE_REAL],
[
AC_ARG_ENABLE([real],
        [AS_HELP_STRING([--enable-real=TYPE],
                [TYPE is float or double;
                 sets the floating point type used internally [default=double]])],
        [my_real_type="${enableval}"],
        [my_real_type=double])
if test x"${my_real_type}" = x"double"; then
   AC_DEFINE([FONTFORGE_CONFIG_USE_DOUBLE],1,[Define if using 'double' precision.])
elif test x"${my_real_type}" != x"float"; then
   AC_MSG_ERROR([Floating point type '${my_real_type}' not recognized.])
fi   
])
