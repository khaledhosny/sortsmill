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


dnl FONTFORGE_ARG_DISABLE_PYTHON_API
dnl --------------------------------
AC_DEFUN([FONTFORGE_ARG_DISABLE_PYTHON_API],
[
AC_ARG_ENABLE([python-api],
        [AS_HELP_STRING([--disable-python-api],[do not build the API for Python])],
        [i_do_have_python_api="${enableval}"],
        [i_do_have_python_api=yes])
if test x"${i_do_have_python_api}" = xyes; then
   FONTFORGE_PATH_PROG([CYTHON],[cython],[Cython compiler command])
   AM_PATH_PYTHON([2.3])
   PKG_CHECK_MODULES([PYTHON],[python-"${PYTHON_VERSION}"],,[i_do_have_python_api=no])
fi
if test x"${i_do_have_python_api}" != xyes; then
   AC_DEFINE([_NO_PYTHON],1,[Define if not using Python.])
fi
])


dnl FONTFORGE_ARG_DISABLE_PYTHON_COMPATIBILITY
dnl ------------------------------------------
AC_DEFUN([FONTFORGE_ARG_DISABLE_PYTHON_COMPATIBILITY],
[
AC_ARG_ENABLE([python-compatibility],
        [AS_HELP_STRING([--disable-python-compatibility],
                        [do not install the Python modules `fontforge' and `psMat'
                         that are for compatibility with traditional FontForge;
                         you can still use `sortsmill.ffcompat' and `sortsmill.psMat'
                         for the same functionality])],
        [i_do_have_python_compatibility="${enableval}"],
        [i_do_have_python_compatibility=yes])
PYTHON_COMPATIBILITY="${i_do_have_python_compatibility}"
AC_SUBST([PYTHON_COMPATIBILITY])
])


dnl FONTFORGE_ARG_ENABLE_LEGACY_SORTSMILL_TOOLS
dnl -------------------------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE_LEGACY_SORTSMILL_TOOLS],
[
AC_ARG_ENABLE([legacy-sortsmill-tools],
        [AS_HELP_STRING([--enable-legacy-sortsmill-tools],
                        [install Python modules corresponding to
                         so-called Sorts Mill Tools version 0.x;
                         these are needed, for now, to build
                         Sorts Mill fonts from source])],
        [i_do_have_legacy_sortsmill_tools="${enableval}"],
        [i_do_have_legacy_sortsmill_tools=yes])
LEGACY_SORTSMILL_TOOLS="${i_do_have_legacy_sortsmill_tools}"
AC_SUBST([LEGACY_SORTSMILL_TOOLS])
])


dnl dnl FONTFORGE_ARG_ENABLE_FORTRAN_API
dnl dnl --------------------------------
dnl AC_DEFUN([FONTFORGE_ARG_ENABLE_FORTRAN_API],
dnl [
dnl AC_ARG_ENABLE([fortran-api],
dnl         [AS_HELP_STRING([--enable-fortran-api],[build the API for Fortran])],
dnl         [i_do_have_fortran_api="${enableval}"],
dnl         [i_do_have_fortran_api=no])
dnl ])
dnl 
dnl 
dnl dnl FONTFORGE_ARG_ENABLE_PURE_API
dnl dnl -----------------------------
dnl AC_DEFUN([FONTFORGE_ARG_ENABLE_PURE_API],
dnl [
dnl AC_ARG_VAR([PURE],[Pure interpreter command])
dnl AC_ARG_VAR([PURE_INCLUDEDIR],[directory for Pure-language included source scripts [DIR=LIBDIR/pure]])
dnl AC_ARG_VAR([PURE_LIBDIR],[directory for Pure-language dynamic libraries [DIR=LIBDIR/pure]])
dnl 
dnl AC_SUBST([pure_includedir], ['${libdir}/pure'])
dnl test x"$PURE_INCLUDEDIR" = x || AC_SUBST([pure_includedir],['${PURE_INCLUDEDIR}'])
dnl 
dnl AC_SUBST([pure_libdir], ['${libdir}/pure'])
dnl test x"$PURE_LIBDIR" = x || AC_SUBST([pure_libdir],['${PURE_LIBDIR}'])
dnl 
dnl AC_ARG_ENABLE([pure-api],
dnl         [AS_HELP_STRING([--enable-pure-api],[build the API for Pure (http://pure-lang.googlecode.com/)])],
dnl         [i_do_have_pure_api="${enableval}"],
dnl         [i_do_have_pure_api=no])
dnl if test x"${i_do_have_pure_api}" = xyes; then
dnl    PKG_CHECK_MODULES([PURE],[pure])
dnl fi
dnl if test x"${i_do_have_pure_api}" = xyes; then
dnl    if test x"${PURE}" = x; then
dnl       AC_MSG_CHECKING([for the `pure' command])
dnl       AC_PATH_PROG([PURE],[pure])
dnl       if test x"${PURE}" = x; then
dnl          AC_MSG_ERROR([could not find `pure'])
dnl       fi
dnl       AC_MSG_RESULT([${PURE}])
dnl    fi
dnl fi
dnl AC_SUBST([i_do_have_pure_api])
dnl ])


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
