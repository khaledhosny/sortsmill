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
   AM_PATH_PYTHON([2.3])
   PKG_CHECK_MODULES([PYTHON],[python-"${PYTHON_VERSION}"],,[i_do_have_python_scripting=no])
fi
if test x"${i_do_have_python_scripting}" != xyes; then
   AC_DEFINE([_NO_PYTHON],1,[Define if not using Python.])
fi
AM_CONDITIONAL([PYTHON_SCRIPTING],[test x"${i_do_have_python_scripting}" = xyes])
])


dnl FONTFORGE_ARG_ENABLE_BIGLOO_API
dnl -------------------------------
AC_DEFUN([FONTFORGE_ARG_ENABLE_BIGLOO_API],
[
AC_ARG_ENABLE([bigloo-api],
        [AS_HELP_STRING([--enable-bigloo-api],[enable the API for the Bigloo Scheme compiler])],
        [i_do_have_bigloo_api="${enableval}"],
        [i_do_have_bigloo_api=no])
if test x"${i_do_have_bigloo_api}" = xyes; then
   AC_PATH_PROG([BIGLOO],[bigloo],[bigloo])
   if ${BIGLOO} -eval '(exit 0)' > /dev/null 2> /dev/null; then
      :
   else
      AC_MSG_ERROR([${BIGLOO} does not seem to be Bigloo Scheme. See ${bigloo_url}])
   fi
   AC_MSG_CHECKING([the configuration of the Bigloo installation])
   m4_foreach_w([__key],
[release-number specific-version library-safety homeurl shell c-compiler-style c-compiler c-compiler-o-option c-compiler-debug-option c-compiler-optim-flag c-flag c-strip-flag c-prof-flag c-object-file-extension c-string-split c-linker-style c-linker-o-option c-linker-debug-option c-linker-optim-flags ld-library-dir library-directory non-custom-gc-directory zip-directory dll-directory user-libraries c-beautifier dirname-cmd library-base-name heap-debug-copt have-shared-library shared-link-option static-link-option auto-finalizer have-dlopen dlopen-lib have-bigloo-abort java jar java-shell jflags jvflags default-back-end gc-lib gc-custom have-bdb dns-cache-enabled shell-mv shell-rm endianess regexp],
[
dnl if `${BIGLOO} -eval '(if (equal? (bigloo-config (quote __key)) #unspecified) (exit 0) (exit 1))'`; then
   AC_SUBST(__BIGLOO_VAR(__key),[`${BIGLOO} -eval '(let ((v (bigloo-config (quote __key)))) (cond ((string? v) (display v)) ((symbol? v) (display v)) ((eq? v #t) (display (quote yes))) ((eq? v #f) (display (quote no))) ((eq? v #unspecified) (display "")) (else (write v)) ))(exit 0)'`])
dnl fi
])
   if test x"${BIGLOO_RELEASE_NUMBER}" = x; then
      AC_MSG_ERROR([${BIGLOO} does not seem to be Bigloo Scheme. See ${bigloo_url}])
   fi
   AC_MSG_RESULT([Bigloo release ${BIGLOO_RELEASE_NUMBER}])
fi

test x"${BGLFLAGS}" == x && BGLFLAGS="-O2"
AC_SUBST([BGLFLAGS])

AC_SUBST([BIGLOO_API],["${i_do_have_bigloo_api}"])
AM_CONDITIONAL([BIGLOO_API],[test x"${i_do_have_bigloo_api}" = xyes])
])
dnl
AC_DEFUN([__BIGLOO_VAR],[AS_TR_SH(m4_toupper(m4_join(,BIGLOO_,$1)))])


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
