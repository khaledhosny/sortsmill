dnl -*- autoconf -*-

dnl FONTFORGE_SEARCH_LIBS(members, [action-if-found], [action-if-not-found], [includes = ‘AC_INCLUDES_DEFAULT’])
dnl ------------------------------------------------------------------------------------------------------------
AC_DEFUN([FONTFORGE_SEARCH_LIBS],
[
   _save_LIBS="${LIBS}"
   found_lib=""
   AC_SEARCH_LIBS([$1],[$2],
                  [if test x"${ac_cv_search_$1}" != x"none required"; then
                     found_lib="${ac_cv_search_$1}"
                   fi
                   $3],
                  [$4],
                  [$5])
   LIBS="${_save_LIBS}"
])

dnl FONTFORGE_CHECK_MODULES(variable-prefix, modules, function, search-libs)
dnl ------------------------------------------------------------------------
AC_DEFUN([FONTFORGE_CHECK_MODULES],[
PKG_CHECK_MODULES([$1],[$2],[:],[
        FONTFORGE_SEARCH_LIBS([$3],[$4],
                [AS_TR_SH([$1_CFLAGS])=""
                 AS_TR_SH([$1_LIBS])="${found_lib}"],
                [AC_MSG_ERROR([Package requirements ($2) were not met:

No package '$2' found

Consider adjusting the PKG_CONFIG_PATH environment variable if you
installed software in a non-standard prefix.

Alternatively, you may set the environment variables $1_CFLAGS
and $1_LIBS to avoid the need to call pkg-config])])])

AC_SUBST([$1_CFLAGS])
AC_SUBST([$1_LIBS])])
