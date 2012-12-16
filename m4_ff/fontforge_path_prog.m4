dnl -*- autoconf -*-

dnl FONTFORGE_PATH_PROG(variable,prog-to-check-for,description[,action-if-found[,action-if-not-found]])
dnl ---------------------------------------------------------------------------------------------------
AC_DEFUN([FONTFORGE_PATH_PROG],
[
   AC_ARG_VAR([$1],[$3])
   if test -z "@S|@{$1}"; then
      AC_PATH_PROG([$1],[$2],[])
   fi
   if test -z "@S|@{$1}"; then
      m4_ifval([$5],[$5],[:])
   else
      m4_ifval([$4],[$4],[:])
   fi
])
