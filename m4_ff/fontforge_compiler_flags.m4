dnl -*- autoconf -*-

dnl FONTFORGE_COMPILER_FLAGS
dnl ------------------------
AC_DEFUN([FONTFORGE_COMPILER_FLAGS],
[
m4_foreach_w([__flag],[$2],
   [AX_CHECK_COMPILE_FLAG([__flag],[$1="[$]{$1} __flag"])])
])
