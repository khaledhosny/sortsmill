dnl -*- autoconf -*-

dnl FONTFORGE_SET_MY_CFLAGS
dnl -----------------------
AC_DEFUN([FONTFORGE_SET_MY_CFLAGS],
[
#my_cflags="${my_cflags} ${PYTHON_CFLAGS}"
#my_cflags="${my_cflags} ${PURE_CFLAGS}"
])

dnl FONTFORGE_SET_MY_LIBS
dnl ---------------------
AC_DEFUN([FONTFORGE_SET_MY_LIBS],
[
#test x"${i_do_have_python_api}" = xyes && my_libs="${my_libs} ${PYTHON_LIBS}"
#test x"${i_do_have_pure_api}" = xyes && my_libs="${my_libs} ${PURE_LIBS}"
])
