dnl -*- autoconf -*-

dnl FONTFORGE_SET_MY_CFLAGS
dnl -----------------------
AC_DEFUN([FONTFORGE_SET_MY_CFLAGS],
[
my_cflags=
if test x"${i_do_have_freetype_debugger}" != xno; then
   my_cflags="${my_cflags} -I${FREETYPE_SOURCE}/src/truetype"
   my_cflags="${my_cflags} -I${FREETYPE_SOURCE}/include"
   my_cflags="${my_cflags} -I${FREETYPE_SOURCE}/include/freetype"
fi
my_cflags="${my_cflags} ${BOEHM_GC_CFLAGS}"
my_cflags="${my_cflags} ${GIFLIB_CFLAGS}"
my_cflags="${my_cflags} ${LIBJPEG_CFLAGS}"
my_cflags="${my_cflags} ${LIBTIFF_CFLAGS}"
my_cflags="${my_cflags} ${LIBSPIRO_CFLAGS}"
my_cflags="${my_cflags} ${LIBUNICODENAMES_CFLAGS}"
#my_cflags="${my_cflags} ${PYTHON_CFLAGS}"
#my_cflags="${my_cflags} ${PURE_CFLAGS}"
my_cflags="${my_cflags} ${PTHREAD_CFLAGS}"
AC_SUBST([MY_CFLAGS],[${my_cflags}])
])

dnl FONTFORGE_SET_MY_LIBS
dnl ---------------------
AC_DEFUN([FONTFORGE_SET_MY_LIBS],
[
#test x"${i_do_have_python_api}" = xyes && my_libs="${my_libs} ${PYTHON_LIBS}"
#test x"${i_do_have_pure_api}" = xyes && my_libs="${my_libs} ${PURE_LIBS}"
test x"${i_do_have_giflib}" = xyes && my_libs="${my_libs} ${GIFLIB_LIBS}"
test x"${i_do_have_libjpeg}" = xyes && my_libs="${my_libs} ${LIBJPEG_LIBS}"
test x"${i_do_have_libtiff}" = xyes && my_libs="${my_libs} ${LIBTIFF_LIBS}"
test x"${i_do_have_libunicodenames}" = xyes && my_libs="${my_libs} ${LIBUNICODENAMES_LIBS}"
test x"${i_do_have_libspiro}" = xyes && my_libs="${my_libs} ${LIBSPIRO_LIBS}"
my_libs="${my_libs} ${PTHREAD_LIBS}"
AC_SUBST([MY_LIBS],[${my_libs}])
])
