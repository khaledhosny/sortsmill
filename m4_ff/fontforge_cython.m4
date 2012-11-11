dnl -*- autoconf -*-

dnl FONTFORGE_PROG_CYTHON_WORKS
dnl ---------------------------
AC_DEFUN([FONTFORGE_PROG_CYTHON_WORKS],
[
AC_MSG_CHECKING([whether CYTHON works])

cat > cythontest_.pyx <<EOF
cdef char *message = "Hello, world!"
print (message)
EOF

${CYTHON} --force --embed -o cythontest_.c cythontest_.pyx >&AS_MESSAGE_LOG_FD 2>&AS_MESSAGE_LOG_FD
AC_LANG_PUSH([C])
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${PYTHON_CFLAGS}"
LIBS="${LIBS} ${PYTHON_LIBS}"
AC_COMPILE_IFELSE(
     [AC_LANG_SOURCE([[#include "cythontest_.c"]])],
     [__result=yes],
     [__result=no]
)
AC_RUN_IFELSE([AC_LANG_SOURCE([],[__result=yes],[__result=no])])
CFLAGS="${__cflags}"
LIBS="${__libs}"
AC_LANG_POP

rm -r -f cythontest_.pyx

AC_MSG_RESULT([${__result}])
if test x"${__result}" != xyes; then
   AC_MSG_FAILURE([Building ${PACKAGE_NAME} requires a working Cython.])
fi
])
