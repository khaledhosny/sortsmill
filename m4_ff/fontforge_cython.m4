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

rm -r -f cythontest_.pyx ./cythontest_.c

AC_MSG_RESULT([${__result}])
if test x"${__result}" != xyes; then
   AC_MSG_FAILURE([Building ${PACKAGE_NAME} requires a working Cython.])
fi

CYTHON_VERSION=`${PYTHON} -c 'from Cython.Compiler.Version import version; print version'`
CYTHON_MAJOR=`AS_ECHO(["${CYTHON_VERSION}"]) | sed -e 's/^\([[0-9]][[0-9]]*\).*/\1/'`
CYTHON_MINOR=`AS_ECHO(["${CYTHON_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
AC_SUBST([CYTHON_VERSION])
AC_SUBST([CYTHON_MAJOR])
AC_SUBST([CYTHON_MINOR])
AC_DEFINE([CYTHON_VERSION],[${CYTHON_VERSION}],[Define CYTHON_VERSION to the full version of Cython.])
AC_DEFINE([CYTHON_MAJOR],[${CYTHON_MAJOR}],[Define CYTHON_MAJOR to the major version of Cython.])
AC_DEFINE([CYTHON_MINOR],[${CYTHON_MINOR}],[Define CYTHON_MINOR to the minor version of Cython.])
if test 1 -le ${CYTHON_MAJOR}; then
   :
elif test 0 -eq ${CYTHON_MAJOR} -a 17 -le ${CYTHON_MINOR}; then
   :
else
   AC_MSG_ERROR([Building ${PACKAGE_NAME} requires Cython >= 0.17. You seem to have version ${CYTHON_VERSION}.])
fi 
])
