dnl -*- autoconf -*-

dnl FONTFORGE_CHECK_PURE_MODULE(MODULE)
dnl -----------------------------------
AC_DEFUN([FONTFORGE_CHECK_PURE_MODULE],
[
AC_MSG_CHECKING([whether Pure module $1 is present])
echo "using $1;" > conftest.pure
${PURE} -x conftest.pure > conftest.out 2> conftest.err
if grep . conftest.err > /dev/null 2> /dev/null; then
   AC_MSG_RESULT([no])
   AC_MSG_FAILURE([Support for Pure in ${PACKAGE_NAME} requires the `$1' module,
                  but it was not found by configure.
                  Please see $2])
else
   AC_MSG_RESULT([yes])
fi
rm -f conftest.pure conftest.out conftest.err
])
