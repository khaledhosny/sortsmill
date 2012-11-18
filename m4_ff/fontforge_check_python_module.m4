dnl -*- autoconf -*-

dnl FONTFORGE_CHECK_PYTHON_MODULE(MODULE)
dnl -------------------------------------
AC_DEFUN([FONTFORGE_CHECK_PYTHON_MODULE],
[
   AC_CACHE_CHECK([for the Python module '$1'],
                  [fontforge_cv_python_$1],
   [
      if ${PYTHON} -c 'import $1' 2> /dev/null; then
         fontforge_cv_python_$1=yes
      else
         fontforge_cv_python_$1=no
      fi
   ])
])


dnl FONTFORGE_REQUIRE_PYTHON_MODULE(MODULE)
dnl ---------------------------------------
AC_DEFUN([FONTFORGE_REQUIRE_PYTHON_MODULE],
[
   FONTFORGE_CHECK_PYTHON_MODULE([$1])
   if test x"${fontforge_cv_python_$1}" = xyes; then
      :
   else
      AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires the module '$1', which was not found.])
   fi
])
