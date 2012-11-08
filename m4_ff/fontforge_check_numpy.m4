dnl -*- autoconf -*-


dnl FONTFORGE_CHECK_NUMPY
dnl ---------------------
AC_DEFUN([FONTFORGE_CHECK_NUMPY],
[
   AC_CACHE_CHECK([for the Python module 'numpy'],
                  [fontforge_cv_python_numpy],
   [
      if ${PYTHON} -c 'import numpy' 2> /dev/null; then
         fontforge_cv_python_numpy=yes
      else
         fontforge_cv_python_numpy=no
      fi
   ])
   if test x"${fontforge_cv_python_numpy}" != xyes; then
      AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires the module 'numpy', which was not found.])
   fi
])


dnl FONTFORGE_CHECK_NUMPY_LINALG
dnl ----------------------------
AC_DEFUN([FONTFORGE_CHECK_NUMPY_LINALG],
[
   FONTFORGE_CHECK_NUMPY
   AC_CACHE_CHECK([for the Python module 'numpy.linalg'],
                  [fontforge_cv_python_numpy_linalg],
   [
      if ${PYTHON} -c 'import numpy.linalg' 2> /dev/null; then
         fontforge_cv_python_numpy_linalg=yes
      else
         fontforge_cv_python_numpy_linalg=no
      fi
      if test x"${fontforge_cv_python_numpy_linalg}" != xyes; then
         AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires the module 'numpy.linalg', which was not found.])
      fi
   ])
])
