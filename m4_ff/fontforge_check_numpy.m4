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
   ])
])
