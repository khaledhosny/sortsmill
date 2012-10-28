dnl -*- autoconf -*-

dnl FONTFORGE_ATOMIC_OPS
dnl --------------------
AC_DEFUN([FONTFORGE_ATOMIC_OPS],
[
AC_SEARCH_LIBS([AO_locks],[atomic_ops])
AC_CHECK_HEADERS([atomic_ops.h])

if test x"${ac_cv_header_atomic_ops_h}" != xyes; then
   AC_MSG_ERROR([${PACKAGE_NAME} requires atomic_ops.
See http://www.hpl.hp.com/research/linux/atomic_ops/])
fi

AC_MSG_CHECKING([whether atomic_ops works])

AC_LANG_PUSH([C])

AC_RUN_IFELSE(
   [AC_LANG_PROGRAM(
[
#include <atomic_ops.h>
#include <stdio.h>

int x;
volatile AO_t is_initialized = 0;

int
get_x (void)
{
  if (!AO_load_acquire_read(&is_initialized))
    {
      if (!is_initialized)
        {
          x = 42;
          AO_store_release_write (&is_initialized, 1);
        }
    }
  return x;
}
],
[
return (get_x () == 42) ? 0 : 1;
])],
   [AC_MSG_RESULT([yes])],
   [AC_MSG_RESULT([no])
    AC_MSG_FAILURE([${PACKAGE_NAME} requires atomic_ops.
See http://www.hpl.hp.com/research/linux/atomic_ops/])])

AC_LANG_POP
])
