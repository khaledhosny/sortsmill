dnl -*- autoconf -*-

AC_DEFUN([FONTFORGE_GSL_CONSTANTS],
[
__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${GUILE_CFLAGS}"
LIBS="${LIBS} ${GUILE_LIBS}"

m4_foreach_w([constant],[
   GSL_SUCCESS
   GSL_FAILURE
   GSL_CONTINUE
   GSL_EDOM
   GSL_ERANGE
   GSL_EFAULT
   GSL_EINVAL
   GSL_EFAILED
   GSL_EFACTOR
   GSL_ESANITY
   GSL_ENOMEM
   GSL_EBADFUNC
   GSL_ERUNAWAY
   GSL_EMAXITER
   GSL_EZERODIV
   GSL_EBADTOL
   GSL_ETOL
   GSL_EUNDRFLW
   GSL_EOVRFLW
   GSL_ELOSS
   GSL_EROUND
   GSL_EBADLEN
   GSL_ENOTSQR
   GSL_ESING
   GSL_EDIVERGE
   GSL_EUNSUP
   GSL_EUNIMPL
   GSL_ECACHE
   GSL_ETABLE
   GSL_ENOPROG
   GSL_ENOPROGJ
   GSL_ETOLF
   GSL_ETOLX
   GSL_ETOLG
   GSL_EOF],
   [
      m4_define([__cache_name],[[fontforge_cv_c_const_]constant])
      FONTFORGE_C_CONST([constant],
                        [__cache_name],
                        ["%d"],
                        [#include <gsl/gsl_errno.h>])
      AC_SUBST(constant,"${__cache_name}")
   ])

CFLAGS="${__cflags}"
LIBS="${__libs}"
])
