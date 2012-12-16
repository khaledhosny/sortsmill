# Adapted from:
#
# ===========================================================================
#  http://www.gnu.org/software/autoconf-archive/ax_f90_module_extension.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_F90_MODULE_EXTENSION
#
# DESCRIPTION
#
#   Find Fortran 90 modules file extension. The module extension is stored
#   in the cached variable ax_f90_modext, or "unknown" if the extension
#   cannot be found.
#
# LICENSE
#
#   Copyright (c) 2009 Luc Maisonobe <luc@spaceroots.org>
#   Copyright (c) 2009 Alexander Pletzer <pletzer@txcorp.com>
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

AC_DEFUN([FONTFORGE_FC_MODULE_CASE],[
AC_CACHE_CHECK([fortran 90 modules case],
[fontforge_cv_fc_module_case],
[
   AC_LANG_PUSH(Fortran)
   i=0
   while test \( -f tmpdir_$i \) -o \( -d tmpdir_$i \) ; do
     i=`expr $i + 1`
   done
   mkdir tmpdir_$i
   cd tmpdir_$i
   AC_COMPILE_IFELSE([
   !234567
         module conftest_module
         contains
         subroutine conftest_routine
         write(*,'(a)') 'gotcha!'
         end subroutine conftest_routine
         end module conftest_module
     ],
     [if ls | grep -F -q conftest_module.; then
         fontforge_cv_fc_module_case="lower"
      elif ls | grep -F -q CONFTEST_MODULE.; then
         fontforge_cv_fc_module_case="upper"
      else
         fontforge_cv_fc_module_case=""
      fi
     ],
     [fontforge_cv_fc_module_case=""])
   cd ..
   rm -fr tmpdir_$i
   AC_LANG_POP(Fortran)
])

FC_MODCASE="${fontforge_cv_fc_module_case}"
AC_SUBST([FC_MODCASE])

FC_MODCASE_LOWER=''
test x"${FC_MODCASE}" = xlower && FC_MODCASE_LOWER=yes
AC_SUBST([FC_MODCASE_LOWER])

FC_MODCASE_UPPER=''
test x"${FC_MODCASE}" = xupper && FC_MODCASE_UPPER=yes
AC_SUBST([FC_MODCASE_UPPER])
])
