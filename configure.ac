#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# Copyright (C) 2000-2012 by George Williams
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

AC_PREREQ([2.68])
AC_INIT([Sorts Mill Tools],
        [2.1.0_alpha1],
        [http://bitbucket.org/sortsmill/sortsmill-tools/issues],
        [sortsmill-tools],
        [http://bitbucket.org/sortsmill/sortsmill-tools])

AC_CONFIG_SRCDIR([fontforge/splinerefigure.c])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([config])        dnl Where things like install-sh go.
AC_CONFIG_TESTDIR([tests],[fontforge]) dnl Test suite setup.
AC_CANONICAL_BUILD                 dnl On what platform are we compiling?
AC_CANONICAL_HOST                  dnl For what platform are we compiling?
AC_CONFIG_HEADERS([inc/config.h])  dnl Where autoheader puts macros.
AC_USE_SYSTEM_EXTENSIONS           dnl Define such macros as _GNU_SOURCE.

AM_INIT_AUTOMAKE([1.11 dist-xz subdir-objects -Wall -Wno-portability -Werror])
AM_SILENT_RULES([yes])

# Libtool-related configuration.
AM_PROG_AR                          dnl Support for Microsoft linker.
LT_INIT([dlopen])                   dnl Initialize libtool.
LT_OBJDIR="${lt_cv_objdir}/"
AC_SUBST([LT_OBJDIR])

StM_ANALYZE_PACKAGE_VERSION

AC_PATH_XTRA
i_do_have_x="${have_x}"
i_do_have_gui="${i_do_have_x}"
HAVE_GUI=0; test x"${i_do_have_x}" = xyes && HAVE_GUI=1
AC_DEFINE_UNQUOTED([HAVE_GUI],[${HAVE_GUI}],
   [Define as 1 if building with the GUI; otherwise define as 0.])
AC_SUBST([HAVE_GUI])

AC_SUBST([HOST],["$host"])

GUILE_API="2.0"
AC_SUBST([GUILE_API])

GUILE_VERSION_MIN=2.0.6

test x"${FF_TEXTDOMAIN}" = x && FF_TEXTDOMAIN="${PACKAGE}"
test x"${FF_SHORTCUTSDOMAIN}" = x && FF_SHORTCUTSDOMAIN="${PACKAGE}-shortcuts"
test x"${FF_MACSHORTCUTSDOMAIN}" = x && FF_MACSHORTCUTSDOMAIN="${PACKAGE}-macshortcuts"
AC_SUBST([FF_TEXTDOMAIN])
AC_SUBST([FF_SHORTCUTSDOMAIN])
AC_SUBST([FF_MACSHORTCUTSDOMAIN])

#--------------------------------------------------------------------------
#
# URLs for dependencies.

giflib_url="http://giflib.sourceforge.net/"
libpng_url="http://www.libpng.org/"
libunicodenames_url="http://bitbucket.org/sortsmill/libunicodenames"
pure_url="http://pure-lang.googlecode.com/"
python_url="http://en.wikipedia.org/wiki/Pythonidae#Feeding"

# Point to the Wikipedia page, primarily because one may want
# libjpeg-turbo rather than the reference implementation.
libjpeg_url="http://en.wikipedia.org/wiki/Libjpeg"

# Point to the Wikipedia page, so people can learn not to be fooled by
# the old website, which some while ago was hijacked by androids from
# beyond the orbit of Neptune.
libtiff_url="http://en.wikipedia.org/wiki/Libtiff"

#--------------------------------------------------------------------------
#
# Checks for programs.

AC_PROG_AWK
AC_PROG_CC
AC_PROG_CC_C99
gl_EARLY
gl_INIT
AC_PROG_CC_C_O
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_FC
AC_PROG_FC_C_O
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PROG_SED
StM_PROG_SORTSMILL_TIG
PKG_PROG_PKG_CONFIG
m4_ifdef([AM_PATH_PYTHON],[AM_PATH_PYTHON])

# Code quality programs.
StM_PROG_GNU_INDENT
test -z "${GNU_INDENT}" && GNU_INDENT=indent
AC_SUBST([INDENT],["${GNU_INDENT}"])dnl FIXME: Change the calls to use GNU_INDENT.
FONTFORGE_PATH_PROG([TIDY],[tidy],[HTML Tidy command])

# Gettext programs.
FONTFORGE_PATH_PROG([MSGFMT],[msgfmt],[`msgfmt' command from GNU gettext])
FONTFORGE_PATH_PROG([XGETTEXT],[xgettext],[`xgettext' command from GNU gettext])

# Freedesktop programs.
FONTFORGE_PATH_PROG([UPDATE_MIME_DATABASE],[update-mime-database],
                    [`update-mime-database' command])
FONTFORGE_PATH_PROG([UPDATE_DESKTOP_DATABASE],[update-desktop-database],
                    [`update-desktop-database' command])

GUILE_PROGS
if test `${GUILE} -c "(import (system foreign)) (write (defined? 'procedure->pointer))"` != "#t"
then
   AC_MSG_ERROR([${PACKAGE_NAME} requires that Guile support `procedure->pointer'.
                 The Guile found by configure seems not to. See
                 http://www.gnu.org/software/guile/manual/guile.html@%:@Dynamic-FFI])
fi
StM_PROG_SORTSMILL_CORE_GUILE

StM_PROG_SORTSMILL_APII_TO_GUILE
if test -z "${SORTSMILL_APII_TO_GUILE}"
then
   AC_MSG_ERROR([`sortsmill-apii-to-guile' the program is needed but was not found.
           At the time of this writing, sortsmill-apii-to-guile is installed by Sorts Mill Core Guile.
           Sorts Mill Core Guile homepage: <https://bitbucket.org/sortsmill/sortsmill-core-guile>])
fi

# Assorted programs.
StM_PROG_WGET
test -z "${WGET}" && WGET=wget

#--------------------------------------------------------------------------

FONTFORGE_ARG_ENABLE_REAL

AC_ARG_ENABLE([programs],
        [AS_HELP_STRING([--disable-programs],[do not build `sortsmill-editor' and related programs
                                              (but do build libraries and possibly the Python extensions)])],
        [i_do_have_programs="${enableval}"],
        [i_do_have_programs=yes])
AM_CONDITIONAL([THE_PROGRAMS],[test x"${i_do_have_programs}" = xyes])
AC_SUBST([THE_PROGRAMS],["${i_do_have_programs}"])

FONTFORGE_ARG_DISABLE_PYTHON_API
FONTFORGE_ARG_DISABLE_PYTHON_COMPATIBILITY
FONTFORGE_ARG_ENABLE_LEGACY_SORTSMILL_TOOLS

# Fortran and pure are disabled. The Pure support may come back some
# day as a set of packages.
#
#FONTFORGE_ARG_ENABLE_FORTRAN_API
#FONTFORGE_ARG_ENABLE_PURE_API
AC_SUBST([i_do_have_fortran_api],[no])
AC_SUBST([i_do_have_pure_api],[no])

AC_ARG_ENABLE([freetype-debugger],
        [AS_HELP_STRING([--enable-freetype-debugger[[=DIR]]],
                [use internal freetype debugger within sortsmill-editor; requires source code of the
                 freetype library with which sortsmill-editor will be linked.
                 You need to set DIR to the top directory of the freetype sources,
                 or alternatively set the environment variable FREETYPE_SOURCE (see below)])],
        [i_do_have_freetype_debugger="${enableval}"],
        [AC_ARG_WITH([freetype-source],
                [AS_HELP_STRING([--with-freetype-source[[=DIR]]],[synonym for --enable-freetype-debugger])],
                [i_do_have_freetype_debugger="${withval}"],
                [i_do_have_freetype_debugger=no])])

FONTFORGE_ARG_ENABLE([capslock-for-alt],
        [AS_HELP_STRING([--enable-capslock-for-alt],
                [use capslock rather than alt/meta to alter behavior of tools in outline/bitmap windows])],
        [FONTFORGE_CONFIG_CAPSLOCK_FOR_ALT])

FONTFORGE_ARG_ENABLE([debug-raw-points],
        [AS_HELP_STRING([--enable-debug-raw-points],
                [add a raw mode to the points window of the debugger])],
        [FONTFORGE_CONFIG_SHOW_RAW_POINTS])

FONTFORGE_ARG_ENABLE([tile-path],
        [AS_HELP_STRING([--enable-tile-path],
                [enable a `tile path' command (a variant of `expand stroke')])],
        [FONTFORGE_CONFIG_TILEPATH])

#--------------------------------------------------------------------------
#
# Checks for libraries.

# Found by Gnulib macros (and way big overkill, while at the same time
# no doubt missing several functions we use).
test -n "${ATAN2_LIBM}" && LIBS="${ATAN2_LIBM} ${LIBS}"
test -n "${COS_LIBM}" && LIBS="${COS_LIBM} ${LIBS}"
test -n "${FABS_LIBM}" && LIBS="${FABS_LIBM} ${LIBS}"
test -n "${FLOOR_LIBM}" && LIBS="${FLOOR_LIBM} ${LIBS}"
test -n "${FMA_LIBM}" && LIBS="${FMA_LIBM} ${LIBS}"
test -n "${FREXP_LIBM}" && LIBS="${FREXP_LIBM} ${LIBS}"
test -n "${HYPOT_LIBM}" && LIBS="${HYPOT_LIBM} ${LIBS}"
test -n "${LDEXP_LIBM}" && LIBS="${LDEXP_LIBM} ${LIBS}"
test -n "${POW_LIBM}" && LIBS="${POW_LIBM} ${LIBS}"
test -n "${RINT_LIBM}" && LIBS="${RINT_LIBM} ${LIBS}"
test -n "${SIN_LIBM}" && LIBS="${SIN_LIBM} ${LIBS}"
test -n "${SQRT_LIBM}" && LIBS="${SQRT_LIBM} ${LIBS}"
test -n "${TAN_LIBM}" && LIBS="${TAN_LIBM} ${LIBS}"

# Found by Gnulib macros.
test -n "${LIB_PTHREAD}" && LIBS="${LIB_PTHREAD} ${LIBS}"
test -n "${LTLIBTHREAD}" && LIBS="${LTLIBTHREAD} ${LIBS}"
test -n "${GETHOSTNAME_LIB}" && LIBS="${GETHOSTNAME_LIB} ${LIBS}"
test -n "${HOSTENT_LIB}" && LIBS="${HOSTENT_LIB} ${LIBS}"
test -n "${LIBSOCKET}" && LIBS="${LIBSOCKET} ${LIBS}"
test -n "${LIB_SELECT}" && LIBS="${LIB_SELECT} ${LIBS}"
test -n "${LTLIBICONV}" && LIBS="${LTLIBICONV} ${LIBS}"
test -n "${LTLIBINTL}" && LIBS="${LTLIBINTL} ${LIBS}"
test -n "${LTLIBTHREAD}" && LIBS="${LTLIBTHREAD} ${LIBS}"
test -n "${LTLIBUNISTRING}" && LIBS="${LTLIBUNISTRING} ${LIBS}"
test -n "${INTL_MACOSX_LIBS}" && LIBS="${INTL_MACOSX_LIBS} ${LIBS}"

StM_LIB_ZLIB
if test x"${HAVE_LIBZ}" != xyes; then
   AC_MSG_ERROR([Necessary library libz was not found.
           Consider installing zlib: <http://zlib.net/>])
fi
test -n "${LTLIBZ}" && LIBS="${LTLIBZ} ${LIBS}"

if test x"${i_do_have_x}" = xyes; then
   PKG_CHECK_MODULES([X11], [x11],
      [test -n "${X11_CFLAGS}" && CFLAGS="${CFLAGS} ${X11_CFLAGS}"
       test -n "${X11_LIBS}" && LIBS="${X11_LIBS} ${LIBS}"])

   PKG_CHECK_MODULES([XI], [xi],
      [test -n "${XI_CFLAGS}" && CFLAGS="${CFLAGS} ${XI_CFLAGS}"
       test -n "${XI_LIBS}" && LIBS="${XI_LIBS} ${LIBS}"],
      [AC_DEFINE([_NO_XINPUT], [1], [Define to 1 if not using XInput.])])

   PKG_CHECK_MODULES([XKBUI], [xkbui],
      [test -n "${XKBUI_CFLAGS}" && CFLAGS="${CFLAGS} ${XKBUI_CFLAGS}"
       test -n "${XKBUI_LIBS}" && LIBS="${XKBUI_LIBS} ${LIBS}"],
      [AC_DEFINE([_NO_XKB], [1], [Define to 1 if not using xkbui.])])
fi

PKG_CHECK_MODULES([BOEHM_GC],[bdw-gc >= 7.1])
if test x"${enable_threads}" = xyes; then
   AC_DEFINE([GC_THREADS],[1],
      [Define to 1 if Boehm GC has to co-exist with POSIX threads.])
fi
test -n "${BOEHM_GC_CFLAGS}" && CFLAGS="${CFLAGS} ${BOEHM_GC_CFLAGS}"
test -n "${BOEHM_GC_LIBS}" && LIBS="${BOEHM_GC_LIBS} ${LIBS}"

PKG_CHECK_MODULES([SORTSMILL_CORE],[sortsmill-core >= 1.0.0])
test -n "${SORTSMILL_CORE_CFLAGS}" && CFLAGS="${CFLAGS} ${SORTSMILL_CORE_CFLAGS}"
test -n "${SORTSMILL_CORE_LIBS}" && LIBS="${SORTSMILL_CORE_LIBS} ${LIBS}"
PKG_CHECK_VAR([SORTSMILL_CORE_LIBDIR],[sortsmill-core],[libdir])

PKG_CHECK_MODULES([LIBPNG],[libpng])
test -n "${LIBPNG_CFLAGS}" && CFLAGS="${CFLAGS} ${LIBPNG_CFLAGS}"
test -n "${LIBPNG_LIBS}" && LIBS="${LIBPNG_LIBS} ${LIBS}"

PKG_CHECK_MODULES([LIBXML],[libxml-2.0])
test -n "${LIBXML_CFLAGS}" && CFLAGS="${CFLAGS} ${LIBXML_CFLAGS}"
test -n "${LIBXML_LIBS}" && LIBS="${LIBXML_LIBS} ${LIBS}"

# FIXME: Do a more sophisticated GMP check.
AC_CHECK_LIB([gmp],[__gmpz_init],[],
  [AC_MSG_ERROR([${PACKAGE_NAME} requires GNU MP, but it was not found. See http://gmplib.org/])])

PKG_CHECK_MODULES([GLIB],[glib-2.0 >= 2.8 gio-2.0])
test -n "${GLIB_CFLAGS}" && CFLAGS="${CFLAGS} ${GLIB_CFLAGS}"
test -n "${GLIB_LIBS}" && LIBS="${GLIB_LIBS} ${LIBS}"

PKG_CHECK_MODULES([GSL],[gsl])
test -n "${GSL_CFLAGS}" && CFLAGS="${CFLAGS} ${GSL_CFLAGS}"
test -n "${GSL_LIBS}" && LIBS="${GSL_LIBS} ${LIBS}"

PKG_CHECK_MODULES([GUILE],[guile-2.0 >= 2.0.6])
GUILE_PKG([2.0])
GUILE_FLAGS
test -n "${GUILE_CFLAGS}" && CFLAGS="${CFLAGS} ${GUILE_CFLAGS}"
test -n "${GUILE_LTLIBS}" && LIBS="${GUILE_LTLIBS} ${LIBS}"

PKG_CHECK_MODULES([SORTSMILL_CORE_GUILE],[sortsmill-core-guile-${GUILE_API} >= 1.0.0])
test -n "${SORTSMILL_CORE_GUILE_CFLAGS}" && CFLAGS="${CFLAGS} ${SORTSMILL_CORE_GUILE_CFLAGS}"
test -n "${SORTSMILL_CORE_GUILE_LIBS}" && LIBS="${SORTSMILL_CORE_GUILE_LIBS} ${LIBS}"

# FIXME: Do we really want this if there is no GUI?
# 11.0.5 is the "libtool" version corresponding to 2.4.0 release as reported by pkg-config
# check doc/VERSION.DLL for mapping between libtool and release versions
PKG_CHECK_MODULES([FREETYPE],[freetype2 >= 11.0.5])
test -n "${FREETYPE_CFLAGS}" && CFLAGS="${CFLAGS} ${FREETYPE_CFLAGS}"
test -n "${FREETYPE_LIBS}" && LIBS="${FREETYPE_LIBS} ${LIBS}"

if test x"${i_do_have_gui}" = xyes; then
   # 11.0.5 is the "libtool" version corresponding to 2.4.0 release as reported by pkg-config
   # check doc/VERSION.DLL for mapping between libtool and release versions
   #PKG_CHECK_MODULES([FREETYPE],[freetype2 >= 11.0.5])
   #test -n "${FREETYPE_CFLAGS}" && CFLAGS="${CFLAGS} ${FREETYPE_CFLAGS}"
   #test -n "${FREETYPE_LIBS}" && LIBS="${FREETYPE_LIBS} ${LIBS}"

   PKG_CHECK_MODULES([CAIRO],[cairo >= 1.6])
   test -n "${CAIRO_CFLAGS}" && CFLAGS="${CFLAGS} ${CAIRO_CFLAGS}"
   test -n "${CAIRO_LIBS}" && LIBS="${CAIRO_LIBS} ${LIBS}"

   PKG_CHECK_MODULES([PANGO],[pango >= 1.10 pangocairo])
   test -n "${PANGO_CFLAGS}" && CFLAGS="${CFLAGS} ${PANGO_CFLAGS}"
   test -n "${PANGO_LIBS}" && LIBS="${PANGO_LIBS} ${LIBS}"

   PKG_CHECK_MODULES([XCURS],[xcursor])
   test -n "${XCURS_CFLAGS}" && CFLAGS="${CFLAGS} ${XCURS_CFLAGS}"
   test -n "${XCURS_LIBS}" && LIBS="${XCURS_LIBS} ${LIBS}"
fi

if test x"${HAVE_LIBUNISTRING}" != xyes; then
   AC_MSG_ERROR([Cannot find GNU libunistring. ${PACKAGE_NAME} cannot be built without it. See http://www.gnu.org/software/libunistring/])
fi

AC_ARG_VAR([FREETYPE_SOURCE],
        [directory where freetype source code is located for --enable-freetype-debugger;
         however, if --enable-freetype-debugger=DIR is specified, then DIR overrides
         the setting of FREETYPE_SOURCE])
test x"${i_do_have_freetype_debugger}" != xno && \
   CFLAGS="-I${FREETYPE_SOURCE}/src/truetype -I${FREETYPE_SOURCE}/include -I${FREETYPE_SOURCE}/include/freetype ${CFLAGS}"

FONTFORGE_ARG_WITH_GIFLIB
test -n "${GIFLIB_CFLAGS}" && CFLAGS="${CFLAGS} ${GIFLIB_CFLAGS}"
test -n "${GIFLIB_LIBS}" && LIBS="${GIFLIB_LIBS} ${LIBS}"

FONTFORGE_ARG_WITH_LIBJPEG
test -n "${LIBJPEG_CFLAGS}" && CFLAGS="${CFLAGS} ${LIBJPEG_CFLAGS}"
test -n "${LIBJPEG_LIBS}" && LIBS="${LIBJPEG_LIBS} ${LIBS}"

FONTFORGE_ARG_WITH_LIBTIFF
test -n "${LIBTIFF_CFLAGS}" && CFLAGS="${CFLAGS} ${LIBTIFF_CFLAGS}"
test -n "${LIBTIFF_LIBS}" && LIBS="${LIBTIFF_LIBS} ${LIBS}"

FONTFORGE_ARG_WITH_LIBUNICODENAMES
test -n "${LIBUNICODENAMES_CFLAGS}" && CFLAGS="${CFLAGS} ${LIBUNICODENAMES_CFLAGS}"
test -n "${LIBUNICODENAMES_LIBS}" && LIBS="${LIBUNICODENAMES_LIBS} ${LIBS}"

FONTFORGE_CONFIG_FREETYPE_DEBUGGER

if test x"${i_do_have_python_api}" = xyes; then
   if test x"${PYTHON_VERSION}" != x"2.7"; then
      AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires Python 2.7.x. You are using Python ${PYTHON_VERSION}.])
   fi
   test -n "${PYTHON_CFLAGS}" && CFLAGS="${CFLAGS} ${PYTHON_CFLAGS}"
   test -n "${PYTHON_LIBS}" && LIBS="${PYTHON_LIBS} ${LIBS}"
   
   FONTFORGE_REQUIRE_PYTHON_MODULE([gmpy])
   FONTFORGE_PROG_CYTHON_WORKS
   FONTFORGE_PYTHON_CONSTANTS
fi

FONTFORGE_GUILE_CONSTANTS
FONTFORGE_GSL_CONSTANTS

#--------------------------------------------------------------------------
#
# Checks for header files.

AC_CHECK_HEADERS([libintl.h limits.h memory.h pthread.h])

if test x"${i_do_have_freetype_debugger}" != xno -a x"${ac_cv_header_pthread_h}" != xyes; then
   AC_MSG_ERROR([I cannot find <pthread.h>;
           consider configure option --disable-freetype-debugger (--without-freetype-source)])
fi

#--------------------------------------------------------------------------
#
# Checks for typedefs, structures, and compiler characteristics.

AC_C_TYPEOF
AC_C_BIGENDIAN
AC_HEADER_ASSERT
AC_HEADER_STDBOOL
AC_TYPE_INT8_T
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT8_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

AC_CHECK_SIZEOF([_Bool])
SIZEOF__BOOL=${ac_cv_sizeof__Bool}
AC_SUBST([SIZEOF__BOOL])
if test ${SIZEOF__BOOL} -ne 1 -a ${SIZEOF__BOOL} -ne 2 -a ${SIZEOF__BOOL} -ne 4; then
   AC_MSG_ERROR([${PACKAGE_NAME} cannot handle this _Bool size.])
fi

AC_CHECK_ALIGNOF([_Bool])
ALIGNOF__BOOL=${ac_cv_alignof__Bool}
AC_SUBST([ALIGNOF__BOOL])

AC_CHECK_SIZEOF([intptr_t],[],[#include <stdint.h>])
SIZEOF_INTPTR_T=${ac_cv_sizeof_intptr_t}
AC_SUBST([SIZEOF_INTPTR_T])

AC_CHECK_SIZEOF([uintptr_t],[],[#include <stdint.h>])
SIZEOF_UINTPTR_T=${ac_cv_sizeof_uintptr_t}
AC_SUBST([SIZEOF_UINTPTR_T])

# This only checks that an int requires 4 bytes of storage, not that
# it actually uses all 32 bits. We actually require all 32 bits.
AC_CHECK_SIZEOF([int])
test 4 -le ${ac_cv_sizeof_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'int' be at least 32 bits wide.])
AC_CHECK_SIZEOF([unsigned int])
test 4 -le ${ac_cv_sizeof_unsigned_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'unsigned int' be at least 32 bits wide.])

AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([double])
AC_SUBST([SIZEOF_FLOAT],[${ac_cv_sizeof_float}])
AC_SUBST([SIZEOF_DOUBLE],[${ac_cv_sizeof_double}])

AC_SUBST([WARNING_CFLAGS])
if test -z "${WARNING_CFLAGS}"; then
   StM_GNU_C_FLAGS([WARNING_CFLAGS],
      [-Wunused -Wreturn-type
       -Wparentheses -Wformat -Wchar-subscripts
       -Wno-unused-but-set-variable -Wno-unused-result
       -Wstrict-aliasing
       -Wcast-align
       -Wsizeof-pointer-memaccess
       -Wpointer-arith
       -Wmissing-prototypes
       -Wimplicit],
      [-Wl,--warn-common])
fi
test -n "${WARNING_CFLAGS}" && CFLAGS="${CFLAGS} ${WARNING_CFLAGS}"

AC_SUBST([CYTHON_EXTRA_CFLAGS])
if test -z "${CYTHON_EXTRA_CFLAGS}"; then
   StM_GNU_C_FLAGS([CYTHON_EXTRA_CFLAGS],
      [-Wno-unused -fwrapv -fno-strict-aliasing -Wno-strict-aliasing])
fi

AC_SUBST([VISIBILITY_DEFAULT_EXTRA_CFLAGS])
if test -z "${VISIBILITY_DEFAULT_EXTRA_CFLAGS}"; then
   StM_GNU_C_FLAGS([VISIBILITY_DEFAULT_EXTRA_CFLAGS],[-fvisibility=default])
fi

AC_SUBST([NO_MISSING_PROTOTYPES_EXTRA_CFLAGS])
if test -z "${NO_MISSING_PROTOTYPES_EXTRA_CFLAGS}"; then
   StM_GNU_C_FLAGS([NO_MISSING_PROTOTYPES_EXTRA_CFLAGS],[-Wno-missing-prototypes])
fi

AC_SUBST([NO_UNUSED_EXTRA_CFLAGS])
if test -z "${NO_UNUSED_EXTRA_CFLAGS}"; then
   StM_GNU_C_FLAGS([NO_UNUSED_EXTRA_CFLAGS],[-Wno-unused])
fi

StM_DEFINE_ATTR_VISIBLE([VISIBLE])
test -n "${CFLAG_VISIBILITY}" && CFLAGS="${CFLAGS} ${CFLAG_VISIBILITY}"

# if test x"${i_do_have_fortran_api}" = xyes; then
#    AC_FC_SRCEXT([f90])
#    AC_FC_PP_SRCEXT([F90])
#    AC_FC_LINE_LENGTH([132])
#    AC_FC_IMPLICIT_NONE([],[:])
#    AC_FC_MAIN
#    AC_FC_LIBRARY_LDFLAGS
#    AC_FC_PP_DEFINE
#    AC_FC_FREEFORM
#    AC_FC_MODULE_EXTENSION
#    AC_FC_MODULE_FLAG
#    FONTFORGE_FC_MODULE_CASE
# fi
# 
# # How to get mangled Fortran names here in configure, for instance to
# # use them from Pure, Guile, or Python ctypes code:
# #
# #AC_FC_FUNC([some_fortran_function])
# #AC_SUBST([some_fortran_function])
# #
# # In code that goes through the C preprocessor you can use the
# # FC_FUNC() and FC_FUNC_() macros defined in inc/config.h by:
# 
# if test x"${i_do_have_fortran_api}" = xyes; then
#    AC_FC_WRAPPERS
# fi
# 
# # With recent Fortran specifications, you can use BIND(C) in your
# # Fortran code and thus avoid name mangling. This method is the best.

#--------------------------------------------------------------------------
# Checks for library functions.

AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_FUNC_STRTOD
AC_CHECK_FUNCS([endpwent])
AC_CHECK_FUNCS([munmap])
AC_CHECK_FUNCS([socket])

__cflags="${CFLAGS}"
__libs="${LIBS}"
CFLAGS="${CFLAGS} ${GUILE_CFLAGS}"
LIBS="${LIBS} ${GUILE_LIBS}"
AC_CHECK_FUNCS([scm_init_guile])
if test x"${i_do_have_python_api}" = xyes -a x"${ac_cv_func_scm_init_guile}" != xyes; then
   # FIXME: It would be better not to have this requirement.
   AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires that Guile support the scm_init_guile() function,
                  which was not found.])
fi
CFLAGS="${__cflags}"
LIBS="${__libs}"

FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UCS4LE],[UCS-4LE ucs-4le UCS4LE ucs4le UTF-32LE utf-32le UTF32LE utf32le])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UCS4BE],[UCS-4BE ucs-4be UCS4BE ucs4be UTF-32BE utf-32be UTF32BE utf32be])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UTF8],[UTF-8 csUTF8 utf-8 UTF8 utf8])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UTF7],[UNICODE-1-1-UTF-7 csUnicode11UTF7 UTF-7 utf-7 UTF7 utf7])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_MAC],[MACINTOSH macintosh MAC mac])

FONTFORGE_ICONV_TOCODE_SUFFIX([MY_ICONV_SUFFIX_TRANSLIT],[//TRANSLIT],[ISO-8859-1],[[$]{MY_ICONV_UTF8}])
FONTFORGE_ICONV_TOCODE_SUFFIX([MY_ICONV_SUFFIX_IGNORE],[//IGNORE],[ISO-8859-1],[[$]{MY_ICONV_UTF8}])

AH_BOTTOM([#include <config-extra.h>])

#if test x"${i_do_have_pure_api}" = xyes; then
#   FONTFORGE_CHECK_PURE_MODULE([ffi],[http://docs.pure-lang.googlecode.com/hg/pure-ffi.html])
#fi

#--------------------------------------------------------------------------

# Use @includemk@ to invoke GNU Make’s ‘include’ command, bypassing
# Automake’s. (FIXME: Is there a different and canonical way to do
# this? Although, in any case, this method works fine.)
AC_SUBST([includemk],[include])

# Use @ifdefmk@, @ifndefmk@, @endifmk@ to invoke GNU Make’s
# conditionals, bypassing Automake’s. (FIXME: Is there a different and
# canonical way to do this? Although, in any case, this method works
# fine.)
AC_SUBST([ifdefmk],[ifdef])
AC_SUBST([ifndefmk],[ifndef])
AC_SUBST([endifmk],[endif])

AM_CONDITIONAL([HAVE_PYTHON_API],[test x"${i_do_have_python_api}" = xyes])
AM_CONDITIONAL([HAVE_PYTHON_COMPATIBILITY],[test x"${i_do_have_python_compatibility}" = xyes])
AM_CONDITIONAL([HAVE_LEGACY_SORTSMILL_TOOLS],[test x"${i_do_have_legacy_sortsmill_tools}" = xyes])
AM_CONDITIONAL([HAVE_PURE_API],[test x"${i_do_have_pure_api}" = xyes])
AM_CONDITIONAL([HAVE_FORTRAN_API],[test x"${i_do_have_fortran_api}" = xyes])
AM_CONDITIONAL([HAVE_GUI],[test x"${i_do_have_gui}" = xyes])
AM_CONDITIONAL([HAVE_FORTRAN_MODFILES],[test x"${FC_MODEXT}" != x])

# The following conditionals are always false, so the directories
# never get built by default. Nevertheless, they will be included by
# "make dist".
AM_CONDITIONAL([FONTTOOLS],[test x != x])
AM_CONDITIONAL([EXAMPLES],[test x != x])

FONTFORGE_OBSCURE_CONFIGURATION

GETTEXT_PACKAGE=$PACKAGE
AC_SUBST(GETTEXT_PACKAGE)
IT_PROG_INTLTOOL(0.41)
AM_GLIB_GNU_GETTEXT

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([mk/Makefile])
AC_CONFIG_FILES([lib/Makefile])
AC_CONFIG_FILES([inc/Makefile])
AC_CONFIG_FILES([auxiliary/Makefile])
AC_CONFIG_FILES([gutils/Makefile])
AC_CONFIG_FILES([gdraw/Makefile])
AC_CONFIG_FILES([fontforge/Makefile])
AC_CONFIG_FILES([main/Makefile])
AC_CONFIG_FILES([po/Makefile.in])
AC_CONFIG_FILES([doc/Makefile])
AC_CONFIG_FILES([htdocs/Makefile])
AC_CONFIG_FILES([python/Makefile])
AC_CONFIG_FILES([tests/Makefile])
AC_CONFIG_FILES([data/Makefile])
AC_CONFIG_FILES([data/cidmap/Makefile])
AC_CONFIG_FILES([data/cursors/Makefile])
AC_CONFIG_FILES([data/pixmaps/Makefile])
AC_CONFIG_FILES([fonttools/Makefile])
AC_CONFIG_FILES([guile/Makefile])
AC_CONFIG_FILES([sfd2sxml/Makefile])
#AC_CONFIG_FILES([fortran/Makefile])
#AC_CONFIG_FILES([pure/Makefile])
AC_CONFIG_FILES([examples/Makefile])

AC_CONFIG_FILES([inc/config-extra.h])
AC_CONFIG_FILES([inc/sortsmill/cython/config.pxi])

AC_CONFIG_COMMANDS([fontforge/sortsmill],[AS_MKDIR_P([fontforge/sortsmill])])

AC_CONFIG_FILES([python/fontforge.py])
AC_CONFIG_FILES([python/psMat.py])
AC_CONFIG_FILES([python/sortsmill/__init__.py])
AC_CONFIG_FILES([python/__sortsmill__/__init__.py])
AC_CONFIG_FILES([python/sortsmill/legacy/__init__.py])

AC_CONFIG_COMMANDS([guile/sortsmill/math],[AS_MKDIR_P([guile/sortsmill/math])])
AC_CONFIG_COMMANDS([guile/sortsmill/math/gsl],[AS_MKDIR_P([guile/sortsmill/math/gsl])])
AC_CONFIG_COMMANDS([guile/sortsmill/math/matrices],[AS_MKDIR_P([guile/sortsmill/math/matrices])])
AC_CONFIG_COMMANDS([guile/sortsmill/pkg-info],[AS_MKDIR_P([guile/sortsmill/pkg-info])])
AC_CONFIG_COMMANDS([guile/sortsmill/strings],[AS_MKDIR_P([guile/sortsmill/strings])])
AC_CONFIG_FILES([guile/sortsmill/math/gsl/error.scm])

FONTFORGE_CONFIG_TESTSUITE
FONTFORGE_CREATE_INDENT_PRO        
FONTFORGE_CREATE_SUMMARY

AC_OUTPUT

#--------------------------------------------------------------------------
