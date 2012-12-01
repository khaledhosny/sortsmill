#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

# Copyright (C) 2000-2012 by George Williams
# Copyright (C) 2012 by Barry Schwartz
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
AC_INIT([Sorts Mill FontForge],
        [2.0.0_alpha1],
        [https://github.com/sortsmill/fontforge/issues],
        [sortsmill-fontforge],
        [https://github.com/sortsmill/fontforge])

AC_CONFIG_SRCDIR([fontforge/splinerefigure.c])

AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([config])        dnl Where things like install-sh go.
AC_CONFIG_TESTDIR([tests],[fontforge]) dnl Test suite setup.
AC_CANONICAL_BUILD                 dnl On what platform are we compiling?
AC_CANONICAL_HOST                  dnl For what platform are we compiling?
AC_CONFIG_HEADERS([inc/config.h])  dnl Where autoheader puts macros.
AC_USE_SYSTEM_EXTENSIONS           dnl Define such macros as _GNU_SOURCE.

AM_INIT_AUTOMAKE([-Wall -Wno-portability -Werror])
AM_MAINTAINER_MODE([enable])

# I am going to try enabling silent-rules by default, so builders are
# more likely to see warnings. Use --disable-silent-rules or "make
# V=1" if you need to see the rules.
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

# Libtool-related configuration.
m4_ifdef([AM_PROG_AR],[AM_PROG_AR]) dnl Support for Microsoft linker.
LT_INIT([dlopen])                   dnl Initialize libtool.
LT_OBJDIR="${lt_cv_objdir}/"
AC_SUBST([LT_OBJDIR])

AC_PATH_XTRA
i_do_have_x="${have_x}"
i_do_have_gui="${i_do_have_x}"
HAVE_GUI=0
if test x"${i_do_have_x}" = xyes; then
   HAVE_GUI=1
fi
AC_DEFINE_UNQUOTED([HAVE_GUI],[${HAVE_GUI}],[Define as 1 if building with the GUI; otherwise define as 0.])
AC_SUBST([HAVE_GUI])

AC_SUBST([HOST],["$host"])
AC_SUBST([MACAPP])

FONTFORGE_PLATFORM_SPECIFICS

VERSION_MAJOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^\([[0-9]][[0-9]]*\).*/\1/'`
VERSION_MINOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
VERSION_PATCH=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
VERSION_EXTRA=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\(.*\)/\1/' \
                                                     -e 's/^_//'`
VERSION_EXTRA_SHORT=`AS_ECHO(["${VERSION_EXTRA}"]) | sed -e 's/alpha/a/' -e 's/beta/b/'`
AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_PATCH])
AC_SUBST([VERSION_EXTRA])
AC_SUBST([VERSION_EXTRA_SHORT])

GUILE_API="2.0"
AC_SUBST([GUILE_API])

#--------------------------------------------------------------------------
#
# URLs for dependencies.

giflib_url="http://giflib.sourceforge.net/"
libpng_url="http://www.libpng.org/"
libspiro_url="http://libspiro.sourceforge.net/"
libunicodenames_url="https://github.com/sortsmill/libunicodenames"

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
AM_PROG_CC_C_O
AC_PROG_CPP
AC_PROG_CXX
AC_PROG_GREP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_MKDIR_P
AC_PROG_SED
PKG_PROG_PKG_CONFIG

# Code quality programs.
AC_PATH_PROG([INDENT],[indent],[indent]) dnl  GNU Indent.
AC_PATH_PROG([TIDY],[tidy],[tidy])       dnl  HTML Tidy.

# Gettext programs.
AC_PATH_PROG([MSGFMT],[msgfmt],[msgfmt])
AC_PATH_PROG([XGETTEXT],[xgettext],[xgettext])

# Freedesktop programs.
AC_PATH_PROG([UPDATE_MIME_DATABASE],[update-mime-database],[update-mime-database])
AC_PATH_PROG([UPDATE_DESKTOP_DATABASE],[update-desktop-database],[update-desktop-database])

# Assorted programs.
AC_PATH_PROG([WGET],[wget],[wget])
AC_PATH_PROG([CYTHON],[cython],[cython])
GUILE_PROGS

#--------------------------------------------------------------------------

FONTFORGE_ARG_ENABLE_REAL

AC_ARG_ENABLE([programs],
        [AS_HELP_STRING([--disable-programs],[do not build "fontforge" and related programs
                                              (but do build libraries and possibly the Python extensions)])],
        [i_do_have_programs="${enableval}"],
        [i_do_have_programs=yes])
AM_CONDITIONAL([THE_PROGRAMS],[test x"${i_do_have_programs}" = xyes])
AC_SUBST([THE_PROGRAMS],["${i_do_have_programs}"])

FONTFORGE_ARG_DISABLE([native-scripting],
        [AS_HELP_STRING([--disable-native-scripting],[disable the legacy fontforge scripting language])],
        [_NO_FFSCRIPT])
AM_CONDITIONAL([NATIVE_SCRIPTING],[test x"${i_do_have_native_scripting}" = xyes])

FONTFORGE_ARG_DISABLE_PYTHON_SCRIPTING

AC_ARG_ENABLE([freetype-debugger],
        [AS_HELP_STRING([--enable-freetype-debugger[[=DIR]]],
                [use internal freetype debugger within fontforge; requires source code of the
                 freetype library with which fontforge will be linked.
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
                [enable a 'tile path' command (a variant of 'expand stroke')])],
        [FONTFORGE_CONFIG_TILEPATH])

#--------------------------------------------------------------------------
#
# Checks for libraries.

FONTFORGE_ATOMIC_OPS
FONTFORGE_CONFIG_X_LIBRARIES
FONTFORGE_LIBPCRE
PKG_CHECK_MODULES([ZLIB],[zlib])
PKG_CHECK_MODULES([GLIB],[glib-2.0 >= 2.6 gio-2.0])
PKG_CHECK_MODULES([LIBPNG],[libpng])
PKG_CHECK_MODULES([LIBXML],[libxml-2.0])
PKG_CHECK_MODULES([GUILE],[guile-${GUILE_API} >= 2.0.6])
PKG_CHECK_MODULES([GSL],[gsl])

# FIXME: Do a more sophisticated GMP check.
AC_CHECK_LIB([gmp],[__gmpz_init],[],
  [AC_MSG_ERROR([${PACKAGE_NAME} requires GNU MP, but it was not found. See http://gmplib.org/])])

PKG_CHECK_MODULES([BOEHM_GC],[bdw-gc >= 7.1])
AC_DEFINE([GC_THREADS],[1],[Define to 1 if Boehm GC has to co-exist with POSIX threads.])

if test x"${i_do_have_gui}" = xyes; then
   # 11.0.5 is the "libtool" version corresponding to 2.4.0 release as reported by pkg-config
   # check doc/VERSION.DLL for mapping between libtool and release versions
   PKG_CHECK_MODULES([FREETYPE],[freetype2 >= 11.0.5])
   PKG_CHECK_MODULES([CAIRO],[cairo >= 1.6])
   PKG_CHECK_MODULES([PANGO],[pango >= 1.10 pangocairo])
   PKG_CHECK_MODULES([XCURS],[xcursor])
fi

if test x"${HAVE_LIBUNISTRING}" != xyes; then
   AC_MSG_ERROR([Cannot find GNU libunistring. ${PACKAGE_NAME} cannot be built without it. See http://www.gnu.org/software/libunistring/])
fi

AC_ARG_VAR([FREETYPE_SOURCE],
        [directory where freetype source code is located for --enable-freetype-debugger;
         however, if --enable-freetype-debugger=DIR is specified, then DIR overrides
         the setting of FREETYPE_SOURCE])
FONTFORGE_ARG_WITH_GIFLIB
FONTFORGE_ARG_WITH_LIBJPEG
FONTFORGE_ARG_WITH_LIBTIFF
FONTFORGE_ARG_WITH_LIBSPIRO
FONTFORGE_ARG_WITH_LIBUNICODENAMES

FONTFORGE_CONFIG_FREETYPE_DEBUGGER

if test x"${i_do_have_python_scripting}" = xyes; then
   if test x"${PYTHON_VERSION}" != x"2.7"; then
      AC_MSG_ERROR([Python support in ${PACKAGE_NAME} requires Python 2.7.x. You are using Python ${PYTHON_VERSION}.])
   fi
   FONTFORGE_REQUIRE_PYTHON_MODULE([gmpy])
   FONTFORGE_PROG_CYTHON_WORKS
fi

#--------------------------------------------------------------------------
#
# Checks for header files.

AC_CHECK_HEADERS([libintl.h limits.h memory.h pthread.h])

#--------------------------------------------------------------------------
#
# Checks for typedefs, structures, and compiler characteristics.

AC_C_TYPEOF
AC_C_BIGENDIAN
AC_HEADER_ASSERT
AC_HEADER_STDBOOL
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT8_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT8_T

# This only checks that an int requires 4 bytes of storage, not that
# it actually uses all 32 bits. We actually require all 32 bits.
AC_CHECK_SIZEOF([int])
test 4 -le ${ac_cv_sizeof_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'int' be at least 32 bits wide.])
AC_CHECK_SIZEOF([unsigned int])
test 4 -le ${ac_cv_sizeof_unsigned_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'unsigned int' be at least 32 bits wide.])

AX_PTHREAD

AC_SUBST([WARNING_CFLAGS])
WARNING_CFLAGS=""
FONTFORGE_COMPILER_FLAGS([WARNING_CFLAGS],
        [-Wunused -Wreturn-type
         -Wparentheses -Wformat -Wchar-subscripts
         -Wno-unused-but-set-variable -Wno-unused-result
         -Werror=missing-prototypes
         -Werror=implicit])

## Some clang flags to turn off for now.
#FONTFORGE_COMPILER_FLAGS([WARNING_CFLAGS],
#        [-Wno-switch -Wno-tautological-compare -Wno-conversion])

AC_SUBST([CYTHON_EXTRA_CFLAGS])
CYTHON_EXTRA_CFLAGS=""
FONTFORGE_COMPILER_FLAGS([CYTHON_EXTRA_CFLAGS],
        [-Wno-unused -fwrapv -fno-strict-aliasing])

AC_SUBST([VISIBILITY_DEFAULT_EXTRA_CFLAGS])
VISIBILITY_DEFAULT_EXTRA_CFLAGS=""
FONTFORGE_COMPILER_FLAGS([VISIBILITY_DEFAULT_EXTRA_CFLAGS],
        [-fvisibility=default])

FONTFORGE_C_CONST_FLT_EPSILON
AC_SUBST([C_CONST_FLT_EPSILON],["${fontforge_cv_c_const_flt_epsilon}"])
FONTFORGE_C_CONST_DBL_EPSILON
AC_SUBST([C_CONST_DBL_EPSILON],["${fontforge_cv_c_const_dbl_epsilon}"])

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

FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UCS4LE],[UCS-4LE ucs-4le UCS4LE ucs4le UTF-32LE utf-32le UTF32LE utf32le])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UCS4BE],[UCS-4BE ucs-4be UCS4BE ucs4be UTF-32BE utf-32be UTF32BE utf32be])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UTF8],[UTF-8 csUTF8 utf-8 UTF8 utf8])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_UTF7],[UNICODE-1-1-UTF-7 csUnicode11UTF7 UTF-7 utf-7 UTF7 utf7])
FONTFORGE_ICONV_CHOOSE_ENCODING([MY_ICONV_MAC],[MACINTOSH macintosh MAC mac])

FONTFORGE_ICONV_TOCODE_SUFFIX([MY_ICONV_SUFFIX_TRANSLIT],[//TRANSLIT],[ISO-8859-1],[[$]{MY_ICONV_UTF8}])
FONTFORGE_ICONV_TOCODE_SUFFIX([MY_ICONV_SUFFIX_IGNORE],[//IGNORE],[ISO-8859-1],[[$]{MY_ICONV_UTF8}])

AH_BOTTOM([#include <config-extra.h>])

#--------------------------------------------------------------------------

AM_CONDITIONAL([HAVE_PYTHON_SCRIPTING],[test x"${i_do_have_python_scripting}" = xyes])
AM_CONDITIONAL([HAVE_GUI],[test x"${i_do_have_gui}" = xyes])
AM_CONDITIONAL([MACINTOSH],[test x"${gww_ismac}" = xyes])

# The following conditionals are always false, so the directories
# never get built by default. Nevertheless, they will be included by
# "make dist".
AM_CONDITIONAL([FONTTOOLS],[test x != x])

FONTFORGE_OBSCURE_CONFIGURATION

GETTEXT_PACKAGE=$PACKAGE
AC_SUBST(GETTEXT_PACKAGE)
IT_PROG_INTLTOOL(0.41)
AM_GLIB_GNU_GETTEXT

FONTFORGE_SET_MY_CFLAGS
FONTFORGE_SET_MY_LIBS
FONTFORGE_CREATE_MAKEFILES
FONTFORGE_CREATE_INDENT_PRO
AC_CONFIG_FILES([inc/config-extra.h])
AC_CONFIG_FILES([inc/sortsmillff/cython/config.pxi])
AC_CONFIG_FILES([python/fontforge.py])
AC_CONFIG_FILES([python/psMat.py])
AC_CONFIG_FILES([python/sortsmillff/__init__.py])
AC_CONFIG_FILES([python/sortsmillff/fontforge_compat.py])
AC_CONFIG_FILES([python/sortsmillff/legacy/__init__.py])
AC_CONFIG_COMMANDS([scheme/sortsmillff],[AS_MKDIR_P([scheme/sortsmillff])])
FONTFORGE_CONFIG_TESTSUITE
FONTFORGE_CREATE_SUMMARY

AC_OUTPUT

#--------------------------------------------------------------------------
