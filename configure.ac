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

AC_CONFIG_MACRO_DIR([m4])          dnl Where autoconf scripts go.
AC_CONFIG_AUX_DIR([config])        dnl Where things like install-sh go.
AC_CONFIG_TESTDIR([tests],[fontforge]) dnl Test suite setup.
AC_CANONICAL_BUILD                 dnl On what platform are we compiling?
AC_CANONICAL_HOST                  dnl For what platform are we compiling?
AC_CONFIG_HEADERS([inc/config.h])  dnl Where autoheader puts macros.
AC_USE_SYSTEM_EXTENSIONS           dnl Define such macros as _GNU_SOURCE.

AM_INIT_AUTOMAKE([subdir-objects -Wall -Wno-portability -Werror])
AM_MAINTAINER_MODE([enable])

# I am going to try enabling silent-rules by default, so builders are
# more likely to see warnings. Use --disable-silent-rules or "make
# V=1" if you need to see the rules.
m4_ifdef([AM_SILENT_RULES],[AM_SILENT_RULES([yes])])

# Libtool-related configuration.
m4_ifdef([AM_PROG_AR],[AM_PROG_AR]) dnl Support for Microsoft linker.
LT_CONFIG_LTDL_DIR([libltdl])       dnl Where libltdl source code goes.
LT_INIT([dlopen])                   dnl Initialize libtool.
LTDL_INIT([nonrecursive])           dnl Initialize libltdl.
FONTFORGE_CHECK_LTDL_VERSION        dnl Check that libltdl is recent enough.

AC_PATH_XTRA
i_do_have_x="${have_x}"
i_do_have_gui="${i_do_have_x}"

AC_SUBST([HOST],["$host"])
AC_SUBST([MACAPP])

FONTFORGE_PLATFORM_SPECIFICS

VERSION_MAJOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^\([[0-9]][[0-9]]*\).*/\1/'`
VERSION_MINOR=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
VERSION_PATCH=`AS_ECHO(["${PACKAGE_VERSION}"]) | sed -e 's/^[[0-9]][[0-9]]*\.[[0-9]][[0-9]]*\.\([[0-9]][[0-9]]*\).*/\1/'`
AC_SUBST([VERSION_MAJOR])
AC_SUBST([VERSION_MINOR])
AC_SUBST([VERSION_PATCH])

#--------------------------------------------------------------------------
#
# URLs for dependencies.

freetype_url="http://www.freetype.org/"
giflib_url="http://giflib.sourceforge.net/"
libpng_url="http://www.libpng.org/"
libxml_url="http://www.xmlsoft.org/"
libspiro_url="http://libspiro.sourceforge.net/"
libunicodenames_url="https://github.com/chemoelectric/libunicodenames"

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
AC_PATH_PROG([MY_SORT],[sort],[cat])
AC_PATH_PROG([TR],[tr],[tr])
AC_PATH_PROG([MSGFMT],[msgfmt],[msgfmt])
AC_PATH_PROG([XGETTEXT],[xgettext],[xgettext])
AC_PATH_PROG([CPROTO],[cproto],[cproto])
AC_PATH_PROG([INDENT],[indent],[indent])
AC_PATH_PROG([UNZIP],[unzip],[unzip])
AC_PATH_PROG([WGET],[wget],[wget])
AC_PATH_PROG([UPDATE_MIME_DATABASE],[update-mime-database],[update-mime-database])
AC_PATH_PROG([UPDATE_DESKTOP_DATABASE],[update-desktop-database],[update-desktop-database])
AC_PATH_PROG([PLUTIL],[plutil],[:])

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
FONTFORGE_ARG_DISABLE_PYTHON_EXTENSION

AC_ARG_ENABLE([freetype-debugger],
        [AS_HELP_STRING([--enable-freetype-debugger[[=DIR]]],
                [use freetype's internal debugger within fontforge; requires source code of the
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

# Enable this experimental feature via an obscure cache variable
# rather than --enable.
AC_CACHE_CHECK([whether to enable native callbacks (experimental)],
               [fontforge_cv_native_callbacks],
               [if test x"${fontforge_cv_native_callbacks}" != yes; then
                   fontforge_cv_native_callbacks=no
                fi])
i_do_have_native_callbacks="${fontforge_cv_native_callbacks}"


#--------------------------------------------------------------------------
#
# Checks for libraries.

AC_SEARCH_LIBS([cos],[m])
FONTFORGE_CONFIG_X_LIBRARIES
PKG_CHECK_MODULES([ZLIB],[zlib])
PKG_CHECK_MODULES([GLIB],[glib-2.0 >= 2.6])
PKG_CHECK_MODULES([GUILE],[guile-2.0])

PKG_CHECK_MODULES([BOEHM_GC],[bdw-gc >= 7.1])
AC_DEFINE([GC_THREADS],[1],[Define to 1 if Boehm GC has to co-exist with POSIX threads.])

if test x"${i_do_have_gui}" = xyes; then
   PKG_CHECK_MODULES([CAIRO],[cairo >= 1.6])
   PKG_CHECK_MODULES([PANGO],[pango >= 1.10 pangocairo])
fi

FONTFORGE_ARG_WITH_FREETYPE
AC_ARG_VAR([FREETYPE_SOURCE],
        [directory where freetype source code is located for --enable-freetype-debugger;
         however, if --enable-freetype-debugger=DIR is specified, then DIR overrides
         the setting of FREETYPE_SOURCE])
FONTFORGE_ARG_WITH_GIFLIB
FONTFORGE_ARG_WITH_LIBJPEG
FONTFORGE_ARG_WITH_LIBPNG
FONTFORGE_ARG_WITH_LIBTIFF
FONTFORGE_ARG_WITH_LIBXML
FONTFORGE_ARG_WITH_LIBSPIRO
FONTFORGE_ARG_WITH_LIBUNICODENAMES

FONTFORGE_CONFIG_FREETYPE_DEBUGGER

#--------------------------------------------------------------------------
#
# Checks for header files.

AC_CHECK_HEADERS([libintl.h limits.h memory.h pthread.h])

#--------------------------------------------------------------------------
#
# Checks for typedefs, structures, and compiler characteristics.

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

AC_TYPE_LONG_LONG_INT
if test x"${ac_cv_type_long_long_int}" = xyes; then
   AC_DEFINE([_HAS_LONGLONG],1,[Define if using 'long long int'.])
fi

# This only checks that an int requires 4 bytes of storage, not that
# it actually uses all 32 bits. We actually require all 32 bits.
AC_CHECK_SIZEOF([int])
test 4 -le ${ac_cv_sizeof_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'int' be at least 32 bits wide.])
AC_CHECK_SIZEOF([unsigned int])
test 4 -le ${ac_cv_sizeof_unsigned_int} || \
     AC_MSG_ERROR([${PACKAGE_NAME} requires that 'unsigned int' be at least 32 bits wide.])

AX_PTHREAD

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

#--------------------------------------------------------------------------
# Checks for library functions.

AC_FUNC_ERROR_AT_LINE
AC_FUNC_FORK
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_MMAP
if test x"${ac_cv_func_mmap_fixed_mapped}" != xyes; then
   AC_DEFINE([_NO_MMAP],1,[Define if not using mmap.])
fi
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_FUNC_STRTOD
AC_CHECK_FUNCS([alarm atexit dup2 endpwent floor])
AC_CHECK_FUNCS([getcwd gethostbyname gettimeofday])
AC_CHECK_FUNCS([localeconv memmove memset mkdir munmap])
AC_CHECK_FUNCS([nl_langinfo pow putenv rint rmdir])
AC_CHECK_FUNCS([select setenv setlocale snprintf socket sqrt])
AC_CHECK_FUNCS([strcasecmp strchr strdup strerror])
AC_CHECK_FUNCS([strncasecmp strpbrk strrchr strstr])
AC_CHECK_FUNCS([strtol strtoul tzset uname])
AC_CHECK_FUNC([snprintf],[:],[AC_DEFINE([_NO_SNPRINTF],1,[Define if not using snprintf.])])
AC_CHECK_FUNC([tzset],[:],[AC_DEFINE([_NO_TZSET],1,[Define if not using tzset.])])

#--------------------------------------------------------------------------

AM_CONDITIONAL([GRAPHICAL_USER_INTERFACE],[test x"${i_do_have_gui}" = xyes])
AM_CONDITIONAL([MACINTOSH],[test x"${gww_ismac}" = xyes])

# The following conditionals are always false, so the directories
# never get built by default. Nevertheless, they will be included by
# "make dist".
AM_CONDITIONAL([DESKTOP],[test x != x])
AM_CONDITIONAL([FONTTOOLS],[test x != x])

FONTFORGE_SET_MY_CFLAGS
FONTFORGE_SET_MY_LIBS
FONTFORGE_CREATE_MAKEFILES
FONTFORGE_CREATE_PKGCONFIG_FILES
FONTFORGE_CREATE_INDENT_PRO
AC_CONFIG_FILES([pycontrib/site_init.py])
AC_CONFIG_FILES([fontforge/darwinsetup],[chmod +x fontforge/darwinsetup])
FONTFORGE_CONFIG_TESTSUITE
FONTFORGE_CREATE_SUMMARY

AC_OUTPUT

#--------------------------------------------------------------------------
