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

include $(top_srcdir)/mk/flags.am
include $(top_srcdir)/mk/guile.mk

POLYSPLINE_PRECOMPUTED_DEGREE_MAX = 19

lib_LTLIBRARIES =

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libsortsmill_aux.la

dist_libsortsmill_aux_la_SOURCES =
nodist_libsortsmill_aux_la_SOURCES =

# Source files inherited from FontForge.
dist_libsortsmill_aux_la_SOURCES += ArabicForms.c alphabet.c	\
	backtrns.c cjk.c ucharmap.c unialt.c ustring.c utype.c		\
	combiners.h

# Recognition of numbers written in PostScript’s notation.
dist_libsortsmill_aux_la_SOURCES += ps_number.c

# Extensions to xalloc and Boehm GC.
dist_libsortsmill_aux_la_SOURCES += xdie_on_null.c xgc.c	\
	xgc_strjoin.c

# Extensions to PCRE.
dist_libsortsmill_aux_la_SOURCES += rexp.c

# Extensions to GNU MP.
dist_libsortsmill_aux_la_SOURCES += gmp_constants.c gmp_gc.c linalg.c

# Extensions to libunistring.
dist_libsortsmill_aux_la_SOURCES += xunicase.c xuniconv.c xuninorm.c	\
	xunistr.c xunistr_strjoin.c

# Polynomial splines.
dist_libsortsmill_aux_la_SOURCES += precomputed_gmp_polyspline_data.c	\
	polyspline_data.c polyspline.c guile_polyspline.c
nodist_libsortsmill_aux_la_SOURCES += precomputed_polyspline_data.c

# Brent’s root-finding algorithm.
dist_libsortsmill_aux_la_SOURCES += brentroot.c qbrentroot.c	\
	guile_brentroot.c

# Miscellaneous C interface to Guile and Guile extensions.
dist_libsortsmill_aux_la_SOURCES += rnrs_conditions.c scm_python.c	\
	guile_gsl.c

libsortsmill_aux_la_CFLAGS = $(AM_CFLAGS) '-I$(srcdir)'						\
	'-D_POLYSPLINE_PRECOMPUTED_DEGREE_MAX=$(POLYSPLINE_PRECOMPUTED_DEGREE_MAX)'
libsortsmill_aux_la_LIBADD = $(top_builddir)/lib/libgnu.la
libsortsmill_aux_la_LDFLAGS = $(call ldflags)
libsortsmill_aux_la_DEPENDENCIES = Makefile $(libsortsmill_aux_la_LIBADD)

LINALG_EXTRAS = linalg_matrix_init.c linalg_matrix_set.c			\
	linalg_matrix_copy.c linalg_matrix_swap.c linalg_matrix_mult.c

linalg.lo: $(LINALG_EXTRAS)

precomputed_polyspline_data.c: precompute-polyspline-data.scm Makefile
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $< $(POLYSPLINE_PRECOMPUTED_DEGREE_MAX) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST = $(LINALG_EXTRAS) precompute-polyspline-data.scm

MOSTLYCLEANFILES = precomputed_polyspline_data.c

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libguile-sortsmill_aux.la
libguile_sortsmill_aux_la_SOURCES = libguile_sortsmill_aux.c
libguile_sortsmill_aux_la_LDFLAGS = -module -release $(VERSION)	\
	$(MY_CFLAGS) $(MY_LIBS)
libguile_sortsmill_aux_la_LIBADD = libsortsmill_aux.la	\
	$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------
#
# Targets for maintainers to use when a new version of Unicode is
# adopted.

generate-utype: LineBreak.txt NamesList.txt PropList.txt UnicodeData.txt makeutype
	LC_ALL=C ./makeutype
	mv utype.h $(top_srcdir)/inc
	@if test '$(top_srcdir)' -ef '$(top_builddir)'; then \
		:; \
	else \
		mv ArabicForms.c unialt.c utype.c $(srcdir); \
		echo mv ArabicForms.c unialt.c utype.c $(srcdir); \
	fi

EXTRA_PROGRAMS = makeutype dump
makeutype_SOURCES = makeutype.c
dump_SOURCES = dump.c

LINEBREAK_URL = http://unicode.org/Public/UNIDATA/LineBreak.txt
NAMESLIST_URL = http://unicode.org/Public/UNIDATA/NamesList.txt
PROPLIST_URL = http://unicode.org/Public/UNIDATA/PropList.txt
UNICODEDATA_URL = http://unicode.org/Public/UNIDATA/UnicodeData.txt

LineBreak.txt:
	$(WGET) '$(LINEBREAK_URL)'

NamesList.txt:
	$(WGET) '$(NAMESLIST_URL)'

PropList.txt:
	$(WGET) '$(PROPLIST_URL)'

UnicodeData.txt:
	$(WGET) '$(UNICODEDATA_URL)'

MOSTLYCLEANFILES += makeutype dump
DISTCLEANFILES = LineBreak.txt NamesList.txt PropList.txt	\
	UnicodeData.txt

#--------------------------------------------------------------------------
