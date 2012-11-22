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

#--------------------------------------------------------------------------

lib_LTLIBRARIES = libsortsmillff_aux.la

dist_libsortsmillff_aux_la_SOURCES = ArabicForms.c alphabet.c		\
	backtrns.c cjk.c ucharmap.c unialt.c ustring.c utype.c			\
	combiners.h xdie_on_null.c xgc.c xgc_strjoin.c xunicase.c		\
	xuniconv.c xuninorm.c xunistr.c xunistr_strjoin.c ps_number.c	\
	rexp.c precomputed_data.c polyspline.c brentroot.c qbrentroot.c	\
	gmp_constants.c linalg.c

libsortsmillff_aux_la_CFLAGS = $(AM_CFLAGS) -I$(srcdir)
libsortsmillff_aux_la_LIBADD = $(top_builddir)/lib/libgnu.la
libsortsmillff_aux_la_LDFLAGS = $(call ldflags)
libsortsmillff_aux_la_DEPENDENCIES = Makefile $(libsortsmillff_aux_la_LIBADD)

LINALG_EXTRAS = linalg_matrix_init.c linalg_matrix_set.c			\
	linalg_matrix_copy.c linalg_matrix_swap.c linalg_matrix_mult.c

linalg.lo: $(LINALG_EXTRAS)

EXTRA_DIST = $(LINALG_EXTRAS)

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

# I am not sure that makebuildtables is of much use anymore.
# Is the source file it generates now to be maintained by hand?
#
#EXTRA_PROGRAMS += makebuildtables
#makebuildtables_SOURCES = makebuildtables.c
#makebuildtables_LDADD = libsortsmillff_aux.la

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

MOSTLYCLEANFILES = makeutype dump makebuildtables
DISTCLEANFILES = LineBreak.txt NamesList.txt PropList.txt	\
	UnicodeData.txt

#--------------------------------------------------------------------------
