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

libsortsmillff_aux_la_SOURCES = ArabicForms.c alphabet.c backtrns.c	\
	char.c cjk.c ucharmap.c unialt.c ustring.c utype.c usprintf.c	\
	combiners.h xdie_on_null.c xgc.c

EXPORTED_XDIE_ON_NULL_SYMBOLS = xdie_on_null xdie_on_enomem

EXPORTED_XGC_SYMBOLS = x_gc_malloc_atomic x_gc_malloc_uncollectable	\
	x_gc_realloc x_gc_malloc_ignore_off_page						\
	x_gc_malloc_atomic_ignore_off_page x_gc_malloc_stubborn			\
	x_gc_strdup x_gc_grabstr x_gc_u8_grabstr x_gc_u16_grabstr		\
	x_gc_u32_grabstr

EXPORTED_AUX_SYMBOLS = $(EXPORTED_XDIE_ON_NULL_SYMBOLS)					\
	$(EXPORTED_XGC_SYMBOLS) AllAscii ArabicForms StripToASCII			\
	____codepointassigned ____tolower ____tomirror ____toupper			\
	____utype ____utype2 alphabets_from_unicode big5_from_unicode		\
	c_to_u cu_copy cu_copyn cu_strcat cu_strcpy cu_strncpy				\
	cu_strstartmatch def2u_copy def2u_strncpy def2utf8_copy				\
	gb2312_from_unicode iconv_local_encoding_name jis_from_unicode		\
	ksc5601_from_unicode latin1_2_utf8_copy local_encoding strmatch		\
	strnmatch strstrmatch u2def_copy u2def_strncpy u2utf8_copy			\
	u2utf8_copyn u2utf8_strcpy uAllAscii u_copy u_copyn u_endswith		\
	u_sprintf u_strcat u_strchr u_strcmp u_strcpy u_strlen u_strmatch	\
	u_strncmp u_strncpy u_strnmatch u_strrchr u_strstartmatch			\
	u_strstrmatch u_strtod u_strtol u_strtoul u_to_c u_vsnprintf		\
	uc_copy uc_copyn uc_strcat uc_strcmp uc_strcpy uc_strmatch			\
	uc_strncat uc_strncmp uc_strncpy uc_strstr uc_strstrmatch			\
	unicode_alternates unicode_from_alphabets unicode_from_big5			\
	unicode_from_big5hkscs unicode_from_gb2312 unicode_from_jis201		\
	unicode_from_jis208 unicode_from_jis212 unicode_from_johab			\
	unicode_from_ksc5601 utf82U_strncpy utf82def_copy utf82u_copy		\
	utf82u_copyn utf82u_strcat utf82u_strcpy utf82u_strlen				\
	utf8_2_latin1_copy utf8_idpb utf8_ildb utf8_strchr utf8_strlen		\
	utf8_strncpy utf8_truncatevalid utf8_valid x_gc_malloc				\
	libsortsmillff_aux_la_CPPFLAGS = $(AM_CPPFLAGS)
libsortsmillff_aux_la_LIBADD = $(top_builddir)/lib/libgnu.la
libsortsmillff_aux_la_LDFLAGS = $(call ldflags,EXPORTED_AUX_SYMBOLS)
libsortsmillff_aux_la_DEPENDENCIES = Makefile $(libsortsmillff_aux_la_LIBADD)

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
