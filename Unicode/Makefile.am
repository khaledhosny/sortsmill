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

AM_CPPFLAGS = "-I$(top_builddir)/lib" "-I$(top_srcdir)/lib"			\
		"-I$(top_builddir)/inc" "-I$(top_srcdir)/inc" $(MY_CFLAGS)

#--------------------------------------------------------------------------

lib_LTLIBRARIES = libgunicode.la

libgunicode_la_SOURCES = ArabicForms.c alphabet.c backtrns.c char.c		\
	 cjk.c memory.c ucharmap.c unialt.c ustring.c utype.c usprintf.c	\
	 gwwiconv.c combiners.h

libgunicode_la_LIBADD = $(top_builddir)/lib/libgnu.la

# Using -release instead of -version-info seems appropriate in the
# absence of a well defined ABI.
libgunicode_la_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS) -release $(VERSION)	\
	$(LTLIBINTL) -export-symbols $(srcdir)/libgunicode.sym

EXTRA_DIST = $(srcdir)/libgunicode.sym

#--------------------------------------------------------------------------

if MAINTAINER_TOOLS

generate-utype: LineBreak.txt NamesList.txt PropList.txt UnicodeData.txt makeutype
	LC_ALL=C ./makeutype
	mv utype.h $(top_srcdir)/inc
	@if test '$(top_srcdir)' -ef '$(top_builddir)'; then \
		:; \
	else \
		mv ArabicForms.c unialt.c utype.c $(srcdir); \
		echo mv ArabicForms.c unialt.c utype.c $(srcdir); \
	fi

EXTRA_PROGRAMS = makeutype
makeutype_SOURCES = makeutype.c

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

MOSTLYCLEANFILES = makeutype
DISTCLEANFILES = LineBreak.txt NamesList.txt PropList.txt	\
	UnicodeData.txt

endif MAINTAINER_TOOLS

#--------------------------------------------------------------------------

