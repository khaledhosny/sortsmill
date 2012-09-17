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

lib_LTLIBRARIES = libgunicode.la

libgunicode_la_SOURCES = ArabicForms.c alphabet.c backtrns.c char.c	\
	 cjk.c ucharmap.c unialt.c ustring.c utype.c usprintf.c			\
	 gwwiconv.c combiners.h
EXTRA_libgunicode_la_SOURCES = memory.c

libgunicode_la_CPPFLAGS = "-I$(top_builddir)/inc"	\
	"-I$(top_srcdir)/inc" $(MY_CFLAGS)

libgunicode_la_LIBADD = memory.lo $(top_builddir)/lib/libgnu.la

# Using -release instead of -version-info seems appropriate in the
# absence of a well defined ABI.
libgunicode_la_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS) -release $(VERSION)	\
	$(LTLIBINTL) -export-symbols $(srcdir)/libgunicode.sym

# For now compile this separately, with Gnulib support, until we have
# "#include <config.h>" at the tops of all our sources and so can use
# Gnulib headers freely.
memory.lo: $(srcdir)/memory.c
	$(LIBTOOL) --mode=compile $(CC) -g -DHAVE_CONFIG_H -c -o			\
		memory.lo $(AM_CPPFLAGS) $(libgunicode_la_CPPFLAGS)				\
		-I$(top_builddir)/lib -I$(top_srcdir)/lib $(srcdir)/memory.c

EXTRA_DIST = $(srcdir)/libgunicode.sym
