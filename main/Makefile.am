# Copyright (C) 2000-2012 by George Williams
# Copyright (C) 2012, 2013 by Barry Schwartz
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

bin_PROGRAMS =

EXTRA_DIST =

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

AM_CPPFLAGS += -I$(top_builddir)/fontforge -I$(top_srcdir)/fontforge
AM_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS)
LDADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la \
		$(top_builddir)/auxiliary/libsortsmill_aux.la \
		$(top_builddir)/gutils/libsortsmill_gutils.la \
		$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

if THE_PROGRAMS
if HAVE_GUI

bin_PROGRAMS += sortsmill-editor smed

dist_sortsmill_editor_SOURCES = editor.c
sortsmill_editor_LDADD =									\
	$(top_builddir)/fontforge/libsortsmill_fontforgeexe.la	\
	$(top_builddir)/lib/libgnu.la
sortsmill_editor_LDFLAGS = $(AM_LDFLAGS) -export-dynamic

dist_smed_SOURCES = $(dist_sortsmill_editor_SOURCES)
smed_LDADD = $(sortsmill_editor_LDADD)
smed = $(sortsmill_editor_LDFLAGS)

endif HAVE_GUI
endif THE_PROGRAMS

#--------------------------------------------------------------------------

if THE_PROGRAMS

bin_PROGRAMS += sortsmill-lint sortsmill-image sortsmill-compare
bin_PROGRAMS += smlint smimage smcompare

dist_sortsmill_lint_SOURCES = $(srcdir)/lint.c
dist_smlint_SOURCES = $(dist_sortsmill_lint_SOURCES)

dist_sortsmill_image_SOURCES = $(srcdir)/image.c
dist_smimage_SOURCES = $(dist_sortsmill_image_SOURCES)

dist_sortsmill_compare_SOURCES = $(srcdir)/compare.c
dist_smcompare_SOURCES = $(dist_sortsmill_compare_SOURCES)

endif THE_PROGRAMS

#--------------------------------------------------------------------------
