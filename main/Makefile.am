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

include $(top_srcdir)/mk/flags.am

bin_PROGRAMS =
bin_SCRIPTS =

EXTRA_DIST =

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

AM_CPPFLAGS += -I$(top_builddir)/fontforge -I$(top_srcdir)/fontforge
AM_LDFLAGS = -export-dynamic $(MY_CFLAGS) $(MY_LIBS)
LDADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la \
		$(top_builddir)/auxiliary/libsortsmill_aux.la \
		$(top_builddir)/gutils/libsortsmill_gutils.la \
		$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

if THE_PROGRAMS
if HAVE_GUI

bin_PROGRAMS += sortsmill-editor smed
sortsmill_editor_SOURCES = sortsmill-editor.c
sortsmill_editor_LDFLAGS = $(GUILE_LIBS) $(GLIB_LIBS)
sortsmill_editor_LDADD = $(top_builddir)/lib/libgnu.la

if HAVE_PYTHON_API
# Python extensions often depend on the main program to export the
# symbols from libpython, so we should link the main program, no
# matter how trivial it is, with libpython.
sortsmill_editor_LDFLAGS += -export-dynamic $(PYTHON_LIBS)
endif HAVE_PYTHON_API

smed_SOURCES = $(sortsmill_editor_SOURCES)
smed_LDFLAGS = $(sortsmill_editor_LDFLAGS)
smed_LDADD = $(sortsmill_editor_LDADD)

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
