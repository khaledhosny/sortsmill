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
AM_LDFLAGS = -export-dynamic -dlopen -lsortsmill-core
LDADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la	\
		$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

if THE_PROGRAMS
if HAVE_GUI

bin_PROGRAMS += sortsmill-editor smed
sortsmill_editor_SOURCES = sortsmill-editor.c
smed_SOURCES = sortsmill-editor.c

endif HAVE_GUI
endif THE_PROGRAMS

#--------------------------------------------------------------------------

if THE_PROGRAMS

bin_PROGRAMS += sortsmill-lint sortsmill-compare
bin_PROGRAMS += smlint smcompare

dist_sortsmill_lint_SOURCES = lint.c
dist_smlint_SOURCES = lint.c

dist_sortsmill_compare_SOURCES = compare.c
dist_smcompare_SOURCES = compare.c

endif THE_PROGRAMS

#--------------------------------------------------------------------------

-include $(top_srcdir)/git.mk
