# Copyright (C) 2012 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/mk/flags.am
include $(top_srcdir)/mk/guile.mk

lib_LTLIBRARIES = libguile-sortsmillff_iconv.la

dist_guileobjmodule_DATA = iconv.scm
nodist_guileobjmodule_DATA = iconv.go

libguile_sortsmillff_iconv_la_SOURCES = guile_iconv.c
libguile_sortsmillff_iconv_la_LIBADD = $(top_builddir)/lib/libgnu.la

AM_LDFLAGS = -module $(MY_CFLAGS) $(MY_LIBS)

install-data-hook:
#	Make sure the source module is no younger than the compiled
#	module.
	touch $(addprefix $(DESTDIR)$(guileobjmoduledir)/, $(nodist_guileobjmodule_DATA))

MOSTLYCLEANFILES = iconv.go
