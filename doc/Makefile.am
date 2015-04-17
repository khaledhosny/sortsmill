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

#--------------------------------------------------------------------------

dist_man_MANS =
nodist_man_MANS =

DISTCLEANFILES = $(nodist_man_MANS)

dist_info_TEXINFOS = sortsmill-tools.texi
sortsmill_tools_TEXINFOS = psMat.interface.texi	\
	ps_number.visible.texi

if THE_PROGRAMS

dist_man_MANS += sortsmill-lint.1 sortsmill-image.1 sortsmill-compare.1
nodist_man_MANS += smlint.1 smimage.1 smcompare.1

smlint.1: Makefile
	echo '.so man1/sortsmill-lint.1' > $@

smimage.1: Makefile
	echo '.so man1/sortsmill-image.1' > $@

smcompare.1: Makefile
	echo '.so man1/sortsmill-compare.1' > $@

if HAVE_GUI

dist_man_MANS += sortsmill-editor.1
nodist_man_MANS += smed.1

smed.1: Makefile
	echo '.so man1/sortsmill-editor.1' > $@

endif HAVE_GUI

endif THE_PROGRAMS
#--------------------------------------------------------------------------


-include $(top_srcdir)/git.mk
