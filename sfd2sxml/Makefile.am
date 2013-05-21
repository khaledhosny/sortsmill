# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

include $(top_srcdir)/mk/flags.am
@includemk@ $(top_srcdir)/mk/guile.mk

nodist_bin_SCRIPTS = sfd2sxml sfd2xml

EXTRA_DIST = sfd2sxml.in sfd2xml.in

%: %.in
	$(AM_V_GEN)
	$(AM_V_at)$(CONFIGURE_SCHEME) < $< > $@
	chmod +x $@

CLEANFILES = $(nodist_bin_SCRIPTS)
