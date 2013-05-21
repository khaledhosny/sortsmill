# Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
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
@includemk@ $(top_srcdir)/mk/fortran.mk
@includemk@ $(top_srcdir)/mk/guile.mk

lib_LTLIBRARIES = libsortsmill_fortran_api.la

libsortsmill_fortran_api_la_SOURCES =
nodist_libsortsmill_fortran_api_la_SOURCES = fontforge_api.f90

FORTRAN_MODULES = sortsmill_fontforge_api

fontforge_api.f90: $(top_builddir)/fontforge/fontforge.types.apii	\
		$(srcdir)/generate-fortran-api.scm							\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-fortran-api.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST = generate-fortran-api.scm

CLEANFILES = fontforge_api.f90

install-data-hook: install-module-files
uninstall-local: uninstall-module-files
mostlyclean-local: mostlyclean-module-files

if HAVE_FORTRAN_MODFILES

# Fortran module files are treated in this makefile as a byproduct of
# the production of targets. They do not appear in rules. Thus hooks
# are needed to install, uninstall, and clean them.

FORTRAN_MODFILES = $(foreach module,$(FORTRAN_MODULES),$(call modfile,$(module)))

install-module-files:
	$(INSTALL_DATA) $(FORTRAN_MODFILES) $(DESTDIR)/$(fcmoduleincludedir)

uninstall-module-files:
	@for f in $(FORTRAN_MODFILES); do 						\
		echo rm -f $(DESTDIR)/$(fcmoduleincludedir)/$${f};	\
		rm -f $(DESTDIR)/$(fcmoduleincludedir)/$${f};		\
	done

mostlyclean-module-files:
	-rm -f $(FORTRAN_MODFILES)

else !HAVE_FORTRAN_MODFILES

install-module-files:

uninstall-module-files:

mostlyclean-module-files:

endif !HAVE_FORTRAN_MODFILES
