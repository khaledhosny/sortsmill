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
include $(top_srcdir)/mk/fortran.mk
include $(top_srcdir)/mk/guile.mk

lib_LTLIBRARIES = libsortsmillff_fortran_api.la

libsortsmillff_fortran_api_la_SOURCES = usermenu.f90
nodist_libsortsmillff_fortran_api_la_SOURCES = fontforge_api.f90

FORTRAN_MODULES = sortsmillff_fontforge_api sortsmillff_usermenu

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
