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
include $(top_srcdir)/mk/guile.mk

#--------------------------------------------------------------------------

nobase_dist_guilemodule_DATA = $(DIST_SCM_FILES)
nobase_nodist_guilemodule_DATA = $(NODIST_SCM_FILES)
EXTRA_DIST = $(DIST_SCM_IN_FILES)
MOSTLYCLEANFILES = $(NODIST_SCM_FILES) $(GO_FILES)

DIST_SCM_FILES = sortsmillff/gsl.scm sortsmillff/iconv.scm	\
	sortsmillff/linalg.scm sortsmillff/precompute.scm		\
	sortsmillff/polyspline.scm sortsmillff/brentroot.scm	\
	sortsmillff/sfd-to-sxml.scm sortsmillff/view.scm		\
	sortsmillff/internal-structures.scm
if HAVE_GUI
DIST_SCM_FILES += sortsmillff/usermenu.scm
endif HAVE_GUI

DIST_SCM_IN_FILES = sortsmillff/math-constants.scm.in	\
	sortsmillff/i18n.scm.in

NODIST_SCM_FILES = sortsmillff/math-constants.scm	\
	sortsmillff/i18n.scm

GO_FILES = sortsmillff/gsl.go sortsmillff/math-constants.go			\
	sortsmillff/i18n.go sortsmillff/iconv.go sortsmillff/linalg.go	\
	sortsmillff/precompute.go sortsmillff/polyspline.go				\
	sortsmillff/brentroot.go sortsmillff/sfd-to-sxml.go				\
	sortsmillff/view.go sortsmillff/internal-structures.go
if HAVE_GUI
GO_FILES += sortsmillff/usermenu.go
endif HAVE_GUI

# The Guile object files do get installed, but by an install hook
# elsewhere in this Makefile.am.
noinst_DATA = $(GO_FILES)

sortsmillff/brentroot.go: sortsmillff/math-constants.go
sortsmillff/i18n.go: libguile-sortsmillff_iconv.la
sortsmillff/gsl.go: libguile-sortsmillff_gsl.la
sortsmillff/precompute.go: sortsmillff/linalg.go
sortsmillff/sfd-to-sxml.go: sortsmillff/gsl.go sortsmillff/i18n.go

#--------------------------------------------------------------------------

nodist_pkgguiledata_DATA = site-init.scm

if HAVE_PYTHON_SCRIPTING
have_python_scripting = t
else !HAVE_PYTHON_SCRIPTING
have_python_scripting = f
endif !HAVE_PYTHON_SCRIPTING

site-init.scm: site-init.scm.in
	$(AM_V_GEN)
	$(AM_V_at)$(SED) 													\
		-e 's|@''PACKAGE''@|$(PACKAGE)|g' 								\
		-e 's|@''sysconfdir''@|$(sysconfdir)|g'							\
		-e 's|@''pkgpythondatadir''@|$(pkgpythondatadir)|g'				\
		-e 's|@''have_python_scripting''@|$(have_python_scripting)|g'	\
		< $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += site-init.scm.in
MOSTLYCLEANFILES += site-init.scm

#--------------------------------------------------------------------------

AM_LDFLAGS = -module $(MY_CFLAGS) $(MY_LIBS)

lib_LTLIBRARIES =

lib_LTLIBRARIES += libguile-sortsmillff_iconv.la
libguile_sortsmillff_iconv_la_SOURCES = guile_iconv.c
libguile_sortsmillff_iconv_la_LIBADD = $(top_builddir)/lib/libgnu.la

lib_LTLIBRARIES += libguile-sortsmillff_aux.la
libguile_sortsmillff_aux_la_SOURCES = guile_polyspline.c	\
	guile_brentroot.c guile_gsl.c
libguile_sortsmillff_aux_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/lib/libgnu.la

lib_LTLIBRARIES += libguile-sortsmillff_fontforge.la
libguile_sortsmillff_fontforge_la_SOURCES =		\
	guile_internal_structures.c
libguile_sortsmillff_fontforge_la_CFLAGS = $(AM_CFLAGS)			\
	'-I$(top_builddir)/fontforge' '-I$(top_srcdir)/fontforge'
libguile_sortsmillff_fontforge_la_LIBADD =					\
	$(top_builddir)/fontforge/libsortsmillff_fontforge.la	\
	$(top_builddir)/gdraw/libsortsmillff_gdraw.la			\
	$(top_builddir)/gutils/libsortsmillff_gutils.la			\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la			\
	$(top_builddir)/lib/libgnu.la

if HAVE_GUI
lib_LTLIBRARIES += libguile-sortsmillff_fontforgeexe.la
libguile_sortsmillff_fontforgeexe_la_SOURCES = guile_usermenu.c
libguile_sortsmillff_fontforgeexe_la_LIBADD =					\
	$(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la	\
	$(top_builddir)/fontforge/libsortsmillff_fontforge.la		\
	$(top_builddir)/gdraw/libsortsmillff_gdraw.la				\
	$(top_builddir)/gutils/libsortsmillff_gutils.la				\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la				\
	$(top_builddir)/lib/libgnu.la
endif HAVE_GUI

#--------------------------------------------------------------------------

install-data-hook:
#	Make sure the source module is no younger than the compiled
#	module.
	@for f in $(GO_FILES); do												\
		if test -f "$${f}"; then 											\
			if test -f $(DESTDIR)$(guileobjmoduledir)/"$${f}"; then			\
				echo "rm -f $(DESTDIR)$(guileobjmoduledir)/$${f}";			\
				rm -f $(DESTDIR)$(guileobjmoduledir)/"$${f}";				\
			fi;																\
			echo "cat $${f} > $(DESTDIR)$(guileobjmoduledir)/$${f}";		\
			cat "$${f}" > $(DESTDIR)$(guileobjmoduledir)/"$${f}";			\
			chmod 0644 $(DESTDIR)$(guileobjmoduledir)/"$${f}";				\
		fi																	\
	done

uninstall-local:
	@for f in $(GO_FILES); do												\
		if test -f $(DESTDIR)$(guileobjmoduledir)/"$${f}"; then 			\
			echo 'rm -f $(DESTDIR)$(guileobjmoduledir)/'"$${f}";			\
			rm -f $(DESTDIR)$(guileobjmoduledir)/"$${f}";					\
		fi																	\
	done

#--------------------------------------------------------------------------
