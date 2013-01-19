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
include $(top_srcdir)/mk/scheme.mk
include $(top_srcdir)/mk/tig.mk

FFINCLDIRS = '-I$(top_builddir)/fontforge'		\
	'-I$(top_srcdir)/fontforge'

EXTRA_DIST =
MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST += tig.scm

#--------------------------------------------------------------------------

DIST_SCM_FILES =
DIST_SCM_IN_FILES =
NODIST_SCM_FILES =

nobase_dist_guilemodule_DATA = $(DIST_SCM_FILES)
nobase_nodist_guilemodule_DATA = $(NODIST_SCM_FILES)
EXTRA_DIST += $(DIST_SCM_IN_FILES)
MOSTLYCLEANFILES += $(NODIST_SCM_FILES)

DIST_SCM_FILES += sortsmillff/alloc.scm sortsmillff/gsl.scm	\
	sortsmillff/iconv.scm sortsmillff/linalg.scm			\
	sortsmillff/polyspline.scm sortsmillff/brentroot.scm	\
	sortsmillff/sfd-to-sxml.scm sortsmillff/views.scm		\
	sortsmillff/api-syntax.scm sortsmillff/notices.scm		\
	sortsmillff/strings.scm
#sortsmillff/precompute.scm
if HAVE_GUI
DIST_SCM_FILES += sortsmillff/usermenu.scm
NODIST_SCM_FILES += sortsmillff/gdraw-api.scm
endif HAVE_GUI

DIST_SCM_IN_FILES += sortsmillff/machine.scm.in						\
	sortsmillff/pkg-info.scm.in sortsmillff/math-constants.scm.in	\
	sortsmillff/i18n.scm.in sortsmillff/python.scm.in				\
	sortsmillff/pure.scm.in

NODIST_SCM_FILES += sortsmillff/machine.scm sortsmillff/pkg-info.scm	\
	sortsmillff/math-constants.scm sortsmillff/i18n.scm					\
	sortsmillff/fontforge-api.scm sortsmillff/python.scm				\
	sortsmillff/pure.scm

sortsmillff/fontforge-api.scm:							\
		$(top_builddir)/fontforge/fontforge.types.apii	\
		$(srcdir)/generate-guile-api.scm				\
		ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmillff fontforge-api)' $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmillff/gdraw-api.scm: $(top_builddir)/gdraw/gdraw.types.apii	\
		$(srcdir)/generate-guile-api.scm							\
		ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmillff gdraw-api)' $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-guile-api.scm ff-internal/generate-types.scm

#--------------------------------------------------------------------------

nodist_pkgguiledata_DATA = site-init.scm site-final.scm

EXTRA_DIST += site-init.scm.in site-final.scm.in
MOSTLYCLEANFILES += site-init.scm site-final.scm

#--------------------------------------------------------------------------

AM_LDFLAGS = -module -release $(VERSION) $(MY_CFLAGS) $(MY_LIBS)

lib_LTLIBRARIES =

lib_LTLIBRARIES += libguile-sortsmillff_iconv.la
libguile_sortsmillff_iconv_la_SOURCES = guile_iconv.c
libguile_sortsmillff_iconv_la_LIBADD = $(top_builddir)/lib/libgnu.la

if HAVE_PURE_API
lib_LTLIBRARIES += libguile-sortsmillff_pure.la
libguile_sortsmillff_pure_la_SOURCES = guile_pure.c
libguile_sortsmillff_pure_la_CFLAGS = $(AM_CFLAGS) $(PURE_CFLAGS)
libguile_sortsmillff_pure_la_LDFLAGS = -module -release $(VERSION)	\
	$(PURE_LIBS)
libguile_sortsmillff_pure_la_LIBADD = $(top_builddir)/lib/libgnu.la
endif HAVE_PURE_API

if HAVE_PYTHON_API
lib_LTLIBRARIES += libguile-sortsmillff_python.la
libguile_sortsmillff_python_la_SOURCES = guile_python.c
libguile_sortsmillff_python_la_LDFLAGS = -module -release $(VERSION)	\
	$(PYTHON_LIBS)
libguile_sortsmillff_python_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/lib/libgnu.la
endif HAVE_PYTHON_API

lib_LTLIBRARIES += libguile-sortsmillff_aux.la
libguile_sortsmillff_aux_la_SOURCES = guile_polyspline.c	\
	guile_brentroot.c guile_gsl.c
libguile_sortsmillff_aux_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/lib/libgnu.la

lib_LTLIBRARIES += libguile-sortsmillff_fontforge.la
libguile_sortsmillff_fontforge_la_SOURCES = guile_notices.c
libguile_sortsmillff_fontforge_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
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

install-data-hook: uninstall-guile-object-files
	@$(MKDIR_P) '$(DESTDIR)$(guileobjmoduledir)/sortsmillff'
	@echo '   Compiling Guile modules ...'
	@for f in $(DIST_SCM_FILES) $(NODIST_SCM_FILES); do						\
		f_go=`echo "$${f}" | $(SED) -e 's/\.scm$$/.go/'`;					\
		full_go="$(DESTDIR)$(guileobjmoduledir)/$${f_go}";					\
		cd '$(DESTDIR)$(guilemoduledir)' && 								\
			LTDL_LIBRARY_PATH='$(abs_top_builddir)/guile'					\
				$(GUILE_TOOLS) compile -o "$${full_go}" "$${f}"; 			\
	done
	@echo '   ... done compiling Guile modules.'

uninstall-local: uninstall-guile-object-files

uninstall-guile-object-files:
	@for f in $(DIST_SCM_FILES) $(NODIST_SCM_FILES); do						\
		f_go=`echo "$${f}" | $(SED) -e 's/\.scm$$/.go/'`;					\
		if test -f $(DESTDIR)$(guileobjmoduledir)/"$${f_go}"; then 			\
			echo 'rm -f $(DESTDIR)$(guileobjmoduledir)/'"$${f_go}";			\
			rm -f $(DESTDIR)$(guileobjmoduledir)/"$${f_go}";				\
		fi																	\
	done

#--------------------------------------------------------------------------
