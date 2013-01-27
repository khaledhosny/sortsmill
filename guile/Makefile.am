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
CONFIGURE_GENERATED_SCM_FILES =
MAKEFILE_GENERATED_SCM_FILES =
GO_FILES =

nobase_dist_guilemodule_DATA = $(DIST_SCM_FILES)
nobase_nodist_guilemodule_DATA = $(CONFIGURE_GENERATED_SCM_FILES)	\
	$(MAKEFILE_GENERATED_SCM_FILES)
EXTRA_DIST += $(DIST_SCM_IN_FILES)
MOSTLYCLEANFILES += $(MAKEFILE_GENERATED_SCM_FILES) $(GO_FILES)

DIST_SCM_FILES += sortsmill/alloc.scm sortsmill/gsl.scm	\
	sortsmill/iconv.scm sortsmill/linalg.scm			\
	sortsmill/polyspline.scm sortsmill/brentroot.scm	\
	sortsmill/sfd-to-sxml.scm sortsmill/views.scm		\
	sortsmill/api-syntax.scm sortsmill/notices.scm		\
	sortsmill/strings.scm
#sortsmill/precompute.scm
GO_FILES += sortsmill/alloc.go sortsmill/gsl.go		\
	sortsmill/iconv.go sortsmill/linalg.go			\
	sortsmill/polyspline.go sortsmill/brentroot.go	\
	sortsmill/sfd-to-sxml.go sortsmill/views.go		\
	sortsmill/api-syntax.go sortsmill/notices.go	\
	sortsmill/strings.go

DIST_SCM_IN_FILES += sortsmill/i18n.scm.in						\
	sortsmill/machine.scm.in sortsmill/math-constants.scm.in	\
	sortsmill/pkg-info.scm.in sortsmill/python.scm.in			\
	sortsmill/pure.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/i18n.scm		\
	sortsmill/machine.scm sortsmill/math-constants.scm	\
	sortsmill/pkg-info.scm sortsmill/python.scm			\
	sortsmill/pure.scm
GO_FILES += sortsmill/i18n.go sortsmill/machine.go		\
	sortsmill/math-constants.go sortsmill/pkg-info.go	\
	sortsmill/python.go sortsmill/pure.go

MAKEFILE_GENERATED_SCM_FILES += sortsmill/fontforge-api.scm
GO_FILES += sortsmill/fontforge-api.go

if HAVE_GUI

DIST_SCM_FILES += sortsmill/usermenu.scm
GO_FILES += sortsmill/usermenu.go

MAKEFILE_GENERATED_SCM_FILES += sortsmill/gdraw-api.scm
GO_FILES += sortsmill/gdraw-api.go

endif HAVE_GUI

sortsmill/fontforge-api.scm:							\
		$(top_builddir)/fontforge/fontforge.types.apii	\
		$(srcdir)/generate-guile-api.scm				\
		ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill fontforge-api)' $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmill/gdraw-api.scm: $(top_builddir)/gdraw/gdraw.types.apii	\
		$(srcdir)/generate-guile-api.scm							\
		ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill gdraw-api)' $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-guile-api.scm ff-internal/generate-types.scm

#--------------------------------------------------------------------------

nodist_pkgguiledata_DATA = site-init.scm site-final.scm

EXTRA_DIST += site-init.scm.in site-final.scm.in
MOSTLYCLEANFILES += site-init.scm site-final.scm

#--------------------------------------------------------------------------

AM_LDFLAGS = -module -release $(VERSION) $(MY_CFLAGS) $(MY_LIBS)

lib_LTLIBRARIES =

lib_LTLIBRARIES += libguile-sortsmill_iconv.la
libguile_sortsmill_iconv_la_SOURCES = guile_iconv.c
libguile_sortsmill_iconv_la_LIBADD = $(top_builddir)/lib/libgnu.la

if HAVE_PURE_API
lib_LTLIBRARIES += libguile-sortsmill_pure.la
libguile_sortsmill_pure_la_SOURCES = guile_pure.c
libguile_sortsmill_pure_la_CFLAGS = $(AM_CFLAGS) $(PURE_CFLAGS)
libguile_sortsmill_pure_la_LDFLAGS = -module -release $(VERSION)	\
	$(PURE_LIBS)
libguile_sortsmill_pure_la_LIBADD = $(top_builddir)/lib/libgnu.la
endif HAVE_PURE_API

if HAVE_PYTHON_API
lib_LTLIBRARIES += libguile-sortsmill_python.la
libguile_sortsmill_python_la_SOURCES = guile_python.c
libguile_sortsmill_python_la_LDFLAGS = -module -release $(VERSION)	\
	$(PYTHON_LIBS)
libguile_sortsmill_python_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmill_aux.la	\
	$(top_builddir)/lib/libgnu.la
endif HAVE_PYTHON_API

lib_LTLIBRARIES += libguile-sortsmill_aux.la
libguile_sortsmill_aux_la_SOURCES = guile_polyspline.c	\
	guile_brentroot.c guile_gsl.c
libguile_sortsmill_aux_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmill_aux.la	\
	$(top_builddir)/lib/libgnu.la

lib_LTLIBRARIES += libguile-sortsmill_fontforge.la
libguile_sortsmill_fontforge_la_SOURCES = guile_notices.c
libguile_sortsmill_fontforge_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
libguile_sortsmill_fontforge_la_LIBADD =					\
	$(top_builddir)/fontforge/libsortsmill_fontforge.la	\
	$(top_builddir)/gdraw/libsortsmill_gdraw.la			\
	$(top_builddir)/gutils/libsortsmill_gutils.la			\
	$(top_builddir)/auxiliary/libsortsmill_aux.la			\
	$(top_builddir)/lib/libgnu.la

if HAVE_GUI
lib_LTLIBRARIES += libguile-sortsmill_fontforgeexe.la
libguile_sortsmill_fontforgeexe_la_SOURCES = guile_usermenu.c
libguile_sortsmill_fontforgeexe_la_LIBADD =					\
	$(top_builddir)/fontforge/libsortsmill_fontforgeexe.la	\
	$(top_builddir)/fontforge/libsortsmill_fontforge.la		\
	$(top_builddir)/gdraw/libsortsmill_gdraw.la				\
	$(top_builddir)/gutils/libsortsmill_gutils.la				\
	$(top_builddir)/auxiliary/libsortsmill_aux.la				\
	$(top_builddir)/lib/libgnu.la
endif HAVE_GUI

#--------------------------------------------------------------------------

all-local: $(GO_FILES)

# The following rule is unpleasant but avoids having to keep track of
# dependencies. Consolation: it is no worse than if we used a
# whole-program compiler.
$(GO_FILES): $(DIST_SCM_FILES) $(CONFIGURE_GENERATED_SCM_FILES)	\
	$(MAKEFILE_GENERATED_SCM_FILES)

# Try to ensure that the installed bitcode files are newer than the
# installed source files, by installing the bitcode files with this
# hook.
install-data-hook: $(GO_FILES)
	@(for f in $(GO_FILES); do									\
		path="$(DESTDIR)$(guileobjmoduledir)/$${f}";			\
		d=`echo "$${path}" | LC_ALL=C $(SED) -e 's|[^/]*$$||'`;	\
		$(MKDIR_P) "$${d}";										\
		echo "$(INSTALL_DATA) $${f} $${d}";						\
		$(INSTALL_DATA) "$${f}" "$${d}";						\
	done)

uninstall-local:
	@(for f in $(GO_FILES); do								\
		path="$(DESTDIR)$(guileobjmoduledir)/$${f}";		\
		echo "rm -f $${path}";								\
		rm -f "$${path}" || : ;								\
	done)

#--------------------------------------------------------------------------
