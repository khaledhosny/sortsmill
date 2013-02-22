# -*- tab-width: 4 -*-

# Copyright (C) 2012, 2013 by Barry Schwartz
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
@includemk@ $(top_srcdir)/mk/scheme.mk
@includemk@ $(top_srcdir)/mk/tig.mk
@includemk@ $(top_srcdir)/mk/cython.mk

AM_CYTHONFLAGS = '-I$(top_builddir)/inc' '-I$(top_srcdir)/inc'	\
	'-I$(top_builddir)/python' '-I$(top_srcdir)/python'

lib_LTLIBRARIES =
EXTRA_DIST =
MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

EXTRA_DIST += generate-reexporter.scm
EXTRA_DIST += ff-internal/find-exports.scm						\
	ff-internal/reexporters.scm ff-internal/find-imports.scm

#--------------------------------------------------------------------------
#
# The Type Inspector Generator.
#
# This program generates C programs that generate ‘API instructions’
# (.apii files).

EXTRA_DIST += tig.scm

#--------------------------------------------------------------------------
#
# The ‘site initialization’ script for sortsmill-editor.

dist_pkgguiledata_DATA = site-init.scm

#--------------------------------------------------------------------------

DIST_SCM_FILES =
DIST_SCM_IN_FILES =
CONFIGURE_GENERATED_SCM_FILES =
MAKEFILE_GENERATED_SCM_FILES =
NODIST_SCM_FILES = $(CONFIGURE_GENERATED_SCM_FILES)	\
	$(MAKEFILE_GENERATED_SCM_FILES)
GO_FILES =

nobase_dist_guilemodule_DATA = $(DIST_SCM_FILES)
nobase_nodist_guilemodule_DATA = $(NODIST_SCM_FILES)
EXTRA_DIST += $(DIST_SCM_IN_FILES)
MOSTLYCLEANFILES += $(MAKEFILE_GENERATED_SCM_FILES) $(GO_FILES)

#--------------------------------------------------------------------------
#
#
# Note: Most of the rules below that have a .go file on the left side
# are simply to speed up the make process. A few may ensure that
# generated files are created before they are needed.
#
#

#--------------------------------------------------------------------------

# The following rule is unpleasant but helps us avoid having to keep
# track of dependencies. Consolation: it is no worse than if we used a
# whole-program compiler.
$(GO_FILES): $(DIST_SCM_FILES) $(NODIST_SCM_FILES)

#--------------------------------------------------------------------------
#
# (sortsmill alloc)

DIST_SCM_FILES += sortsmill/alloc.scm
GO_FILES += sortsmill/alloc.go

sortsmill/alloc.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill api-syntax)

DIST_SCM_FILES += sortsmill/api-syntax.scm
GO_FILES += sortsmill/api-syntax.go

sortsmill/api-syntax.go: sortsmill/pkg-info.go
sortsmill/api-syntax.go: sortsmill/machine.go

#--------------------------------------------------------------------------
#
# (sortsmill argv)

DIST_SCM_FILES += sortsmill/argv.scm
GO_FILES += sortsmill/argv.go

sortsmill/argv.go: sortsmill/kwargs.go
sortsmill/argv.go: sortsmill/machine.go

#--------------------------------------------------------------------------
#
# (sortsmill brentroot)

DIST_SCM_FILES += sortsmill/brentroot.scm
GO_FILES += sortsmill/brentroot.go

sortsmill/brentroot.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/brentroot.go: sortsmill/math-constants.go

#--------------------------------------------------------------------------
#
# (sortsmill contours)

DIST_SCM_FILES += sortsmill/contours.scm
GO_FILES += sortsmill/contours.go

sortsmill/contours.go: sortsmill/fontforge-api.go

#--------------------------------------------------------------------------
#
# (sortsmill dynlink)

DIST_SCM_FILES += sortsmill/dynlink.scm
GO_FILES += sortsmill/dynlink.go

#--------------------------------------------------------------------------
#
# (sortsmill editor ...)

DIST_SCM_FILES += sortsmill/editor/finalization.scm
GO_FILES += sortsmill/editor/finalization.go

DIST_SCM_FILES += sortsmill/editor/main.scm
GO_FILES += sortsmill/editor/main.go

sortsmill/editor/main.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/editor/main.go: sortsmill/argv.go

DIST_SCM_FILES += sortsmill/editor/main-loop.scm
GO_FILES += sortsmill/editor/main-loop.go

sortsmill/editor/main-loop.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/editor/main-loop.go: sortsmill/kwargs.go

#--------------------------------------------------------------------------
#
# (sortsmill ffcompat)

DIST_SCM_FILES += sortsmill/ffcompat.scm
GO_FILES += sortsmill/ffcompat.go

sortsmill/ffcompat.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill fontforge-api)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/fontforge-api.scm
GO_FILES += sortsmill/fontforge-api.go

#--------------------------------------------------------------------------
#
# (sortsmill gdraw-api)

if HAVE_GUI

MAKEFILE_GENERATED_SCM_FILES += sortsmill/gdraw-api.scm
GO_FILES += sortsmill/gdraw-api.go

endif HAVE_GUI

#--------------------------------------------------------------------------
#
# (sortsmill gsl ...)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/gsl.scm
GO_FILES += sortsmill/gsl.go
GSL_REEXPORTED_SCM_FILES =

sortsmill/gsl.scm: $(GSL_REEXPORTED_SCM_FILES)
	$(AM_V_GEN)
	$(AM_V_at)$(call generate_reexporter, (sortsmill gsl), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

CONFIGURE_GENERATED_SCM_FILES += sortsmill/gsl/error.scm
GO_FILES += sortsmill/gsl/error.go
GSL_REEXPORTED_SCM_FILES += sortsmill/gsl/error.scm

sortsmill/gsl/error.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/gsl/error.go: sortsmill/kwargs.go

DIST_SCM_FILES += sortsmill/gsl/matrices.scm
GO_FILES += sortsmill/gsl/matrices.go
GSL_REEXPORTED_SCM_FILES += sortsmill/gsl/matrices.scm

sortsmill/gsl/matrices.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill hash-guillemet)

DIST_SCM_FILES += sortsmill/hash-guillemet.scm
GO_FILES += sortsmill/hash-guillemet.go

#--------------------------------------------------------------------------
#
# (sortsmill i18n)

DIST_SCM_FILES += sortsmill/i18n.scm
GO_FILES += sortsmill/i18n.go

sortsmill/i18n.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/i18n.go: sortsmill/pkg-info.go

#--------------------------------------------------------------------------
#
# (sortsmill iconv)

DIST_SCM_FILES += sortsmill/iconv.scm
GO_FILES += sortsmill/iconv.go

sortsmill/iconv.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill kwargs)

DIST_SCM_FILES += sortsmill/kwargs.scm
GO_FILES += sortsmill/kwargs.go

sortsmill/kwargs.go: sortsmill/i18n.go

#--------------------------------------------------------------------------
#
# (sortsmill linalg)   FIXME: Get rid of this.

DIST_SCM_FILES += sortsmill/linalg.scm
GO_FILES += sortsmill/linalg.go

#--------------------------------------------------------------------------
#
# (sortsmill machine)

DIST_SCM_IN_FILES += sortsmill/machine.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/machine.scm
GO_FILES += sortsmill/machine.go

sortsmill/machine.go: sortsmill/math-constants.go

#--------------------------------------------------------------------------
#
# (sortsmill math-constants)

DIST_SCM_IN_FILES += sortsmill/math-constants.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/math-constants.scm
GO_FILES += sortsmill/math-constants.go

#--------------------------------------------------------------------------
#
# (sortsmill matrices)

DIST_SCM_FILES += sortsmill/matrices.scm
GO_FILES += sortsmill/matrices.go

sortsmill/matrices.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/matrices.go: sortsmill/i18n.go
sortsmill/matrices.go: sortsmill/kwargs.go

#--------------------------------------------------------------------------
#
# (sortsmill notices)

DIST_SCM_FILES += sortsmill/notices.scm
GO_FILES += sortsmill/notices.go

sortsmill/notices.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill options)   FIXME: Reimplement this differently.

DIST_SCM_FILES += sortsmill/options.scm
GO_FILES += sortsmill/options.go

sortsmill/options.go: sortsmill/alloc.go
sortsmill/options.go: sortsmill/argv.go
sortsmill/options.go: sortsmill/machine.go
sortsmill/options.go: sortsmill/pkg-info.go
sortsmill/options.go: sortsmill/strings.go

#--------------------------------------------------------------------------
#
# (sortsmill pkg-info ...)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info.scm
GO_FILES += sortsmill/pkg-info.go
PKG_INFO_REEXPORTED_SCM_FILES =

sortsmill/pkg-info.scm: $(PKG_INFO_REEXPORTED_SCM_FILES)
	$(AM_V_GEN)
	$(AM_V_at)$(call generate_reexporter, (sortsmill pkg-info), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmill/pkg-info.go: sortsmill/hash-guillemet.go
sortsmill/pkg-info.go: sortsmill/pkg-info/directory-layout.go
sortsmill/pkg-info.go: sortsmill/pkg-info/i18n.go
sortsmill/pkg-info.go: sortsmill/pkg-info/package.go
sortsmill/pkg-info.go: sortsmill/pkg-info/pure.go
sortsmill/pkg-info.go: sortsmill/pkg-info/python.go
sortsmill/pkg-info.go: sortsmill/pkg-info/version.go

DIST_SCM_FILES += sortsmill/pkg-info/directory-layout.scm
GO_FILES += sortsmill/pkg-info/directory-layout.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/directory-layout.scm

sortsmill/pkg-info/directory-layout.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

DIST_SCM_IN_FILES += sortsmill/pkg-info/i18n.scm.in
MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info/i18n.scm
GO_FILES += sortsmill/pkg-info/i18n.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/i18n.scm

DIST_SCM_IN_FILES += sortsmill/pkg-info/package.scm.in
MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info/package.scm
GO_FILES += sortsmill/pkg-info/package.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/package.scm

DIST_SCM_IN_FILES += sortsmill/pkg-info/pure.scm.in
MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info/pure.scm
GO_FILES += sortsmill/pkg-info/pure.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/pure.scm

DIST_SCM_IN_FILES += sortsmill/pkg-info/python.scm.in
MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info/python.scm
GO_FILES += sortsmill/pkg-info/python.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/python.scm

DIST_SCM_IN_FILES += sortsmill/pkg-info/version.scm.in
MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info/version.scm
GO_FILES += sortsmill/pkg-info/version.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/version.scm

#--------------------------------------------------------------------------
#
# (sortsmill polyspline)

DIST_SCM_FILES += sortsmill/polyspline.scm
GO_FILES += sortsmill/polyspline.go

sortsmill/polyspline.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# FIXME: Get rid of this.

#DIST_SCM_FILES += sortsmill/precompute.scm
#GO_FILES += sortsmill/precompute.go

#--------------------------------------------------------------------------
#
# (sortsmill pure)

if HAVE_PURE_API

DIST_SCM_FILES += sortsmill/pure.scm
GO_FILES += sortsmill/pure.go

sortsmill/pure.go: sortsmill/argv.go
sortsmill/pure.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/pure.go: sortsmill/hash-guillemet.go
sortsmill/pure.go: sortsmill/i18n.go

endif HAVE_PURE_API

#--------------------------------------------------------------------------
#
# (sortsmill python)

if HAVE_PYTHON_API

DIST_SCM_FILES += sortsmill/python.scm
GO_FILES += sortsmill/python.go

sortsmill/python.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/python.go: sortsmill/hash-guillemet.go
sortsmill/python.go: sortsmill/i18n.go
sortsmill/python.go: sortsmill/pkg-info.go

endif HAVE_PYTHON_API

#--------------------------------------------------------------------------
#
# (sortsmill sfd-to-sxml)    FIXME: Implement this for real, eventually.

DIST_SCM_FILES += sortsmill/sfd-to-sxml.scm
GO_FILES += sortsmill/sfd-to-sxml.go

sortsmill/sfd-to-sxml.go: sortsmill/i18n.go
sortsmill/sfd-to-sxml.go: sortsmill/math-constants.go

#--------------------------------------------------------------------------
#
# (sortsmill strings)

DIST_SCM_FILES += sortsmill/strings.scm
GO_FILES += sortsmill/strings.go

sortsmill/strings.go: sortsmill/alloc.go
sortsmill/strings.go: sortsmill/hash-guillemet.go
sortsmill/strings: sortsmill/machine.go

#--------------------------------------------------------------------------
#
# (sortsmill usermenu ...)

if HAVE_GUI

DIST_SCM_FILES += sortsmill/usermenu.scm
GO_FILES += sortsmill/usermenu.go

sortsmill/usermenu.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/usermenu.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu.go: sortsmill/hash-guillemet.go
sortsmill/usermenu.go: sortsmill/i18n.go
sortsmill/usermenu.go: sortsmill/machine.go

if HAVE_PURE_API

DIST_SCM_FILES += sortsmill/usermenu/pure.scm
GO_FILES += sortsmill/usermenu/pure.go

sortsmill/usermenu/pure.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu/pure.go: sortsmill/hash-guillemet.go

endif HAVE_PURE_API

if HAVE_PYTHON_API

DIST_SCM_FILES += sortsmill/usermenu/python.scm
GO_FILES += sortsmill/usermenu/python.go

sortsmill/usermenu/python.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)
sortsmill/usermenu/python.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu/python.go: sortsmill/machine.go
sortsmill/usermenu/python.go: sortsmill/pkg-info.go
sortsmill/usermenu/python.go: sortsmill/strings.go
sortsmill/usermenu/python.go: sortsmill/usermenu.go

endif HAVE_PYTHON_API

endif HAVE_GUI

#--------------------------------------------------------------------------
#
# (sortsmill views)

DIST_SCM_FILES += sortsmill/views.scm
GO_FILES += sortsmill/views.go

sortsmill/views.go: sortsmill/fontforge-api.go
sortsmill/views.go: sortsmill/i18n.go

#--------------------------------------------------------------------------
#
# Guile sources generated from ‘API instructions’ (.apii files).

sortsmill/fontforge-api.scm:											\
		$(top_builddir)/fontforge/fontforge.types.apii					\
		$(srcdir)/generate-guile-api.scm								\
		ff-internal/generate-types.scm libguile-sortsmill_symbols.la
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill fontforge-api)' $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmill/gdraw-api.scm: $(top_builddir)/gdraw/gdraw.types.apii			\
		$(srcdir)/generate-guile-api.scm								\
		ff-internal/generate-types.scm libguile-sortsmill_symbols.la
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill gdraw-api)' $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-guile-api.scm ff-internal/generate-types.scm

#--------------------------------------------------------------------------
#
# Automatic generation of an extension module for foreign symbols used
# by Guile.

EXTRA_DIST += $(srcdir)/generate-libguile-sortsmill_symbols.scm	\
	ff-internal/extract-dynlink.scm

AM_LDFLAGS = -module -release $(VERSION) $(MY_CFLAGS) $(MY_LIBS)

lib_LTLIBRARIES += libguile-sortsmill_symbols.la
libguile_sortsmill_symbols_la_SOURCES = libguile_sortsmill_symbols.c
libguile_sortsmill_symbols_la_CPPFLAGS = $(AM_CPPFLAGS) $(FFINCLDIRS)
libguile_sortsmill_symbols_la_LIBADD =						\
	$(top_builddir)/fontforge/libsortsmill_fontforgeexe.la	\
	$(top_builddir)/fontforge/libsortsmill_fontforge.la		\
	$(top_builddir)/auxiliary/libsortsmill_aux.la

CLEANFILES += libguile_sortsmill_symbols.c

SCM_SYMBOL_FILES = $(filter-out sortsmill/fontforge-api.scm			\
	sortsmill/gdraw-api.scm, $(DIST_SCM_FILES) $(NODIST_SCM_FILES))

libguile_sortsmill_symbols.c:										\
	$(srcdir)/generate-libguile-sortsmill_symbols.scm				\
	ff-internal/extract-dynlink.scm sortsmill/pkg-info/python.scm	\
	sortsmill/hash-guillemet.scm

libguile_sortsmill_symbols.c: $(SCM_SYMBOL_FILES)
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET)										\
		$(srcdir)/generate-libguile-sortsmill_symbols.scm $^ > $@-tmp
	$(AM_V_at)mv $@-tmp $@

#--------------------------------------------------------------------------
#
# A Guile extension module written in Cython, for Python support.

if HAVE_PYTHON_API

lib_LTLIBRARIES += libguile-sortsmill_cython.la
libguile_sortsmill_cython_la_SOURCES = libguile_sortsmill_cython.c
libguile_sortsmill_cython_la_CFLAGS = $(AM_CFLAGS)	\
	$(CYTHON_EXTRA_CFLAGS)
libguile_sortsmill_cython_la_LDFLAGS = -module -release $(VERSION)	\
	$(PYTHON_LIBS)
libguile_sortsmill_cython_la_LIBADD =				\
	$(top_builddir)/auxiliary/libsortsmill_aux.la	\
	$(top_builddir)/lib/libgnu.la

libguile_sortsmill_cython.c: libguile_sortsmill_cython.pyx	\
	$(top_builddir)/inc/sortsmill/cython/config.pxi			\
	$(top_srcdir)/inc/sortsmill/cython/guile.pxd			\
	$(top_srcdir)/inc/sortsmill/cython/xgc.pxd

EXTRA_DIST += libguile_sortsmill_cython.pyx
MOSTLYCLEANFILES += libguile_sortsmill_cython.c	\
	libguile_sortsmill_cython.h

endif HAVE_PYTHON_API

#--------------------------------------------------------------------------

all-local: $(GO_FILES)

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
