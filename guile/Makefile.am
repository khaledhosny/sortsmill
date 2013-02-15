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

EXTRA_DIST += tig.scm

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

DIST_SCM_FILES += sortsmill/kwargs.scm sortsmill/alloc.scm			\
	sortsmill/matrices.scm sortsmill/iconv.scm sortsmill/linalg.scm	\
	sortsmill/polyspline.scm sortsmill/brentroot.scm				\
	sortsmill/sfd-to-sxml.scm sortsmill/views.scm					\
	sortsmill/api-syntax.scm sortsmill/notices.scm					\
	sortsmill/hash-guillemet.scm sortsmill/strings.scm				\
	sortsmill/ffcompat.scm sortsmill/i18n.scm sortsmill/dynlink.scm	\
	sortsmill/contours.scm
#sortsmill/precompute.scm
GO_FILES += sortsmill/kwargs.go sortsmill/alloc.go					\
	sortsmill/matrices.go sortsmill/iconv.go sortsmill/linalg.go	\
	sortsmill/polyspline.go sortsmill/brentroot.go					\
	sortsmill/sfd-to-sxml.go sortsmill/views.go						\
	sortsmill/api-syntax.go sortsmill/notices.go					\
	sortsmill/hash-guillemet.go sortsmill/strings.go				\
	sortsmill/ffcompat.go sortsmill/i18n.go sortsmill/dynlink.go	\
	sortsmill/contours.go

DIST_SCM_IN_FILES += sortsmill/machine.scm.in					\
	sortsmill/math-constants.scm.in sortsmill/pkg-info.scm.in	\
	sortsmill/gsl.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/machine.scm	\
	sortsmill/math-constants.scm sortsmill/pkg-info.scm	\
	sortsmill/gsl.scm
GO_FILES += sortsmill/machine.go sortsmill/math-constants.go	\
	sortsmill/pkg-info.go sortsmill/gsl.go

MAKEFILE_GENERATED_SCM_FILES += sortsmill/fontforge-api.scm
GO_FILES += sortsmill/fontforge-api.go

if HAVE_PURE_API
DIST_SCM_FILES += sortsmill/pure.scm
GO_FILES += sortsmill/pure.go
endif HAVE_PURE_API

if HAVE_PYTHON_API
DIST_SCM_FILES += sortsmill/python.scm
GO_FILES += sortsmill/python.go
endif HAVE_PYTHON_API

if HAVE_GUI

DIST_SCM_FILES += sortsmill/usermenu.scm
GO_FILES += sortsmill/usermenu.go

if HAVE_PURE_API
DIST_SCM_FILES += sortsmill/usermenu/pure.scm
GO_FILES += sortsmill/usermenu/pure.go
endif HAVE_PURE_API

if HAVE_PYTHON_API
DIST_SCM_FILES += sortsmill/usermenu/python.scm
GO_FILES += sortsmill/usermenu/python.go
endif HAVE_PYTHON_API

MAKEFILE_GENERATED_SCM_FILES += sortsmill/gdraw-api.scm
GO_FILES += sortsmill/gdraw-api.go

endif HAVE_GUI

sortsmill/gsl.go sortsmill/matrices.go: sortsmill/kwargs.go

sortsmill/alloc.go sortsmill/brentroot.go sortsmill/gsl.go: sortsmill/dynlink.go
sortsmill/ffcompat.go sortsmill/matrices.go sortsmill/i18n.go: sortsmill/dynlink.go
sortsmill/iconv.go sortsmill/notices.go sortsmill/polyspline.go: sortsmill/dynlink.go
sortsmill/pure.go sortsmill/python.go: sortsmill/dynlink.go
sortsmill/usermenu/python.go sortsmill/usermenu.go: sortsmill/dynlink.go

sortsmill/kwargs.go: sortsmill/i18n.go
sortsmill/pure.go sortsmill/python.go sortsmill/sfd-to-sxml.go: sortsmill/i18n.go
sortsmill/usermenu.go sortsmill/views.go sortsmill/matrices.go: sortsmill/i18n.go

sortsmill/api-syntax.go sortsmill/i18n.go sortsmill/python.go: sortsmill/pkg-info.go

sortsmill/machine.go sortsmill/brentroot.go sortsmill/sfd-to-sxml.go: sortsmill/math-constants.go

sortsmill/api-syntax.go sortsmill/usermenu.go sortsmill/usermenu/python.go: sortsmill/machine.go

sortsmill/usermenu.go sortsmill/usermenu/pure.go sortsmill/usermenu/python.go: sortsmill/gdraw-api.go

sortsmill/usermenu.go sortsmill/usermenu/pure.go sortsmill/usermenu/python.go: sortsmill/fontforge-api.go
sortsmill/views.go sortsmill/contours.go: sortsmill/fontforge-api.go

sortsmill/strings.go sortsmill/pure.go sortsmill/python.go: sortsmill/hash-guillemet.go
sortsmill/usermenu.go sortsmill/usermenu/pure.go: sortsmill/hash-guillemet.go

sortsmill/usermenu/python.go: sortsmill/strings.go

sortsmill/usermenu/python.go: sortsmill/usermenu.go

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

nodist_pkgguiledata_DATA = site-init.scm site-final.scm

EXTRA_DIST += site-init.scm.in site-final.scm.in
MOSTLYCLEANFILES += site-init.scm site-final.scm

#--------------------------------------------------------------------------

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

libguile_sortsmill_symbols.c:							\
	$(srcdir)/generate-libguile-sortsmill_symbols.scm	\
	ff-internal/extract-dynlink.scm

libguile_sortsmill_symbols.c: $(SCM_SYMBOL_FILES)
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET)										\
		$(srcdir)/generate-libguile-sortsmill_symbols.scm $^ > $@-tmp
	$(AM_V_at)mv $@-tmp $@

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

# The following rules are unpleasant but help us avoid having to keep
# track of dependencies. Consolation: it is no worse than if we used a
# whole-program compiler.
$(filter-out sortsmill/pkg-info.go,$(GO_FILES)): $(DIST_SCM_FILES)	\
	$(NODIST_SCM_FILES) $(lib_LTLIBRARIES)
sortsmill/pkg-info.go: $(DIST_SCM_FILES) $(NODIST_SCM_FILES)

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
