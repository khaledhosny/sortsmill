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

#--------------------------------------------------------------------------
#
# Automatic generation of .go file dependencies.
#
# FIXME: Currently this examines only R⁶RS-style imports.
#
# FIXME: THIS IS NOT YET WORKING WELL ENOUGH AND IS DISABLED BY DEFAULT.

# Leave this undefined, to enable handwritten and/or manually
# generated dependencies.
GENERATED_GO_DEPS =

EXTRA_DIST += generate-go-dependencies.scm		\
	ff-internal/find-imports.scm

%.Dscm: %.scm
	$(AM_V_at)set -e; 														\
	$(MKDIR_P) $(dir $@);													\
	rm -f $@;																\
	$(GUILE_INTERPRET) $(srcdir)/generate-go-dependencies.scm $< > $@-tmp;	\
	$(SED) -e 's|\($*\)\.go[ :]*|\1.go $@ : \1.scm |g' < $@-tmp > $@;		\
	rm -f $@-tmp

@ifdefmk@ GENERATED_GO_DEPS
@includemk@ $(SCM_FILES:.scm=.Dscm)
@endifmk@

distclean-local::
	test -z "$(SCM_FILES:.scm=.Dscm)" || rm -f $(SCM_FILES:.scm=.Dscm)

#--------------------------------------------------------------------------
#
# Visualization of Guile module dependencies.
#
# FIXME: Currently this examines only R⁶RS-style imports.

EXTRA_DIST += generate-guile-modules-graph.scm	\
	ff-internal/find-imports.scm

guile-modules.dot: $(SCM_FILES)
	$(GUILE_INTERPRET) $(srcdir)/generate-guile-modules-graph.scm $^ > $@-tmp
	mv $@-tmp $@

guile-modules.pdf: guile-modules.dot
	dot -Gratio=0.8 -Tpdf $< > $@-tmp
	mv $@-tmp $@

view-guile-modules: guile-modules.pdf
	xdg-open $<

circo-guile-modules.pdf: guile-modules.dot
	circo -Tpdf $< > $@-tmp
	mv $@-tmp $@

circo-guile-modules: circo-guile-modules.pdf
	xdg-open $<

DISTCLEANFILES += guile-modules.dot guile-modules.pdf	\
	circo-guile-modules.pdf

#--------------------------------------------------------------------------
#
# Automatic generation of ‘reëxporter’ libraries.

EXTRA_DIST += generate-reexporter.scm ff-internal/find-exports.scm	\
	ff-internal/reexporters.scm

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
SCM_FILES = $(DIST_SCM_FILES) $(NODIST_SCM_FILES)
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
@ifndefmk@ GENERATED_GO_DEPS
$(GO_FILES): $(DIST_SCM_FILES) $(NODIST_SCM_FILES)
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill)

MAKEFILE_GENERATED_SCM_FILES += sortsmill.scm
GO_FILES += sortsmill.go
SORTSMILL_REEXPORTED_SCM_FILES = 

sortsmill.scm: $(SORTSMILL_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill.go: $(SORTSMILL_REEXPORTED_SCM_FILES:%.scm=%.go)
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill alloc)

DIST_SCM_FILES += sortsmill/alloc.scm
GO_FILES += sortsmill/alloc.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/alloc.scm

sortsmill/alloc.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill api-syntax)

DIST_SCM_FILES += sortsmill/api-syntax.scm
GO_FILES += sortsmill/api-syntax.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/api-syntax.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/api-syntax.go: sortsmill/pkg-info.go
sortsmill/api-syntax.go: sortsmill/machine.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill argv)

DIST_SCM_FILES += sortsmill/argv.scm
GO_FILES += sortsmill/argv.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/argv.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/argv.go: sortsmill/kwargs.go
sortsmill/argv.go: sortsmill/machine.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill arrays)

DIST_SCM_FILES += sortsmill/arrays.scm
GO_FILES += sortsmill/arrays.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/arrays.scm

sortsmill/arrays.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill contours)

# FIXME: Put this under a category along with (sortsmill views), etc.

DIST_SCM_FILES += sortsmill/contours.scm
GO_FILES += sortsmill/contours.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/contours.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/contours.go: sortsmill/fontforge-api.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill dynlink)

DIST_SCM_FILES += sortsmill/dynlink.scm
GO_FILES += sortsmill/dynlink.go

#--------------------------------------------------------------------------
#
# (sortsmill editor)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/editor.scm
GO_FILES += sortsmill/editor.go
EDITOR_REEXPORTED_SCM_FILES = 
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/editor.scm

sortsmill/editor.scm: $(EDITOR_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill editor), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill.go: $(EDITOR_REEXPORTED_SCM_FILES:%.scm=%.go)
@endifmk@

# (sortsmill editor finalization)

DIST_SCM_FILES += sortsmill/editor/finalization.scm
GO_FILES += sortsmill/editor/finalization.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/finalization.scm

# (sortsmill editor main)

DIST_SCM_FILES += sortsmill/editor/main.scm
GO_FILES += sortsmill/editor/main.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/main.scm

sortsmill/editor/main.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/editor/main.go: sortsmill/argv.go
@endifmk@

# (sortsmill editor main-loop)

DIST_SCM_FILES += sortsmill/editor/main-loop.scm
GO_FILES += sortsmill/editor/main-loop.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/main-loop.scm

sortsmill/editor/main-loop.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/editor/main-loop.go: sortsmill/kwargs.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill ffcompat)

# FIXME: Put this under a (sortsmill python) category.

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
# (sortsmill i18n)

DIST_SCM_FILES += sortsmill/i18n.scm
GO_FILES += sortsmill/i18n.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/i18n.scm

sortsmill/i18n.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/i18n.go: sortsmill/pkg-info.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill iconv)

DIST_SCM_FILES += sortsmill/iconv.scm
GO_FILES += sortsmill/iconv.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/iconv.scm

sortsmill/iconv.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill kwargs)

DIST_SCM_FILES += sortsmill/kwargs.scm
GO_FILES += sortsmill/kwargs.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/kwargs.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/kwargs.go: sortsmill/i18n.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill machine)

DIST_SCM_IN_FILES += sortsmill/machine.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/machine.scm
GO_FILES += sortsmill/machine.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/machine.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/machine.go: sortsmill/math/math-constants.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill math)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math.scm
GO_FILES += sortsmill/math.go
MATH_REEXPORTED_SCM_FILES =
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/math.scm

sortsmill/math.scm: $(MATH_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/math.go: $(MATH_REEXPORTED_SCM_FILES:%.scm=%.go)
@endifmk@

# (sortsmill math brentroot)

DIST_SCM_FILES += sortsmill/math/brentroot.scm
GO_FILES += sortsmill/math/brentroot.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/brentroot.scm

sortsmill/math/brentroot.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/math/brentroot.go: sortsmill/math/math-constants.go
@endifmk@

# (sortsmill math functions)

DIST_SCM_FILES += sortsmill/math/functions.scm
GO_FILES += sortsmill/math/functions.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/functions.scm

sortsmill/math/functions.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)


# (sortsmill math gsl)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math/gsl.scm
GO_FILES += sortsmill/math/gsl.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/gsl.scm
MATH_GSL_REEXPORTED_SCM_FILES =

sortsmill/math/gsl.scm: $(MATH_GSL_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math gsl), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/math/gsl.go: $(MATH_GSL_REEXPORTED_SCM_FILES:%.scm=%.go)
@endifmk@

# (sortsmill math gsl error)

CONFIGURE_GENERATED_SCM_FILES += sortsmill/math/gsl/error.scm
GO_FILES += sortsmill/math/gsl/error.go
MATH_GSL_REEXPORTED_SCM_FILES += sortsmill/math/gsl/error.scm

sortsmill/math/gsl/error.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/math/gsl/error.go: sortsmill/kwargs.go
@endifmk@


# (sortsmill math gsl matrices)

DIST_SCM_FILES += sortsmill/math/gsl/matrices.scm
GO_FILES += sortsmill/math/gsl/matrices.go
MATH_GSL_REEXPORTED_SCM_FILES += sortsmill/math/gsl/matrices.scm

sortsmill/math/gsl/matrices.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)


# (sortsmill math math-constants)

DIST_SCM_IN_FILES += sortsmill/math/math-constants.scm.in
CONFIGURE_GENERATED_SCM_FILES += sortsmill/math/math-constants.scm
GO_FILES += sortsmill/math/math-constants.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/math-constants.scm

#--------------------------------------------------------------------------
#
# (sortsmill math matrices)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math/matrices.scm
GO_FILES += sortsmill/math/matrices.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/matrices.scm
MATH_MATRICES_REEXPORTED_SCM_FILES =

sortsmill/math/matrices.scm: $(MATH_MATRICES_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math matrices), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill math matrices arithmetic)

DIST_SCM_FILES += sortsmill/math/matrices/arithmetic.scm
GO_FILES += sortsmill/math/matrices/arithmetic.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/arithmetic.scm


# (sortsmill math matrices base)

DIST_SCM_FILES += sortsmill/math/matrices/base.scm
GO_FILES += sortsmill/math/matrices/base.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/base.scm


# (sortsmill math matrices linalg)

DIST_SCM_FILES += sortsmill/math/matrices/linalg.scm
GO_FILES += sortsmill/math/matrices/linalg.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/linalg.scm


# (sortsmill math matrices mpqmat)

DIST_SCM_FILES += sortsmill/math/matrices/mpqmat.scm
GO_FILES += sortsmill/math/matrices/mpqmat.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/mpqmat.scm

sortsmill/math/matrices/mpqmat.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------

# (sortsmill math polyspline)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math/polyspline.scm
GO_FILES += sortsmill/math/polyspline.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/polyspline.scm
MATH_POLYSPLINE_REEXPORTED_SCM_FILES =
MATH_REEXPORTED_SCM_FILES += sortsmill/math/polyspline.scm

sortsmill/math/polyspline.scm: $(MATH_POLYSPLINE_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math polyspline), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill math polyspline f64)

DIST_SCM_FILES += sortsmill/math/polyspline/f64.scm
GO_FILES += sortsmill/math/polyspline/f64.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/f64.scm

sortsmill/math/polyspline/f64.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)


# (sortsmill math polyspline gmp)

DIST_SCM_FILES += sortsmill/math/polyspline/gmp.scm
GO_FILES += sortsmill/math/polyspline/gmp.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/gmp.scm


# (sortsmill math polyspline scm)

DIST_SCM_FILES += sortsmill/math/polyspline/scm.scm
GO_FILES += sortsmill/math/polyspline/scm.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/scm.scm

#--------------------------------------------------------------------------
#
# (sortsmill notices)

# FIXME: Maybe put this in the (sortsmill editor) category or some
# such.

DIST_SCM_FILES += sortsmill/notices.scm
GO_FILES += sortsmill/notices.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/notices.scm

sortsmill/notices.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

#--------------------------------------------------------------------------
#
# (sortsmill options)   FIXME: Reimplement this differently.

DIST_SCM_FILES += sortsmill/options.scm
GO_FILES += sortsmill/options.go

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/options.go: sortsmill/alloc.go
sortsmill/options.go: sortsmill/argv.go
sortsmill/options.go: sortsmill/machine.go
sortsmill/options.go: sortsmill/pkg-info.go
sortsmill/options.go: sortsmill/strings.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill pkg-info ...)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/pkg-info.scm
GO_FILES += sortsmill/pkg-info.go
PKG_INFO_REEXPORTED_SCM_FILES =
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/pkg-info.scm

sortsmill/pkg-info.scm: $(PKG_INFO_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill pkg-info), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/pkg-info.go: $(PKG_INFO_REEXPORTED_SCM_FILES:%.scm=%.go)
sortsmill/pkg-info.go: sortsmill/strings/hash-guillemet.go
@endifmk@

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
# FIXME: Get rid of this.

#DIST_SCM_FILES += sortsmill/precompute.scm
#GO_FILES += sortsmill/precompute.go

#--------------------------------------------------------------------------
#
# (sortsmill pure)

if HAVE_PURE_API

DIST_SCM_FILES += sortsmill/pure.scm
GO_FILES += sortsmill/pure.go

sortsmill/pure.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/pure.go: sortsmill/argv.go
sortsmill/pure.go: sortsmill/strings/hash-guillemet.go
sortsmill/pure.go: sortsmill/i18n.go
@endifmk@

endif HAVE_PURE_API

#--------------------------------------------------------------------------
#
# (sortsmill python)

if HAVE_PYTHON_API

DIST_SCM_FILES += sortsmill/python.scm
GO_FILES += sortsmill/python.go

sortsmill/python.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/python.go: sortsmill/strings/hash-guillemet.go
sortsmill/python.go: sortsmill/i18n.go
sortsmill/python.go: sortsmill/pkg-info.go
@endifmk@

endif HAVE_PYTHON_API

#--------------------------------------------------------------------------
#
# (sortsmill sfd-to-sxml)    FIXME: Implement this for real, eventually.

DIST_SCM_FILES += sortsmill/sfd-to-sxml.scm
GO_FILES += sortsmill/sfd-to-sxml.go

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/sfd-to-sxml.go: sortsmill/i18n.go
sortsmill/sfd-to-sxml.go: sortsmill/math/math-constants.go
@endifmk@

#--------------------------------------------------------------------------
#
# (sortsmill strings)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/strings.scm
GO_FILES += sortsmill/strings.go
STRINGS_REEXPORTED_SCM_FILES =
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/strings.scm

sortsmill/strings.scm: $(STRINGS_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill strings), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/strings.go: $(STRINGS_REEXPORTED_SCM_FILES:%.scm=%.go)
@endifmk@

# (sortsmill strings grabbed-strings)

DIST_SCM_FILES += sortsmill/strings/grabbed-strings.scm
GO_FILES += sortsmill/strings/grabbed-strings.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/grabbed-strings.scm

# (sortsmill strings hash-guillemet)

DIST_SCM_FILES += sortsmill/strings/hash-guillemet.scm
GO_FILES += sortsmill/strings/hash-guillemet.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/hash-guillemet.scm

# (sortsmill strings text-embedding)

DIST_SCM_FILES += sortsmill/strings/text-embedding.scm
GO_FILES += sortsmill/strings/text-embedding.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/text-embedding.scm

#--------------------------------------------------------------------------
#
# (sortsmill usermenu ...)

if HAVE_GUI

DIST_SCM_FILES += sortsmill/usermenu.scm
GO_FILES += sortsmill/usermenu.go

sortsmill/usermenu.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/usermenu.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu.go: sortsmill/strings/hash-guillemet.go
sortsmill/usermenu.go: sortsmill/i18n.go
sortsmill/usermenu.go: sortsmill/machine.go
@endifmk@

if HAVE_PURE_API

DIST_SCM_FILES += sortsmill/usermenu/pure.scm
GO_FILES += sortsmill/usermenu/pure.go

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/usermenu/pure.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu/pure.go: sortsmill/strings/hash-guillemet.go
@endifmk@

endif HAVE_PURE_API

if HAVE_PYTHON_API

DIST_SCM_FILES += sortsmill/usermenu/python.scm
GO_FILES += sortsmill/usermenu/python.go

sortsmill/usermenu/python.go: sortsmill/dynlink.go $(lib_LTLIBRARIES)

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/usermenu/python.go: sortsmill/fontforge-api.go sortsmill/gdraw-api.go
sortsmill/usermenu/python.go: sortsmill/machine.go
sortsmill/usermenu/python.go: sortsmill/pkg-info.go
sortsmill/usermenu/python.go: sortsmill/strings.go
sortsmill/usermenu/python.go: sortsmill/usermenu.go
@endifmk@

endif HAVE_PYTHON_API

endif HAVE_GUI

#--------------------------------------------------------------------------
#
# (sortsmill views)

# FIXME: This probably belongs in a category with (sortsmill contours).

DIST_SCM_FILES += sortsmill/views.scm
GO_FILES += sortsmill/views.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/views.scm

@ifndefmk@ GENERATED_GO_DEPS
sortsmill/views.go: sortsmill/fontforge-api.go
sortsmill/views.go: sortsmill/i18n.go
@endifmk@

#--------------------------------------------------------------------------
#
# Guile sources generated from ‘API instructions’ (.apii files).

sortsmill/fontforge-api.scm:											\
		$(top_builddir)/fontforge/fontforge.types.apii					\
		$(srcdir)/generate-guile-api.scm								\
		ff-internal/generate-types.scm libguile-sortsmill_symbols.la
	$(AM_V_REEXPORT)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill fontforge-api)' $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmill/gdraw-api.scm: $(top_builddir)/gdraw/gdraw.types.apii			\
		$(srcdir)/generate-guile-api.scm								\
		ff-internal/generate-types.scm libguile-sortsmill_symbols.la
	$(AM_V_REEXPORT)
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
	sortsmill/strings/hash-guillemet.scm

libguile_sortsmill_symbols.c: $(SCM_SYMBOL_FILES)
	$(AM_V_REEXPORT)
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
