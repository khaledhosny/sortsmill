# -*- tab-width: 4 -*-

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
# A temporary (FIXME) mechanism for generating Guile dependencies.

# FIXME FIXME FIXME FIXME FIXME!!!!! -- This has to be run by someone
# who has all the different language options enabled.
depend: $(SCM_FILES)
	$(GUILE_INTERPRET) $(srcdir)/generate-go-dependencies.scm $^ > $(srcdir)/guile-deps.mk

-@includemk@ $(srcdir)/guile-deps.mk

EXTRA_DIST += $(srcdir)/guile-deps.mk

# Anything that depends on sortsmill/dynlink.go is going to need
# $(lib_LTLIBRARIES) as well, so for now let us encode that this way:
sortsmill/dynlink.go: $(lib_LTLIBRARIES)

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
# (sortsmill)

MAKEFILE_GENERATED_SCM_FILES += sortsmill.scm
GO_FILES += sortsmill.go
SORTSMILL_REEXPORTED_SCM_FILES = 

sortsmill.scm: $(SORTSMILL_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

#--------------------------------------------------------------------------
#
# (sortsmill __internals__)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/__internals__.scm
GO_FILES += sortsmill/__internals__.go
__INTERNALS___REEXPORTED_SCM_FILES = 

sortsmill/__internals__.scm: $(__INTERNALS___REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill __internals__), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill __internals__ anchors)

DIST_SCM_FILES += sortsmill/__internals__/anchors.scm
GO_FILES += sortsmill/__internals__/anchors.go
__INTERNALS___REEXPORTED_SCM_FILES += sortsmill/__internals__/anchors.scm


# (sortsmill __internals__ glyphs)

DIST_SCM_FILES += sortsmill/__internals__/glyphs.scm
GO_FILES += sortsmill/__internals__/glyphs.go
__INTERNALS___REEXPORTED_SCM_FILES += sortsmill/__internals__/glyphs.scm


# (sortsmill __internals__ lookups)

DIST_SCM_FILES += sortsmill/__internals__/lookups.scm
GO_FILES += sortsmill/__internals__/lookups.go
__INTERNALS___REEXPORTED_SCM_FILES += sortsmill/__internals__/lookups.scm

#--------------------------------------------------------------------------
#
# (sortsmill api-syntax)

DIST_SCM_FILES += sortsmill/api-syntax.scm
GO_FILES += sortsmill/api-syntax.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/api-syntax.scm

#--------------------------------------------------------------------------
#
# (sortsmill argv)

DIST_SCM_FILES += sortsmill/argv.scm
GO_FILES += sortsmill/argv.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/argv.scm

#--------------------------------------------------------------------------
#
# (sortsmill arrays)

DIST_SCM_FILES += sortsmill/arrays.scm
GO_FILES += sortsmill/arrays.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/arrays.scm

#--------------------------------------------------------------------------
#
# (sortsmill containers)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/containers.scm
GO_FILES += sortsmill/containers.go
CONTAINERS_REEXPORTED_SCM_FILES = 
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/containers.scm

sortsmill/containers.scm: $(CONTAINERS_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill containers), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill containers rbmap)

DIST_SCM_FILES += sortsmill/containers/rbmap.scm
GO_FILES += sortsmill/containers/rbmap.go
CONTAINERS_REEXPORTED_SCM_FILES += sortsmill/containers/rbmap.scm


# (sortsmill containers rnrs-hashtable)

DIST_SCM_FILES += sortsmill/containers/rnrs-hashtable.scm
GO_FILES += sortsmill/containers/rnrs-hashtable.go
CONTAINERS_REEXPORTED_SCM_FILES += sortsmill/containers/rnrs-hashtable.scm

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

# (sortsmill editor finalization)

DIST_SCM_FILES += sortsmill/editor/finalization.scm
GO_FILES += sortsmill/editor/finalization.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/finalization.scm

# (sortsmill editor main)

DIST_SCM_FILES += sortsmill/editor/main.scm
GO_FILES += sortsmill/editor/main.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/main.scm

# (sortsmill editor main-loop)

DIST_SCM_FILES += sortsmill/editor/main-loop.scm
GO_FILES += sortsmill/editor/main-loop.go
EDITOR_REEXPORTED_SCM_FILES += sortsmill/editor/main-loop.scm

#--------------------------------------------------------------------------
#
# (sortsmill ffcompat)

# FIXME: Put this under a (sortsmill python) category.

DIST_SCM_FILES += sortsmill/ffcompat.scm
GO_FILES += sortsmill/ffcompat.go

#--------------------------------------------------------------------------
#
# (sortsmill fontforge-api)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/fontforge-api.scm
GO_FILES += sortsmill/fontforge-api.go

#--------------------------------------------------------------------------
#
# (sortsmill fonts)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/fonts.scm
GO_FILES += sortsmill/fonts.go
FONTS_REEXPORTED_SCM_FILES = 
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/fonts.scm

sortsmill/fonts.scm: $(FONTS_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill fonts), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill fonts anchors)

DIST_SCM_FILES += sortsmill/fonts/anchors.scm
GO_FILES += sortsmill/fonts/anchors.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/anchors.scm


# (sortsmill fonts contours)

DIST_SCM_FILES += sortsmill/fonts/contours.scm
GO_FILES += sortsmill/fonts/contours.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/contours.scm


# (sortsmill font-formats)

DIST_SCM_FILES += sortsmill/fonts/font-formats.scm
GO_FILES += sortsmill/fonts/font-formats.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/font-formats.scm


# (sortsmill fontinfo-dict)

DIST_SCM_FILES += sortsmill/fonts/fontinfo-dict.scm
GO_FILES += sortsmill/fonts/fontinfo-dict.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/fontinfo-dict.scm


# (sortsmill fonts general)

DIST_SCM_FILES += sortsmill/fonts/general.scm
GO_FILES += sortsmill/fonts/general.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/general.scm


# (sortsmill fonts glyphs)

DIST_SCM_FILES += sortsmill/fonts/glyphs.scm
GO_FILES += sortsmill/fonts/glyphs.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/glyphs.scm


# (sortsmill fonts head-table)

DIST_SCM_FILES += sortsmill/fonts/head-table.scm
GO_FILES += sortsmill/fonts/head-table.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/head-table.scm


# (sortsmill fonts hhea-table)

DIST_SCM_FILES += sortsmill/fonts/hhea-table.scm
GO_FILES += sortsmill/fonts/hhea-table.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/hhea-table.scm


# (sortsmill fonts name-table)

DIST_SCM_FILES += sortsmill/fonts/name-table.scm
GO_FILES += sortsmill/fonts/name-table.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/name-table.scm


# (sortsmill fonts opentype-io)

DIST_SCM_FILES += sortsmill/fonts/opentype-io.scm
GO_FILES += sortsmill/fonts/opentype-io.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/opentype-io.scm


# (sortsmill fonts os2-table)

DIST_SCM_FILES += sortsmill/fonts/os2-table.scm
GO_FILES += sortsmill/fonts/os2-table.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/os2-table.scm


# (sortsmill peg-spacing)

DIST_SCM_FILES += sortsmill/fonts/peg-spacing.scm
GO_FILES += sortsmill/fonts/peg-spacing.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/peg-spacing.scm


# (sortsmill private-dict)

DIST_SCM_FILES += sortsmill/fonts/private-dict.scm
GO_FILES += sortsmill/fonts/private-dict.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/private-dict.scm


# (sortsmill fonts psmat)

DIST_SCM_FILES += sortsmill/fonts/psmat.scm
GO_FILES += sortsmill/fonts/psmat.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/psmat.scm


# (sortsmill t1font-dict)

DIST_SCM_FILES += sortsmill/fonts/t1font-dict.scm
GO_FILES += sortsmill/fonts/t1font-dict.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/t1font-dict.scm


# (sortsmill fonts ufo)

DIST_SCM_FILES += sortsmill/fonts/ufo.scm
GO_FILES += sortsmill/fonts/ufo.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/ufo.scm


# (sortsmill fonts views)

DIST_SCM_FILES += sortsmill/fonts/views.scm
GO_FILES += sortsmill/fonts/views.go
FONTS_REEXPORTED_SCM_FILES += sortsmill/fonts/views.scm

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

#--------------------------------------------------------------------------
#
# (sortsmill iconv)

DIST_SCM_FILES += sortsmill/iconv.scm
GO_FILES += sortsmill/iconv.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/iconv.scm

#--------------------------------------------------------------------------
#
# (sortsmill kwargs)

DIST_SCM_FILES += sortsmill/kwargs.scm
GO_FILES += sortsmill/kwargs.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/kwargs.scm

#--------------------------------------------------------------------------
#
# (sortsmill mac-encodings)

DIST_SCM_FILES += sortsmill/mac-encodings.scm
GO_FILES += sortsmill/mac-encodings.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/mac-encodings.scm

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


# (sortsmill math brentroot)

DIST_SCM_FILES += sortsmill/math/brentroot.scm
GO_FILES += sortsmill/math/brentroot.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/brentroot.scm


# (sortsmill math functions)

DIST_SCM_FILES += sortsmill/math/functions.scm
GO_FILES += sortsmill/math/functions.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/functions.scm

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# (sortsmill math geometry)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math/geometry.scm
GO_FILES += sortsmill/math/geometry.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/geometry.scm
MATH_GEOMETRY_REEXPORTED_SCM_FILES =

sortsmill/math/geometry.scm: $(MATH_GEOMETRY_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math geometry), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill math geometry lines)

DIST_SCM_FILES += sortsmill/math/geometry/lines.scm
GO_FILES += sortsmill/math/geometry/lines.go
MATH_GEOMETRY_REEXPORTED_SCM_FILES += sortsmill/math/geometry/lines.scm

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# (sortsmill math gsl)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/math/gsl.scm
GO_FILES += sortsmill/math/gsl.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/gsl.scm
MATH_GSL_REEXPORTED_SCM_FILES =

sortsmill/math/gsl.scm: $(MATH_GSL_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill math gsl), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill math gsl error)

CONFIGURE_GENERATED_SCM_FILES += sortsmill/math/gsl/error.scm
GO_FILES += sortsmill/math/gsl/error.go
MATH_GSL_REEXPORTED_SCM_FILES += sortsmill/math/gsl/error.scm


# (sortsmill math gsl matrices)

DIST_SCM_FILES += sortsmill/math/gsl/matrices.scm
GO_FILES += sortsmill/math/gsl/matrices.go
MATH_GSL_REEXPORTED_SCM_FILES += sortsmill/math/gsl/matrices.scm

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# (sortsmill math multivariate-polynomials)

DIST_SCM_FILES += sortsmill/math/multivariate-polynomials.scm
GO_FILES += sortsmill/math/multivariate-polynomials.go
MATH_REEXPORTED_SCM_FILES += sortsmill/math/multivariate-polynomials.scm

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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


# (sortsmill math matrices bezout)

DIST_SCM_FILES += sortsmill/math/matrices/bezout.scm
GO_FILES += sortsmill/math/matrices/bezout.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/bezout.scm


# (sortsmill math matrices linalg)

DIST_SCM_FILES += sortsmill/math/matrices/linalg.scm
GO_FILES += sortsmill/math/matrices/linalg.go
MATH_MATRICES_REEXPORTED_SCM_FILES += sortsmill/math/matrices/linalg.scm

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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


# (sortsmill math polyspline add)

DIST_SCM_FILES += sortsmill/math/polyspline/add.scm
GO_FILES += sortsmill/math/polyspline/add.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/add.scm


# (sortsmill math polyspline bases)

DIST_SCM_FILES += sortsmill/math/polyspline/bases.scm
GO_FILES += sortsmill/math/polyspline/bases.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/bases.scm


# (sortsmill math polyspline compose)

DIST_SCM_FILES += sortsmill/math/polyspline/compose.scm
GO_FILES += sortsmill/math/polyspline/compose.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/compose.scm


# (sortsmill math polyspline deriv)

DIST_SCM_FILES += sortsmill/math/polyspline/deriv.scm
GO_FILES += sortsmill/math/polyspline/deriv.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/deriv.scm


# (sortsmill math polyspline div)

DIST_SCM_FILES += sortsmill/math/polyspline/div.scm
GO_FILES += sortsmill/math/polyspline/div.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/div.scm


# (sortsmill math polyspline elev)

DIST_SCM_FILES += sortsmill/math/polyspline/elev.scm
GO_FILES += sortsmill/math/polyspline/elev.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/elev.scm


# (sortsmill math polyspline ellipses)

DIST_SCM_FILES += sortsmill/math/polyspline/ellipses.scm
GO_FILES += sortsmill/math/polyspline/ellipses.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/ellipses.scm


# (sortsmill math polyspline eval)

DIST_SCM_FILES += sortsmill/math/polyspline/eval.scm
GO_FILES += sortsmill/math/polyspline/eval.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/eval.scm


# (sortsmill math polyspline implicit)

DIST_SCM_FILES += sortsmill/math/polyspline/implicit.scm
GO_FILES += sortsmill/math/polyspline/implicit.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/implicit.scm


# (sortsmill math polyspline intersection)

DIST_SCM_FILES += sortsmill/math/polyspline/intersection.scm
GO_FILES += sortsmill/math/polyspline/intersection.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/intersection.scm


# (sortsmill math polyspline inversion)

DIST_SCM_FILES += sortsmill/math/polyspline/inversion.scm
GO_FILES += sortsmill/math/polyspline/inversion.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/inversion.scm


# (sortsmill math polyspline mul)

DIST_SCM_FILES += sortsmill/math/polyspline/mul.scm
GO_FILES += sortsmill/math/polyspline/mul.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/mul.scm


# (sortsmill math polyspline reduce)

DIST_SCM_FILES += sortsmill/math/polyspline/reduce.scm
GO_FILES += sortsmill/math/polyspline/reduce.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/reduce.scm


# (sortsmill math polyspline roots)

DIST_SCM_FILES += sortsmill/math/polyspline/roots.scm
GO_FILES += sortsmill/math/polyspline/roots.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/roots.scm


## (sortsmill math polyspline self-intersection)
#
#DIST_SCM_FILES += sortsmill/math/polyspline/self-intersection.scm
#GO_FILES += sortsmill/math/polyspline/self-intersection.go
#MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/self-intersection.scm


# (sortsmill math polyspline subdiv)

DIST_SCM_FILES += sortsmill/math/polyspline/subdiv.scm
GO_FILES += sortsmill/math/polyspline/subdiv.go
MATH_POLYSPLINE_REEXPORTED_SCM_FILES += sortsmill/math/polyspline/subdiv.scm

#--------------------------------------------------------------------------
#
# (sortsmill nearness)

DIST_SCM_FILES += sortsmill/nearness.scm
GO_FILES += sortsmill/nearness.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/nearness.scm

#--------------------------------------------------------------------------
#
# (sortsmill notices)

# FIXME: Maybe put this in the (sortsmill editor) category or some
# such.

DIST_SCM_FILES += sortsmill/notices.scm
GO_FILES += sortsmill/notices.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/notices.scm

#--------------------------------------------------------------------------
#
# (sortsmill options)   FIXME: Reimplement this differently.

DIST_SCM_FILES += sortsmill/options.scm
GO_FILES += sortsmill/options.go

#--------------------------------------------------------------------------
#
# (sortsmill postscript)

DIST_SCM_FILES += sortsmill/postscript.scm
GO_FILES += sortsmill/postscript.go
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/postscript.scm

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

DIST_SCM_FILES += sortsmill/pkg-info/directory-layout.scm
GO_FILES += sortsmill/pkg-info/directory-layout.go
PKG_INFO_REEXPORTED_SCM_FILES += sortsmill/pkg-info/directory-layout.scm

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
# (sortsmill pure)

if HAVE_PURE_API

DIST_SCM_FILES += sortsmill/pure.scm
GO_FILES += sortsmill/pure.go

endif HAVE_PURE_API

#--------------------------------------------------------------------------
#
# (sortsmill python)

if HAVE_PYTHON_API

DIST_SCM_FILES += sortsmill/python.scm
GO_FILES += sortsmill/python.go

endif HAVE_PYTHON_API

#--------------------------------------------------------------------------
#
# (sortsmill sfd-to-sxml)    FIXME: Implement this for real, eventually.

DIST_SCM_FILES += sortsmill/sfd-to-sxml.scm
GO_FILES += sortsmill/sfd-to-sxml.go

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

# (sortsmill strings grabbed-strings)

DIST_SCM_FILES += sortsmill/strings/grabbed-strings.scm
GO_FILES += sortsmill/strings/grabbed-strings.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/grabbed-strings.scm

# (sortsmill strings hash-guillemet)

DIST_SCM_FILES += sortsmill/strings/hash-guillemet.scm
GO_FILES += sortsmill/strings/hash-guillemet.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/hash-guillemet.scm

# (sortsmill strings ietf-language-tags)

DIST_SCM_FILES += sortsmill/strings/ietf-language-tags.scm
GO_FILES += sortsmill/strings/ietf-language-tags.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/ietf-language-tags.scm

# (sortsmill strings rexp)

DIST_SCM_FILES += sortsmill/strings/rexp.scm
GO_FILES += sortsmill/strings/rexp.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/rexp.scm

# (sortsmill strings text-embedding)

DIST_SCM_FILES += sortsmill/strings/text-embedding.scm
GO_FILES += sortsmill/strings/text-embedding.go
STRINGS_REEXPORTED_SCM_FILES += sortsmill/strings/text-embedding.scm

#--------------------------------------------------------------------------
#
# (sortsmill svg)

MAKEFILE_GENERATED_SCM_FILES += sortsmill/svg.scm
GO_FILES += sortsmill/svg.go
SVG_REEXPORTED_SCM_FILES =
SORTSMILL_REEXPORTED_SCM_FILES += sortsmill/svg.scm

sortsmill/svg.scm: $(SVG_REEXPORTED_SCM_FILES)
	$(AM_V_REEXPORT)
	$(AM_V_at)$(call generate_reexporter, (sortsmill svg), #t, $(^:%="%")) > $@-tmp
	$(AM_V_at)mv $@-tmp $@


# (sortsmill svg path-data)

DIST_SCM_FILES += sortsmill/svg/path-data.scm
GO_FILES += sortsmill/svg/path-data.go
SVG_REEXPORTED_SCM_FILES += sortsmill/svg/path-data.scm

#--------------------------------------------------------------------------
#
# (sortsmill usermenu ...)

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

endif HAVE_GUI

#--------------------------------------------------------------------------
#
# Guile sources generated from ‘API instructions’ (.apii files).

sortsmill/fontforge-api.scm:							\
		$(top_builddir)/fontforge/fontforge.types.apii	\
		$(srcdir)/generate-guile-api.scm				\
		ff-internal/generate-types.scm
	$(AM_V_GUILEAPI)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-guile-api.scm	\
			'(sortsmill fontforge-api)' $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

sortsmill/gdraw-api.scm: $(top_builddir)/gdraw/gdraw.types.apii	\
		$(srcdir)/generate-guile-api.scm						\
		ff-internal/generate-types.scm
	$(AM_V_GUILEAPI)
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
	$(top_builddir)/fontforge/libsortsmill_fontforge.la		\
	$(top_builddir)/lib/libgnu.la

CLEANFILES += libguile_sortsmill_symbols.c

SCM_SYMBOL_FILES = $(filter-out sortsmill/fontforge-api.scm			\
	sortsmill/gdraw-api.scm, $(DIST_SCM_FILES) $(NODIST_SCM_FILES))

libguile_sortsmill_symbols.c:										\
	$(srcdir)/generate-libguile-sortsmill_symbols.scm				\
	ff-internal/extract-dynlink.scm sortsmill/pkg-info/python.scm	\
	sortsmill/strings/hash-guillemet.scm

AM_V_GENSYM = $(AM_V_GENSYM_$(V))
AM_V_GENSYM_ = $(AM_V_GENSYM_$(AM_DEFAULT_VERBOSITY))
AM_V_GENSYM_0 = @echo "  GENSYM  " $@;

libguile_sortsmill_symbols.c: $(SCM_SYMBOL_FILES)
	$(AM_V_GENSYM)
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
