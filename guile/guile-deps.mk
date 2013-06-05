sortsmill/__internals__/anchors.go: sortsmill/dynlink.go
sortsmill/__internals__/glyphs.go: sortsmill/dynlink.go
sortsmill/__internals__/lookups.go: sortsmill/dynlink.go
sortsmill/alloc/alloc-die.go: sortsmill/dynlink.go
sortsmill/argv.go: sortsmill/machine.go sortsmill/kwargs.go
sortsmill/arrays.go: sortsmill/dynlink.go
sortsmill/containers/rbmap.go: sortsmill/dynlink.go
sortsmill/editor/main.go: sortsmill/dynlink.go sortsmill/argv.go
sortsmill/editor/main-loop.go: sortsmill/editor/finalization.go sortsmill/dynlink.go sortsmill/kwargs.go
sortsmill/ffcompat.go: sortsmill/machine.go sortsmill/dynlink.go
sortsmill/fonts/anchors.go: sortsmill/fonts/views.go sortsmill/fontforge-api.go sortsmill/i18n.go sortsmill/__internals__.go
sortsmill/fonts/contours.go: sortsmill/svg/path-data.go sortsmill/nearness.go sortsmill/fontforge-api.go sortsmill/dynlink.go sortsmill/i18n.go sortsmill/kwargs.go
sortsmill/fonts/fontinfo-dict.go: sortsmill/dynlink.go
sortsmill/fonts/general.go: sortsmill/fonts/views.go sortsmill/fonts/fontinfo-dict.go sortsmill/fontforge-api.go sortsmill/alloc.go
sortsmill/fonts/glyphs.go: sortsmill/fonts/views.go sortsmill/dynlink.go sortsmill/fontforge-api.go sortsmill/i18n.go sortsmill/__internals__/glyphs.go
sortsmill/fonts/os2-table.go: sortsmill/dynlink.go
sortsmill/fonts/peg-spacing.go: sortsmill/fonts/glyphs.go sortsmill/fonts/anchors.go sortsmill/fontforge-api.go sortsmill/dynlink.go
sortsmill/fonts/private-dict.go: sortsmill/dynlink.go
sortsmill/fonts/psmat.go: sortsmill/math/matrices.go
sortsmill/fonts/t1font-dict.go: sortsmill/dynlink.go
sortsmill/fonts/ufo.go: sortsmill/i18n.go sortsmill/fonts/fontinfo-dict.go sortsmill/fonts/general.go sortsmill/fonts/os2-table.go sortsmill/fonts/private-dict.go sortsmill/fonts/t1font-dict.go
sortsmill/fonts/views.go: sortsmill/fontforge-api.go sortsmill/i18n.go sortsmill/dynlink.go sortsmill/kwargs.go
sortsmill/i18n.go: sortsmill/pkg-info.go sortsmill/dynlink.go
sortsmill/iconv.go: sortsmill/dynlink.go
sortsmill/kwargs.go: sortsmill/i18n.go
sortsmill/math/brentroot.go: sortsmill/math/math-constants.go sortsmill/dynlink.go
sortsmill/math/functions.go: sortsmill/dynlink.go
sortsmill/math/geometry/lines.go: sortsmill/math/matrices.go sortsmill/kwargs.go sortsmill/i18n.go
sortsmill/math/gsl/matrices.go: sortsmill/arrays.go sortsmill/dynlink.go sortsmill/i18n.go
sortsmill/math/multivariate-polynomials.go: sortsmill/math/matrices.go sortsmill/dynlink.go
sortsmill/math/matrices/arithmetic.go: sortsmill/math/matrices/base.go sortsmill/math/gsl/matrices.go sortsmill/arrays.go sortsmill/i18n.go
sortsmill/math/matrices/base.go: sortsmill/dynlink.go sortsmill/i18n.go
sortsmill/math/matrices/bezout.go: sortsmill/math/matrices/base.go sortsmill/math/polyspline/elev.go sortsmill/kwargs.go sortsmill/i18n.go
sortsmill/math/matrices/linalg.go: sortsmill/math/matrices/base.go sortsmill/math/matrices/arithmetic.go sortsmill/math/gsl/matrices.go sortsmill/arrays.go sortsmill/math/math-constants.go sortsmill/i18n.go sortsmill/kwargs.go
sortsmill/math/polyspline/add.go: sortsmill/dynlink.go
sortsmill/math/polyspline/bases.go: sortsmill/math/matrices.go sortsmill/dynlink.go sortsmill/i18n.go
sortsmill/math/polyspline/compose.go: sortsmill/dynlink.go
sortsmill/math/polyspline/deriv.go: sortsmill/dynlink.go
sortsmill/math/polyspline/div.go: sortsmill/math/polyspline/add.go sortsmill/math/polyspline/deriv.go sortsmill/math/polyspline/reduce.go sortsmill/math/matrices.go sortsmill/dynlink.go
sortsmill/math/polyspline/elev.go: sortsmill/dynlink.go
sortsmill/math/polyspline/ellipses.go: sortsmill/dynlink.go
sortsmill/math/polyspline/eval.go: sortsmill/dynlink.go
sortsmill/math/polyspline/implicit.go: sortsmill/math/polyspline/add.go sortsmill/math/polyspline/mul.go sortsmill/math/multivariate-polynomials.go sortsmill/math/matrices.go sortsmill/dynlink.go sortsmill/kwargs.go
sortsmill/math/polyspline/intersection.go: sortsmill/math/polyspline/roots.go sortsmill/dynlink.go
sortsmill/math/polyspline/inversion.go: sortsmill/math/polyspline/add.go sortsmill/math/polyspline/eval.go sortsmill/math/polyspline/reduce.go sortsmill/math/polyspline/roots.go sortsmill/math/matrices.go sortsmill/math/math-constants.go sortsmill/kwargs.go sortsmill/i18n.go
sortsmill/math/polyspline/mul.go: sortsmill/dynlink.go
sortsmill/math/polyspline/reduce.go: sortsmill/dynlink.go
sortsmill/math/polyspline/roots.go: sortsmill/math/polyspline/bases.go sortsmill/math/polyspline/div.go sortsmill/math/polyspline/subdiv.go sortsmill/math/matrices.go sortsmill/dynlink.go sortsmill/kwargs.go sortsmill/i18n.go
sortsmill/math/polyspline/subdiv.go: sortsmill/dynlink.go
sortsmill/nearness.go: sortsmill/dynlink.go
sortsmill/notices.go: sortsmill/dynlink.go
sortsmill/options.go: sortsmill/dynlink.go sortsmill/kwargs.go sortsmill/pkg-info.go sortsmill/api-syntax.go sortsmill/strings.go sortsmill/argv.go sortsmill/alloc.go sortsmill/machine.go
sortsmill/postscript.go: sortsmill/dynlink.go sortsmill/i18n.go
sortsmill/pkg-info/directory-layout.go: sortsmill/dynlink.go
sortsmill/pure.go: sortsmill/i18n.go sortsmill/dynlink.go sortsmill/editor/finalization.go sortsmill/strings.go sortsmill/argv.go
sortsmill/python.go: sortsmill/dynlink.go sortsmill/i18n.go sortsmill/pkg-info.go sortsmill/ffcompat.go sortsmill/editor/finalization.go sortsmill/strings.go
sortsmill/sfd-to-sxml.go: sortsmill/math/math-constants.go sortsmill/i18n.go sortsmill/iconv.go
sortsmill/strings/grabbed-strings.go: sortsmill/alloc.go
sortsmill/strings/rexp.go: sortsmill/dynlink.go
sortsmill/svg/path-data.go: sortsmill/kwargs.go sortsmill/math/polyspline.go sortsmill/math/matrices.go sortsmill/math/geometry.go sortsmill/math/math-constants.go sortsmill/nearness.go
sortsmill/usermenu.go: sortsmill/dynlink.go sortsmill/fontforge-api.go sortsmill/gdraw-api.go sortsmill/i18n.go sortsmill/machine.go sortsmill/notices.go sortsmill/fonts/views.go sortsmill/strings.go sortsmill/strings/hash-guillemet.go
sortsmill/usermenu/pure.go: sortsmill/pure.go sortsmill/fontforge-api.go sortsmill/gdraw-api.go sortsmill/fonts/views.go sortsmill/editor/finalization.go sortsmill/strings/hash-guillemet.go
sortsmill/usermenu/python.go: sortsmill/python.go sortsmill/usermenu.go sortsmill/fontforge-api.go sortsmill/gdraw-api.go sortsmill/machine.go sortsmill/fonts/views.go sortsmill/dynlink.go sortsmill/pkg-info.go sortsmill/notices.go
sortsmill/machine.go: sortsmill/math/math-constants.go
sortsmill/math/gsl/error.go: sortsmill/dynlink.go sortsmill/kwargs.go
sortsmill.go: sortsmill/alloc.go sortsmill/argv.go sortsmill/arrays.go sortsmill/containers.go sortsmill/editor.go sortsmill/fonts.go sortsmill/i18n.go sortsmill/iconv.go sortsmill/kwargs.go sortsmill/machine.go sortsmill/math.go sortsmill/nearness.go sortsmill/notices.go sortsmill/postscript.go sortsmill/pkg-info.go sortsmill/strings.go sortsmill/svg.go
sortsmill/__internals__.go: sortsmill/__internals__/anchors.go sortsmill/__internals__/glyphs.go sortsmill/__internals__/lookups.go
sortsmill/alloc.go: sortsmill/alloc/alloc-base.go sortsmill/alloc/alloc-die.go
sortsmill/containers.go: sortsmill/containers/rbmap.go sortsmill/containers/rnrs-hashtable.go
sortsmill/editor.go: sortsmill/editor/finalization.go sortsmill/editor/main.go sortsmill/editor/main-loop.go
sortsmill/fontforge-api.go: sortsmill/alloc/alloc-base.go sortsmill/pkg-info/package.go sortsmill/machine.go
sortsmill/fonts.go: sortsmill/fonts/anchors.go sortsmill/fonts/contours.go sortsmill/fonts/fontinfo-dict.go sortsmill/fonts/general.go sortsmill/fonts/glyphs.go sortsmill/fonts/os2-table.go sortsmill/fonts/peg-spacing.go sortsmill/fonts/private-dict.go sortsmill/fonts/psmat.go sortsmill/fonts/t1font-dict.go sortsmill/fonts/ufo.go sortsmill/fonts/views.go
sortsmill/gdraw-api.go: sortsmill/alloc/alloc-base.go sortsmill/pkg-info/package.go sortsmill/machine.go
sortsmill/math.go: sortsmill/math/brentroot.go sortsmill/math/functions.go sortsmill/math/geometry.go sortsmill/math/gsl.go sortsmill/math/math-constants.go sortsmill/math/multivariate-polynomials.go sortsmill/math/matrices.go sortsmill/math/polyspline.go
sortsmill/math/geometry.go: sortsmill/math/geometry/lines.go
sortsmill/math/gsl.go: sortsmill/math/gsl/error.go sortsmill/math/gsl/matrices.go
sortsmill/math/matrices.go: sortsmill/math/matrices/arithmetic.go sortsmill/math/matrices/base.go sortsmill/math/matrices/bezout.go sortsmill/math/matrices/linalg.go
sortsmill/math/polyspline.go: sortsmill/math/polyspline/add.go sortsmill/math/polyspline/bases.go sortsmill/math/polyspline/compose.go sortsmill/math/polyspline/deriv.go sortsmill/math/polyspline/div.go sortsmill/math/polyspline/elev.go sortsmill/math/polyspline/ellipses.go sortsmill/math/polyspline/eval.go sortsmill/math/polyspline/implicit.go sortsmill/math/polyspline/intersection.go sortsmill/math/polyspline/inversion.go sortsmill/math/polyspline/mul.go sortsmill/math/polyspline/reduce.go sortsmill/math/polyspline/roots.go sortsmill/math/polyspline/subdiv.go
sortsmill/pkg-info.go: sortsmill/pkg-info/directory-layout.go sortsmill/pkg-info/i18n.go sortsmill/pkg-info/package.go sortsmill/pkg-info/pure.go sortsmill/pkg-info/python.go sortsmill/pkg-info/version.go
sortsmill/strings.go: sortsmill/strings/gc.go sortsmill/strings/grabbed-strings.go sortsmill/strings/hash-guillemet.go sortsmill/strings/rexp.go sortsmill/strings/text-embedding.go
sortsmill/svg.go: sortsmill/svg/path-data.go
