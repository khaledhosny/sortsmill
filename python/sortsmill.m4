# Copyright (C) 2010, 2013 Khaled Hosny and Barry Schwartz
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

#serial 7

# STM_DISABLE_OPENTYPE
# --------------------
AC_DEFUN([STM_DISABLE_OPENTYPE],
[AC_ARG_ENABLE(opentype,
        [AS_HELP_STRING([--disable-opentype],
                        [do not build PostScript-flavored OpenType fonts (default is to build them)])],
        [build_opentype=${enableval}],
        [build_opentype=yes])
opentype_func=nullify
test x"${build_opentype}" = x"yes" && opentype_func=opentype
AC_SUBST(opentype_func)])

# STM_DISABLE_TRUETYPE
# --------------------
AC_DEFUN([STM_DISABLE_TRUETYPE],
[AC_ARG_ENABLE(truetype,
        [AS_HELP_STRING([--disable-truetype],
                        [do not build TrueType fonts (default is to build them)])],
        [build_truetype=${enableval}],
        [build_truetype=yes])
truetype_func=nullify
test x"${build_truetype}" = x"yes" && truetype_func=truetype
AC_SUBST(truetype_func)])

# STM_ENABLE_TRUETYPE
# --------------------
AC_DEFUN([STM_ENABLE_TRUETYPE],
[AC_ARG_ENABLE(truetype,
        [AS_HELP_STRING([--enable-truetype],
                        [build TrueType fonts (default is not to build them)])],
        [build_truetype=${enableval}],
        [build_truetype=no])
truetype_func=nullify
test x"${build_truetype}" = x"yes" && truetype_func=truetype
AC_SUBST(truetype_func)])

# STM_ENABLE_OFL
# --------------
AC_DEFUN([STM_ENABLE_OFL],
[AC_ARG_ENABLE(ofl,
        [AS_HELP_STRING([--enable-ofl],
                        [build SIL Open Font License versions of fonts (default is not to build them)])],
        [build_ofl=${enableval}],
        [build_ofl=no])
ofl_func=nullify
test x"${build_ofl}" = x"yes" && ofl_func=ofl
AC_SUBST(ofl_func)
])

# STM_DISABLE_MIT
# ---------------
AC_DEFUN([STM_DISABLE_MIT],
[AC_ARG_ENABLE(mit,
        [AS_HELP_STRING([--disable-mit],
                        [do not build MIT-license versions of fonts (default is to build them)])],
        [build_mit=${enableval}],
        [build_mit=yes])
mit_func=nullify
test x"${build_mit}" = x"yes" && mit_func=mit
AC_SUBST(mit_func)
])

# STM_ENABLE_GRAPHITE
# -------------------
AC_DEFUN([STM_ENABLE_GRAPHITE],
[AC_ARG_ENABLE(graphite,
        [AS_HELP_STRING([--enable-graphite],
                        [include Graphite support in TrueType fonts (default is not to include Graphite support)])],
        [build_graphite=${enableval}],
        [build_graphite=no])
MKFONT_GRAPHITE_FLAGS=""
test x"${build_graphite}" = x"yes" && MKFONT_GRAPHITE_FLAGS="--graphite"
AC_SUBST(MKFONT_GRAPHITE_FLAGS)
])

# STM_SORTSMILL_ENABLES
# ---------------------
AC_DEFUN([STM_SORTSMILL_ENABLES],
[STM_DISABLE_OPENTYPE
STM_DISABLE_TRUETYPE
STM_ENABLE_OFL
STM_DISABLE_MIT])

# STM_SORTSMILL_MIT_ENABLES
# ---------------------
AC_DEFUN([STM_SORTSMILL_MIT_ENABLES],
[STM_DISABLE_OPENTYPE
STM_ENABLE_TRUETYPE
mit_func=mit
AC_SUBST(mit_func)])

# STM_SORTSMILL_NONLICENSE_ENABLES
# --------------------------------
AC_DEFUN([STM_SORTSMILL_NONLICENSE_ENABLES],
[STM_DISABLE_OPENTYPE
STM_DISABLE_TRUETYPE])

# STM_PROG_SORTSMILL_LINT
# -----------------------
AC_DEFUN([STM_PROG_SORTSMILL_LINT],
[AC_CHECK_PROG(HAVE_SORTSMILL_LINT, [sortsmill-lint], [yes])
test x"${HAVE_SORTSMILL_LINT}" = x"yes" || AC_MSG_ERROR([I need sortsmill-lint, which is part of Sorts Mill Tools.])])

# STM_PROG_GRCOMPILER
# -------------------
AC_DEFUN([STM_PROG_GRCOMPILER],
[if test x"${build_graphite}" = x"yes" -a x"${build_truetype}" = x"yes" ; then
AC_CHECK_PROG(HAVE_GRCOMPILER, [grcompiler], [yes])
test x"${HAVE_GRCOMPILER}" = x"yes" || AC_MSG_ERROR([Graphite compiler not found.])
fi
])

# STM_PROG_ASYMPTOTE
# ------------------
AC_DEFUN([STM_PROG_ASYMPTOTE],
[AC_CHECK_PROG(HAVE_ASYMPTOTE, [asy], [yes])
test x"${HAVE_ASYMPTOTE}" = x"yes" || AC_MSG_ERROR([I need Asymptote (http://asymptote.sourceforge.net/).])])

# STM_PROG_OCAMLFIND
# ------------------
AC_DEFUN([STM_PROG_OCAMLFIND],
[AC_ARG_VAR([OCAMLFIND], [ocamlfind command])
AC_ARG_VAR([OCAMLFINDFLAGS], [ocamlfind flags])
if test -z "${OCAMLFIND}" ; then
   AC_CHECK_PROGS(OCAMLFIND, [ocamlfind])
   if test -z "${OCAMLFIND}" ; then
      AC_MSG_ERROR([The ocamlfind program is required but not found. Giving up.])
   fi
fi])

# STM_CHECK_FONTFORGE_EXTENSION
# -----------------------------
AC_DEFUN([STM_CHECK_FONTFORGE_EXTENSION],
[AC_MSG_CHECKING([for the 'fontforge' extension for Python])
if ${PYTHON} -c 'import fontforge' 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need the 'fontforge' extension.])
fi])

# STM_FONTSDIR
# ------------
AC_DEFUN([STM_FONTSDIR],
[AC_ARG_WITH(fontsdir,
        [AS_HELP_STRING([--with-fontsdir=DIR],
                        [directory for generated fonts [DIR=DATAROOTDIR/fonts/sortsmill]])],
        [fontsdir=${withval}],
        [fontsdir=${datarootdir}/fonts/sortsmill])
AC_SUBST([fontsdir])
])

# STM_SORTSMILL_PREREQUISITES
# ---------------------------
AC_DEFUN([STM_SORTSMILL_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([STM_SORTSMILL_ENABLES])
AC_REQUIRE([STM_PROG_SORTSMILL_LINT])
AC_REQUIRE([STM_FONTSDIR])
])

# STM_SORTSMILL_MIT_PREREQUISITES
# ---------------------------
AC_DEFUN([STM_SORTSMILL_MIT_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([STM_SORTSMILL_MIT_ENABLES])
AC_REQUIRE([STM_PROG_SORTSMILL_LINT])
AC_REQUIRE([STM_FONTSDIR])
])

# STM_SORTSMILL_NONLICENSE_PREREQUISITES
# --------------------------------------
AC_DEFUN([STM_SORTSMILL_NONLICENSE_PREREQUISITES],
[AM_PATH_PYTHON([2.6])
AC_REQUIRE([STM_SORTSMILL_NONLICENSE_ENABLES])
AC_REQUIRE([STM_PROG_SORTSMILL_LINT])
AC_REQUIRE([STM_FONTSDIR])
])

# STM_INIT_SORTSMILL
# ------------------
AC_DEFUN([STM_INIT_SORTSMILL],
[
AC_SUBST([nullify],[])
AC_SUBST([opentype],['$(foreach f, ${1}, ${f}.otf)'])
AC_SUBST([truetype],['$(foreach f, ${1}, $(shell echo ${f} | sed -e "/-/{s/\(.*\)\(-.*\)/\1TT\2.ttf/; p; d};s/\(.*\)/\1TT.ttf/"))'])
AC_SUBST([mit],['${1}'])
AC_SUBST([ofl],['$(foreach f, ${1}, OFL${f})'])

AC_SUBST([MAKEFONTS],[make-fonts])
AC_SUBST([MKFONT],['${MAKEFONTS} --input-directory=${srcdir} --output-directory=${builddir} ${MKFONT_FLAGS}'])

AC_SUBST([expand_fonts],[' \
	$(call ${mit_func}, $(call ${opentype_func}, ${1})) \
	$(call ${mit_func}, $(call ${truetype_func}, ${1})) \
	$(call ${ofl_func}, $(call ${opentype_func}, ${1})) \
	$(call ${ofl_func}, $(call ${truetype_func}, ${1}))'])
AC_SUBST([expand_fonts_without_license],[' \
	$(call opentype, ${1}) \
	$(call truetype, ${1})'])
AC_SUBST([expand_mit_fonts],[' \
	$(call mit, $(call opentype, ${1})) \
	$(call mit, $(call truetype, ${1}))'])
AC_SUBST([expand_mit_opentype_fonts],[' \
	$(call mit, $(call opentype, ${1}))'])
AC_SUBST([expand_ofl_fonts],[' \
	$(call ofl, $(call opentype, ${1})) \
	$(call ofl, $(call truetype, ${1}))'])
AC_SUBST([expand_nonlicense_fonts],[' \
	$(call ${opentype_func}, ${1}) \
	$(call ${truetype_func}, ${1})'])

AM_SUBST_NOTMAKE(sortsmill_rules)
AC_SUBST([sortsmill_rules],['
OFL%.otf              : %.sfd  ; ${MKFONT} $(basename [$]@)
%.otf                 : %.sfd  ; ${MKFONT} $(basename [$]@)
OFL%TT.ttf            : %.sfd  ; ${MKFONT} $(basename [$]@)
%TT.ttf               : %.sfd  ; ${MKFONT} $(basename [$]@)

# Is there a more general and simple way to handle the following
# within make?
OFL%TT-Italic.ttf     : %-Italic.sfd  ; ${MKFONT} $(basename [$]@)
%TT-Italic.ttf        : %-Italic.sfd  ; ${MKFONT} $(basename [$]@)
OFL%TT-Bold.ttf       : %-Bold.sfd    ; ${MKFONT} $(basename [$]@)
%TT-Bold.ttf          : %-Bold.sfd    ; ${MKFONT} $(basename [$]@)
OFL%TT-BoldItalic.ttf : %-BoldItalic.sfd  ; ${MKFONT} $(basename [$]@)
%TT-BoldItalic.ttf    : %-BoldItalic.sfd  ; ${MKFONT} $(basename [$]@)
'
])
])

# STM_INIT_ASYMPTOTE
# ------------------
AC_DEFUN([STM_INIT_ASYMPTOTE],
 [
AC_SUBST([asymptote_rules],['
%.otf %.ttf %.woff : %.asy
	((cd [$]{srcdir}; asy -u "generate(\"[$]@\",\"opentype\")" [$]*) | [$](PYTHON) -)
 
%.svg %.ufo : %.asy
	((cd [$]{srcdir}; asy -u "generate(\"[$]@\")" [$]*) | [$](PYTHON) -)

%.pfa %.pfb %.cff %.t42 %.pt3 %.ps %.bin : %.asy
	((cd [$]{srcdir}; asy -u "generate(\"[$]@\",\"afm\")" [$]*) | [$](PYTHON) -)

%.sfd : %.asy
	((cd [$]{srcdir}; asy -u "save(\"[$]@\")" [$]*) | [$](PYTHON) -)
'
])
])

# STM_INIT_OCAML
# --------------
AC_DEFUN([STM_INIT_OCAML],
[
AC_SUBST([ocaml_rules],['
%.cmi: %.mli      ; [$](OCAMLC) -c [$]< -o [$]@
%.cmo: %.ml %.cmi ; [$](OCAMLC) -c [$]< -o [$]@
%.cmx: %.ml %.cmi ; [$](OCAMLOPT) -c [$]< -o [$]@

%.otf %.ttf %.woff: %_program ; [$](builddir)/[$]< --font=[$]@ --flags=['\"\'opentype\'\"'] | [$](PYTHON) -
%.svg %.ufo: %_program ; [$](builddir)/[$]< --font=[$]@ --flags=['\"\"'] | [$](PYTHON) -
%.pfa %.pfb %.cff %.t42 %.pt3 %.ps %.bin: %_program ; [$](builddir)/[$]< --font=[$]@ --flags=['\"\'afm\'\"'] | [$](PYTHON) -
%.sfd: %_program ; [$](builddir)/[$]< --sfd=[$]@ | [$](PYTHON) -
'
])
])

# STM_ENABLE_OCAML
# ----------------
AC_DEFUN([STM_ENABLE_OCAMLOPT],
[AC_ARG_ENABLE(ocamlopt,
        [AS_HELP_STRING([--enable-ocamlopt],
                        [use ocamlopt instead of ocamlc])],
        [ocamlopt=${enableval}],
        [ocamlopt=no])
AM_CONDITIONAL([USE_OCAMLOPT], [test x"${ocamlopt}" = xyes])
])

# STM_CHECK_OCAML_BATTERIES
# -------------------------
AC_DEFUN([STM_CHECK_OCAML_BATTERIES],
[AC_MSG_CHECKING([for the OCaml Batteries module])
if ${OCAMLFIND} query batteries 1>/dev/null 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need OCaml Batteries. Giving up.])
fi])

# STM_CHECK_OCAML_SORTSMILL
# -------------------------
AC_DEFUN([STM_CHECK_OCAML_SORTSMILL],
[AC_MSG_CHECKING([for the Sorts Mill OCaml module])
if ${OCAMLFIND} query sortsmill 1>/dev/null 2>/dev/null ; then
   AC_MSG_RESULT([yes])
else
   AC_MSG_RESULT([no])
   AC_MSG_ERROR([I need the Sorts Mill OCaml module. Giving up.])
fi])

# STM_TARGET_MIT_BINPACK
# ----------------------
AC_DEFUN([STM_TARGET_MIT_BINPACK],[
AC_SUBST([MIT_BINPACK_FILES],['${srcdir}/COPYING $(call expand_mit_fonts, ${FONTS})'])
AC_SUBST([MIT_BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_mit_binpack_rules])
AC_SUBST([sortsmill_mit_binpack_rules],['
${MIT_BINPACK}: ${MIT_BINPACK_FILES}
	rm -f ${MIT_BINPACK}
	zip -j ${MIT_BINPACK} ${MIT_BINPACK_FILES}
'
])
])

# STM_TARGET_MIT_OPENTYPE_BINPACK
# -------------------------------
AC_DEFUN([STM_TARGET_MIT_OPENTYPE_BINPACK],[
AC_SUBST([MIT_OPENTYPE_BINPACK_FILES],['${srcdir}/COPYING $(call expand_mit_opentype_fonts, ${FONTS})'])
AC_SUBST([MIT_OPENTYPE_BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_mit_opentype_binpack_rules])
AC_SUBST([sortsmill_mit_opentype_binpack_rules],['
${MIT_OPENTYPE_BINPACK}: ${MIT_OPENTYPE_BINPACK_FILES}
	rm -f ${MIT_OPENTYPE_BINPACK}
	zip -j ${MIT_OPENTYPE_BINPACK} ${MIT_OPENTYPE_BINPACK_FILES}
'
])
])

# STM_TARGET_OFL_BINPACK
# ----------------------
AC_DEFUN([STM_TARGET_OFL_BINPACK],[
AC_SUBST([OFL_BINPACK_FILES],
    ['${srcdir}/OFL/FONTLOG.txt ${srcdir}/OFL/OFL.txt ${srcdir}/OFL/OFL-FAQ.txt \
      $(call expand_ofl_fonts, ${FONTS})'])
AC_SUBST([OFL_BINPACK],['ofl-${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_ofl_binpack_rules])
AC_SUBST([sortsmill_ofl_binpack_rules],['
${OFL_BINPACK}: ${OFL_BINPACK_FILES}
	rm -f ${OFL_BINPACK}
	zip -j ${OFL_BINPACK} ${OFL_BINPACK_FILES}
'
])
])

# STM_TARGET_NONLICENSE_BINPACK
# -----------------------------
AC_DEFUN([STM_TARGET_NONLICENSE_BINPACK],[
AC_SUBST([NONLICENSE_BINPACK_FILES],['${srcdir}/COPYING $(call expand_nonlicense_fonts, ${FONTS})'])
AC_SUBST([NONLICENSE_BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_nonlicense_binpack_rules])
AC_SUBST([sortsmill_nonlicense_binpack_rules],['
${NONLICENSE_BINPACK}: ${NONLICENSE_BINPACK_FILES}
	rm -f ${NONLICENSE_BINPACK}
	zip -j ${NONLICENSE_BINPACK} ${NONLICENSE_BINPACK_FILES}
'
])
])

# STM_TARGET_BINPACK
# -------------------------------
AC_DEFUN([STM_TARGET_BINPACK],[
AC_SUBST([BINPACK_FILES],['${srcdir}/COPYING $(addsuffix .otf, ${FONTS})'])
AC_SUBST([BINPACK],['${FAMILYNAME}-${PACKAGE_VERSION}.zip'])
AM_SUBST_NOTMAKE([sortsmill_binpack_rules])
AC_SUBST([sortsmill_binpack_rules],['
${BINPACK}: ${BINPACK_FILES}
	rm -f ${BINPACK}
	zip -j ${BINPACK} ${BINPACK_FILES}
'
])
])