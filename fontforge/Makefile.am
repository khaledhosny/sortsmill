# -*- tab-width: 4 -*- 

# Copyright (C) 2000-2012 by George Williams
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
@includemk@ $(top_srcdir)/mk/tig.mk

bin_PROGRAMS =
noinst_PROGRAMS =
lib_LTLIBRARIES =
nodist_noinst_DATA =
dist_man_MANS =

EXTRA_DIST =
EXTRA_SCRIPTS =

MOSTLYCLEANFILES =
CLEANFILES =
DISTCLEANFILES =

LIBADD = $(top_builddir)/auxiliary/libsortsmill_aux.la	\
	$(top_builddir)/gutils/libsortsmill_gutils.la

GUILIBADD =
if HAVE_GUI
GUILIBADD += $(top_builddir)/gdraw/libsortsmill_gdraw.la
endif HAVE_GUI

LDADD = $(LIBADD) $(GUILIBADD)

#--------------------------------------------------------------------------

noinst_HEADERS = fontforgeui.h sftextfieldP.h views.h is_LIGATURE.h		\
	flaglist.h namelist_data.h autowidth2.h fontforge.h psfont.h		\
	stemdb.h autowidth.h delta.h fontforgevw.h lookups.h generatefont.h		\
	ttf.h baseviews.h fvmetrics.h mm.h ttfinstrs.h edgelist2.h			\
	namehash.h uiinterface.h bezctx_ff.h edgelist.h groups.h			\
	nonlineartrans.h sd.h unicoderange.h bitmapcontrol.h encoding.h		\
	ofl.h search.h usermenu.h fffreetype.h PfEd.h sfd1.h ffpython.h		\
	import.h sflayoutP.h print.h splinefont.h annotations.h compare.h	\
	fontimage.h activeinui.h bezctx.h bezctx_intf.h spiro.h				\
	spiroentrypoints.h zmisc.h

nobase_nodist_include_HEADERS = sortsmill/fontforge_api.h

CLEANFILES += $(nobase_nodist_include_HEADERS)

sortsmill/fontforge_api.h: fontforge.types.apii			\
		$(srcdir)/generate-c-api.h.scm						\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-c-api.h.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-c-api.h.scm

#--------------------------------------------------------------------------

# Generate `API instructions' for relatively opaque access to internal
# types.
fontforge.types.apii: fontforge_type_inspector
	./$< > $@

noinst_PROGRAMS += fontforge_type_inspector
fontforge_type_inspector_LDADD = $(AM_LDADD)	\
	$(top_builddir)/lib/libgnu.la

nodist_noinst_DATA += fontforge.types.apii

EXTRA_DIST += fontforge_type_inspector.tig

CLEANFILES += fontforge_type_inspector.c fontforge.types.apii

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libsortsmill_fontforge.la

libsortsmill_fontforge_la_SOURCES =
libsortsmill_fontforge_la_SOURCES += autohint.c
libsortsmill_fontforge_la_SOURCES += autosave.c
libsortsmill_fontforge_la_SOURCES += autotrace.c
libsortsmill_fontforge_la_SOURCES += autowidth.c
libsortsmill_fontforge_la_SOURCES += bezctx_ff.c
libsortsmill_fontforge_la_SOURCES += bitmapchar.c
libsortsmill_fontforge_la_SOURCES += bitmapcontrol.c
libsortsmill_fontforge_la_SOURCES += bvedit.c
libsortsmill_fontforge_la_SOURCES += clipnoui.c
libsortsmill_fontforge_la_SOURCES += crctab.c
libsortsmill_fontforge_la_SOURCES += cvexport.c
libsortsmill_fontforge_la_SOURCES += cvimages.c
libsortsmill_fontforge_la_SOURCES += cvundoes.c
libsortsmill_fontforge_la_SOURCES += dumpbdf.c
libsortsmill_fontforge_la_SOURCES += dumppfa.c
libsortsmill_fontforge_la_SOURCES += effects.c
libsortsmill_fontforge_la_SOURCES += encoding.c
libsortsmill_fontforge_la_SOURCES += featurefile.c
libsortsmill_fontforge_la_SOURCES += fontviewbase.c
libsortsmill_fontforge_la_SOURCES += fvcomposite.c
libsortsmill_fontforge_la_SOURCES += fvfonts.c
libsortsmill_fontforge_la_SOURCES += fvimportbdf.c
libsortsmill_fontforge_la_SOURCES += fvmetrics.c
libsortsmill_fontforge_la_SOURCES += glyphcomp.c
libsortsmill_fontforge_la_SOURCES += http.c
libsortsmill_fontforge_la_SOURCES += ikarus.c
libsortsmill_fontforge_la_SOURCES += lookups.c
libsortsmill_fontforge_la_SOURCES += macbinary.c
libsortsmill_fontforge_la_SOURCES += macenc.c
libsortsmill_fontforge_la_SOURCES += mathconstants.c
libsortsmill_fontforge_la_SOURCES += mm.c
libsortsmill_fontforge_la_SOURCES += namelist.c
libsortsmill_fontforge_la_SOURCES += namelist_data.c
libsortsmill_fontforge_la_SOURCES += nonlineartrans.c
libsortsmill_fontforge_la_SOURCES += noprefs.c
libsortsmill_fontforge_la_SOURCES += nouiutil.c
libsortsmill_fontforge_la_SOURCES += nowakowskittfinstr.c
libsortsmill_fontforge_la_SOURCES += ofl.c
libsortsmill_fontforge_la_SOURCES += othersubrs.c
libsortsmill_fontforge_la_SOURCES += palmfonts.c
libsortsmill_fontforge_la_SOURCES += parsepdf.c
libsortsmill_fontforge_la_SOURCES += parsepfa.c
libsortsmill_fontforge_la_SOURCES += parsettfatt.c
libsortsmill_fontforge_la_SOURCES += parsettfbmf.c
libsortsmill_fontforge_la_SOURCES += parsettf.c
libsortsmill_fontforge_la_SOURCES += print.c
libsortsmill_fontforge_la_SOURCES += psread.c
libsortsmill_fontforge_la_SOURCES += pua.c
libsortsmill_fontforge_la_SOURCES += python.c
libsortsmill_fontforge_la_SOURCES += generatefont.c
libsortsmill_fontforge_la_SOURCES += scstyles.c
libsortsmill_fontforge_la_SOURCES += search.c
libsortsmill_fontforge_la_SOURCES += sfd1.c
libsortsmill_fontforge_la_SOURCES += sfd.c
libsortsmill_fontforge_la_SOURCES += sflayout.c
libsortsmill_fontforge_la_SOURCES += splinechar.c
libsortsmill_fontforge_la_SOURCES += splinefill.c
libsortsmill_fontforge_la_SOURCES += splinefont.c
libsortsmill_fontforge_la_SOURCES += splineorder2.c
libsortsmill_fontforge_la_SOURCES += splineoverlap.c
libsortsmill_fontforge_la_SOURCES += splinesaveafm.c
libsortsmill_fontforge_la_SOURCES += splinesave.c
libsortsmill_fontforge_la_SOURCES += splinestroke.c
libsortsmill_fontforge_la_SOURCES += splineutil2.c
libsortsmill_fontforge_la_SOURCES += splineutil.c
libsortsmill_fontforge_la_SOURCES += start.c
libsortsmill_fontforge_la_SOURCES += stemdb.c
libsortsmill_fontforge_la_SOURCES += svg.c
libsortsmill_fontforge_la_SOURCES += tottfaat.c
libsortsmill_fontforge_la_SOURCES += tottfgpos.c
libsortsmill_fontforge_la_SOURCES += tottf.c
libsortsmill_fontforge_la_SOURCES += ttfinstrs.c
libsortsmill_fontforge_la_SOURCES += ttfspecial.c
libsortsmill_fontforge_la_SOURCES += ufo.c
libsortsmill_fontforge_la_SOURCES += unicoderange.c
libsortsmill_fontforge_la_SOURCES += utils.c
libsortsmill_fontforge_la_SOURCES += winfonts.c
libsortsmill_fontforge_la_SOURCES += zapfnomen.c
libsortsmill_fontforge_la_SOURCES += groups.c
libsortsmill_fontforge_la_SOURCES += langfreq.c
libsortsmill_fontforge_la_SOURCES += autowidth2.c
libsortsmill_fontforge_la_SOURCES += woff.c
libsortsmill_fontforge_la_SOURCES += activeinui.c
libsortsmill_fontforge_la_SOURCES += is_LIGATURE.c
libsortsmill_fontforge_la_SOURCES += flaglist.c
libsortsmill_fontforge_la_SOURCES += contour_interface.c
libsortsmill_fontforge_la_SOURCES += freetype.c
libsortsmill_fontforge_la_SOURCES += ftdelta.c
libsortsmill_fontforge_la_SOURCES += some_global_variables.c
libsortsmill_fontforge_la_SOURCES += getfontnames.c
libsortsmill_fontforge_la_SOURCES += arrayfree.c
libsortsmill_fontforge_la_SOURCES += spiro_ff.c
libsortsmill_fontforge_la_SOURCES += bezctx.c
libsortsmill_fontforge_la_SOURCES += spiro.c
libsortsmill_fontforge_la_SOURCES += spiroentrypoints.c
libsortsmill_fontforge_la_SOURCES += guile_notices.c
libsortsmill_fontforge_la_SOURCES += guile_fonts_anchors.c
libsortsmill_fontforge_la_SOURCES += splinerefigure.c

nodist_libsortsmill_fontforge_la_SOURCES = fontforge_api.c
#EXTRA_libsortsmill_fontforge_la_SOURCES = splinerefigure.c

#libsortsmill_fontforge_la_LIBADD = splinerefigure.lo $(LIBADD)	\
#	$(top_builddir)/lib/libgnu.la
libsortsmill_fontforge_la_LIBADD = $(LIBADD)
libsortsmill_fontforge_la_LIBADD += $(top_builddir)/lib/libgnu.la
libsortsmill_fontforge_la_LDFLAGS = $(call ldflags) -export-dynamic
libsortsmill_fontforge_la_DEPENDENCIES = splinerefigure.lo	\
	$(top_builddir)/lib/libgnu.la

# # In splinerefigure.c, the slight error introduced occasionally by
# # optimization turns out to have disastrous effects. Compile it
# # without optimization.
# splinerefigure.lo: $(srcdir)/splinerefigure.c $(srcdir)/splinefont.h
# 	$(AM_V_CC) $(LIBTOOL) $(AM_V_lt) --mode=compile $(CC) -g	\
# 	-c -o splinerefigure.lo $(AM_CPPFLAGS)						\
# 	$(srcdir)/splinerefigure.c

EXTRA_SCRIPTS += generate_codepoint_selector.py

fontforge_api.c: fontforge.types.apii sortsmill/fontforge_api.h	\
		$(srcdir)/generate-c-api.c.scm							\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-c-api.c.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-c-api.c.scm

CLEANFILES += fontforge_api.c

# "make is_LIGATURE" or "make -C fontforge is_LIGATURE" to regenerate
# is_LIGATURE.[ch].
is_LIGATURE:
	$(PYTHON) $(srcdir)/generate_codepoint_selector.py \
		'LIGATURE|VULGAR FRACTION' \
		is_LIGATURE_or_VULGAR_FRACTION > $(srcdir)/is_LIGATURE.c
	$(SED) -i -e '1i\
\#include <config.h>\
\#include <is_LIGATURE.h>\
' $(srcdir)/is_LIGATURE.c

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libsortsmill_fontforgeexe.la

if HAVE_GUI

libsortsmill_fontforgeexe_la_SOURCES = alignment.c anchorsaway.c	\
	autowidth2dlg.c basedlg.c bdfinfo.c bitmapdlg.c bitmapview.c	\
	charinfo.c charview.c clipui.c combinations.c contextchain.c	\
	cursors.c cvaddpoints.c cvdebug.c cvdgloss.c cvexportdlg.c		\
	cvfreehand.c cvgetinfo.c cvgridfit.c cvhand.c cvhints.c			\
	cvimportdlg.c cvknife.c cvpalettes.c cvpointer.c cvruler.c		\
	cvshapes.c cvstroke.c cvtranstools.c displayfonts.c effectsui.c	\
	encodingui.c fontinfo.c fontview.c freetypeui.c fvfontsdlg.c	\
	fvmetricsdlg.c gotodlg.c groupsdlg.c histograms.c kernclass.c	\
	layer2layer.c lookupui.c math.c metricsview.c mmdlg.c			\
	nonlineartransui.c openfontdlg.c prefs.c problems.c				\
	generatefontdlg.c scriptingdlg.c scstylesui.c searchview.c		\
	sftextfield.c showatt.c simplifydlg.c startui.c tilepath.c		\
	transform.c ttfinstrsui.c uiutil.c windowmenu.c justifydlg.c	\
	deltaui.c usermenu.c guile_editor_main_loop.c

else !HAVE_GUI

libsortsmill_fontforgeexe_la_SOURCES = fontforgeexe_stub.c

endif !HAVE_GUI

libsortsmill_fontforgeexe_la_LIBADD = libsortsmill_fontforge.la		\
	$(LTDLDEPS) $(LIBADD) $(GUILIBADD) $(top_builddir)/lib/libgnu.la
libsortsmill_fontforgeexe_la_LDFLAGS = $(call ldflags) -export-dynamic
