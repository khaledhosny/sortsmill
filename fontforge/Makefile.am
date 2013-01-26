# Copyright (C) 2000-2012 by George Williams
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
include $(top_srcdir)/mk/tig.mk

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

LIBADD = $(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/gutils/libsortsmillff_gutils.la

GUILIBADD =
if HAVE_GUI
GUILIBADD += $(top_builddir)/gdraw/libsortsmillff_gdraw.la	\
	$(XINPUT_LIBS) $(XKB_LIBS)
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

nobase_nodist_include_HEADERS = sortsmillff/fontforge_api.h

CLEANFILES += $(nobase_nodist_include_HEADERS)

sortsmillff/fontforge_api.h: fontforge.types.apii			\
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

lib_LTLIBRARIES += libsortsmillff_fontforge.la

libsortsmillff_fontforge_la_SOURCES = autohint.c autosave.c	\
	autotrace.c autowidth.c bezctx_ff.c bitmapchar.c bitmapcontrol.c	\
	bvedit.c clipnoui.c crctab.c cvexport.c cvimages.c cvundoes.c		\
	dumpbdf.c dumppfa.c effects.c encoding.c featurefile.c				\
	fontviewbase.c fvcomposite.c fvfonts.c fvimportbdf.c fvmetrics.c	\
	glyphcomp.c http.c ikarus.c lookups.c macbinary.c macenc.c			\
	mathconstants.c mm.c namelist.c namelist_data.c nonlineartrans.c	\
	noprefs.c nouiutil.c nowakowskittfinstr.c ofl.c othersubrs.c		\
	palmfonts.c parsepdf.c parsepfa.c parsettfatt.c parsettfbmf.c		\
	parsettf.c print.c psread.c pua.c python.c			\
	generatefont.c scstyles.c search.c sfd1.c sfd.c sflayout.c				\
	splinechar.c splinefill.c splinefont.c splineorder2.c				\
	splineoverlap.c splinesaveafm.c splinesave.c splinestroke.c			\
	splineutil2.c splineutil.c start.c stemdb.c svg.c tottfaat.c		\
	tottfgpos.c tottf.c tottfvar.c ttfinstrs.c ttfspecial.c ufo.c		\
	unicoderange.c utils.c winfonts.c zapfnomen.c groups.c langfreq.c	\
	autowidth2.c woff.c activeinui.c is_LIGATURE.c flaglist.c			\
	contour_interface.c freetype.c ftdelta.c some_global_variables.c	\
	getfontnames.c arrayfree.c spiro_ff.c bezctx.c spiro.c				\
	spiroentrypoints.c

nodist_libsortsmillff_fontforge_la_SOURCES = fontforge_api.c
EXTRA_libsortsmillff_fontforge_la_SOURCES = splinerefigure.c

libsortsmillff_fontforge_la_LIBADD = splinerefigure.lo $(LIBADD)	\
	$(top_builddir)/lib/libgnu.la
libsortsmillff_fontforge_la_LDFLAGS = $(call ldflags)
libsortsmillff_fontforge_la_DEPENDENCIES = splinerefigure.lo	\
	$(top_builddir)/lib/libgnu.la

# In splinerefigure.c, the slight error introduced occasionally by
# optimization turns out to have disastrous effects. Compile it
# without optimization.
splinerefigure.lo: $(srcdir)/splinerefigure.c $(srcdir)/splinefont.h
	$(AM_V_CC) $(LIBTOOL) $(AM_V_lt) --mode=compile $(CC) -g	\
	-DHAVE_CONFIG_H -c -o splinerefigure.lo $(AM_CPPFLAGS)		\
	$(srcdir)/splinerefigure.c

EXTRA_SCRIPTS += generate_codepoint_selector.py

fontforge_api.c: fontforge.types.apii sortsmillff/fontforge_api.h	\
		$(srcdir)/generate-c-api.c.scm								\
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

lib_LTLIBRARIES += libsortsmillff_fontforgeexe.la

if HAVE_GUI

libsortsmillff_fontforgeexe_la_SOURCES = alignment.c anchorsaway.c		\
	autowidth2dlg.c basedlg.c bdfinfo.c bitmapdlg.c bitmapview.c		\
	charinfo.c charview.c clipui.c combinations.c contextchain.c		\
	cursors.c cvaddpoints.c cvdebug.c cvdgloss.c cvexportdlg.c			\
	cvfreehand.c cvgetinfo.c cvgridfit.c cvhand.c cvhints.c				\
	cvimportdlg.c cvknife.c cvpalettes.c cvpointer.c cvruler.c			\
	cvshapes.c cvstroke.c cvtranstools.c displayfonts.c effectsui.c		\
	encodingui.c fontinfo.c fontview.c freetypeui.c fvfontsdlg.c		\
	fvmetricsdlg.c gotodlg.c groupsdlg.c histograms.c kernclass.c		\
	layer2layer.c lookupui.c math.c metricsview.c mmdlg.c	\
	nonlineartransui.c openfontdlg.c prefs.c problems.c generatefontdlg.c	\
	scriptingdlg.c scstylesui.c searchview.c sftextfield.c showatt.c	\
	simplifydlg.c startui.c tilepath.c transform.c		\
	ttfinstrsui.c uiutil.c windowmenu.c justifydlg.c deltaui.c			\
	usermenu.c

else !HAVE_GUI

libsortsmillff_fontforgeexe_la_SOURCES = fontforgeexe_stub.c

endif !HAVE_GUI

libsortsmillff_fontforgeexe_la_LIBADD = libsortsmillff_fontforge.la		\
	$(LTDLDEPS) $(LIBADD) $(GUILIBADD) $(top_builddir)/lib/libgnu.la
libsortsmillff_fontforgeexe_la_LDFLAGS = $(call ldflags)

#--------------------------------------------------------------------------
