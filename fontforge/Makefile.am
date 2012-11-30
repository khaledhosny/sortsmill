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

#--------------------------------------------------------------------------

if THE_PROGRAMS

bin_PROGRAMS = fontforge
dist_man_MANS = fontforge.1

if NATIVE_SCRIPTING
nodist_bin_SCRIPTS = fontimage fontlint sfddiff
dist_man_MANS += fontimage.1 fontlint.1 sfddiff.1
endif NATIVE_SCRIPTING

endif THE_PROGRAMS

lib_LTLIBRARIES = libsortsmillff_fontforge.la	\
	libsortsmillff_fontforgeexe.la

#--------------------------------------------------------------------------

FFSCRIPT_HEADER = "\#!$(bindir)/fontforge"

# FIXME: Rewrite these scripts in Guile.
%: %.pe
	echo "${FFSCRIPT_HEADER}" | cat - $< > $@-tmp
	chmod +x $@-tmp
	mv $@-tmp $@

#--------------------------------------------------------------------------

noinst_HEADERS = fontforgeui.h sftextfieldP.h views.h is_LIGATURE.h		\
	flaglist.h namelist_data.h autowidth2.h fontforge.h psfont.h		\
	stemdb.h autowidth.h delta.h fontforgevw.h lookups.h savefont.h		\
	ttf.h baseviews.h fvmetrics.h mm.h scriptfuncs.h ttfinstrs.h		\
	edgelist2.h namehash.h scripting.h uiinterface.h bezctx_ff.h		\
	edgelist.h groups.h nonlineartrans.h sd.h unicoderange.h			\
	bitmapcontrol.h encoding.h ofl.h search.h usermenu.h fffreetype.h	\
	PfEd.h sfd1.h ffpython.h import.h sflayoutP.h print.h				\
	splinefont.h annotations.h

#--------------------------------------------------------------------------

if THE_PROGRAMS

fontforge_SOURCES = main.c
fontforge_LDADD = libsortsmillff_fontforgeexe.la $(LDADD)	\
	$(top_builddir)/lib/libgnu.la
fontforge_LDFLAGS = $(MY_CFLAGS) $(MY_LIBS) -export-dynamic

endif THE_PROGRAMS

#--------------------------------------------------------------------------

libsortsmillff_fontforge_la_SOURCES = asmfpst.c autohint.c autosave.c	\
	autotrace.c autowidth.c bezctx_ff.c bitmapchar.c bitmapcontrol.c	\
	bvedit.c clipnoui.c crctab.c cvexport.c cvimages.c cvundoes.c		\
	dumpbdf.c dumppfa.c effects.c encoding.c featurefile.c				\
	fontviewbase.c freetype.c fvcomposite.c fvfonts.c fvimportbdf.c		\
	fvmetrics.c glyphcomp.c http.c ikarus.c lookups.c macbinary.c		\
	macenc.c mathconstants.c mm.c namelist.c namelist_data.c			\
	nonlineartrans.c noprefs.c nouiutil.c nowakowskittfinstr.c ofl.c	\
	othersubrs.c palmfonts.c parsepdf.c parsepfa.c parsettfatt.c		\
	parsettfbmf.c parsettf.c parsettfvar.c print.c psread.c pua.c		\
	python.c savefont.c scripting.c scstyles.c search.c sfd1.c sfd.c	\
	sflayout.c spiro.c splinechar.c splinefill.c splinefont.c			\
	splineorder2.c splineoverlap.c splinesaveafm.c splinesave.c			\
	splinestroke.c splineutil2.c splineutil.c start.c stemdb.c svg.c	\
	tottfaat.c tottfgpos.c tottf.c tottfvar.c ttfinstrs.c				\
	ttfspecial.c ufo.c unicoderange.c utils.c winfonts.c zapfnomen.c	\
	groups.c langfreq.c ftdelta.c autowidth2.c woff.c activeinui.c		\
	is_LIGATURE.c flaglist.c contour_interface.c
EXTRA_libsortsmillff_fontforge_la_SOURCES = splinerefigure.c

libsortsmillff_fontforge_la_LIBADD = splinerefigure.lo $(LIBADD)	\
	$(top_builddir)/lib/libgnu.la
libsortsmillff_fontforge_la_LDFLAGS = $(call ldflags)
libsortsmillff_fontforge_la_DEPENDENCIES = splinerefigure.lo	\
	$(top_builddir)/lib/libgnu.la

EXTRA_SCRIPTS = generate_codepoint_selector.py

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

if GRAPHICAL_USER_INTERFACE

libsortsmillff_fontforgeexe_la_SOURCES = alignment.c anchorsaway.c		\
	autowidth2dlg.c basedlg.c bdfinfo.c bitmapdlg.c bitmapview.c		\
	charinfo.c charview.c clipui.c combinations.c contextchain.c		\
	cursors.c cvaddpoints.c cvdebug.c cvdgloss.c cvexportdlg.c			\
	cvfreehand.c cvgetinfo.c cvgridfit.c cvhand.c cvhints.c				\
	cvimportdlg.c cvknife.c cvpalettes.c cvpointer.c cvruler.c			\
	cvshapes.c cvstroke.c cvtranstools.c displayfonts.c effectsui.c		\
	encodingui.c fontinfo.c fontview.c freetypeui.c fvfontsdlg.c		\
	fvmetricsdlg.c gotodlg.c groupsdlg.c histograms.c kernclass.c		\
	layer2layer.c lookupui.c macencui.c math.c metricsview.c mmdlg.c	\
	nonlineartransui.c openfontdlg.c prefs.c problems.c savefontdlg.c	\
	scriptingdlg.c scstylesui.c searchview.c sftextfield.c showatt.c	\
	simplifydlg.c startui.c statemachine.c tilepath.c transform.c		\
	ttfinstrsui.c uiutil.c windowmenu.c justifydlg.c deltaui.c			\
	usermenu.c

else !GRAPHICAL_USER_INTERFACE

libsortsmillff_fontforgeexe_la_SOURCES = startnoui.c

endif !GRAPHICAL_USER_INTERFACE

libsortsmillff_fontforgeexe_la_LIBADD = libsortsmillff_fontforge.la		\
	$(LTDLDEPS) $(LIBADD) $(GUILIBADD) $(top_builddir)/lib/libgnu.la
libsortsmillff_fontforgeexe_la_LDFLAGS = $(call ldflags)

#--------------------------------------------------------------------------

LIBADD = $(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/gutils/libsortsmillff_gutils.la

GUILIBADD =
if GRAPHICAL_USER_INTERFACE
GUILIBADD += $(top_builddir)/gdraw/libsortsmillff_gdraw.la	\
	$(XINPUT_LIBS) $(XKB_LIBS)
endif GRAPHICAL_USER_INTERFACE

LDADD = $(LIBADD) $(GUILIBADD)

#--------------------------------------------------------------------------

# In splinerefigure.c, the slight error introduced occasionally by
# optimization turns out to have disastrous effects. Compile it
# without optimization.
splinerefigure.lo: $(srcdir)/splinerefigure.c $(srcdir)/splinefont.h
	$(AM_V_CC) $(LIBTOOL) $(AM_V_lt) --mode=compile $(CC) -g	\
	-DHAVE_CONFIG_H -c -o splinerefigure.lo $(AM_CPPFLAGS)		\
	$(srcdir)/splinerefigure.c

#--------------------------------------------------------------------------

EXTRA_DIST = fontimage.pe fontlint.pe sfddiff.pe fontimage.1	\
	fontlint.1 sfddiff.1
MOSTLYCLEANFILES = fontimage fontlint sfddiff

#--------------------------------------------------------------------------
