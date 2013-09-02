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

# Copyright (C) 2000-2012 by George Williams
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

# See
# http://www.gnu.org/software/libtool/manual/libtool.html#Versioning
#VERSION_INFO = -version-info 0:0:0
VERSION_INFO = -release $(VERSION)

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

dist_libsortsmill_fontforge_la_SOURCES =
nodist_libsortsmill_fontforge_la_SOURCES =

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libsortsmill_fontforge.la

if HAVE_GUI
libsortsmill_fontforge_la_LIBADD = \
	$(top_builddir)/gdraw/libsortsmill_gdraw.la \
	$(top_builddir)/gutils/libsortsmill_gutils.la \
	$(top_builddir)/auxiliary/libsortsmill_aux.la \
	$(top_builddir)/lib/libgnu.la
else !HAVE_GUI
libsortsmill_fontforge_la_LIBADD = \
	$(top_builddir)/gutils/libsortsmill_gutils.la \
	$(top_builddir)/auxiliary/libsortsmill_aux.la \
	$(top_builddir)/lib/libgnu.la
endif !HAVE_GUI

libsortsmill_fontforge_la_LDFLAGS = $(MY_CFLAGS) -export-dynamic	\
	$(VERSION_INFO) $(MY_LIBS)

#--------------------------------------------------------------------------

noinst_HEADERS =
noinst_HEADERS += fontforgeui.h
noinst_HEADERS += sftextfieldP.h
noinst_HEADERS += views.h
noinst_HEADERS += is_LIGATURE.h
noinst_HEADERS += flaglist.h
noinst_HEADERS += namelist_data.h
noinst_HEADERS += autowidth2.h
noinst_HEADERS += fontforge.h
noinst_HEADERS += psfont.h
noinst_HEADERS += stemdb.h
noinst_HEADERS += autowidth.h
noinst_HEADERS += delta.h
noinst_HEADERS += fontforgevw.h
noinst_HEADERS += lookups.h
noinst_HEADERS += generatefont.h
noinst_HEADERS += ttf.h
noinst_HEADERS += baseviews.h
noinst_HEADERS += fvmetrics.h
noinst_HEADERS += mm.h
noinst_HEADERS += ttfinstrs.h
noinst_HEADERS += edgelist2.h
noinst_HEADERS += namehash.h
noinst_HEADERS += uiinterface.h
noinst_HEADERS += bezctx_ff.h
noinst_HEADERS += edgelist.h
noinst_HEADERS += groups.h
noinst_HEADERS += nonlineartrans.h
noinst_HEADERS += sd.h
noinst_HEADERS += unicoderange.h
noinst_HEADERS += bitmapcontrol.h
noinst_HEADERS += encoding.h
noinst_HEADERS += ofl.h
noinst_HEADERS += search.h
noinst_HEADERS += usermenu.h
noinst_HEADERS += fffreetype.h
noinst_HEADERS += PfEd.h
noinst_HEADERS += sfd1.h
noinst_HEADERS += ffpython.h
noinst_HEADERS += sflayoutP.h
noinst_HEADERS += print.h
noinst_HEADERS += splinefont.h
noinst_HEADERS += annotations.h
noinst_HEADERS += compare.h
noinst_HEADERS += fontimage.h
noinst_HEADERS += activeinui.h
noinst_HEADERS += bezctx.h
noinst_HEADERS += bezctx_intf.h
noinst_HEADERS += spiro.h
noinst_HEADERS += spiroentrypoints.h
noinst_HEADERS += zmisc.h
noinst_HEADERS += guile_fonts_os2_hhea.h
noinst_HEADERS += guile_fonts_table.h
nobase_nodist_include_HEADERS = sortsmill/fontforge_api.h
CLEANFILES += sortsmill/fontforge_api.h

dist_libsortsmill_fontforge_la_SOURCES += autohint.c
dist_libsortsmill_fontforge_la_SOURCES += autosave.c
dist_libsortsmill_fontforge_la_SOURCES += autotrace.c
dist_libsortsmill_fontforge_la_SOURCES += autowidth.c
dist_libsortsmill_fontforge_la_SOURCES += baseviews.c
dist_libsortsmill_fontforge_la_SOURCES += bezctx_ff.c
dist_libsortsmill_fontforge_la_SOURCES += bitmapchar.c
dist_libsortsmill_fontforge_la_SOURCES += bitmapcontrol.c
dist_libsortsmill_fontforge_la_SOURCES += bvedit.c
dist_libsortsmill_fontforge_la_SOURCES += charviewbase.c
dist_libsortsmill_fontforge_la_SOURCES += clipnoui.c
dist_libsortsmill_fontforge_la_SOURCES += crctab.c
dist_libsortsmill_fontforge_la_SOURCES += cvexport.c
dist_libsortsmill_fontforge_la_SOURCES += cvimages.c
dist_libsortsmill_fontforge_la_SOURCES += cvundoes.c
dist_libsortsmill_fontforge_la_SOURCES += dumpbdf.c
dist_libsortsmill_fontforge_la_SOURCES += dumppfa.c
dist_libsortsmill_fontforge_la_SOURCES += effects.c
dist_libsortsmill_fontforge_la_SOURCES += encmap.c
dist_libsortsmill_fontforge_la_SOURCES += encoding.c
dist_libsortsmill_fontforge_la_SOURCES += featurefile.c
dist_libsortsmill_fontforge_la_SOURCES += fontviewbase.c
dist_libsortsmill_fontforge_la_SOURCES += fvcomposite.c
dist_libsortsmill_fontforge_la_SOURCES += fvfonts.c
dist_libsortsmill_fontforge_la_SOURCES += fvimportbdf.c
dist_libsortsmill_fontforge_la_SOURCES += fvmetrics.c
dist_libsortsmill_fontforge_la_SOURCES += glyphcomp.c
dist_libsortsmill_fontforge_la_SOURCES += glyphs.c
dist_libsortsmill_fontforge_la_SOURCES += http.c
dist_libsortsmill_fontforge_la_SOURCES += ikarus.c
dist_libsortsmill_fontforge_la_SOURCES += lookups.c
dist_libsortsmill_fontforge_la_SOURCES += macbinary.c
dist_libsortsmill_fontforge_la_SOURCES += macenc.c
dist_libsortsmill_fontforge_la_SOURCES += mathconstants.c
dist_libsortsmill_fontforge_la_SOURCES += mm.c
dist_libsortsmill_fontforge_la_SOURCES += namelist.c
dist_libsortsmill_fontforge_la_SOURCES += namelist_data.c
dist_libsortsmill_fontforge_la_SOURCES += nonlineartrans.c
dist_libsortsmill_fontforge_la_SOURCES += noprefs.c
dist_libsortsmill_fontforge_la_SOURCES += nouiutil.c
dist_libsortsmill_fontforge_la_SOURCES += nowakowskittfinstr.c
dist_libsortsmill_fontforge_la_SOURCES += ofl.c
dist_libsortsmill_fontforge_la_SOURCES += othersubrs.c
dist_libsortsmill_fontforge_la_SOURCES += palmfonts.c
dist_libsortsmill_fontforge_la_SOURCES += parsepdf.c
dist_libsortsmill_fontforge_la_SOURCES += parsepfa.c
dist_libsortsmill_fontforge_la_SOURCES += parsettfatt.c
dist_libsortsmill_fontforge_la_SOURCES += parsettfbmf.c
dist_libsortsmill_fontforge_la_SOURCES += parsettf.c
dist_libsortsmill_fontforge_la_SOURCES += print.c
dist_libsortsmill_fontforge_la_SOURCES += psread.c
dist_libsortsmill_fontforge_la_SOURCES += pua.c
dist_libsortsmill_fontforge_la_SOURCES += python.c
dist_libsortsmill_fontforge_la_SOURCES += generatefont.c
dist_libsortsmill_fontforge_la_SOURCES += scstyles.c
dist_libsortsmill_fontforge_la_SOURCES += search.c
dist_libsortsmill_fontforge_la_SOURCES += sfd1.c
dist_libsortsmill_fontforge_la_SOURCES += sfd.c
dist_libsortsmill_fontforge_la_SOURCES += sflayout.c
dist_libsortsmill_fontforge_la_SOURCES += splinechar.c
dist_libsortsmill_fontforge_la_SOURCES += splinefill.c
dist_libsortsmill_fontforge_la_SOURCES += splinefont.c
dist_libsortsmill_fontforge_la_SOURCES += splineorder2.c
dist_libsortsmill_fontforge_la_SOURCES += splineoverlap.c
dist_libsortsmill_fontforge_la_SOURCES += splinesaveafm.c
dist_libsortsmill_fontforge_la_SOURCES += splinesave.c
dist_libsortsmill_fontforge_la_SOURCES += splinestroke.c
dist_libsortsmill_fontforge_la_SOURCES += splineutil2.c
dist_libsortsmill_fontforge_la_SOURCES += splineutil.c
dist_libsortsmill_fontforge_la_SOURCES += start.c
dist_libsortsmill_fontforge_la_SOURCES += stemdb.c
dist_libsortsmill_fontforge_la_SOURCES += svg.c
dist_libsortsmill_fontforge_la_SOURCES += tottfaat.c
dist_libsortsmill_fontforge_la_SOURCES += tottfgpos.c
dist_libsortsmill_fontforge_la_SOURCES += tottf.c
dist_libsortsmill_fontforge_la_SOURCES += ttfinstrs.c
dist_libsortsmill_fontforge_la_SOURCES += ttfspecial.c
dist_libsortsmill_fontforge_la_SOURCES += ufo.c
dist_libsortsmill_fontforge_la_SOURCES += unicoderange.c
dist_libsortsmill_fontforge_la_SOURCES += winfonts.c
dist_libsortsmill_fontforge_la_SOURCES += zapfnomen.c
dist_libsortsmill_fontforge_la_SOURCES += groups.c
dist_libsortsmill_fontforge_la_SOURCES += langfreq.c
dist_libsortsmill_fontforge_la_SOURCES += autowidth2.c
dist_libsortsmill_fontforge_la_SOURCES += woff.c
dist_libsortsmill_fontforge_la_SOURCES += activeinui.c
dist_libsortsmill_fontforge_la_SOURCES += is_LIGATURE.c
dist_libsortsmill_fontforge_la_SOURCES += flaglist.c
dist_libsortsmill_fontforge_la_SOURCES += contour_interface.c
dist_libsortsmill_fontforge_la_SOURCES += freetype.c
dist_libsortsmill_fontforge_la_SOURCES += ftdelta.c
dist_libsortsmill_fontforge_la_SOURCES += some_global_variables.c
dist_libsortsmill_fontforge_la_SOURCES += getfontnames.c
dist_libsortsmill_fontforge_la_SOURCES += arrayfree.c
dist_libsortsmill_fontforge_la_SOURCES += spiro_ff.c
dist_libsortsmill_fontforge_la_SOURCES += bezctx.c
dist_libsortsmill_fontforge_la_SOURCES += spiro.c
dist_libsortsmill_fontforge_la_SOURCES += spiroentrypoints.c
dist_libsortsmill_fontforge_la_SOURCES += splinerefigure.c
dist_libsortsmill_fontforge_la_SOURCES += guile_internals_anchors.c
dist_libsortsmill_fontforge_la_SOURCES += guile_internals_glyphs.c
dist_libsortsmill_fontforge_la_SOURCES += guile_internals_lookups.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_anchors.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_contours.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_font_formats.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_fontinfo_dict.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_glyphs.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_head_table.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_hhea_table.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_os2_hhea.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_os2_table.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_peg_spacing.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_private_dict.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_t1font_dict.c
dist_libsortsmill_fontforge_la_SOURCES += guile_fonts_views.c
dist_libsortsmill_fontforge_la_SOURCES += guile_notices.c
nodist_libsortsmill_fontforge_la_SOURCES += fontforge_api.c
CLEANFILES += fontforge_api.c

if HAVE_GUI
dist_libsortsmill_fontforge_la_SOURCES += alignment.c
dist_libsortsmill_fontforge_la_SOURCES += anchorsaway.c
dist_libsortsmill_fontforge_la_SOURCES += autowidth2dlg.c
dist_libsortsmill_fontforge_la_SOURCES += basedlg.c
dist_libsortsmill_fontforge_la_SOURCES += bdfinfo.c
dist_libsortsmill_fontforge_la_SOURCES += bitmapdlg.c
dist_libsortsmill_fontforge_la_SOURCES += bitmapview.c
dist_libsortsmill_fontforge_la_SOURCES += charinfo.c
dist_libsortsmill_fontforge_la_SOURCES += charview.c
dist_libsortsmill_fontforge_la_SOURCES += clipui.c
dist_libsortsmill_fontforge_la_SOURCES += combinations.c
dist_libsortsmill_fontforge_la_SOURCES += contextchain.c
dist_libsortsmill_fontforge_la_SOURCES += cursors.c
dist_libsortsmill_fontforge_la_SOURCES += cvaddpoints.c
dist_libsortsmill_fontforge_la_SOURCES += cvdebug.c
dist_libsortsmill_fontforge_la_SOURCES += cvdgloss.c
dist_libsortsmill_fontforge_la_SOURCES += cvexportdlg.c
dist_libsortsmill_fontforge_la_SOURCES += cvfreehand.c
dist_libsortsmill_fontforge_la_SOURCES += cvgetinfo.c
dist_libsortsmill_fontforge_la_SOURCES += cvgridfit.c
dist_libsortsmill_fontforge_la_SOURCES += cvhand.c
dist_libsortsmill_fontforge_la_SOURCES += cvhints.c
dist_libsortsmill_fontforge_la_SOURCES += cvimportdlg.c
dist_libsortsmill_fontforge_la_SOURCES += cvknife.c
dist_libsortsmill_fontforge_la_SOURCES += cvpalettes.c
dist_libsortsmill_fontforge_la_SOURCES += cvpointer.c
dist_libsortsmill_fontforge_la_SOURCES += cvruler.c
dist_libsortsmill_fontforge_la_SOURCES += cvshapes.c
dist_libsortsmill_fontforge_la_SOURCES += cvstroke.c
dist_libsortsmill_fontforge_la_SOURCES += cvtranstools.c
dist_libsortsmill_fontforge_la_SOURCES += displayfonts.c
dist_libsortsmill_fontforge_la_SOURCES += effectsui.c
dist_libsortsmill_fontforge_la_SOURCES += encodingui.c
dist_libsortsmill_fontforge_la_SOURCES += fontinfo.c
dist_libsortsmill_fontforge_la_SOURCES += fontview.c
dist_libsortsmill_fontforge_la_SOURCES += freetypeui.c
dist_libsortsmill_fontforge_la_SOURCES += fvfontsdlg.c
dist_libsortsmill_fontforge_la_SOURCES += fvmetricsdlg.c
dist_libsortsmill_fontforge_la_SOURCES += gotodlg.c
dist_libsortsmill_fontforge_la_SOURCES += groupsdlg.c
dist_libsortsmill_fontforge_la_SOURCES += histograms.c
dist_libsortsmill_fontforge_la_SOURCES += kernclass.c
dist_libsortsmill_fontforge_la_SOURCES += layer2layer.c
dist_libsortsmill_fontforge_la_SOURCES += lookupui.c
dist_libsortsmill_fontforge_la_SOURCES += math.c
dist_libsortsmill_fontforge_la_SOURCES += metricsview.c
dist_libsortsmill_fontforge_la_SOURCES += mmdlg.c
dist_libsortsmill_fontforge_la_SOURCES += nonlineartransui.c
dist_libsortsmill_fontforge_la_SOURCES += openfontdlg.c
dist_libsortsmill_fontforge_la_SOURCES += prefs.c
dist_libsortsmill_fontforge_la_SOURCES += problems.c
dist_libsortsmill_fontforge_la_SOURCES += generatefontdlg.c
dist_libsortsmill_fontforge_la_SOURCES += scriptingdlg.c
dist_libsortsmill_fontforge_la_SOURCES += scstylesui.c
dist_libsortsmill_fontforge_la_SOURCES += searchview.c
dist_libsortsmill_fontforge_la_SOURCES += sftextfield.c
dist_libsortsmill_fontforge_la_SOURCES += showatt.c
dist_libsortsmill_fontforge_la_SOURCES += simplifydlg.c
dist_libsortsmill_fontforge_la_SOURCES += startui.c
dist_libsortsmill_fontforge_la_SOURCES += tilepath.c
dist_libsortsmill_fontforge_la_SOURCES += transform.c
dist_libsortsmill_fontforge_la_SOURCES += ttfinstrsui.c
dist_libsortsmill_fontforge_la_SOURCES += uiutil.c
dist_libsortsmill_fontforge_la_SOURCES += windowmenu.c
dist_libsortsmill_fontforge_la_SOURCES += justifydlg.c
dist_libsortsmill_fontforge_la_SOURCES += deltaui.c
dist_libsortsmill_fontforge_la_SOURCES += usermenu.c
dist_libsortsmill_fontforge_la_SOURCES += guile_editor_main_loop.c
endif HAVE_GUI

#--------------------------------------------------------------------------

# Generate `API instructions' for relatively opaque access to internal
# types.
fontforge.types.apii: fontforge_type_inspector
	./$< > $@

noinst_PROGRAMS += fontforge_type_inspector
fontforge_type_inspector_LDADD = $(libsortsmill_fontforge_la_LIBADD)
nodist_noinst_DATA += fontforge.types.apii
EXTRA_DIST += fontforge_type_inspector.tig
CLEANFILES += fontforge_type_inspector.c fontforge.types.apii

#--------------------------------------------------------------------------

EXTRA_SCRIPTS += generate_codepoint_selector.py

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

AM_V_HAPI = $(AM_V_HAPI_$(V))
AM_V_HAPI_ = $(AM_V_HAPI_$(AM_DEFAULT_VERBOSITY))
AM_V_HAPI_0 = @echo "  GENHAPI " $@;

AM_V_CAPI = $(AM_V_CAPI_$(V))
AM_V_CAPI_ = $(AM_V_CAPI_$(AM_DEFAULT_VERBOSITY))
AM_V_CAPI_0 = @echo "  GENCAPI " $@;

sortsmill/fontforge_api.h: fontforge.types.apii		\
		$(srcdir)/generate-c-api.h.scm				\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_HAPI)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-c-api.h.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

fontforge_api.c: fontforge.types.apii sortsmill/fontforge_api.h	\
		$(srcdir)/generate-c-api.c.scm							\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_CAPI)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-c-api.c.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-c-api.h.scm generate-c-api.c.scm

#--------------------------------------------------------------------------
