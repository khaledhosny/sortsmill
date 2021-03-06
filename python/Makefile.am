# -*- tab-width: 4 -*-

# Copyright (C) 2012, 2013, 2015 Khaled Hosny and Barry Schwartz
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

include $(top_srcdir)/mk/flags.am
@includemk@ $(top_srcdir)/mk/cython.mk
@includemk@ $(top_srcdir)/mk/guile.mk

EXTRA_DIST =

AM_CFLAGS = $(CYTHON_EXTRA_CFLAGS)
AM_CYTHONFLAGS = '-I$(top_builddir)/inc' '-I$(top_srcdir)/inc'	\
	'-I$(top_builddir)/python' '-I$(top_srcdir)/python'

AM_LDFLAGS = -prefer-pic -module -shared -avoid-version $(LIBS)
LIBADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la	\
	$(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

if HAVE_LEGACY_SORTSMILL_TOOLS
aclocaldir = ${datadir}/aclocal
dist_aclocal_DATA = sortsmill.m4
dist_bin_SCRIPTS = make-fonts make-eot
endif HAVE_LEGACY_SORTSMILL_TOOLS

#--------------------------------------------------------------------------

if HAVE_PYTHON_COMPATIBILITY
nodist_python_PYTHON = fontforge.py psMat.py
endif HAVE_PYTHON_COMPATIBILITY

nobase_dist_python_PYTHON = ffContrib/__init__.py					\
	ffContrib/excepthook.py ffContrib/allow_core.py					\
	sortsmill/psMat.py sortsmill/ffcompat.py sortsmill/pyguile.py	\
	sortsmill/contours.py sortsmill/conditions.py

if HAVE_GUI
nobase_dist_python_PYTHON += sortsmill/usermenu.py
endif HAVE_GUI

if HAVE_LEGACY_SORTSMILL_TOOLS
nobase_dist_python_PYTHON += sortsmill/cap_spacing.py
nobase_dist_python_PYTHON += sortsmill/glyphbuild.py
nobase_dist_python_PYTHON += sortsmill/make_tt_font.py
nobase_dist_python_PYTHON += sortsmill/tools.py
nobase_dist_python_PYTHON += sortsmill/persistent_data.py
nobase_dist_python_PYTHON += sortsmill/licensing.py
nobase_dist_python_PYTHON += sortsmill/readfeatures.py
nobase_dist_python_PYTHON += sortsmill/font_db.py
nobase_dist_python_PYTHON += sortsmill/loadbuild.py
nobase_dist_python_PYTHON += sortsmill/spacing_by_anchors.py
endif HAVE_LEGACY_SORTSMILL_TOOLS

nobase_nodist_python_PYTHON = __sortsmill__/__init__.py	\
	sortsmill/__init__.py sortsmill/legacy/__init__.py
#sortsmill/fontforge_compat.py

lib_LTLIBRARIES =
nobase_pyexec_LTLIBRARIES =

nobase_pyexec_LTLIBRARIES += sortsmill/ffcompat.la
sortsmill_ffcompat_la_SOURCES = sortsmill/ffcompat.c
sortsmill_ffcompat_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
#sortsmill_ffcompat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmill_ffcompat_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/legacy/fontforge.la
sortsmill_legacy_fontforge_la_SOURCES =		\
	sortsmill/legacy/fontforgepyhook.c
sortsmill_legacy_fontforge_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
#sortsmill_legacy_fontforge_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmill_legacy_fontforge_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/psMat.la
sortsmill_psMat_la_SOURCES = sortsmill/psMat.c
sortsmill_psMat_la_LIBADD = $(LIBADD)
#sortsmill_psMat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)

nobase_pyexec_LTLIBRARIES += sortsmill/conditions.la
sortsmill_conditions_la_SOURCES = sortsmill/conditions.c
sortsmill_conditions_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/fontforge_api.la
sortsmill_fontforge_api_la_SOURCES = sortsmill/fontforge_api.c
sortsmill_fontforge_api_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/views.la
sortsmill_views_la_SOURCES = sortsmill/views.c
sortsmill_views_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)

if HAVE_GUI
nobase_pyexec_LTLIBRARIES += sortsmill/usermenu.la
sortsmill_usermenu_la_SOURCES = sortsmill/usermenu.c
sortsmill_usermenu_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_usermenu_la_LIBADD = $(LIBADD)
endif HAVE_GUI

nobase_pyexec_LTLIBRARIES += sortsmill/notices.la
sortsmill_notices_la_SOURCES = sortsmill/notices.c
sortsmill_notices_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_notices_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/pkg_info.la
sortsmill_pkg_info_la_SOURCES = sortsmill/pkg_info.c
sortsmill_pkg_info_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/guile.la
sortsmill_guile_la_SOURCES = sortsmill/guile.c
sortsmill_guile_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/linalg.la
sortsmill_linalg_la_SOURCES = sortsmill/linalg.c
sortsmill_linalg_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/polyspline.la
sortsmill_polyspline_la_SOURCES = sortsmill/polyspline.c
sortsmill_polyspline_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/pyguile.la
sortsmill_pyguile_la_SOURCES = sortsmill/pyguile.c
sortsmill_pyguile_la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += __sortsmill__/__pyguile__.la
__sortsmill_____pyguile___la_SOURCES = __sortsmill__/__pyguile__.c
__sortsmill_____pyguile___la_LIBADD = $(LIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/contours.la
sortsmill_contours_la_SOURCES = sortsmill/contours.c
sortsmill_contours_la_LIBADD = $(LIBADD)

PYX_FILES = __sortsmill__/__pyguile__.pyx sortsmill/views.pyx			\
	sortsmill/pkg_info.pyx sortsmill/notices.pyx sortsmill/guile.pyx	\
	sortsmill/linalg.pyx sortsmill/polyspline.pyx

NODIST_PYX_FILES = sortsmill/fontforge_api.pyx

cythonbuildinc = $(top_builddir)/inc/sortsmill/cython
cythonsrcinc = $(top_srcdir)/inc/sortsmill/cython

PXD_FILES = $(cythonsrcinc)/xgc.pxd $(cythonsrcinc)/guile.pxd	\
	$(cythonsrcinc)/gsl.pxd $(cythonsrcinc)/linalg.pxd			\
	$(cythonsrcinc)/gmpy.pxd $(cythonsrcinc)/polyspline.pxd		\
	$(cythonsrcinc)/splinefont.pxd

PXI_FILES = $(cythonsrcinc)/gmpy.pxi

PXI_IN_FILES = $(cythonbuildinc)/config.pxi

AM_V_PYAPI = $(AM_V_PYAPI_$(V))
AM_V_PYAPI_ = $(AM_V_PYAPI_$(AM_DEFAULT_VERBOSITY))
AM_V_PYAPI_0 = @echo "  GENPYAPI" $@;

sortsmill/fontforge_api.pyx:								\
		$(top_builddir)/fontforge/fontforge.types.apii		\
		$(srcdir)/generate-python-api.scm					\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_PYAPI)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-python-api.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-python-api.scm

# Just make any change in the pxd or pxi files result in a complete
# rebuild from Cython sources. FIXME: This is inefficient.
$(PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) $(PXI_IN_FILES)
$(NODIST_PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) $(PXI_IN_FILES)

EXTRA_DIST += $(PYX_FILES) $(PXD_FILES) $(PXI_FILES)

MOSTLYCLEANFILES = __sortsmill__/__pyguile__.c sortsmill/conditions.c	\
	sortsmill/fontforge_api.pyx sortsmill/fontforge_api.c				\
	sortsmill/views.c sortsmill/ffcompat.c sortsmill/psMat.c			\
	sortsmill/pkg_info.c sortsmill/notices.c sortsmill/guile.c			\
	sortsmill/linalg.c sortsmill/polyspline.c sortsmill/pyguile.c		\
	sortsmill/contours.c sortsmill/usermenu.c

# FIXME FIXME FIXME: Support extensions other than .so.
sortsmill/legacy/%.so: sortsmill/legacy/%.la
	-rm -f $@
	cp sortsmill/legacy/$(LT_OBJDIR)$*.so $@

sortsmill/%.so: sortsmill/%.la
	-rm -f $@
	cp sortsmill/$(LT_OBJDIR)$*.so $@

__sortsmill__/%.so: __sortsmill__/%.la
	-rm -f $@
	cp __sortsmill__/$(LT_OBJDIR)$*.so $@

# These shared object files are needed by the test suite.
all-local: $(HAVE_GUI_SO) __sortsmill__/__pyguile__.so					\
	sortsmill/psMat.so sortsmill/ffcompat.so sortsmill/conditions.so	\
	sortsmill/fontforge_api.so sortsmill/views.so						\
	sortsmill/pkg_info.so sortsmill/notices.so sortsmill/guile.so		\
	sortsmill/linalg.so sortsmill/polyspline.so							\
	sortsmill/legacy/fontforge.so sortsmill/pyguile.so					\
	sortsmill/contours.so
HAVE_GUI_SO = sortsmill/usermenu.so

check-local: all-local

extract_text = $(PERL) -n -e								\
	'print $$_ if (s|^\s*\#\s*$(1)--\s*(.*)\s*|\1\n|)'

$(top_srcdir)/doc/%.interface.texi: sortsmill/%.py
	$(call extract_text,i) $< > $@

$(top_srcdir)/doc/%.interface.texi: sortsmill/%.pyx
	$(call extract_text,i) $< > $@

extract-text: $(top_srcdir)/doc/psMat.interface.texi

mostlyclean-local:
	@for f in `find $(builddir) -name '*.py[co]' -print`; do	\
		echo "rm -f $${f}";										\
		rm -f "$${f}";											\
	done
	-rm -f $(builddir)/__sortsmill__/*.so
	-rm -f $(builddir)/sortsmill/*.so
	-rm -f $(builddir)/sortsmill/legacy/*.so

-include $(top_srcdir)/git.mk
