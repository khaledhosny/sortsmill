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
include $(top_srcdir)/mk/cython.mk
include $(top_srcdir)/mk/guile.mk

EXTRA_DIST =

#--------------------------------------------------------------------------

BASELIBADD = $(top_builddir)/lib/libgnu.la

AUXLIBADD = $(BASELIBADD) $(top_builddir)/auxiliary/libsortsmill_aux.la

FFLIBADD = $(AUXLIBADD)									\
	$(top_builddir)/fontforge/libsortsmill_fontforge.la	\
	$(top_builddir)/gutils/libsortsmill_gutils.la

if HAVE_GUI
FFLIBADD += $(top_builddir)/gdraw/libsortsmill_gdraw.la	\
	$(XINPUT_LIBS) $(XKB_LIBS)
endif HAVE_GUI

EXELIBADD = $(FFLIBADD)										\
	$(top_builddir)/fontforge/libsortsmill_fontforgeexe.la

FFINCLDIRS = '-I$(top_builddir)/fontforge'		\
	'-I$(top_srcdir)/fontforge'

#--------------------------------------------------------------------------

if HAVE_PYTHON_COMPATIBILITY
nodist_python_PYTHON = fontforge.py psMat.py
endif HAVE_PYTHON_COMPATIBILITY

nobase_dist_python_PYTHON = ffContrib/__init__.py	\
	ffContrib/excepthook.py ffContrib/allow_core.py

nobase_nodist_python_PYTHON = sortsmill/__init__.py				\
	sortsmill/legacy/__init__.py sortsmill/internal/__init__.py
#sortsmill/fontforge_compat.py

nobase_pyexec_LTLIBRARIES =

nobase_pyexec_LTLIBRARIES += sortsmill/ffcompat.la
sortsmill_ffcompat_la_SOURCES = sortsmill/ffcompat.c
sortsmill_ffcompat_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_ffcompat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmill_ffcompat_la_LIBADD = $(EXELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/legacy/fontforge.la
sortsmill_legacy_fontforge_la_SOURCES =		\
	sortsmill/legacy/fontforgepyhook.c
sortsmill_legacy_fontforge_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_legacy_fontforge_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmill_legacy_fontforge_la_LIBADD = $(FFLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/psMat.la
sortsmill_psMat_la_SOURCES = sortsmill/psMat.c
sortsmill_psMat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmill_psMat_la_LIBADD = $(BASELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/fontforge_api.la
sortsmill_fontforge_api_la_SOURCES = sortsmill/fontforge_api.c
sortsmill_fontforge_api_la_LIBADD = $(FFLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/views.la
sortsmill_views_la_SOURCES = sortsmill/views.c
sortsmill_views_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_views_la_LIBADD = $(FFLIBADD)

if HAVE_GUI
nobase_pyexec_LTLIBRARIES += sortsmill/usermenu.la
sortsmill_usermenu_la_SOURCES = sortsmill/usermenu.c
sortsmill_usermenu_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_usermenu_la_LIBADD = $(EXELIBADD)
endif HAVE_GUI

nobase_pyexec_LTLIBRARIES += sortsmill/notices.la
sortsmill_notices_la_SOURCES = sortsmill/notices.c
sortsmill_notices_la_CFLAGS = $(AM_CFLAGS) $(FFINCLDIRS)
sortsmill_notices_la_LIBADD = $(FFLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/pkg_info.la
sortsmill_pkg_info_la_SOURCES = sortsmill/pkg_info.c
sortsmill_pkg_info_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/guile.la
sortsmill_guile_la_SOURCES = sortsmill/guile.c
sortsmill_guile_la_LDFLAGS = $(GUILE_CFLAGS) $(GUILE_LIBS) $(AM_LDFLAGS)
sortsmill_guile_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/linalg.la
sortsmill_linalg_la_SOURCES = sortsmill/linalg.c
sortsmill_linalg_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/polyspline.la
sortsmill_polyspline_la_SOURCES = sortsmill/polyspline.c
sortsmill_polyspline_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/brentroot.la
sortsmill_brentroot_la_SOURCES = sortsmill/brentroot.c
sortsmill_brentroot_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/internal/__exec.la
sortsmill_internal___exec_la_SOURCES = sortsmill/internal/__exec.c
sortsmill_internal___exec_la_LDFLAGS = $(GUILE_CFLAGS) $(GUILE_LIBS) $(AM_LDFLAGS)
sortsmill_internal___exec_la_LIBADD = $(BASELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmill/internal/__guile_support.la
sortsmill_internal___guile_support_la_SOURCES =	\
	sortsmill/internal/__guile_support.c
sortsmill_internal___guile_support_la_LDFLAGS = $(GUILE_CFLAGS) $(GUILE_LIBS) $(AM_LDFLAGS)
sortsmill_internal___guile_support_la_LIBADD = $(BASELIBADD)

PYX_FILES = sortsmill/views.pyx sortsmill/ffcompat.pyx					\
	sortsmill/psMat.pyx sortsmill/pkg_info.pyx sortsmill/notices.pyx	\
	sortsmill/guile.pyx sortsmill/linalg.pyx sortsmill/polyspline.pyx	\
	sortsmill/brentroot.pyx sortsmill/internal/__exec.pyx				\
	sortsmill/internal/__guile_support.pyx
if HAVE_GUI
PYX_FILES += sortsmill/usermenu.pyx
endif HAVE_GUI

NODIST_PYX_FILES = sortsmill/fontforge_api.pyx

cythonbuildinc = $(top_builddir)/inc/sortsmill/cython
cythonsrcinc = $(top_srcdir)/inc/sortsmill/cython

PXD_FILES = $(cythonsrcinc)/usermenu.pxd $(cythonsrcinc)/xgc.pxd	\
	$(cythonsrcinc)/guile.pxd $(cythonsrcinc)/gsl.pxd				\
	$(cythonsrcinc)/linalg.pxd $(cythonsrcinc)/brentroot.pxd		\
	$(cythonsrcinc)/gmpy.pxd $(cythonsrcinc)/polyspline.pxd			\
	$(cythonsrcinc)/contour_interface.pxd							\
	$(cythonsrcinc)/splinefont.pxd

PXI_FILES = $(cythonsrcinc)/gmpy.pxi

PXI_IN_FILES = $(cythonbuildinc)/config.pxi

sortsmill/fontforge_api.pyx:								\
		$(top_builddir)/fontforge/fontforge.types.apii		\
		$(srcdir)/generate-python-api.scm					\
		$(top_srcdir)/guile/ff-internal/generate-types.scm
	$(AM_V_GEN)
	$(AM_V_at)$(GUILE_INTERPRET) $(srcdir)/generate-python-api.scm $(filter %.apii, $^) > $@-tmp
	$(AM_V_at)mv $@-tmp $@

EXTRA_DIST += generate-python-api.scm

# Just make any change in the pxd or pxi files result in a complete
# rebuild from Cython sources. FIXME: This is inefficient.
$(PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) $(PXI_IN_FILES)
$(NODIST_PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) $(PXI_IN_FILES)

EXTRA_DIST += $(PYX_FILES) $(PXD_FILES) $(PXI_FILES)

MOSTLYCLEANFILES = sortsmill/fontforge_api.pyx							\
	sortsmill/fontforge_api.c sortsmill/views.c sortsmill/usermenu.c	\
	sortsmill/ffcompat.c sortsmill/psMat.c sortsmill/pkg_info.c			\
	sortsmill/notices.c sortsmill/guile.c sortsmill/linalg.c			\
	sortsmill/polyspline.c sortsmill/brentroot.c						\
	sortsmill/internal/__exec.c sortsmill/internal/__exec.h				\
	sortsmill/internal/__guile_support.c								\
	sortsmill/internal/__guile_support.h

AM_LDFLAGS = -prefer-pic -module -shared -avoid-version
AM_CFLAGS += $(CYTHON_EXTRA_CFLAGS)
AM_CYTHONFLAGS = '-I$(top_builddir)/inc' '-I$(top_srcdir)/inc'	\
	'-I$(builddir)/sortsmill' '-I$(srcdir)/sortsmill'

# FIXME: Maybe support extensions other than .so.
sortsmill/legacy/%.so: sortsmill/legacy/%.la
	-rm -f $@
	cp sortsmill/legacy/$(LT_OBJDIR)$*.so $@

sortsmill/internal/%.so: sortsmill/internal/%.la
	-rm -f $@
	cp sortsmill/internal/$(LT_OBJDIR)$*.so $@

sortsmill/%.so: sortsmill/%.la
	-rm -f $@
	cp sortsmill/$(LT_OBJDIR)$*.so $@

# These shared object files are needed by the test suite.
all-local: all-local-HAVE_GUI sortsmill/psMat.so					\
	sortsmill/ffcompat.so sortsmill/fontforge_api.so				\
	sortsmill/views.so sortsmill/pkg_info.so sortsmill/notices.so	\
	sortsmill/guile.so sortsmill/linalg.so sortsmill/polyspline.so	\
	sortsmill/brentroot.so sortsmill/legacy/fontforge.so			\
	sortsmill/internal/__exec.so									\
	sortsmill/internal/__guile_support.so
if HAVE_GUI
all-local-HAVE_GUI: sortsmill/usermenu.so
else !HAVE_GUI
all-local-HAVE_GUI:
endif !HAVE_GUI

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
	-rm -f $(builddir)/sortsmill/*.so
	-rm -f $(builddir)/sortsmill/legacy/*.so
	-rm -f $(builddir)/sortsmill/internal/*.so
