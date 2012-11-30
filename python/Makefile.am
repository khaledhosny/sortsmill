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

#--------------------------------------------------------------------------

BASELIBADD = $(top_builddir)/lib/libgnu.la

AUXLIBADD = $(BASELIBADD) $(top_builddir)/auxiliary/libsortsmillff_aux.la

FFLIBADD = $(AUXLIBADD)										\
	$(top_builddir)/fontforge/libsortsmillff_fontforge.la	\
	$(top_builddir)/gutils/libsortsmillff_gutils.la

if HAVE_GUI
FFLIBADD += $(top_builddir)/gdraw/libsortsmillff_gdraw.la	\
	$(XINPUT_LIBS) $(XKB_LIBS)
endif HAVE_GUI

EXELIBADD = $(FFLIBADD)										\
	$(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la

#--------------------------------------------------------------------------

nodist_pkgpythondata_PYTHON = site_init.py

# Compatibility module. (FIXME: Make this optional.)
nodist_python_PYTHON = psMat.py

# Compatibility module. (FIXME: Make this optional.)
nodist_python_PYTHON += fontforge.py

nobase_dist_python_PYTHON = ffContrib/__init__.py	\
	ffContrib/excepthook.py

nobase_nodist_python_PYTHON = sortsmillff/__init__.py	\
	sortsmillff/legacy/__init__.py
#sortsmillff/fontforge_compat.py

nobase_pyexec_LTLIBRARIES =

nobase_pyexec_LTLIBRARIES += sortsmillff/ffcompat.la
sortsmillff_ffcompat_la_SOURCES = sortsmillff/ffcompat.c
sortsmillff_ffcompat_la_CFLAGS = $(AM_CFLAGS)					\
	'-I$(top_builddir)/fontforge' '-I$(top_srcdir)/fontforge'
sortsmillff_ffcompat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmillff_ffcompat_la_LIBADD = $(EXELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/legacy/fontforge.la
sortsmillff_legacy_fontforge_la_SOURCES =		\
	sortsmillff/legacy/fontforgepyhook.c
sortsmillff_legacy_fontforge_la_CFLAGS = $(AM_CFLAGS)			\
	'-I$(top_builddir)/fontforge' '-I$(top_srcdir)/fontforge'
sortsmillff_legacy_fontforge_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmillff_legacy_fontforge_la_LIBADD = $(FFLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/psMat.la
sortsmillff_psMat_la_SOURCES = sortsmillff/psMat.c
sortsmillff_psMat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmillff_psMat_la_LIBADD = $(BASELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/pkg_info.la
sortsmillff_pkg_info_la_SOURCES = sortsmillff/pkg_info.c
sortsmillff_pkg_info_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/guile.la
sortsmillff_guile_la_SOURCES = sortsmillff/guile.c
sortsmillff_guile_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/linalg.la
sortsmillff_linalg_la_SOURCES = sortsmillff/linalg.c
sortsmillff_linalg_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/polyspline.la
sortsmillff_polyspline_la_SOURCES = sortsmillff/polyspline.c
sortsmillff_polyspline_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/brentroot.la
sortsmillff_brentroot_la_SOURCES = sortsmillff/brentroot.c
sortsmillff_brentroot_la_LIBADD = $(AUXLIBADD)

PYX_FILES = sortsmillff/ffcompat.pyx sortsmillff/psMat.pyx	\
	sortsmillff/pkg_info.pyx sortsmillff/guile.pyx			\
	sortsmillff/linalg.pyx sortsmillff/polyspline.pyx		\
	sortsmillff/brentroot.pyx

cythonbuildinc = $(top_builddir)/inc/sortsmillff/cython
cythonsrcinc = $(top_srcdir)/inc/sortsmillff/cython

PXD_FILES = $(cythonsrcinc)/usermenu.pxd $(cythonsrcinc)/xgc.pxd	\
	$(cythonsrcinc)/guile.pxd $(cythonsrcinc)/gsl.pxd				\
	$(cythonsrcinc)/linalg.pxd $(cythonsrcinc)/brentroot.pxd		\
	$(cythonsrcinc)/gmpy.pxd $(cythonsrcinc)/polyspline.pxd			\
	$(cythonsrcinc)/contour_interface.pxd							\
	$(cythonsrcinc)/splinefont.pxd

PXI_FILES = $(cythonsrcinc)/gmpy.pxi

PXI_IN_FILES = $(cythonbuildinc)/config.pxi

# Just make any change in the pxd or pxi files result in a complete
# rebuild from Cython sources. FIXME: This is inefficient.
$(PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) $(PXI_IN_FILES)

site_init.py: site_init.py.in
	$(SED) 															\
		-e 's|@''PACKAGE''@|$(PACKAGE)|g' 							\
		-e 's|@''sysconfdir''@|$(sysconfdir)|g' < $< > $@-tmp
	mv $@-tmp $@

EXTRA_DIST = $(PYX_FILES) $(PXD_FILES) $(PXI_FILES) site_init.py.in

MOSTLYCLEANFILES = site_init.py sortsmillff/ffcompat.c				\
	sortsmillff/psMat.c sortsmillff/pkg_info.c sortsmillff/guile.c	\
	sortsmillff/linalg.c sortsmillff/polyspline.c					\
	sortsmillff/brentroot.c

AM_LDFLAGS = -prefer-pic -module -shared -avoid-version
AM_CFLAGS += $(CYTHON_EXTRA_CFLAGS)
AM_CYTHONFLAGS = '-I$(top_builddir)/inc' '-I$(top_srcdir)/inc'	\
	'-I$(builddir)/sortsmillff' '-I$(srcdir)/sortsmillff'

# FIXME: Maybe support extensions other than .so.
sortsmillff/%.so: sortsmillff/%.la
	-rm -f $@
	cp sortsmillff/$(LT_OBJDIR)$*.so $@

# These shared object files are needed by the test suite.
all-local: sortsmillff/psMat.so sortsmillff/ffcompat.so	\
	sortsmillff/pkg_info.so sortsmillff/guile.so		\
	sortsmillff/linalg.so sortsmillff/polyspline.so		\
	sortsmillff/brentroot.so

check-local: all-local

extract_text = $(PERL) -n -e								\
	'print $$_ if (s|^\s*\#\s*$(1)--\s*(.*)\s*|\1\n|)'

$(top_srcdir)/doc/%.interface.texi: sortsmillff/%.py
	$(call extract_text,i) $< > $@

$(top_srcdir)/doc/%.interface.texi: sortsmillff/%.pyx
	$(call extract_text,i) $< > $@

extract-text: $(top_srcdir)/doc/psMat.interface.texi

mostlyclean-local:
	-rm -f $(builddir)/*.pyc $(builddir)/*.pyo
	-rm -f $(srcdir)/*.pyc $(srcdir)/*.pyo
	-rm -f $(builddir)/*/*.pyc $(builddir)/*/*.pyo
	-rm -f $(srcdir)/*/*.pyc $(srcdir)/*/*.pyo
	-rm -f $(builddir)/sortsmillff/*.so
	-rm -f $(builddir)/sortsmillff/legacy/*.so
