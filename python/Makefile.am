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

if GRAPHICAL_USER_INTERFACE
FFLIBADD += $(top_builddir)/gdraw/libsortsmillff_gdraw.la	\
	$(XINPUT_LIBS) $(XKB_LIBS)
endif GRAPHICAL_USER_INTERFACE

EXELIBADD = $(FFLIBADD)										\
	$(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la

#--------------------------------------------------------------------------

nodist_pkgpythondata_PYTHON = site_init.py

# Compatibility module. (FIXME: Make this optional.)
nodist_python_PYTHON = psMat.py

# Compatibility module. (FIXME: Does not exist yet.
# FIXME: Make it optional.)
#nodist_python_PYTHON += fontforge.py

nobase_dist_python_PYTHON = ffContrib/__init__.py	\
	ffContrib/excepthook.py

nobase_nodist_python_PYTHON = sortsmillff/__init__.py

nobase_pyexec_LTLIBRARIES =

nobase_pyexec_LTLIBRARIES += sortsmillff/psMat.la
sortsmillff_psMat_la_SOURCES = sortsmillff/psMat.c
sortsmillff_psMat_la_LDFLAGS = $(GSL_CFLAGS) $(GSL_LIBS) $(AM_LDFLAGS)
sortsmillff_psMat_la_LIBADD = $(BASELIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/linalg.la
sortsmillff_linalg_la_SOURCES = sortsmillff/linalg.c
sortsmillff_linalg_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/polyspline.la
sortsmillff_polyspline_la_SOURCES = sortsmillff/polyspline.c
sortsmillff_polyspline_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/brentroot.la
sortsmillff_brentroot_la_SOURCES = sortsmillff/brentroot.c
sortsmillff_brentroot_la_LIBADD = $(AUXLIBADD)

nobase_pyexec_LTLIBRARIES += sortsmillff/fontforge_compat.la
sortsmillff_fontforge_compat_la_SOURCES = sortsmillff/fontforge_compat.c
sortsmillff_fontforge_compat_la_CFLAGS = $(AM_CFLAGS)			\
	'-I$(top_builddir)/fontforge' '-I$(top_srcdir)/fontforge'
sortsmillff_fontforge_compat_la_LDFLAGS = $(AM_LDFLAGS) $(MY_CFLAGS)	\
	$(MY_LIBS)
sortsmillff_fontforge_compat_la_LIBADD = $(FFLIBADD)

PYX_FILES = sortsmillff/psMat.pyx sortsmillff/linalg.pyx	\
	sortsmillff/polyspline.pyx sortsmillff/brentroot.pyx	\
	sortsmillff/fontforge_compat.pyx

PXD_FILES = sortsmillff/gsl.pxd sortsmillff/linalg.pxd				\
	sortsmillff/brentroot_c.pxd sortsmillff/gmpy.pxd				\
	sortsmillff/polyspline_c.pxd sortsmillff/contour_interface.pxd	\
	sortsmillff/splinefont.pxd

PXI_FILES = sortsmillff/gmpy.pxi

# Just make any change in the pxd or pxi files result in a complete
# rebuild from Cython sources. FIXME: This is inefficient.
$(PYX_FILES:.pyx=.c): $(PXD_FILES) $(PXI_FILES) sortsmillff/psMat.pxi.in

EXTRA_DIST = $(PYX_FILES) $(PXD_FILES) $(PXI_FILES)

MOSTLYCLEANFILES = sortsmillff/psMat.c sortsmillff/linalg.c	\
	sortsmillff/polyspline.c sortsmillff/brentroot.c

AM_LDFLAGS = -module -shared -avoid-version
AM_CFLAGS += $(CYTHON_EXTRA_CFLAGS)
AM_CYTHONFLAGS = -I$(builddir)/sortsmillff -I$(srcdir)/sortsmillff

# FIXME: Maybe support extensions other than .so.
sortsmillff/%.so: sortsmillff/%.la
	-rm -f $@
	cp sortsmillff/$(LT_OBJDIR)$*.so $@

# These shared object files are needed by the test suite.
all-local: sortsmillff/psMat.so sortsmillff/linalg.so	\
	sortsmillff/polyspline.so sortsmillff/brentroot.so	\
	sortsmillff/fontforge_compat.so

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
