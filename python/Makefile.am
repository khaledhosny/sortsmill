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

nodist_pkgpythondata_PYTHON = site_init.py

# Compatibility module.
nodist_python_PYTHON = psMat.py

nobase_dist_python_PYTHON = ffContrib/__init__.py	\
	ffContrib/excepthook.py

nobase_nodist_python_PYTHON = sortsmillff/__init__.py	\
	sortsmillff/psMat.py

nobase_pyexec_LTLIBRARIES =

nobase_pyexec_LTLIBRARIES += sortsmillff/bernstein.la
sortsmillff_bernstein_la_SOURCES = sortsmillff/bernstein.c
sortsmillff_bernstein_la_LIBADD =					\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la	\
	$(top_builddir)/lib/libgnu.la

EXTRA_DIST = sortsmillff/bernstein.pyx
MOSTLYCLEANFILES = sortsmillff/bernstein.c

AM_LDFLAGS = -module -shared -avoid-version
AM_CFLAGS += $(NUMPY_CFLAGS) $(CYTHON_EXTRA_CFLAGS)

extract_text = $(PERL) -n -e								\
	'print $$_ if (s|^\s*\#\s*$(1)--\s*(.*)\s*|\1\n|)'

$(top_srcdir)/doc/%.interface.texi: sortsmillff/%.py
	$(call extract_text,i) $< > $@

extract-text: $(top_srcdir)/doc/psMat.interface.texi

mostlyclean-local:
	-rm -f $(builddir)/*.pyc $(builddir)/*.pyo
	-rm -f $(srcdir)/*.pyc $(srcdir)/*.pyo
	-rm -f $(builddir)/*/*.pyc $(builddir)/*/*.pyo
	-rm -f $(srcdir)/*/*.pyc $(srcdir)/*/*.pyo
