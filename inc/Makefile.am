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

include $(top_srcdir)/mk/layout.am

EXTRA_DIST = sortsmill/.dir-locals.el

noinst_HEADERS = config.h config-extra.h intl.h basics.h gmpy.h			\
	null_passthru.h xunistring.h chardata.h gdraw.h gfile.h ggadget.h	\
	gimage.h gio.h gkeysym.h gprogress.h gresedit.h gresource.h			\
	gwidget.h ustring.h utype.h annotations_base.h ffthread.h			\
	precomputed_polyspline_data.h contour_interface.h moveto_funcs.h	\
	invoke_funcs.h

nobase_include_HEADERS = 

# C entry point for Sorts Mill Editor.
nobase_include_HEADERS += sortsmill/fontforge_main.h

# FIXME: Make this obsolete and be rid of it.
nobase_include_HEADERS += sortsmill/usermenu.h

# What version of C is in use?
nobase_include_HEADERS += sortsmill/c_version.h

# Attribute tags to put in the code for the benefit of the compiler.
nobase_include_HEADERS += sortsmill/attributes.h

# Allocation.
nobase_include_HEADERS += sortsmill/null_passthru.h					\
	sortsmill/xdie_on_null.h sortsmill/xgc.h sortsmill/x_alloc.h

# Extensions to GNU libunistring.
nobase_include_HEADERS += sortsmill/xunicase.h sortsmill/xuniconv.h	\
	sortsmill/xuninorm.h sortsmill/xunistr.h sortsmill/xunistring.h

# Parsing of PostScript-style numerals.
nobase_include_HEADERS += sortsmill/ps_number.h

# `Easy' regular expression processing, based on PCRE.
nobase_include_HEADERS += sortsmill/rexp.h

# Pre-computed constants for GNU MP.
nobase_include_HEADERS += sortsmill/gmp_constants.h

# GSL-like matrix operations for GNU MP and Guile numbers.
nobase_include_HEADERS += sortsmill/transmatrix.h	\
	sortsmill/gmp_matrix.h sortsmill/scm_matrix.h

# Brent’s method for root-finding.
nobase_include_HEADERS += sortsmill/brentroot.h	\
	sortsmill/qbrentroot.h

# Polynomial splines.
nobase_include_HEADERS += sortsmill/polyspline.h

# FIXME: These headers need to be examined for relevance and for
# whether the functions they declare need to be moved into the
# auxiliary library. If they are declared here, they probably should
# be moved, rather than left in DLLs.
nobase_include_HEADERS += sortsmill/guile/notices.h

nobase_include_HEADERS += sortsmill/guile.h

nobase_include_HEADERS += sortsmill/guile/iconv.h				\
	sortsmill/guile/contours.h sortsmill/guile/arrays.h			\
	sortsmill/guile/rnrs_conditions.h sortsmill/guile/format.h

nobase_include_HEADERS += sortsmill/guile/editor.h	\
	sortsmill/guile/editor/main_loop.h

nobase_include_HEADERS += sortsmill/guile/math.h						\
	sortsmill/guile/math/brentroot.h sortsmill/guile/math/gmp.h			\
	sortsmill/guile/math/matrices.h sortsmill/guile/math/polyspline.h	\
	sortsmill/guile/math/gsl.h sortsmill/guile/math/gsl/error.h			\
	sortsmill/guile/math/gsl/matrices.h									\
	sortsmill/guile/math/gsl/permutations.h

if HAVE_PURE_API
nobase_include_HEADERS += sortsmill/guile/pure.h
endif HAVE_PURE_API

if HAVE_PYTHON_API
nobase_include_HEADERS += sortsmill/__init__.pxd						\
	sortsmill/cython/__init__.pxd sortsmill/cython/const_pointers.pxd	\
	sortsmill/cython/brentroot.pxd sortsmill/cython/gmpy.pxi			\
	sortsmill/cython/guile.pxd sortsmill/cython/linalg.pxd				\
	sortsmill/cython/splinefont.pxd sortsmill/cython/xgc.pxd			\
	sortsmill/cython/config.pxi sortsmill/cython/gmpy.pxd				\
	sortsmill/cython/gsl.pxd sortsmill/cython/polyspline.pxd
nobase_include_HEADERS += sortsmill/guile/python.h
endif HAVE_PYTHON_API

#--------------------------------------------------------------------------

extract_text = $(PERL) -n -e											\
	'print $$_ if (s|^\s*(//\|[*])\s*$(1)--\s*(.*)\s*|\2\n|)'

$(top_srcdir)/doc/%.visible.texi: sortsmill/%.h
	$(call extract_text,vis) $< > $@

extract-text: $(top_srcdir)/doc/ps_number.visible.texi

#--------------------------------------------------------------------------
