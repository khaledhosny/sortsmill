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

noinst_HEADERS = 
nobase_include_HEADERS = 

noinst_HEADERS += config.h
noinst_HEADERS += config-extra.h
noinst_HEADERS += intl.h
noinst_HEADERS += basics.h
noinst_HEADERS += gmpy.h
noinst_HEADERS += null_passthru.h
noinst_HEADERS += xunistring.h
noinst_HEADERS += chardata.h
noinst_HEADERS += gdraw.h
noinst_HEADERS += gfile.h
noinst_HEADERS += ggadget.h
noinst_HEADERS += gimage.h
noinst_HEADERS += gio.h
noinst_HEADERS += gkeysym.h
noinst_HEADERS += gprogress.h
noinst_HEADERS += gresedit.h
noinst_HEADERS += gresource.h
noinst_HEADERS += gwidget.h
noinst_HEADERS += ustring.h
noinst_HEADERS += utype.h
noinst_HEADERS += annotations_base.h
noinst_HEADERS += ffthread.h
noinst_HEADERS += precomputed_polyspline_data.h
noinst_HEADERS += contour_interface.h
noinst_HEADERS += moveto_funcs.h
noinst_HEADERS += invoke_funcs.h

# C entry point for Sorts Mill Editor.
nobase_include_HEADERS += sortsmill/fontforge_main.h

# FIXME: Make this obsolete and be rid of it.
nobase_include_HEADERS += sortsmill/usermenu.h

# What version of C is in use?
nobase_include_HEADERS += sortsmill/c_version.h

# Attribute tags to put in the code for the benefit of the compiler.
nobase_include_HEADERS += sortsmill/attributes.h

# Automatic, thread-safe initialization of nontrivial global
# constants.
nobase_include_HEADERS += sortsmill/initialized_global_constants.h

# Allocation.
nobase_include_HEADERS += sortsmill/null_passthru.h
nobase_include_HEADERS += sortsmill/xdie_on_null.h
nobase_include_HEADERS += sortsmill/xgc.h
nobase_include_HEADERS += sortsmill/x_alloc.h

# Extensions to GNU libunistring.
nobase_include_HEADERS += sortsmill/xunicase.h
nobase_include_HEADERS += sortsmill/xuniconv.h
nobase_include_HEADERS += sortsmill/xuninorm.h
nobase_include_HEADERS += sortsmill/xunistr.h
nobase_include_HEADERS += sortsmill/xunistring.h

# `Easy' regular expression processing, based on PCRE.
nobase_include_HEADERS += sortsmill/rexp.h

# Parsing of PostScript-style numerals.
nobase_include_HEADERS += sortsmill/ps_number.h

#--------------------------------------------------------------------------
#
# Mathematical heirarchy.

nobase_include_HEADERS += sortsmill/math.h

# Mathematical functions.
nobase_include_HEADERS += sortsmill/math/bincoef.h

# Pre-computed constants for GNU MP.
nobase_include_HEADERS += sortsmill/math/gmp_constants.h

# GSL-like matrix operations for GNU MP and Guile numbers.
nobase_include_HEADERS += sortsmill/math/transmatrix.h
nobase_include_HEADERS += sortsmill/math/gmp_matrix.h
nobase_include_HEADERS += sortsmill/math/scm_matrix.h

# Floating point matrix operations supplementary to those of GSL.
nobase_include_HEADERS += sortsmill/math/f64_matrix.h

# Brent’s method for root-finding.
nobase_include_HEADERS += sortsmill/math/brentroot.h

# Polynomial splines.
nobase_include_HEADERS += sortsmill/math/polyspline.h
nobase_include_HEADERS += sortsmill/math/polyspline/bases.h

#--------------------------------------------------------------------------

# FIXME: These headers need to be examined for relevance and for
# whether the functions they declare need to be moved into the
# auxiliary library. If they are declared here, they probably should
# be moved, rather than left in DLLs.
nobase_include_HEADERS += sortsmill/guile/notices.h

nobase_include_HEADERS += sortsmill/guile.h

nobase_include_HEADERS += sortsmill/guile/iconv.h
nobase_include_HEADERS += sortsmill/guile/contours.h
nobase_include_HEADERS += sortsmill/guile/arrays.h
nobase_include_HEADERS += sortsmill/guile/rnrs_conditions.h
nobase_include_HEADERS += sortsmill/guile/format.h

nobase_include_HEADERS += sortsmill/guile/editor.h
nobase_include_HEADERS += sortsmill/guile/editor/main_loop.h

nobase_include_HEADERS += sortsmill/guile/math.h
nobase_include_HEADERS += sortsmill/guile/math/brentroot.h
nobase_include_HEADERS += sortsmill/guile/math/gmp.h
nobase_include_HEADERS += sortsmill/guile/math/matrices.h
nobase_include_HEADERS += sortsmill/guile/math/matrices/mpqmat.h
nobase_include_HEADERS += sortsmill/guile/math/polyspline.h
nobase_include_HEADERS += sortsmill/guile/math/gsl.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/error.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/matrices.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/permutations.h

if HAVE_PURE_API
nobase_include_HEADERS += sortsmill/guile/pure.h
endif HAVE_PURE_API

#--------------------------------------------------------------------------

if HAVE_PYTHON_API
nobase_include_HEADERS += sortsmill/__init__.pxd
nobase_include_HEADERS += sortsmill/cython/__init__.pxd
nobase_include_HEADERS += sortsmill/cython/const_pointers.pxd
nobase_include_HEADERS += sortsmill/cython/brentroot.pxd
nobase_include_HEADERS += sortsmill/cython/gmpy.pxi
nobase_include_HEADERS += sortsmill/cython/guile.pxd
nobase_include_HEADERS += sortsmill/cython/linalg.pxd
nobase_include_HEADERS += sortsmill/cython/splinefont.pxd
nobase_include_HEADERS += sortsmill/cython/xgc.pxd
nobase_include_HEADERS += sortsmill/cython/config.pxi
nobase_include_HEADERS += sortsmill/cython/gmpy.pxd
nobase_include_HEADERS += sortsmill/cython/gsl.pxd
nobase_include_HEADERS += sortsmill/cython/polyspline.pxd
nobase_include_HEADERS += sortsmill/guile/python.h
endif HAVE_PYTHON_API

#--------------------------------------------------------------------------

extract_text = $(PERL) -n -e											\
	'print $$_ if (s|^\s*(//\|[*])\s*$(1)--\s*(.*)\s*|\2\n|)'

$(top_srcdir)/doc/%.visible.texi: sortsmill/%.h
	$(call extract_text,vis) $< > $@

extract-text: $(top_srcdir)/doc/ps_number.visible.texi

#--------------------------------------------------------------------------
