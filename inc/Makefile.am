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
noinst_HEADERS += contour_interface.h
noinst_HEADERS += moveto_funcs.h
noinst_HEADERS += invoke_funcs.h
noinst_HEADERS += libguile_sortsmill_aux.h
noinst_HEADERS += real_types.h

# C entry point for Sorts Mill Editor.
nobase_include_HEADERS += sortsmill/fontforge_main.h

# FIXME: Make this obsolete and be rid of it.
nobase_include_HEADERS += sortsmill/usermenu.h

# What version of C is in use?
nobase_include_HEADERS += sortsmill/c_version.h

# Automatic, thread-safe initialization of nontrivial global
# constants.
nobase_include_HEADERS += sortsmill/initialized_global_constants.h
nobase_include_HEADERS += sortsmill/guile/initialized_global_constants.h

# Different kinds of approximate equality, mostly inherited from
# FontForge.
nobase_include_HEADERS += sortsmill/nearness.h

#--------------------------------------------------------------------------
#
# Mathematical hierarchy.

nobase_include_HEADERS += sortsmill/math.h

# Mathematical functions.
nobase_include_HEADERS += sortsmill/math/bincoef.h
nobase_include_HEADERS += sortsmill/math/floor_mod.h

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
nobase_include_HEADERS += sortsmill/math/polyspline/add.h
nobase_include_HEADERS += sortsmill/math/polyspline/bases.h
nobase_include_HEADERS += sortsmill/math/polyspline/compose.h
nobase_include_HEADERS += sortsmill/math/polyspline/deriv.h
nobase_include_HEADERS += sortsmill/math/polyspline/div.h
nobase_include_HEADERS += sortsmill/math/polyspline/elev.h
nobase_include_HEADERS += sortsmill/math/polyspline/ellipses.h
nobase_include_HEADERS += sortsmill/math/polyspline/eval.h
nobase_include_HEADERS += sortsmill/math/polyspline/implicit.h
nobase_include_HEADERS += sortsmill/math/polyspline/intersection.h
nobase_include_HEADERS += sortsmill/math/polyspline/mul.h
nobase_include_HEADERS += sortsmill/math/polyspline/reduce.h
nobase_include_HEADERS += sortsmill/math/polyspline/roots.h
nobase_include_HEADERS += sortsmill/math/polyspline/subdiv.h

#--------------------------------------------------------------------------

# FIXME: These headers need to be examined for relevance and for
# whether the functions they declare need to be moved into the
# auxiliary library. If they are declared here, they probably should
# be moved, rather than left in DLLs.
nobase_include_HEADERS += sortsmill/guile/notices.h

nobase_include_HEADERS += sortsmill/guile.h

nobase_include_HEADERS += sortsmill/guile/copy_with_strides.h
nobase_include_HEADERS += sortsmill/guile/iconv.h
nobase_include_HEADERS += sortsmill/guile/arrays.h
nobase_include_HEADERS += sortsmill/guile/rnrs_conditions.h
nobase_include_HEADERS += sortsmill/guile/fonts.h
nobase_include_HEADERS += sortsmill/guile/format.h
nobase_include_HEADERS += sortsmill/guile/wrap.h

nobase_include_HEADERS += sortsmill/guile/containers.h
nobase_include_HEADERS += sortsmill/guile/containers/rbmap.h

nobase_include_HEADERS += sortsmill/guile/editor.h
nobase_include_HEADERS += sortsmill/guile/editor/main_loop.h

nobase_include_HEADERS += sortsmill/guile/fonts/anchors.h
nobase_include_HEADERS += sortsmill/guile/fonts/contours.h
nobase_include_HEADERS += sortsmill/guile/fonts/font_formats.h
nobase_include_HEADERS += sortsmill/guile/fonts/fontinfo_dict.h
nobase_include_HEADERS += sortsmill/guile/fonts/glyphs.h
nobase_include_HEADERS += sortsmill/guile/fonts/head_table.h
nobase_include_HEADERS += sortsmill/guile/fonts/hhea_table.h
nobase_include_HEADERS += sortsmill/guile/fonts/os2_table.h
nobase_include_HEADERS += sortsmill/guile/fonts/peg_spacing.h
nobase_include_HEADERS += sortsmill/guile/fonts/private_dict.h
nobase_include_HEADERS += sortsmill/guile/fonts/t1font_dict.h
nobase_include_HEADERS += sortsmill/guile/fonts/views.h

nobase_include_HEADERS += sortsmill/guile/nearness.h

nobase_include_HEADERS += sortsmill/guile/math.h
nobase_include_HEADERS += sortsmill/guile/math/brentroot.h
nobase_include_HEADERS += sortsmill/guile/math/functions.h
nobase_include_HEADERS += sortsmill/guile/math/gmp.h
nobase_include_HEADERS += sortsmill/guile/math/gsl.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/error.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/matrices.h
nobase_include_HEADERS += sortsmill/guile/math/gsl/permutations.h
nobase_include_HEADERS += sortsmill/guile/math/matrices.h
nobase_include_HEADERS += sortsmill/guile/math/matrices/base.h
nobase_include_HEADERS += sortsmill/guile/math/multivariate_polynomials.h
nobase_include_HEADERS += sortsmill/guile/math/polyspline.h
nobase_include_HEADERS += sortsmill/guile/math/polyspline/ellipses.h

nobase_include_HEADERS += sortsmill/guile/ps_number.h
nobase_include_HEADERS += sortsmill/guile/postscript.h

nobase_include_HEADERS += sortsmill/guile/strings.h
nobase_include_HEADERS += sortsmill/guile/strings/rexp.h

nobase_include_HEADERS += sortsmill/guile/symbols.h

nobase_include_HEADERS += sortsmill/guile/scm_type.h
nobase_include_HEADERS += sortsmill/guile/types.h

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
