# Copyright (C) 2012, 2013, 2014 Khaled Hosny and Barry Schwartz
#
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
@includemk@ $(top_srcdir)/mk/guile.mk

noinst_LTLIBRARIES =
EXTRA_DIST =
MOSTLYCLEANFILES =

#--------------------------------------------------------------------------

noinst_LTLIBRARIES += libsortsmill_aux.la

dist_libsortsmill_aux_la_SOURCES =
nodist_libsortsmill_aux_la_SOURCES =

# Source files inherited from FontForge. Try to avoid using them and
# PLEASE FIXME by doing away with them. Use libunistring and such,
# instead.
dist_libsortsmill_aux_la_SOURCES += ArabicForms.c
dist_libsortsmill_aux_la_SOURCES += alphabet.c
dist_libsortsmill_aux_la_SOURCES += backtrns.c
dist_libsortsmill_aux_la_SOURCES += cjk.c
dist_libsortsmill_aux_la_SOURCES += ucharmap.c
dist_libsortsmill_aux_la_SOURCES += unialt.c
dist_libsortsmill_aux_la_SOURCES += ustring.c
dist_libsortsmill_aux_la_SOURCES += utype.c
dist_libsortsmill_aux_la_SOURCES += combiners.h

# Approximate equality routines moved here from FontForge’s
# splineutil2.c.
dist_libsortsmill_aux_la_SOURCES += nearness.c

# Extensions to GNU MP.
dist_libsortsmill_aux_la_SOURCES += gmp_constants.c
dist_libsortsmill_aux_la_SOURCES += math_gmp_matrix.c
dist_libsortsmill_aux_la_SOURCES += math_mpq_lu.c

# C matrices of SCM (Guile) numbers.
dist_libsortsmill_aux_la_SOURCES += math_scm_matrix.c math_scm_lu.c

# C matrices of double precision numbers. (Supplementary to the
# routines in GSL.)
dist_libsortsmill_aux_la_SOURCES += math_f64_matrix.c

# Mathematical functions.
dist_libsortsmill_aux_la_SOURCES += bincoef.c
dist_libsortsmill_aux_la_SOURCES += floor_mod.c

# Moving data around, taking into account non-unit strides.
dist_libsortsmill_aux_la_SOURCES += guile_copy_with_strides.c

# Polynomial splines.
dist_libsortsmill_aux_la_SOURCES += math_polyspline_add.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_bases.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_compose.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_deriv.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_div.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_elev.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_ellipses.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_eval.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_implicit.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_intersection.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_mul.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_reduce.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_roots.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_subdiv.c

# ‘Contours’ similar to those in the original FontForge’s Python
# interface.
dist_libsortsmill_aux_la_SOURCES += guile_fonts_contours.c

# Brent’s root-finding algorithm. FIXME: No matter the effort I put
# into them, I do not feel like testing these routines, so switch to
# using the GSL routines instead. And do away with the
# non-floating-point versions, because we are going to avoid them like
# the plague, anyway.
dist_libsortsmill_aux_la_SOURCES += math_brentroot.c
dist_libsortsmill_aux_la_SOURCES += math_mpq_brentroot.c
dist_libsortsmill_aux_la_SOURCES += guile_math_brentroot.c

# Initialized global constants implemented as small functions, using
# atomic_ops.
dist_libsortsmill_aux_la_SOURCES += initialized_global_constants.c
dist_libsortsmill_aux_la_SOURCES += guile_initialized_global_constants.c

# Guile-Pure interface
if HAVE_PURE_API
dist_libsortsmill_aux_la_SOURCES += guile_pure.c
endif HAVE_PURE_API

# Guile-Python interface.
if HAVE_PYTHON_API
# FIXME: Now that all this Guile-Python support has been moved to one
# library, the source files can be merged or broken up differently.
dist_libsortsmill_aux_la_SOURCES += scm_python.c
dist_libsortsmill_aux_la_SOURCES += guile_python.c
endif HAVE_PYTHON_API

# Miscellaneous C interface to Guile and Guile extensions.
# FIXME: Move some of these to Sorts Mill Core Guile.
dist_libsortsmill_aux_la_SOURCES += guile_math_functions.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_error.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_matrices.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_permutations.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gmp.c
dist_libsortsmill_aux_la_SOURCES += guile_math_matrices_base.c
dist_libsortsmill_aux_la_SOURCES += guile_math_multivariate_polynomials.c
dist_libsortsmill_aux_la_SOURCES += guile_arrays.c
dist_libsortsmill_aux_la_SOURCES += guile_iconv.c
dist_libsortsmill_aux_la_SOURCES += guile_directory_layout.c
dist_libsortsmill_aux_la_SOURCES += guile_options.c
dist_libsortsmill_aux_la_SOURCES += guile_postscript.c
dist_libsortsmill_aux_la_SOURCES += guile_strings_rexp.c
dist_libsortsmill_aux_la_SOURCES += guile_symbols.c
dist_libsortsmill_aux_la_SOURCES += guile_types.c

# The GMP_MATRIX_EXTRAS files are included by math_gmp_matrix.c rather
# than compiled directly.
GMP_MATRIX_EXTRAS =
GMP_MATRIX_EXTRAS += math_gmp_matrix_init.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_set.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_copy.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_swap.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_mult.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_add.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_pred.c
GMP_MATRIX_EXTRAS += math_gmp_matrix_trsv.c

EXTRA_DIST += $(GMP_MATRIX_EXTRAS)

gmp_matrix.lo: $(GMP_MATRIX_EXTRAS)

#--------------------------------------------------------------------------
#
# Targets for maintainers to use when a new version of Unicode is
# adopted.

generate-utype: LineBreak.txt NamesList.txt PropList.txt UnicodeData.txt makeutype
	LC_ALL=C ./makeutype
	mv utype.h $(top_srcdir)/inc
	@if test '$(top_srcdir)' -ef '$(top_builddir)'; then \
		:; \
	else \
		mv ArabicForms.c unialt.c utype.c $(srcdir); \
		echo mv ArabicForms.c unialt.c utype.c $(srcdir); \
	fi

EXTRA_PROGRAMS = makeutype dump
makeutype_SOURCES = makeutype.c
dump_SOURCES = dump.c

LINEBREAK_URL = http://unicode.org/Public/UNIDATA/LineBreak.txt
NAMESLIST_URL = http://unicode.org/Public/UNIDATA/NamesList.txt
PROPLIST_URL = http://unicode.org/Public/UNIDATA/PropList.txt
UNICODEDATA_URL = http://unicode.org/Public/UNIDATA/UnicodeData.txt

LineBreak.txt:
	$(WGET) '$(LINEBREAK_URL)'

NamesList.txt:
	$(WGET) '$(NAMESLIST_URL)'

PropList.txt:
	$(WGET) '$(PROPLIST_URL)'

UnicodeData.txt:
	$(WGET) '$(UNICODEDATA_URL)'

MOSTLYCLEANFILES += makeutype dump
DISTCLEANFILES = LineBreak.txt NamesList.txt PropList.txt	\
	UnicodeData.txt

#--------------------------------------------------------------------------
