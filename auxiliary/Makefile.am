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

include $(top_srcdir)/mk/flags.am
@includemk@ $(top_srcdir)/mk/guile.mk

lib_LTLIBRARIES =
EXTRA_DIST =
MOSTLYCLEANFILES =

#--------------------------------------------------------------------------

lib_LTLIBRARIES += libsortsmill_aux.la

dist_libsortsmill_aux_la_SOURCES =
nodist_libsortsmill_aux_la_SOURCES =

# Source files inherited from FontForge.
dist_libsortsmill_aux_la_SOURCES += ArabicForms.c
dist_libsortsmill_aux_la_SOURCES += alphabet.c
dist_libsortsmill_aux_la_SOURCES += backtrns.c
dist_libsortsmill_aux_la_SOURCES += cjk.c
dist_libsortsmill_aux_la_SOURCES += ucharmap.c
dist_libsortsmill_aux_la_SOURCES += unialt.c
dist_libsortsmill_aux_la_SOURCES += ustring.c
dist_libsortsmill_aux_la_SOURCES += utype.c
dist_libsortsmill_aux_la_SOURCES += combiners.h

# Recognition of numbers written in PostScript’s notation.
dist_libsortsmill_aux_la_SOURCES += ps_number.c

# Extensions to xalloc and Boehm GC.
dist_libsortsmill_aux_la_SOURCES += xdie_on_null.c
dist_libsortsmill_aux_la_SOURCES += x_alloc.c
dist_libsortsmill_aux_la_SOURCES += xgc.c
dist_libsortsmill_aux_la_SOURCES += xgc_strjoin.c

# Extensions to PCRE.
dist_libsortsmill_aux_la_SOURCES += rexp.c

# Extensions to GNU MP.
dist_libsortsmill_aux_la_SOURCES += gmp_constants.c
dist_libsortsmill_aux_la_SOURCES += math_gmp_matrix.c
dist_libsortsmill_aux_la_SOURCES += math_mpq_lu.c

# C matrices of SCM (Guile) numbers.
dist_libsortsmill_aux_la_SOURCES += math_scm_matrix.c math_scm_lu.c

# C matrices of double precision numbers. (Supplementary to the
# routines in GSL.)
dist_libsortsmill_aux_la_SOURCES += math_f64_matrix.c

# Extensions to libunistring.
dist_libsortsmill_aux_la_SOURCES += xunicase.c
dist_libsortsmill_aux_la_SOURCES += xuniconv.c
dist_libsortsmill_aux_la_SOURCES += xuninorm.c
dist_libsortsmill_aux_la_SOURCES += xunistr.c
dist_libsortsmill_aux_la_SOURCES += xunistr_strjoin.c

# Mathematical functions.
dist_libsortsmill_aux_la_SOURCES += bincoef.c

# Moving data around, taking into account non-unit strides.
dist_libsortsmill_aux_la_SOURCES += copy_with_strides.c

# Polynomial splines.
dist_libsortsmill_aux_la_SOURCES += math_polyspline_add.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_bases.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_compose.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_deriv.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_div.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_elev.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_eval.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_implicit.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_mul.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_reduce.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_roots.c
dist_libsortsmill_aux_la_SOURCES += math_polyspline_subdiv.c

# ‘Contours’ similar to those in the original FontForge’s Python
# interface.
dist_libsortsmill_aux_la_SOURCES += guile_contours.c

# Brent’s root-finding algorithm.
dist_libsortsmill_aux_la_SOURCES += math_brentroot.c
dist_libsortsmill_aux_la_SOURCES += math_mpq_brentroot.c
dist_libsortsmill_aux_la_SOURCES += guile_math_brentroot.c

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
dist_libsortsmill_aux_la_SOURCES += rnrs_conditions.c
dist_libsortsmill_aux_la_SOURCES += guile_math_functions.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_error.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_matrices.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gsl_permutations.c
dist_libsortsmill_aux_la_SOURCES += guile_math_gmp.c
dist_libsortsmill_aux_la_SOURCES += guile_math_multivariate_polynomials.c
dist_libsortsmill_aux_la_SOURCES += guile_arrays.c
dist_libsortsmill_aux_la_SOURCES += guile_iconv.c
dist_libsortsmill_aux_la_SOURCES += guile_directory_layout.c
dist_libsortsmill_aux_la_SOURCES += guile_options.c
dist_libsortsmill_aux_la_SOURCES += guile_format.c

libsortsmill_aux_la_CFLAGS = $(AM_CFLAGS) '-I$(srcdir)'
libsortsmill_aux_la_LIBADD = $(top_builddir)/lib/libgnu.la
libsortsmill_aux_la_LDFLAGS = $(call ldflags) -export-dynamic
libsortsmill_aux_la_DEPENDENCIES = Makefile $(libsortsmill_aux_la_LIBADD)

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
