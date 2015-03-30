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
@includemk@ $(top_srcdir)/mk/guile.mk

# The `:;' works around a Bash 3.2 bug when the output is not writable.
$(srcdir)/package.m4: $(top_srcdir)/configure.ac
	:;{ \
		echo '# Signature of the current package.' && \
		echo 'm4_define([AT_PACKAGE_NAME],' && \
		echo '  [$(PACKAGE_NAME)])' && \
		echo 'm4_define([AT_PACKAGE_TARNAME],' && \
		echo '  [$(PACKAGE_TARNAME)])' && \
		echo 'm4_define([AT_PACKAGE_VERSION],' && \
		echo '  [$(PACKAGE_VERSION)])' && \
		echo 'm4_define([AT_PACKAGE_STRING],' && \
		echo '  [$(PACKAGE_STRING)])' && \
		echo 'm4_define([AT_PACKAGE_BUGREPORT],' && \
		echo '  [$(PACKAGE_BUGREPORT)])'; \
		echo 'm4_define([AT_PACKAGE_URL],' && \
		echo '  [$(PACKAGE_URL)])'; \
	} >'$(srcdir)/package.m4'

EXTRA_DIST = testsuite.at $(srcdir)/package.m4 $(TESTSUITE) atlocal.in
TESTSUITE = $(srcdir)/testsuite

check-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' $(TESTSUITEFLAGS)

installcheck-local: atconfig atlocal $(TESTSUITE)
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)'	\
	$(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean

check-local: override TESTSUITEFLAGS += DOING_INSTALLCHECK=no				\
	LC_ALL="C" PYTHONPATH="$(abs_top_builddir)/python" $(GUILE_ENV)			\
	LTDL_LIBRARY_PATH='$(SORTSMILL_CORE_LIBDIR):$(SORTSMILL_CORE_GUILE)'

installcheck-local: override TESTSUITEFLAGS += DOING_INSTALLCHECK=yes				\
	LC_ALL="C" PYTHONPATH="$(pythondir)" $(GUILE_INSTALLED_ENV)						\
	LTDL_LIBRARY_PATH='$(libdir):$(SORTSMILL_CORE_LIBDIR):$(SORTSMILL_CORE_GUILE)'

AUTOM4TE = $(SHELL) $(srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4 $(MY_AT_FILES)
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

MY_AT_FILES = gfile_c.at sortsmill_precompute.at precomputed_data.at	\
	math_functions.at bernstein.at monomial.at gmp_constants.at			\
	linalg.at polyspline.at sortsmill_iconv.at psMat.at					\
	old_python_tests.at
#sortsmill_linalg.at <-- Needs to be adapted to newer modules.

TESTS_GFILE_C = test_GFileGetUserConfigDir test_GFileGetUserCacheDir	\
	test_GFileGetUserDataDir test_GFileGetHomeDir test_GFileBuildName	\
	test_GFileBaseName test_GFileAppendFile

TESTS_PRECOMPUTEDDATA = #test_precomputed_data
TESTS_BERNSTEIN = #test_bernstein
TESTS_MONOMIAL = #test_monomial

TESTS_GMP_CONSTANTS = test_gmp_constants

TESTS_MATH_FUNCTIONS = test_bincoef

TESTS_LINALG = $(TESTS_LINALG_MPZ) $(TESTS_LINALG_MPQ)	\
	$(TESTS_LINALG_SCM) $(TESTS_LINALG_F64)

TESTS_LINALG_MPZ = test_mpz_matrix_set_all test_mpz_matrix_set_zero		\
	test_mpz_matrix_set_identity test_mpz_matrix_memcpy					\
	test_mpz_matrix_swap test_mpz_matrix_swap_rows						\
	test_mpz_matrix_swap_columns test_mpz_matrix_swap_rowcol			\
	test_mpz_matrix_transpose_memcpy test_mpz_matrix_scale				\
	test_mpz_matrix_mul_elements test_mpz_matrix_tdiv_q_elements		\
	test_mpz_matrix_fdiv_q_elements test_mpz_matrix_cdiv_q_elements		\
	test_mpz_matrix_tdiv_r_elements test_mpz_matrix_fdiv_r_elements		\
	test_mpz_matrix_cdiv_r_elements test_mpz_matrix_tdiv_qr_elements	\
	test_mpz_matrix_fdiv_qr_elements test_mpz_matrix_cdiv_qr_elements	\
	test_mpz_matrix_add test_mpz_matrix_sub								\
	test_mpz_matrix_add_constant										\
	test_mpz_matrix_elementwise_predicate test_mpz_matrix_equal			\
	test_mpz_matrix_mul_diagonal test_mpz_matrix_gemm					\
	test_mpz_matrix_trmm

TESTS_LINALG_MPQ = test_mpq_matrix_set_all test_mpq_matrix_set_zero	\
	test_mpq_matrix_set_identity test_mpq_matrix_memcpy				\
	test_mpq_matrix_swap test_mpq_matrix_swap_rows					\
	test_mpq_matrix_swap_columns test_mpq_matrix_swap_rowcol		\
	test_mpq_matrix_transpose_memcpy test_mpq_matrix_scale			\
	test_mpq_matrix_mul_elements test_mpq_matrix_div_elements		\
	test_mpq_matrix_add test_mpq_matrix_sub							\
	test_mpq_matrix_add_constant									\
	test_mpq_matrix_elementwise_predicate test_mpq_matrix_equal		\
	test_mpq_matrix_mul_diagonal test_mpq_matrix_gemm				\
	test_mpq_matrix_trmm test_mpq_matrix_trsv test_mpq_lu

TESTS_LINALG_SCM = test_scm_matrix_set_all test_scm_matrix_set_zero	\
	test_scm_matrix_set_identity test_scm_matrix_memcpy				\
	test_scm_matrix_swap test_scm_matrix_swap_rows					\
	test_scm_matrix_swap_columns test_scm_matrix_swap_rowcol		\
	test_scm_matrix_transpose_memcpy test_scm_matrix_scale			\
	test_scm_matrix_mul_elements test_scm_matrix_div_elements		\
	test_scm_matrix_add test_scm_matrix_sub							\
	test_scm_matrix_add_constant									\
	test_scm_matrix_elementwise_predicate test_scm_matrix_equal		\
	test_scm_matrix_mul_diagonal test_scm_matrix_gemm				\
	test_scm_matrix_trmm test_scm_matrix_trsv test_scm_lu

TESTS_LINALG_F64 = test_f64_matrix_mul_diagonal

check_PROGRAMS =
check_PROGRAMS += $(TESTS_GFILE_C)
#check_PROGRAMS += $(TESTS_PRECOMPUTEDDATA)
#check_PROGRAMS += $(TESTS_BERNSTEIN)
#check_PROGRAMS += $(TESTS_MONOMIAL)
check_PROGRAMS += $(TESTS_GMP_CONSTANTS)
check_PROGRAMS += $(TESTS_MATH_FUNCTIONS)
check_PROGRAMS += $(TESTS_LINALG)

# Just make all C programs dependent, rather than sort out who really
# is dependent.
$(check_PROGRAMS): main_with_guile.x

AM_DEFAULT_SOURCE_EXT = .c
LDADD = $(top_builddir)/fontforge/libsortsmill_fontforge.la	\
	$(top_builddir)/lib/libgnu.la
AM_LDFLAGS = -export-dynamic -avoid-version

EXTRA_DIST += main_with_guile.x

EXTRA_DIST += $(MY_AT_FILES)

EXTRA_DIST += test_utf7_to_string.scm test_guile_linalg.scm			\
	test_guile_polyspline_bases.scm test_guile_polyspline_eval.scm	\
	test_guile_precompute.scm

EXTRA_DIST += test_guile_bernstein.scm test_python_bernstein.py	\
	test_guile_monomial.scm test_python_monomial.py				\
	test_python_mpz_matrix_set_all.py							\
	test_python_mpq_matrix_set_all.py							\
	test_python_mpz_matrix_set_zero.py							\
	test_python_mpq_matrix_set_zero.py							\
	test_python_mpz_matrix_set_identity.py

EXTRA_DIST += test_psMat_identity.py test_psMat_inverse.py			\
	test_psMat_compose.py test_psMat_rotate.py test_psMat_scale.py	\
	test_psMat_skew.py test_psMat_translate.py

EXTRA_DIST += findoverlapbugs.py test0001.py test0101.py test1001.py	\
	test1002.py test1003.py test1004.py test1005.py test1006.py			\
	test1007.py

EXTRA_DIST += fonts/AddExtremaTest2.sfd fonts/ayn+meem.init.svg		\
	fonts/dvng10a-crash.sfd fonts/NumberPoints.sfd					\
	fonts/SplineOverlapBug1.sfd fonts/AddExtremumTest.sfd			\
	fonts/Caliban.sfd fonts/feta20.pfb fonts/OmittedCharBugs.sfd	\
	fonts/StrokeTests.sfd fonts/AHBugs.sfd fonts/CaslonMM.sfd		\
	fonts/FormalScript.sfd fonts/OverlapBugs.sfd fonts/VKern.sfd	\
	fonts/AmbrosiaBold.sfd fonts/DataURI.sfd fonts/Hinting.sfd		\
	fonts/QuadOverlapBugs.sfd fonts/AmbrosiaItalic.sfd				\
	fonts/DejaVuSerif.sfd fonts/ItalicHand.sfd						\
	fonts/QuadraticConversionBug.sfd fonts/Ambrosia.sfd				\
	fonts/DirectionTest.sfd fonts/NimbusLGCUni-Regular.sfd			\
	fonts/SimplifyBugs.sfd

DISTCLEANFILES = atconfig
