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
	$(SHELL) '$(TESTSUITE)' AUTOTEST_PATH='$(bindir)' $(TESTSUITEFLAGS)

clean-local:
	test ! -f '$(TESTSUITE)' || $(SHELL) '$(TESTSUITE)' --clean

check-local: override TESTSUITEFLAGS +=	DOING_INSTALLCHECK=no \
	PYTHONPATH="$(abs_top_builddir)/python"
#	PYTHONPATH="$(abs_top_builddir)/python:$(abs_top_srcdir)/python"

installcheck-local: override TESTSUITEFLAGS += DOING_INSTALLCHECK=yes

AUTOM4TE = $(SHELL) $(srcdir)/build-aux/missing --run autom4te
AUTOTEST = $(AUTOM4TE) --language=autotest
$(TESTSUITE): $(srcdir)/testsuite.at $(srcdir)/package.m4 $(MY_AT_FILES)
	$(AUTOTEST) -I '$(srcdir)' -o $@.tmp $@.at
	mv $@.tmp $@

MY_AT_FILES = xgc_h.at xunicase_h.at xuniconv_h.at xuninorm_h.at	\
	xunistr_h.at gfile_c.at ps_number_h.at rexp_h.at				\
	pascals_triangle.at bernstein.at sortsmillff_iconv.at psMat.at	\
	old_python_tests.at

TESTS_XGC_H = test_x_gc_strndup test_x_gc_strjoin

TESTS_XUNISTRING_H = test_xunicase_compare test_xunicase_ncompare	\
	test_xuniconv_h test_xuninorm_h test_xunistr_h					\
	test_xunistr_strtol_NULL test_xunistr_strtol_nonNULL			\
	test_xunistr_strtoul_NULL test_xunistr_strtoul_nonNULL			\
	test_xunistr_strtod_NULL test_xunistr_strtod_nonNULL			\
	test_xunistr_valid_prefix test_xunistr_get_next					\
	test_x_gc_uNN_strjoin test_uNN_to_uMM_boundaries

TESTS_GFILE_C = test_GFileGetUserConfigDir test_GFileGetUserCacheDir	\
	test_GFileGetUserDataDir test_GFileGetHomeDir test_GFileBuildName	\
	test_GFileBaseName test_GFileAppendFile

TESTS_PS_NUMBER_H = test_ps_number_h

TESTS_REXP_H = test_rexp_h test_rexp_h_u8

TESTS_PASCALS_TRIANGLE = test_pascals_triangle

TESTS_BERNSTEIN = test_bernstein

check_PROGRAMS = $(TESTS_XGC_H) $(TESTS_XUNISTRING_H)		\
	$(TESTS_GFILE_C) $(TESTS_PS_NUMBER_H) $(TESTS_REXP_H)	\
	$(TESTS_PASCALS_TRIANGLE) $(TESTS_BERNSTEIN)

AM_DEFAULT_SOURCE_EXT = .c
LDADD = $(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la	\
	$(top_builddir)/gdraw/libsortsmillff_gdraw.la					\
	$(top_builddir)/fontforge/libsortsmillff_fontforge.la			\
	$(top_builddir)/gutils/libsortsmillff_gutils.la					\
	$(top_builddir)/auxiliary/libsortsmillff_aux.la					\
	$(top_builddir)/lib/libgnu.la
AM_LDFLAGS = -export-dynamic -avoid-version

EXTRA_DIST += $(MY_AT_FILES)

EXTRA_DIST += test_xunistr_strtol.txt test_xunistr_strtod.txt

EXTRA_DIST += test_utf7_to_string.scm test_guile_bernstein.scm

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
