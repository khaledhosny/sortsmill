# -*- tab-width: 4 -*-

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

CONFIGURE_SCHEME =														\
	$(SED) 																\
		-e 's|@''GUILE''@|$(GUILE)|g' 									\
		-e 's|@''SHAREDIR''@|$(SHAREDIR)|g' 							\
		-e 's|@''DOCDIR''@|$(DOCDIR)|g' 								\
		-e 's|@''LOCALEDIR''@|$(LOCALEDIR)|g' 							\
		-e 's|@''guileextensiondir''@|$(guileextensiondir)|g'			\
		-e 's|@''guilemoduledir''@|$(guilemoduledir)|g'					\
		-e 's|@''guileobjmoduledir''@|$(guileobjmoduledir)|g'			\
		-e 's|@''sysconfdir''@|$(sysconfdir)|g'							\
		-e 's|@''FF_TEXTDOMAIN''@|$(FF_TEXTDOMAIN)|g' 					\
		-e 's|@''FF_SHORTCUTSDOMAIN''@|$(FF_SHORTCUTSDOMAIN)|g'			\
		-e 's|@''FF_MACSHORTCUTSDOMAIN''@|$(FF_MACSHORTCUTSDOMAIN)|g'	\
		-e 's|@''VERSION_MAJOR''@|$(VERSION_MAJOR)|g' 					\
		-e 's|@''VERSION_MINOR''@|$(VERSION_MINOR)|g' 					\
		-e 's|@''VERSION_PATCH''@|$(VERSION_PATCH)|g' 					\
		-e 's|@''VERSION_EXTRA''@|$(VERSION_EXTRA)|g' 					\
		-e 's|@''VERSION_EXTRA_SHORT''@|$(VERSION_EXTRA_SHORT)|g' 		\
		-e 's|@''PACKAGE''@|$(PACKAGE)|g'				 				\
		-e 's|@''PACKAGE_BUGREPORT''@|$(PACKAGE_BUGREPORT)|g'			\
		-e 's|@''PACKAGE_NAME''@|$(PACKAGE_NAME)|g'						\
		-e 's|@''PACKAGE_STRING''@|$(PACKAGE_STRING)|g'					\
		-e 's|@''PACKAGE_TARNAME''@|$(PACKAGE_TARNAME)|g'				\
		-e 's|@''PACKAGE_URL''@|$(PACKAGE_URL)|g'						\
		-e 's|@''PACKAGE_VERSION''@|$(PACKAGE_VERSION)|g'				\
		-e 's|@''C_CONST_FLT_EPSILON''@|$(C_CONST_FLT_EPSILON)|g'		\
		-e 's|@''C_CONST_DBL_EPSILON''@|$(C_CONST_DBL_EPSILON)|g'		\
		-e 's|@''SIZEOF__BOOL''@|$(SIZEOF__BOOL)|g'						\
		-e 's|@''SIZEOF_INTPTR_T''@|$(SIZEOF_INTPTR_T)|g'				\
		-e 's|@''SIZEOF_UINTPTR_T''@|$(SIZEOF_UINTPTR_T)|g'				\
		-e 's|@''i_do_have_pure_api''@|$(i_do_have_pure_api)|g'			\
		-e 's|@''i_do_have_python_api''@|$(i_do_have_python_api)|g'		\
		-e 's|@''PYTHON_COMPATIBILITY''@|$(PYTHON_COMPATIBILITY)|g'		\
		-e 's|@''PY_MAJOR_VERSION''@|$(PY_MAJOR_VERSION)|g'				\
		-e 's|@''PY_MINOR_VERSION''@|$(PY_MINOR_VERSION)|g'				\
		-e 's|@''PY_MICRO_VERSION''@|$(PY_MICRO_VERSION)|g'				\
		-e 's|@''PY_RELEASE_LEVEL''@|$(PY_RELEASE_LEVEL)|g'				\
		-e 's|@''PY_RELEASE_SERIAL''@|$(PY_RELEASE_SERIAL)|g'			\
		-e 's|@''PY_VERSION''@|$(PY_VERSION)|g'							\
		-e 's|@''PY_VERSION_QUOTED''@|$(PY_VERSION_QUOTED)|g'			\
		-e 's|@''PY_VERSION_HEX''@|$(PY_VERSION_HEX)|g'					\
		-e 's|@''GSL_SUCCESS''@|$(GSL_SUCCESS)|g'						\
		-e 's|@''GSL_FAILURE''@|$(GSL_FAILURE)|g'						\
		-e 's|@''GSL_CONTINUE''@|$(GSL_CONTINUE)|g'						\
		-e 's|@''GSL_EDOM''@|$(GSL_EDOM)|g'								\
		-e 's|@''GSL_ERANGE''@|$(GSL_ERANGE)|g'							\
		-e 's|@''GSL_EFAULT''@|$(GSL_EFAULT)|g'							\
		-e 's|@''GSL_EINVAL''@|$(GSL_EINVAL)|g'							\
		-e 's|@''GSL_EFAILED''@|$(GSL_EFAILED)|g'						\
		-e 's|@''GSL_EFACTOR''@|$(GSL_EFACTOR)|g'						\
		-e 's|@''GSL_ESANITY''@|$(GSL_ESANITY)|g'						\
		-e 's|@''GSL_ENOMEM''@|$(GSL_ENOMEM)|g'							\
		-e 's|@''GSL_EBADFUNC''@|$(GSL_EBADFUNC)|g'						\
		-e 's|@''GSL_ERUNAWAY''@|$(GSL_ERUNAWAY)|g'						\
		-e 's|@''GSL_EMAXITER''@|$(GSL_EMAXITER)|g'						\
		-e 's|@''GSL_EZERODIV''@|$(GSL_EZERODIV)|g'						\
		-e 's|@''GSL_EBADTOL''@|$(GSL_EBADTOL)|g'						\
		-e 's|@''GSL_ETOL''@|$(GSL_ETOL)|g'								\
		-e 's|@''GSL_EUNDRFLW''@|$(GSL_EUNDRFLW)|g'						\
		-e 's|@''GSL_EOVRFLW''@|$(GSL_EOVRFLW)|g'						\
		-e 's|@''GSL_ELOSS''@|$(GSL_ELOSS)|g'							\
		-e 's|@''GSL_EROUND''@|$(GSL_EROUND)|g'							\
		-e 's|@''GSL_EBADLEN''@|$(GSL_EBADLEN)|g'						\
		-e 's|@''GSL_ENOTSQR''@|$(GSL_ENOTSQR)|g'						\
		-e 's|@''GSL_ESING''@|$(GSL_ESING)|g'							\
		-e 's|@''GSL_EDIVERGE''@|$(GSL_EDIVERGE)|g'						\
		-e 's|@''GSL_EUNSUP''@|$(GSL_EUNSUP)|g'							\
		-e 's|@''GSL_EUNIMPL''@|$(GSL_EUNIMPL)|g'						\
		-e 's|@''GSL_ECACHE''@|$(GSL_ECACHE)|g'							\
		-e 's|@''GSL_ETABLE''@|$(GSL_ETABLE)|g'							\
		-e 's|@''GSL_ENOPROG''@|$(GSL_ENOPROG)|g'						\
		-e 's|@''GSL_ENOPROGJ''@|$(GSL_ENOPROGJ)|g'						\
		-e 's|@''GSL_ETOLF''@|$(GSL_ETOLF)|g'							\
		-e 's|@''GSL_ETOLX''@|$(GSL_ETOLX)|g'							\
		-e 's|@''GSL_ETOLG''@|$(GSL_ETOLG)|g'							\
		-e 's|@''GSL_EOF''@|$(GSL_EOF)|g'

#--------------------------------------------------------------------------
#
# Automatic generation of reëxporters for heirarchical R⁶RS-style
# libraries.

EXTRA_DIST += ff-internal/find-exports.scm		\
	ff-internal/reexporters.scm

generate_reexporter = $(GUILE_INTERPRET) $(top_srcdir)/guile/generate-reexporter.scm '$1' '$2' '$3'

#--------------------------------------------------------------------------
