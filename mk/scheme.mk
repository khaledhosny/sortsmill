# -*- tab-width: 4 -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

AM_V_IN_TO_SCM = $(AM_V_IN_TO_SCM_$(V))
AM_V_IN_TO_SCM_ = $(AM_V_IN_TO_SCM_$(AM_DEFAULT_VERBOSITY))
AM_V_IN_TO_SCM_0 = @echo "  IN->SCM" $@;

%.scm: %.scm.in
	$(AM_V_IN_TO_SCM)
	$(AM_V_at)$(CONFIGURE_SCHEME) < $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

#--------------------------------------------------------------------------
#
# Automatic generation of reëxporters for heirarchical R⁶RS-style
# libraries.

AM_V_REEXPORT = $(AM_V_REEXPORT_$(V))
AM_V_REEXPORT_ = $(AM_V_REEXPORT_$(AM_DEFAULT_VERBOSITY))
AM_V_REEXPORT_0 = @echo "  REEXPORT" $@;

generate_reexporter = $(GUILE_INTERPRET) $(top_srcdir)/guile/generate-reexporter.scm '$1' '$2' '$3'

#--------------------------------------------------------------------------
