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

#--------------------------------------------------------------------------

%.go: %.scm
	LTDL_LIBRARY_PATH="$(top_builddir)/scheme:$${LTDL_LIBRARY_PATH}" \
	$(GUILE_TOOLS) compile -L$(top_builddir)/scheme -L$(top_srcdir)/scheme -o $@ $<

# Do this here rather than in configure.ac so some of these settings
# can be overridden when Make is run.
CONFIGURE_SCHEME =														\
	$(SED) 																\
		-e 's|@''GUILE''@|$(GUILE)|g' 									\
		-e 's|@''SHAREDIR''@|$(SHAREDIR)|g' 							\
		-e 's|@''DOCDIR''@|$(DOCDIR)|g' 								\
		-e 's|@''LOCALEDIR''@|$(LOCALEDIR)|g' 							\
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
		-e 's|@''PACKAGE_VERSION''@|$(PACKAGE_VERSION)|g'

%.scm: %.scm.in
	$(AM_V_GEN)
	$(AM_V_at)$(CONFIGURE_SCHEME) < $< > $@

SWIG_TO_GUILE_POSTPROCESS = $(PERL) -i -p -e '							\
$$/ = undef;															\
s|\#\s*include\s*<libguile.h>|\#include <libguile_wrapper.h>|;			\
s|\bSCM_STRINGP\b|scm_is_string|g;										\
s|\bSCM_STRING_LENGTH\b|scm_c_string_length|g;							\
s|\bSCM_STRING_CHARS\b|repl_scm_string_chars|g;							\
s|\bscm_sym2var\s*\(([^,]+),\s*scm_module_lookup_closure\(([^)]+)\),\s*SCM_BOOL_T\)|scm_module_ensure_local_variable(\2,\1)|g;	\
s|(SCM\s+scm_init_[[:alnum:]_]+_module\s*\(\s*void\s*\)\s*{[^}]*}\s*)$$|VISIBLE \1|;'

SWIG_TO_GUILE = $(SWIG) -I/usr/include $(filter -I%,$(MY_CFLAGS))	\
	-guile -Linkage module -package sortsmillff

swig_to_guile = $(SWIG_TO_GUILE) -o $(1) $(2);	\
	$(SWIG_TO_GUILE_POSTPROCESS) $(1)

%.c: %.i
	$(AM_V_GEN)
	$(AM_V_at)$(call swig_to_guile, $@, $<)
