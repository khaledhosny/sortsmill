# Copyright (C) 2012, 2013 Barry Schwartz
# Based in part on am/guilec which is part of Guile 2.0.7.
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#--------------------------------------------------------------------------

# Borrowed from am/guilec of the Guile sources.
AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC" $@;

ENV_GUILE_LOAD_PATH := $(shell echo $(ECHO_N) "$${GUILE_LOAD_PATH}$(ECHO_C)")
ENV_GUILE_LOAD_COMPILED_PATH := $(shell echo $(ECHO_N) "$${GUILE_LOAD_COMPILED_PATH}$(ECHO_C)")
ENV_LTDL_LIBRARY_PATH := $(shell echo $(ECHO_N) "$${LTDL_LIBRARY_PATH}$(ECHO_C)")
MY_GUILE_LOAD_PATH = $(if $(ENV_GUILE_LOAD_PATH),:$(ENV_GUILE_LOAD_PATH))
MY_GUILE_LOAD_COMPILED_PATH = $(if $(ENV_GUILE_LOAD_COMPILED_PATH),:$(ENV_GUILE_LOAD_COMPILED_PATH))
MY_LTDL_LIBRARY_PATH = $(if $(ENV_LTDL_LIBRARY_PATH),:$(ENV_LTDL_LIBRARY_PATH))

GUILE_ENV = GUILE_AUTO_COMPILE=0																\
	GUILE_LOAD_PATH='$(abs_top_srcdir)/guile:$(abs_top_builddir)/guile$(MY_GUILE_LOAD_PATH)'	\
	GUILE_LOAD_COMPILED_PATH='$(abs_top_builddir)/guile$(MY_GUILE_LOAD_COMPILED_PATH)'			\
	LTDL_LIBRARY_PATH='$(abs_top_builddir)/guile$(MY_LTDL_LIBRARY_PATH)'

GUILE_INSTALLED_ENV = GUILE_AUTO_COMPILE=0									\
	GUILE_LOAD_PATH='$(guilemoduledir)$(MY_GUILE_LOAD_COMPILED_PATH)'		\
	GUILE_LOAD_COMPILED_PATH='$(guileobjmoduledir)$(MY_LTDL_LIBRARY_PATH)'	\
	LTDL_LIBRARY_PATH='$(libdir)$(MY_GUILE_LOAD_PATH)'

GUILE_FLAGS = -L $(top_builddir)/guile -L $(top_srcdir)/guile

GUILE_INTERPRET = $(GUILE_ENV) $(GUILE) $(GUILE_FLAGS) --no-auto-compile -s

GUILE_COMMAND = $(GUILE_ENV) $(GUILE) $(GUILE_FLAGS) --no-auto-compile -c

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch	\
-Wduplicate-case-datum -Wbad-case-datum -Wformat

GUILE_COMPILE = $(GUILE_ENV) $(GUILE_TOOLS) compile $(GUILE_WARNINGS)	\
	$(GUILE_FLAGS)

%.go: %.scm
	$(AM_V_GUILEC)$(GUILE_COMPILE) $< -o $@

%.scm: %.scm.in
	$(AM_V_GEN)
	$(AM_V_at)$(CONFIGURE_SCHEME) < $< > $@-tmp
	$(AM_V_at)mv $@-tmp $@

#--------------------------------------------------------------------------
