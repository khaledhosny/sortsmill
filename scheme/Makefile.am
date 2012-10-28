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
include $(top_srcdir)/mk/guile.mk
include $(top_srcdir)/mk/recipe-tools.mk

#--------------------------------------------------------------------------

nobase_dist_guileobjmodule_DATA = sortsmillff/iconv.scm		\
	sortsmillff/sfd-to-sxml.scm

nobase_nodist_guileobjmodule_DATA = sortsmillff/i18n.scm	\
	sortsmillff/i18n.go sortsmillff/iconv.go				\
	sortsmillff/sfd-to-sxml.go

sortsmillff/sfd-to-sxml.go: sortsmillff/i18n.go libguile-sortsmillff_iconv.la

EXTRA_DIST = sortsmillff/i18n.scm.in

#--------------------------------------------------------------------------

AM_LDFLAGS = -module $(MY_CFLAGS) $(MY_LIBS)

lib_LTLIBRARIES =

lib_LTLIBRARIES += libguile-sortsmillff_iconv.la
libguile_sortsmillff_iconv_la_SOURCES = guile_iconv.c
libguile_sortsmillff_iconv_la_LIBADD = $(top_builddir)/lib/libgnu.la

#--------------------------------------------------------------------------

install-data-hook:
#	Make sure the source module is no younger than the compiled
#	module.
	@$(call touch_files,								\
		$(addprefix $(DESTDIR)$(guileobjmoduledir)/,	\
					$(nodist_guileobjmodule_DATA)))

MOSTLYCLEANFILES = $(nobase_nodist_guileobjmodule_DATA)

#--------------------------------------------------------------------------






##--------------------------------------------------------------------------
#
#dist_guilemodule_DATA = fontforge-main.scm sfd.scm
#guileextension_LTLIBRARIES = libguile-sortsmillff_fontforgeexe.la
#
#libguile_sortsmillff_fontforgeexe_la_SOURCES = fontforgeexe_ffi.c
#
#libguile_sortsmillff_fontforgeexe_la_CPPFLAGS = \
#	$(AM_CPPFLAGS) 								\
#	-I$(top_builddir)/fontforge					\
#	-I$(top_srcdir)/fontforge
#
#libguile_sortsmillff_fontforgeexe_la_LIBADD =					\
#	$(top_builddir)/auxiliary/libsortsmillff_aux.la				\
#	$(top_builddir)/gutils/libsortsmillff_gutils.la				\
#	$(top_builddir)/fontforge/libsortsmillff_fontforge.la		\
#	$(top_builddir)/fontforge/libsortsmillff_fontforgeexe.la
#
#libguile_sortsmillff_fontforgeexe_la_LDFLAGS = -module	\
#	$(COMMON_LD_FLAGS)
#
##--------------------------------------------------------------------------
##
## Experimental start-up of the FontForge UI from a Guile script.
#
#EXTRA_SCRIPTS = fontforge.scm
#
#install-fontforge.scm: fontforge.scm
#	$(INSTALL_SCRIPT) fontforge.scm "$(DESTDIR)$(bindir)/"
#
#uninstall-local:
#	rm -f "$(DESTDIR)$(bindir)/fontforge.scm"
#
##--------------------------------------------------------------------------
