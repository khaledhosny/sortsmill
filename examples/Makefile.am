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

dist_noinst_DATA =
dist_noinst_SCRIPTS =

dist_noinst_DATA += sortsmill-scheme.el
dist_noinst_SCRIPTS += hello.py

lib_LTLIBRARIES =

# To try 'simple_menu_extension_in_C' without installing it,
# use the full path to
# $(build_dir)/simple_menu_extension_in_C.la in your
# ${HOME}/.config/sortsmill-tools/user-init.scm file:
#
# (let ((dll (dynamic-link
#              "/full/path/to/simple_menu_extension_in_C.la")))
#
#   (register-fontforge-menu-entry
#      #:window 'glyph
#      #:menu-path '("Tools" "Useless tools"
#                    "Glyph view extension written in C")
#      #:action (c-menu-entry-action->procedure
#                  (dynamic-func "glyph_menu_action" dll)
#                  (string->pointer
#                     "This is a glyph view, and the glyph is not 'question'."))
#      #:enabled (c-menu-entry-enabled->procedure
#                   (dynamic-func "glyph_menu_enabled" dll)))
#
#   (register-fontforge-menu-entry
#      #:window 'font
#      #:menu-path '("Tools" "Font view extension written in C")
#      #:action (c-menu-entry-action->procedure
#                  (dynamic-func "font_menu_action" dll)
#                  (string->pointer "This is a font view."))))
#
lib_LTLIBRARIES += simple_menu_extension_in_C.la
simple_menu_extension_in_C_la_SOURCES =	\
	simple_menu_extension_in_C.c
simple_menu_extension_in_C_la_CPPFLAGS = -I$(top_builddir)/inc	\
	''-I$(top_srcdir)/inc -I$(top_builddir)/fontforge
simple_menu_extension_in_C_la_LDFLAGS = -prefer-pic -module -shared	\
	''-avoid-version

if HAVE_FORTRAN_API

# To try 'simple_menu_extension_in_Fortran' without installing it,
# use the full path to
# $(build_dir)/simple_menu_extension_in_Fortran.la in your
# ${HOME}/.config/sortsmill-tools/user-init.scm file:
#
# (let ((dll (dynamic-link
#              "/full/path/to/simple_menu_extension_in_Fortran.la")))
#
#   (register-fontforge-menu-entry
#      #:window 'glyph
#      #:menu-path '("Tools" "Useless tools"
#                    "Glyph view extension written in Fortran")
#      #:action (c-menu-entry-action->procedure
#                  (dynamic-func "glyph_menu_action" dll)
#                  (string->pointer
#                     "This is a glyph view, and the glyph is not 'question'."))
#      #:enabled (c-menu-entry-enabled->procedure
#                   (dynamic-func "glyph_menu_enabled" dll)))
#
#   (register-fontforge-menu-entry
#      #:window 'font
#      #:menu-path '("Tools" "Font view extension written in Fortran")
#      #:action (c-menu-entry-action->procedure
#                  (dynamic-func "font_menu_action" dll)
#                  (string->pointer "This is a font view."))))
#
lib_LTLIBRARIES += simple_menu_extension_in_Fortran.la
simple_menu_extension_in_Fortran_la_SOURCES =	\
	simple_menu_extension_in_Fortran.f90
simple_menu_extension_in_Fortran_la_FCFLAGS = $(AM_FCFLAGS)	\
	$(FC_MODINC)$(top_builddir)/fortran						\
	$(FC_MODINC)$(top_srcdir)/fortran
simple_menu_extension_in_Fortran_la_LDFLAGS = -prefer-pic -module	\
	''-shared -avoid-version -lsortsmill_fortran_api

mostlyclean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

else !HAVE_FORTRAN_API

mostlyclean-modules:

endif !HAVE_FORTRAN_API

mostlyclean-local: mostlyclean-modules
