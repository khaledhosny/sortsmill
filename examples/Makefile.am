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

lib_LTLIBRARIES =

#********************************************************************
# FIXME: The ‘simple_menu_extension_in_C’ method of registering
# probably will be eliminated before long, because reworked menus are
# likely to be implemented largely in Guile, and have more
# complicated operations (involving locales, insertions, deletions,
# and so forth).
#********************************************************************
#
# To try 'simple_menu_extension_in_C' without installing it, use the
# full path to $(build_dir)/simple_menu_extension_in_C.la in your
# ${HOME}/.config/sortsmill-fontforge/user-init.scm file:
#
#    (load-extension
#        "/full/path/to/simple_menu_extension_in_C.la"
#        "my_menu_extension_init")
#
lib_LTLIBRARIES += simple_menu_extension_in_C.la
simple_menu_extension_in_C_la_SOURCES =	\
	simple_menu_extension_in_C.c
simple_menu_extension_in_C_la_CPPFLAGS = -I$(top_builddir)/inc	\
	''-I$(top_srcdir)/inc -I$(top_builddir)/fontforge
simple_menu_extension_in_C_la_LDFLAGS = -prefer-pic	\
	''-module -shared -avoid-version

# To try 'alternate_simple_menu_extension_in_C' without installing it,
# use the full path to
# $(build_dir)/alternate_simple_menu_extension_in_C.la in your
# ${HOME}/.config/sortsmill-fontforge/user-init.scm file:
#
# (let ((dll (dynamic-link
#              "/full/path/to/alternate_simple_menu_extension_in_C.la")))
#
#   (register-fontforge-menu-entry
#      #:window 'glyph
#      #:menu-path '("Tools" "Useless tools"
#                    "Glyph view alternate extension written in C")
#      #:action (wrap-ff_menu_entry_action_t
#                  (dynamic-func "glyph_menu_action" dll)
#                  (string->pointer
#                     "This is a glyph view, and the glyph is not 'question'."))
#      #:enabled (wrap-ff_menu_entry_enabled_t
#                   (dynamic-func "glyph_menu_enabled" dll)))
#
#   (register-fontforge-menu-entry
#      #:window 'font
#      #:menu-path '("Tools" "Font view alternate extension written in C")
#      #:action (wrap-ff_menu_entry_action_t
#                  (dynamic-func "font_menu_action" dll)
#                  (string->pointer "This is a font view."))))
#
# This method requires less C code, and it can be used to register
# Fortran procedures without resorting to C. Also it lets you send
# different data to the ‘action’ and ‘enabled’ functions.
lib_LTLIBRARIES += alternate_simple_menu_extension_in_C.la
alternate_simple_menu_extension_in_C_la_SOURCES =	\
	alternate_simple_menu_extension_in_C.c
alternate_simple_menu_extension_in_C_la_CPPFLAGS =	\
	''-I$(top_builddir)/inc -I$(top_srcdir)/inc		\
	-I$(top_builddir)/fontforge
alternate_simple_menu_extension_in_C_la_LDFLAGS = -prefer-pic -module	\
	''-shared -avoid-version

# To try 'simple_menu_extension_in_Fortran' without installing it,
# use the full path to
# $(build_dir)/simple_menu_extension_in_Fortran.la in your
# ${HOME}/.config/sortsmill-fontforge/user-init.scm file:
#
# (let ((dll (dynamic-link
#              "/full/path/to/simple_menu_extension_in_Fortran.la")))
#
#   (register-fontforge-menu-entry
#      #:window 'glyph
#      #:menu-path '("Tools" "Useless tools"
#                    "Glyph view alternate extension written in Fortran")
#      #:action (wrap-ff_menu_entry_action_t
#                  (dynamic-func "glyph_menu_action" dll)
#                  (string->pointer
#                     "This is a glyph view, and the glyph is not 'question'."))
#      #:enabled (wrap-ff_menu_entry_enabled_t
#                   (dynamic-func "glyph_menu_enabled" dll)))
#
#   (register-fontforge-menu-entry
#      #:window 'font
#      #:menu-path '("Tools" "Font view alternate extension written in Fortran")
#      #:action (wrap-ff_menu_entry_action_t
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
	''-shared -avoid-version -lsortsmillff_fortran_api
