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
