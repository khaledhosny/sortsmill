# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Barry Schwartz
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

cdef extern from "config.h":
  pass

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

cimport sortsmillff.cython.const_pointers as constp

cdef extern from "libguile.h":
  # FIXME: THIS ASSUMES SCM_DEBUG_TYPING_STRICTNESS == 1
  # FIXME: Either provide support for the other levels, or
  # FIXME: force this level more carefully.
  cdef struct scm_unused_struct:
    pass
  ctypedef scm_unused_struct *SCM

cdef extern from "sortsmillff/usermenu.h":
  enum:
    FF_FONT_WINDOW = 0x01
    FF_GLYPH_WINDOW = 0x02
    FF_CHAR_WINDOW = FF_GLYPH_WINDOW
    FF_METRICS_WINDOW = 0x03

  void register_fontforge_menu_entry (int window,
                                      constp.const_char_ptr_ptr menu_path,
                                      SCM action, SCM enabled,
                                      constp.const_char_ptr shortcut)

#  ctypedef void (*ff_menu_entry_action_t) (void *obj, void *data)
#  ctypedef bool (*ff_menu_entry_enabled_t) (void *obj, void *data)
#
#  void register_fontforge_menu_entry (int window,
#                                      constp.const_char_ptr_ptr menu_path,
#                                      ff_menu_entry_action_t action,
#                                      ff_menu_entry_enabled_t enabled,
#                                      constp.const_char_ptr shortcut,
#                                      void *data)
