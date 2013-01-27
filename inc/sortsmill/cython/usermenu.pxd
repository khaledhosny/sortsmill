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

from sortsmill.cython.const_pointers cimport const_char_ptr, const_char_ptr_ptr
from sortsmill.cython.guile cimport SCM

cdef extern from "sortsmill/usermenu.h":
  enum:
    FF_FONT_WINDOW = 0x01
    FF_GLYPH_WINDOW = 0x02
    FF_CHAR_WINDOW = FF_GLYPH_WINDOW
    FF_METRICS_WINDOW = 0x03

  void register_fontforge_menu_entry (int window,
                                      const_char_ptr_ptr menu_path,
                                      SCM action, SCM enabled,
                                      const_char_ptr shortcut)
