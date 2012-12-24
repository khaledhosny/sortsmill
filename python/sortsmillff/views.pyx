# -*- coding: utf-8; python-indent: 2 -*-

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

cdef extern from 'config.h': pass

import internal_types

from libc.stdint cimport uintptr_t

cdef class font_view (object):

  cdef uintptr_t ptr

  def __cinit__ (self):
    self.ptr = <uintptr_t> NULL

  def __init__ (self, uintptr_t ptr):
    self.ptr = ptr

  def to_internal_type (self):
    return internal_types.FontViewBase (self.ptr)

  def __repr__ (self):
    return '{}.font_view(0x{:x})'.format (__name__, self.ptr)

  def __str__ (self):
    cdef uintptr_t sf_ptr
    cdef uintptr_t font_name_ptr
    fvb = self.to_internal_type ()
    sf_ptr = fvb._sf
    if sf_ptr == <uintptr_t> NULL:
      font_name = '<_sf=NULL>'
    else:
      sf = internal_types.SplineFont (sf_ptr)
      font_name_ptr = sf._font_name
      if font_name_ptr == <uintptr_t> NULL:
        font_name = '<_font_name=NULL>'
      else:
        font_name = str (<char *> font_name_ptr)
    return ('<font_view "{:s}" 0x{:x}>'.format (font_name, self.ptr))

cdef class glyph_view (object):

  cdef uintptr_t ptr

  def __cinit__ (self):
    self.ptr = <uintptr_t> NULL

  def __init__ (self, uintptr_t ptr):
    self.ptr = ptr

  def to_internal_type (self):
    return internal_types.CharViewBase (self.ptr)

  def __repr__ (self):
    return '{}.glyph_view(0x{:x})'.format (__name__, self.ptr)

  def __str__ (self):
    cdef uintptr_t sf_ptr
    cdef uintptr_t font_name_ptr
    cdef uintptr_t glyph_name_ptr
    sc = internal_types.CharViewBase (self.to_internal_type ()).sc
    sf_ptr = sc._parent
    if sf_ptr == <uintptr_t> NULL:
      font_name = '<_parent=NULL>'
    else:
      sf = internal_types.SplineFont (sf_ptr)
      font_name_ptr = sf._font_name
      if font_name_ptr == <uintptr_t> NULL:
        font_name = '<_font_name=NULL>'
      else:
        font_name = str (<char *> font_name_ptr)
    glyph_name_ptr = <uintptr_t> sc._name
    if glyph_name_ptr == <uintptr_t> NULL:
      glyph_name = '<_name=NULL>'
    else:
      glyph_name = str (<char *> glyph_name_ptr)
    return ('<glyph_view "{:s}":"{:s}" 0x{:x}>'.format (font_name, glyph_name, self.ptr))
