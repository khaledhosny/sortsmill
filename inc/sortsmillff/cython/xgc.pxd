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

#--------------------------------------------------------------------------

from libc.stdint cimport uint8_t, uint16_t, uint32_t

cdef extern from "sortsmillff/xgc.h":

  ctypedef struct va_list

  void *x_gc_malloc (size_t sz)
  void *x_gc_malloc_atomic (size_t sz)
  void *x_gc_malloc_uncollectable (size_t sz)
  void *x_gc_realloc (void *old_pointer, size_t sz)
  void *x_gc_malloc_ignore_off_page (size_t sz)
  void *x_gc_malloc_atomic_ignore_off_page (size_t sz)
  void *x_gc_malloc_stubborn (size_t sz)
  char *x_gc_strdup (char *s)

  char *x_gc_strndup (char *s, size_t n)

  char *x_gc_strjoin (char *s1, ...)
  char *x_gc_vstrjoin (char *s1, va_list ap)
  char *x_gc_grabstr (char *s)
  uint8_t *x_gc_u8_grabstr (uint8_t *s)
  uint16_t *x_gc_u16_grabstr (uint16_t *s)
  uint32_t *x_gc_u32_grabstr (uint32_t *s)

#--------------------------------------------------------------------------
