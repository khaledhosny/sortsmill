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

import array

cimport contour_interface as ci
from cpython cimport array
#from cython cimport view
#from cython.view cimport array as cvarray
from libcpp cimport bool

cdef extern from "stdbool.h": pass

#--------------------------------------------------------------------------

class fontforge_compat_exception (Exception):
  """Base class for some exceptions raised by
  sortsmillff.fontforge_compat.

  """
  pass

# FIXME: More fully implement this exception.
class fontforge_compat_unsupported (fontforge_compat_exception):
  pass

#--------------------------------------------------------------------------

def loadPlugin (filename):
  raise fontforge_compat_unsupported

def loadPluginDir (filename):
  raise fontforge_compat_unsupported

def version ():
  return '99999999'             # FIXME: This is a workaround.

#--------------------------------------------------------------------------

cdef class point (object):

  cdef double __x
  cdef double __y
  cdef bool __on_curve
  cdef bool __selected
  cdef object __name

  def __init__ (self, double x = 0, double y = 0,
                bool on_curve = True, bool selected = False,
                name = None):
    self.__x = x
    self.__y = y
    self.__on_curve = on_curve
    self.__selected = selected
    self.name = name            # An extension from regular FontForge.

  def __get_x (self):
    return self.__x

  def __set_x (self, double x):
    self.__x = x

  def __get_y (self):
    return self.__y

  def __set_y (self, double y):
    self.__y = y

  def __get_on_curve (self):
    return self.__on_curve

  def __set_on_curve (self, bool on_curve):
    self.__on_curve = on_curve

  def __get_selected (self):
    return self.__selected

  def __set_selected (self, bool selected):
    self.__selected = selected

  def __get_name (self):
    return self.__name

  def __set_name (self, name):
    if name == None:
      self.__name = None
    else:
      self.__name = str (name)

  x = property (__get_x, __set_x)
  y = property (__get_y, __set_y)
  on_curve = property (__get_on_curve, __set_on_curve)
  selected = property (__get_selected, __set_selected)
  name = property (__get_name, __set_name)

  def __repr__ (self):
    return ('{}.point({:g},{:g},{})'
            .format (__name__, self.__x, self.__y, self.__on_curve))

  def dup (self):
    p = point (self.__x, self.__y, self.__on_curve)
    p.__selected = self.__selected
    return p

#--------------------------------------------------------------------------

cdef class contour (object):

  cdef object __x
  cdef object __y
  cdef object __on_curve
  cdef object __selected
  cdef bool __closed
  cdef bool __is_quadratic
  cdef object __name

  def __init__ (self, points = None, closed = False, is_quadratic = False, name = None):
    if points is not None:
      x_vals = [p.x for p in points]
      y_vals = [p.y for p in points]
      on_curve_vals = [p.on_curve for p in points]
      selected_vals = [p.selected for p in points]
    else:
      x_vals = []
      y_vals = []
      on_curve_vals = []
      selected_vals = []
    self.__x = array.array ('d', x_vals)
    self.__y = array.array ('d', y_vals)
    self.__on_curve = array.array ('i', on_curve_vals)
    self.__selected = array.array ('i', selected_vals)
    self.__closed = closed
    self.__is_quadratic = is_quadratic
    self.name = name
    
  def __get_closed (self):
    return self.__closed

  def __set_closed (self, bool closed):
    self.__closed = closed

  def __get_is_quadratic (self):
    return self.__is_quadratic

  def __set_is_quadratic (self, bool is_quadratic):
    if ((self.__is_quadratic and not is_quadratic)
        or (not self.__is_quadratic and is_quadratic)):
      # FIXME: Here goes code to change the degree.
      pass
    self.__is_quadratic = is_quadratic

  def __get_name (self):
    return self.__name

  def __set_name (self, name):
    if name == None:
      self.__name = None
    else:
      self.__name = str (name)

  closed = property (__get_closed, __set_closed)
  is_quadratic = property (__get_is_quadratic, __set_is_quadratic)
  name = property (__get_name, __set_name)

  def __len__ (self):
    return len (self.__x)

  def __getitem__ (self, key):
    cdef int i
    cdef array.array[double] x_vals = self.__x
    cdef array.array[double] y_vals = self.__y
    cdef array.array[int] on_curve_vals = self.__on_curve
    cdef array.array[int] selected_vals = self.__selected
    x = x_vals[key]
    y = y_vals[key]
    on_curve = on_curve_vals[key]
    selected = selected_vals[key]
    if type (x) == type (self.__x):
      result = contour ([point (x = x[i], y = y[i],
                                on_curve = on_curve[i],
                                selected = selected[i])
                         for i from 0 <= i < len (x)])
    else:
      result = point (x = x, y = y, on_curve = on_curve,
                      selected = selected)
    return result

  def __setitem__ (self, key, points):
    cdef array.array[double] x_vals = self.__x
    cdef array.array[double] y_vals = self.__y
    cdef array.array[int] on_curve_vals = self.__on_curve
    cdef array.array[int] selected_vals = self.__selected
    # FIXME: Not yet implemented.

#  def __repr__ (self):
    

#--------------------------------------------------------------------------

class layer (object):
  pass

#--------------------------------------------------------------------------

class glyphPen (object):
  pass

#--------------------------------------------------------------------------

class glyph (object):
  pass

#--------------------------------------------------------------------------

class selection (object):
  pass

#--------------------------------------------------------------------------

class private (object):
  pass

#--------------------------------------------------------------------------

class math (object):
  pass

#--------------------------------------------------------------------------

class font (object):
  pass

#--------------------------------------------------------------------------
