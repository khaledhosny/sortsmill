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

cimport contour_interface as ci
from cython cimport view
from cython.view cimport array as cvarray
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
    return ('{}.point(x={:g},y={:g},on_curve={},selected={},name={})'
            .format (__name__, self.__x, self.__y,
                     repr (self.__on_curve),
                     repr (self.__selected),
                     repr (self.__name)))

  def __str__ (self):
    return ('{}.point({:g},{:g},{})'
            .format (__name__, self.__x, self.__y, self.__on_curve))

  def dup (self):
    p = point (self.__x, self.__y, self.__on_curve)
    p.__selected = self.__selected
    return p

#--------------------------------------------------------------------------
#
# FIXME: Some operations on contours in regular FontForge are either
# broken outright or have unusual semantics. Our current attitude is
# that these are all bugs, and programs depending on the behaviors
# therefore are broken. Our ‘contour’ class is a subclass of list and
# inherits the semantics of lists.

cdef class contour (list):

  cdef bool __closed
  cdef bool __is_quadratic
  cdef object __name

  def __init__ (self, points = [], closed = False, is_quadratic = False, name = None):
    super (contour, self).__init__ (points)
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

  def __repr__ (self):
    return (__name__ + '.contour(' + repr (list (self)) +
            (',closed={},is_quadratic={},name={})'
             .format (repr (self.__closed),
                      repr (self.__is_quadratic),
                      repr (self.__name))))

  def __str__ (self):
    cdef size_t i
    s = '<Contour(' + ('quadratic' if self.__is_quadratic else 'cubic') + ')\n'
    for i from 0 <= i < self.__n:
      s += '  ({:g},{:g}) {}\n'.format (self.__x[i], self.__y[i],
                                       'on' if self.__on_curve[i] else 'off')
    s += '>'
    return s

  def dup (self):
    return contour ([p.dup () for p in self], closed = self.__closed,
                    is_quadratic = self.__is_quadratic, name = self.__name)

  def isEmpty (self):
    return (len (self) == 0)

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
