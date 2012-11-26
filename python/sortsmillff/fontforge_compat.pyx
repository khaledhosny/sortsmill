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
from cpython cimport ref
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

  def __richcmp__ (self, other, op):
    if not isinstance (self, point) or not isinstance (other, point):
      result = False
    elif op == 2:               # ==
      result = (self.x == other.x and self.y == other.y)
    elif op == 3:               # !=
      result = (self.x != other.x or self.y != other.y)
    else:
      result = False
    return result

  def __repr__ (self):
    return ('{}.point(x={},y={},on_curve={},selected={},name={})'
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
# FIXME: List-like operations on contours in regular FontForge are
# either broken outright or seem to have unusual semantics. Our
# current attitude is that these behaviors all are BUGS; programs
# depending on those behaviors therefore are BROKEN and unsupported by
# us.
#
# Our ‘contour’ class simply borrows the semantics of lists.

cdef class contour (object):

  cdef object __points
  cdef bool __closed
  cdef bool __is_quadratic
  cdef object __name

  def __init__ (self, points = [], closed = False, is_quadratic = False, name = None):

    self.__points = list (points)
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

  def __len__ (self):
    return len (self.__points)

  def __getitem__ (self, key):
    result = self.__points[key]
    if not isinstance (result, point):
      result = contour (result, closed = False,
                        is_quadratic = self.__is_quadratic,
                        name = None)
    return result

  def __setitem__ (self, key, item):
    if isinstance (item, point):
      self.__points[key] = item
    elif all ((isinstance (it, point) for it in item)):
      self.__points[key] = list (item)
    else:
      raise TypeError ('expected a point or points')

  def __delitem__ (self, key):
    self.__points.__delitem__ (key)

  def __contains__ (self, item):
    return (item in self.__points)

  def __iter__ (self):
    return self.__points.__iter__ ()

  def __add__ (self, other):
    if isinstance (self, contour):
      result = self.dup ()
      result.__iadd__ (other)
    else:
      result = NotImplemented
    return result

  def __iadd__ (self, other):
    if isinstance (other, point):
      self.__points.append (other)
    elif all ((isinstance (p, point) for p in other)):
      # An extension over regular FontForge: accept general
      # iterables of points.
      if isinstance (other, contour) and self.is_quadratic != other.is_quadratic:
        raise ValueError ('mixed contour degrees')
      self.__points += list (other)
    else:
      raise TypeError ('expected a point or points')

  def dup (self):
    return contour ([p.dup () for p in self.__points],
                    closed = self.__closed,
                    is_quadratic = self.__is_quadratic,
                    name = self.__name)

  def isEmpty (self):
    return (len (self.__points) == 0)

  def moveTo (self, x, y, bool selected = False, name = None):
    if len (self.__points) != 0:
      raise ValueError ('contour not empty')
    self += point (x, y, on_curve = True, selected = selected, name = name)
    ref.Py_INCREF (self)
    return self

  def lineTo (self, double x, double y, pos = None,
              bool selected = False, name = None):
    cdef size_t i = self.__find_on_curve_point (pos)
    self.__points = (self.__points[:i + 1] +
                     [point (x, y, selected = selected, name = name)] +
                     self.__points[i + 1:])
    ref.Py_INCREF (self)
    return self

  def __find_on_curve_point (self, pos, bool check_empty = True):
    if check_empty and len (self.__points) == 0:
      raise ValueError ('contour empty')
    if pos is None:
      pos = len (self.__points) - 1
    if pos < 0 or len (self.__points) <= pos:
      raise IndexError ('expected 0 <= index < ' + str (len (self.__points)))
    cdef size_t i = pos
    while 0 < i and not self.__points[i].on_curve:
      i -= 1
    if not self.__points[i].on_curve:
      raise ValueError ('no on-curve points before position ' + str (pos))
    return i

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
