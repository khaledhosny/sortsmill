# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2013 by Barry Schwartz
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

from . import (pyguile, guile, conditions)

__closed_p_keyword   = guile.string_to_guile_keyword ('closed?')
__degree_keyword     = guile.string_to_guile_keyword ('degree')
__name_keyword       = guile.string_to_guile_keyword ('name')
__on_curve_p_keyword = guile.string_to_guile_keyword ('on-curve?')
__selected_p_keyword = guile.string_to_guile_keyword ('selected?')

__make_contour_point             = guile.public_ref ('sortsmill contours', 'make-contour-point')
__contour_point_x                = guile.public_ref ('sortsmill contours', 'contour-point-x')
__contour_point_x_set_x          = guile.public_ref ('sortsmill contours', 'contour-point-x-set!')
__contour_point_y                = guile.public_ref ('sortsmill contours', 'contour-point-y')
__contour_point_y_set_x          = guile.public_ref ('sortsmill contours', 'contour-point-y-set!')
__contour_point_on_curve_p       = guile.public_ref ('sortsmill contours', 'contour-point-on-curve?')
__contour_point_on_curve_p_set_x = guile.public_ref ('sortsmill contours', 'contour-point-on-curve?-set!')
__contour_point_selected_p       = guile.public_ref ('sortsmill contours', 'contour-point-selected?')
__contour_point_selected_p_set_x = guile.public_ref ('sortsmill contours', 'contour-point-selected?-set!')
__contour_point_name             = guile.public_ref ('sortsmill contours', 'contour-point-name')
__contour_point_name_set_x       = guile.public_ref ('sortsmill contours', 'contour-point-name-set!')

__make_contour                   = guile.public_ref ('sortsmill contours', 'make-contour')
__contour_points                 = guile.public_ref ('sortsmill contours', 'contour-points')
__contour_points_set_x           = guile.public_ref ('sortsmill contours', 'contour-points-set!')
__contour_closed_p               = guile.public_ref ('sortsmill contours', 'contour-closed?')
__contour_closed_p_set_x         = guile.public_ref ('sortsmill contours', 'contour-closed?-set!')
__contour_degree                 = guile.public_ref ('sortsmill contours', 'contour-degree')
__contour_degree_set_x           = guile.public_ref ('sortsmill contours', 'contour-degree-set!')
__contour_name                   = guile.public_ref ('sortsmill contours', 'contour-name')
__contour_name_set_x             = guile.public_ref ('sortsmill contours', 'contour-name-set!')

def __get_value_is_string (value, *args):
  return True if isinstance (value, unicode) \
      else 'result is not a Unicode string: {!r}'.format (value)

def __set_value_is_string (self, value):
  return True if isinstance (value, unicode) or isinstance (value, bytes) \
      else 'argument is not a string: {!r}'.format (value)

def __get_value_is_number (value, *args):
  return True if guile.number_is_guile_compatible (value) \
      else 'result is not a Guile-compatible number: {!r}'.format (value)

def __set_value_is_number (self, value):
  return True if guile.number_is_guile_compatible (value) \
      else 'argument is not a Guile-compatible number: {!r}'.format (value)

def __get_value_is_2_or_3 (value, *args):
  return True if value == 2 or value == 3 \
      else 'result is not 2 or 3: {!r}'.format (value)

def __set_value_is_2_or_3 (self, value):
  return True if value == 2 or value == 3 \
      else 'argument is not 2 or 3: {!r}'.format (value)

def __get_value_is_bool (value, *args):
  return True if isinstance (value, bool) \
      else 'result is not a bool: {!r}'.format (value)

def __points_acceptable (points):
  try:
    acceptable = all ([isinstance (p, contour_point) for p in points])
  except:
    acceptable = False
  return acceptable

def __is_pair_of_guile_compatible_numbers (obj):
  try:
    x = obj[0]
    y = obj[1]
  except:
    x = None
    y = None
  return guile.number_is_guile_compatible (x) and guile.number_is_guile_compatible (y)

class contour_point (pyguile.pyguile):

  def __init_preconditions (self, x, y, on_curve = True, selected = False, name = ''):
    result = True
    if not guile.number_is_guile_compatible (x):
      result = 'x is not a Guile-compatible number: {!r}'.format (x)
    elif not guile.number_is_guile_compatible (y):
      result = 'y is not a Guile-compatible number: {!r}'.format (y)
    elif not isinstance (name, unicode) and not isinstance (name, bytes):
      result = 'name is not a string: {!r}'.format (name)
    return result

  @conditions.pre (__init_preconditions)
  def __init__ (self, x, y, on_curve = True, selected = False, name = ''):
    on_curve = not not on_curve
    selected = not not selected
    point = guile.call (__make_contour_point,
                        guile.number_to_pyguile (x),
                        guile.number_to_pyguile (y),
                        __on_curve_p_keyword, guile.bool_to_pyguile (on_curve),
                        __selected_p_keyword, guile.bool_to_pyguile (selected),
                        __name_keyword, guile.string_to_pyguile (name))
    super (contour_point, self).__init__ (point.address)

  @conditions.post (__get_value_is_number)
  def __get_x (self):
    return guile.pyguile_to_number (guile.call (__contour_point_x, self))

  @conditions.pre (__set_value_is_number)
  def __set_x (self, value):
    guile.call (__contour_point_x_set_x, self, guile.number_to_pyguile (value))

  @conditions.post (__get_value_is_number)
  def __get_y (self):
    return guile.pyguile_to_number (guile.call (__contour_point_y, self))

  @conditions.pre (__set_value_is_number)
  def __set_y (self, value):
    guile.call (__contour_point_y_set_x, self, guile.number_to_pyguile (value))

  @conditions.post (__get_value_is_bool)
  def __get_on_curve (self):
    return guile.pyguile_to_bool (guile.call (__contour_point_on_curve_p, self))

  def __set_on_curve (self, value):
    value = not not value
    guile.call (__contour_point_on_curve_p_set_x, self, guile.bool_to_pyguile (value))

  @conditions.post (__get_value_is_bool)
  def __get_selected (self):
    return guile.pyguile_to_bool (guile.call (__contour_point_selected_p, self))

  def __set_selected (self, value):
    value = not not value
    guile.call (__contour_point_selected_p_set_x, self, guile.bool_to_pyguile (value))

  @conditions.post (__get_value_is_string)
  def __get_name (self):
    return guile.pyguile_to_string (guile.call (__contour_point_name, self))

  @conditions.pre (__set_value_is_string)
  def __set_name (self, value):
    guile.call (__contour_point_name_set_x, self, guile.string_to_pyguile (value))

  x = property (__get_x, __set_x)
  y = property (__get_y, __set_y)
  on_curve = property (__get_on_curve, __set_on_curve)
  selected = property (__get_selected, __set_selected)
  name = property (__get_name, __set_name)

  @conditions.pre (lambda self, other: True if (isinstance (other, contour_point)
                                                or __is_pair_of_guile_compatible_numbers (other))
                   else 'object of unexpected type: {!r}'.format (other))
  @conditions.post (lambda result, self, other: result is NotImplemented or isinstance (result, bool))
  def __eq__ (self, other):
    if isinstance (other, contour_point):
      # Taking account of whether the point is on-curve or not
      # probably differs from legacy FontForge’s behavior. You can
      # test ‘equality’ with a pair (the other clause of this if-else)
      # if you want to ignore the on-curve settings.
      result = (self.x == other.x) and (self.y == other.y) and (self.on_curve == other.on_curve)
    else:
      result = (self.x == other[0]) and (self.y == other[1])
    return result

  def __ne__ (self, other):
    return not self.__eq__ (other)

class contour (pyguile.pyguile):

  def __init_preconditions (self, points = (), closed = True, degree = 3, name = ''):
    result = True
    if not __points_acceptable (points):
      result = 'expected a sequence of contour_point objects: {!r}'.format (points)
    elif degree != 2 and degree != 3:
      result = '‘degree’ must be 2 or 3: {!r}'.format (degree)
    elif not isinstance (name, unicode) and not isinstance (name, bytes):
      result = 'name is not a string: {!r}'.format (name)
    return result

  @conditions.pre (__init_preconditions)
  def __init__ (self, points, closed = False, degree = 3, name = ''):
    closed = not not closed
    degree = int (degree)
    c = guile.call (__make_contour,
                    guile.sequence_to_pyguile (points),
                    __closed_p_keyword, guile.bool_to_pyguile (closed),
                    __degree_keyword, guile.number_to_pyguile (degree),
                    __name_keyword, guile.string_to_pyguile (name))
    super (contour, self).__init__ (c.address)

  @conditions.post (lambda result, self: __points_acceptable (result))
  def get_points (self):
    return guile.pyguile_to_list (guile.call (__contour_points, self))

  @conditions.pre (lambda self, value: __points_acceptable (value))
  def set_points (self, value):
    guile.call (__contour_points_set_x, self, guile.sequence_to_pyguile (value))

  @conditions.post (__get_value_is_bool)
  def __get_closed (self):
    return guile.pyguile_to_bool (guile.call (__contour_closed_p, self))

  def __set_closed (self, value):
    value = not not value
    guile.call (__contour_closed_p_set_x, self, guile.bool_to_pyguile (value))

  @conditions.post (__get_value_is_2_or_3)
  def __get_degree (self):
    return guile.pyguile_to_number (guile.call (__contour_degree, self))

  @conditions.pre (__set_value_is_2_or_3)
  def __set_degree (self, value):
    value = int (value)
    guile.call (__contour_degree_set_x, self, guile.number_to_pyguile (value))

  @conditions.post (__get_value_is_string)
  def __get_name (self):
    return guile.pyguile_to_string (guile.call (__contour_name, self))

  @conditions.pre (__set_value_is_string)
  def __set_name (self, value):
    guile.call (__contour_name_set_x, self, guile.string_to_pyguile (value))

  closed = property (__get_closed, __set_closed)
  degree = property (__get_degree, __set_degree)
  name = property (__get_name, __set_name)

  def __len__ (self):
    lst = self.get_points ()
    return len (lst)

  def __getitem__ (self, key):
    lst = self.get_points ()[key]
    return lst

  def __setitem__ (self, key, value):
    lst = self.get_points ()
    lst[key] = value
    self.set_points (lst)

  def __delitem__ (self, key):
    lst = self.get_points ()
    del lst[key]
    self.set_points (lst)

  def __iter__ (self):
    lst = self.get_points ()
    return lst.__iter__ ()

  def __reversed__ (self):
    lst = self.get_points ()
    self.set_points (reversed (lst))

  def __contains__ (self, item):
    lst = self.get_points ()
    return lst.__contains__ (item)

  def __add__ (self, other):
    if other.degree == self.degree:
      lst = self.get_points () + other.get_points ()
    else:
      # FIXME: Implement coercion of the other contour to the same
      # degree as this one.
      assert False, 'Not yet implemented'
    # The result is unnamed. This may differ from legacy FontForge
    # behavior.
    return contour (points = lst, on_curve = self.on_curve, degree = self.degree)

  def __iadd__ (self, other):
    lst = self.get_points ()
    if other.degree == self.degree:
      lst += other.get_points ()
    else:
      # FIXME: Implement coercion of the other contour to the same
      # degree as this one.
      assert False, 'Not yet implemented'
    self.set_points (lst)

  def __mul__ (self, other):
    lst = self.get_points ()
    lst = lst.__mul__ (other)
    # The result is unnamed. This may differ from legacy FontForge
    # behavior.
    return contour (points = lst, on_curve = self.on_curve, degree = self.degree)

  def __imul__ (self, other):
    lst = self.get_points ()
    lst = lst.__mul__ (other)
    self.set_points (lst)

  def copy (self):
    return contour (points = self.get_points (),
                    closed = self.closed,
                    degree = self.degree,
                    name = self.name)

  def append (self, item):
    lst = self.get_points ()
    lst.append (item)
    self.set_points (lst)

  def count (self, item):
    lst = self.get_points ()
    return lst.count (item)

  def extend (self, iterable):
    lst = self.get_points ()
    lst.extend (iterable)
    self.set_points (lst)

  def index (self, item):
    lst = self.get_points ()
    return lst.index (item)

  def insert (self, i, item):
    lst = self.get_points ()
    lst.insert (i, item)
    self.set_points (lst)

  def pop (self, i = -1):
    lst = self.get_points ()
    item = lst.pop (i)
    self.set_points (lst)
    return item

  def remove (self, item):
    lst = self.get_points ()
    lst.remove (item)
    self.set_points (lst)

  def reverse (self):
    lst = self.get_points ()
    lst.reverse ()
    self.set_points (lst)
