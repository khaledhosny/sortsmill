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

__on_curve_p_keyword = guile.string_to_guile_keyword ('on-curve?')
__selected_p_keyword = guile.string_to_guile_keyword ('selected?')
__name_keyword       = guile.string_to_guile_keyword ('name')

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

##
## PROPHYLACTIC FIXME: When converting to SplinePointList, first
## convert numbers to double, to avoid Guile exceptions.
##

class contour_point (pyguile.pyguile):

  def __init_preconditions (self, x, y, on_curve = True, selected = False, name = ''):
    result = True
    if not guile.number_is_guile_compatible (x):
      result = 'x is not a Guile-compatible number: {}'.format (x)
    elif not guile.number_is_guile_compatible (y):
      result = 'y is not a Guile-compatible number: {}'.format (y)
    elif not isinstance (name, unicode) and not isinstance (name, bytes):
      result = 'name is not a string: {}'.format (name)
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

  def __get_value_is_number (value, *args):
    return True if guile.number_is_guile_compatible (value) \
        else 'result is not a Guile-compatible number: {}'.format (value)

  def __set_value_is_number (self, value):
    return True if guile.number_is_guile_compatible (value) \
        else 'argument is not a Guile-compatible number: {}'.format (value)

  def __get_value_is_bool (value, *args):
    return True if isinstance (value, bool) \
        else 'result is not a bool: {}'.format (value)

  def __get_value_is_string (value, *args):
    return True if isinstance (value, unicode) \
        else 'result is not a Unicode string: {}'.format (value)

  def __set_value_is_string (self, value):
    return True if isinstance (value, unicode) or isinstance (value, bytes) \
        else 'argument is not a string: {}'.format (value)

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
