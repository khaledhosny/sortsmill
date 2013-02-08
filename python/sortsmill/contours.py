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

__make_contour_point    = guile.public_ref ('sortsmill contours', 'make-contour-point')
__contour_point_x       = guile.public_ref ('sortsmill contours', 'contour-point-x')
__contour_point_x_set_x = guile.public_ref ('sortsmill contours', 'contour-point-x-set!')
__contour_point_y       = guile.public_ref ('sortsmill contours', 'contour-point-y')
__contour_point_y_set_x = guile.public_ref ('sortsmill contours', 'contour-point-y-set!')

##
## PROPHYLACTIC FIXME: When converting to SplinePointList, first
## convert numbers to double, to avoid Guile exceptions.
##

class contour_point (pyguile.pyguile):

  def __init__ (self, x, y, on_curve = True, selected = False, name = ''):
    assert guile.number_is_guile_compatible (x)
    assert guile.number_is_guile_compatible (y)
    assert isinstance (name, unicode) or isinstance (name, bytes)
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

  x = property (__get_x, __set_x)
  y = property (__get_y, __set_y)
