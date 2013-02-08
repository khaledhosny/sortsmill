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

conditions_enabled = __debug__

def pre (*assertions):
  """Decorate a function with preconditions."""
  global conditions_enabled
  if conditions_enabled:
    def check_preconditions (f):
      def new_f (*args, **kwargs):
        for a in assertions:
          passed = a (*args, **kwargs)
          if passed is not True:
            if passed is False:
              raise AssertionError ('{} precondition failed: {}'.format (f, a, args, kwargs))
            else:
              raise AssertionError ('{} precondition failed: {}'.format (f, passed))
        return f (*args, **kwargs)
      return new_f
    return check_preconditions
  else:
    def ignore_preconditions (f):
      return f
    return ignore_preconditions

def post (*assertions):
  """Decorate a function with postconditions."""
  global conditions_enabled
  if conditions_enabled:
    def check_postconditions (f):
      def new_f (*args, **kwargs):
        result = f (*args, **kwargs)
        for a in assertions:
          passed = a (result)
          if passed is not True:
            if passed is False:
              raise AssertionError ('{} postcondition failed: {}'.format (f, a, result))
            else:
              raise AssertionError ('{} postcondition failed: {}'.format (f, passed))
        return result
      return new_f
    return check_postconditions
  else:
    def ignore_postconditions (f):
      return f
    return ignore_postconditions
