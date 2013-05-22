# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
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
              raise AssertionError ('{} precondition failed: {!r}'.format (f, (a, args, kwargs)))
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
          passed = a (result, *args, **kwargs)
          if passed is not True:
            if passed is False:
              raise AssertionError ('{} postcondition failed: {!r}'.format (f, (a, result, args, kwargs)))
            else:
              raise AssertionError ('{} postcondition failed: {}'.format (f, passed))
        return result
      return new_f
    return check_postconditions
  else:
    def ignore_postconditions (f):
      return f
    return ignore_postconditions

# For use as follows: @code{@@conditions.post (conditions.result_type_is (Type1, Type2, Type3))}
def result_type_is (*Type):
  if len (Type) == 1:
    Type = Type[0]
    def assertion (result, *args, **kwargs):
      return True if isinstance (result, Type) \
          else 'expected result type was {}, but got: {!r}'.format (Type, result)
  else:
    def assertion (result, *args, **kwargs):
      return True if any ((isinstance (result, T) for T in Type)) \
          else 'expected result type was one of {}, but got: {!r}'.format (Type, result)
  return assertion
