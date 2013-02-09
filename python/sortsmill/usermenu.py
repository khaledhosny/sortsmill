# -*- coding: utf-8; python-indent: 2 -*-

# Copyright (C) 2012, 2013 Barry Schwartz
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

from . import (guile, conditions)

def __force_unicode (s):
  if isinstance (s, bytes):
    s = s.decode ('UTF-8')
  return s

# FIXME: Centralize this information.
__window_cases = ('font', 'glyph', 'char')

def __acceptable_as_window (w):
  global __window_cases
  w = __force_unicode (w)
  return isinstance (w, unicode) and w.lower () in __window_cases

def __acceptable_as_menu_path (mp):
  try:
    acceptable = all ([isinstance (__force_unicode (s), unicode) for s in mp])
  except:
    acceptable = False
  return acceptable

def __register_fontforge_menu_entry_preconditions (window, menu_path, action,
                                                   enabled = None,
                                                   shortcut = None):
  result = True
  if not __acceptable_as_window (window):
    result = 'expected one of {} for ‘window’, but got: {!r}'.format (__window_cases, window)
  elif not __acceptable_as_menu_path (menu_path):
    result = 'expected a string sequence for ‘menu_path’, but got: {!r}'.format (menu_path)
  elif not callable (action):
    result = '‘action’ is not callable: {!r}'.format (action)
  elif enabled is not None and not callable (enabled):
    result = '‘enabled’ is not callable: {!r}'.format (enabled)
  elif shortcut is not None and not isinstance (__force_unicode (shortcut), unicode):
    result = '‘shortcut’ is not a string: {!r}'.format (shortcut)
  return result

@conditions.pre (__register_fontforge_menu_entry_preconditions)
def register_fontforge_menu_entry (window, menu_path, action,
                                   enabled = None,
                                   shortcut = None):

  # FIXME: Get rid of this or revise it for the menu design we end up
  # with.

  guile.call (guile.public_ref ('sortsmill usermenu python',
                                'register-python-menu-entry'),
              guile.pyobject (window),
              guile.pyobject (menu_path),
              guile.pyobject (action),
              guile.pyobject (enabled),
              guile.pyobject (shortcut))
