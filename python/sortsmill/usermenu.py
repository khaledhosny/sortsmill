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

from . import guile

# FIXME: Get rid of this or revise it for the menu design we end up
# with.
def register_fontforge_menu_entry (window, menu_path, action,
                                   enabled = None,
                                   shortcut = None):
  assert window is not None
  assert menu_path is not None
  assert action is not None
  guile.call (guile.public_ref ('sortsmill usermenu python',
                                'register-python-menu-entry'),
              guile.pyobject (window),
              guile.pyobject (menu_path),
              guile.pyobject (action),
              guile.pyobject (enabled),
              guile.pyobject (shortcut))
