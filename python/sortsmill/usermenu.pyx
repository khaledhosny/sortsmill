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

cdef extern from 'config.h':
  pass

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

from cpython.ref cimport PyObject

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()

def register_fontforge_menu_entry (window not None,
                                   menu_path not None,
                                   action not None,
                                   enabled = None,
                                   shortcut = None):
  if not get_no_windowing_ui ():
    menu_path = tuple (menu_path)
    scm.scm_call_5 (scm.scm_c_private_ref ('sortsmill usermenu',
                                           'register-python-menu-entry'),
                    scm.scm_from_pointer (<PyObject *> window, NULL),
                    scm.scm_from_pointer (<PyObject *> menu_path, NULL),
                    scm.scm_from_pointer (<PyObject *> action, NULL),
                    scm.scm_from_pointer (<PyObject *> enabled, NULL),
                    scm.scm_from_pointer (<PyObject *> shortcut, NULL))
