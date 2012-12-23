# -*- coding: utf-8; python-indent: 2 -*-

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

cdef extern from 'config.h': pass

from . import (views, notices)

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

cimport sortsmillff.cython.usermenu as c_usermenu
cimport sortsmillff.cython.const_pointers as constp
cimport sortsmillff.cython.xgc as xgc
from cpython.ref cimport PyObject, Py_XINCREF
from cpython.object cimport PyObject_IsTrue
from libc.stdint cimport uintptr_t

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()

import sys
import traceback

ctypedef struct __c_data:
  int window_tag
  PyObject *action_func
  PyObject *enable_func

def __c_window (window):
  message = "expected 'glyph' or 'font'"
  cdef int window_tag = 0
  glyph = ["glyph", "char"]
  font = ["font"]
  if isinstance (window, str):
    if window.lower () in glyph:
      window_tag = c_usermenu.FF_GLYPH_WINDOW
    elif window.lower () in font:
      window_tag = c_usermenu.FF_FONT_WINDOW
    else:
      raise ValueError (message)
  else:
    raise TypeError (message)
  return window_tag

cdef char **__c_menu_path (object menu_path):
  cdef size_t length = len (menu_path)
  cdef char **result = \
      <char **> xgc.x_gc_malloc ((length + 1) * sizeof (char *))
  cdef size_t i
  for i from 0 <= i < length:
    result[i] = xgc.x_gc_strdup (menu_path[i])
  return result

cdef char *__c_shortcut (object shortcut):
  cdef char *result = NULL
  if shortcut is not None:
    result = xgc.x_gc_strdup (shortcut)
  return result

cdef object __ff_obj_to_py (int window_tag, void *ff_obj):
  assert (window_tag == c_usermenu.FF_GLYPH_WINDOW
          or window_tag == c_usermenu.FF_FONT_WINDOW)
  result = None
  if window_tag == c_usermenu.FF_GLYPH_WINDOW:
    result = views.glyph_view (<uintptr_t> ff_obj)
  elif window_tag == c_usermenu.FF_FONT_WINDOW:
    result = views.font_view (<uintptr_t> ff_obj)
  return result

cdef void __do_action (void *ff_obj, void *data):
  cdef __c_data *py_data = <__c_data *> data
  view = __ff_obj_to_py (py_data.window_tag, ff_obj)
  action = <object> py_data.action_func
  call_python_action (action, view)

cdef bool __check_enabled (void *ff_obj, void *data):
  cdef bint is_enabled = True
  cdef __c_data *py_data = <__c_data *> data
  if py_data.enable_func != <PyObject *> None:
    view = __ff_obj_to_py (py_data.window_tag, ff_obj)
    enabled = <object> py_data.enable_func
    retval = call_python_enabled (enabled, view)
    is_enabled = PyObject_IsTrue (retval)
  return is_enabled

cdef void __registerMenuItem (int c_window,
                              object action_function,
                              object enable_function,
                              object shortcut,
                              object menu_path):
  assert not get_no_windowing_ui ()

  cdef __c_data *c_data
  cdef char **c_menu_path
  cdef char *c_shortcut

  c_data = <__c_data *> xgc.x_gc_malloc (sizeof (__c_data))
  Py_XINCREF (<PyObject *> action_function)
  Py_XINCREF (<PyObject *> enable_function)
  c_data.window_tag = c_window
  c_menu_path = __c_menu_path (menu_path)
  c_data.action_func = <PyObject *> action_function
  c_data.enable_func = <PyObject *> enable_function
  c_shortcut = __c_shortcut (shortcut)
  c_usermenu.register_fontforge_menu_entry (c_window,
                                            <constp.const_char_ptr_ptr> c_menu_path,
                                            __do_action, __check_enabled,
                                            <constp.const_char_ptr> c_shortcut,
                                            <void *> c_data)

#--------------------------------------------------------------------------

def register_fontforge_menu_entry (window not None,
                                   menu_path not None,
                                   action not None,
                                   enabled = lambda _: True,
                                   shortcut = None):
  if not get_no_windowing_ui ():
    __registerMenuItem (__c_window (window), action, enabled,
                        shortcut, menu_path)

#--------------------------------------------------------------------------

def call_python_action (action not None, view not None):
  try:
    result = action (view)
  except:
    tb = ''.join (traceback.format_exc ())
    msg = ''.join (traceback.format_exception_only (sys.exc_type, sys.exc_value))
    notices.log_fontforge_warning (tb.replace('%', '%%'))
    notices.post_fontforge_error ('Unhandled exception', msg.replace('%','%%'))
  return result

def call_python_enabled (enabled not None, view not None):
  # Take advantage of the similarity of ‘action’ and ‘enabled’
  # functions.
  return call_python_action ((lambda v: not not (enabled (v))), view)

#--------------------------------------------------------------------------
