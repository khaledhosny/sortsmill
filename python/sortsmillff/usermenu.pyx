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

import sortsmillff.views as views

# FIXME:
# FIXME: Get rid of this dependency.
# FIXME:
from ffcompat import logWarning, postError

cdef extern from "stdbool.h": pass
from libcpp cimport bool

cimport sortsmillff.cython.usermenu as usermenu
cimport sortsmillff.cython.xgc as xgc
from cpython.ref cimport PyObject, Py_XINCREF
from cpython.object cimport PyObject_CallObject, PyObject_IsTrue
from libc.stdint cimport intptr_t

cdef extern from "baseviews.h":
  ctypedef struct FontViewBase:
    pass

cdef extern from "splinefont.h":
  ctypedef struct SplineChar:
    pass

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()
  void set_no_windowing_ui (bool)
  bool get_running_script ()
  void set_running_script (bool)

import sys
import warnings
import traceback

ctypedef struct __menu_item_data:
  int flag
  PyObject *menu_func
  PyObject *enable_func

def __menu_flag (window):
  message = "expected 'glyph' or 'font'"
  cdef int flag = 0
  glyph = ["glyph", "char"]
  font = ["font"]
  if isinstance (window, str):
    if window.lower () in glyph:
      flag = usermenu.menu_cv
    elif window.lower () in font:
      flag = usermenu.menu_fv
    else:
      raise ValueError (message)
  else:
    raise TypeError (message)
  return flag

cdef char **__submenu_names (object names):
  if isinstance (names, str):
    names = [names]
  cdef size_t length = len (names)
  cdef char **result = \
      <char **> xgc.x_gc_malloc ((length + 1) * sizeof (char *))
  cdef size_t i
  for i from 0 <= i < length:
    result[i] = xgc.x_gc_strdup (names[i])
  return result

cdef char *__shortcut_str (object shortcut_string):
  cdef char *result = NULL
  if shortcut_string is not None:
    result = xgc.x_gc_strdup (shortcut_string)
  return result

cdef object __ff_obj_to_py (int flag, void *ff_obj):
  assert (flag == usermenu.menu_cv or flag == usermenu.menu_fv)
  result = None
  if flag == usermenu.menu_cv:
    result = views.glyph_view (<intptr_t> ff_obj)
  elif flag == usermenu.menu_fv:
    result = views.font_view (<intptr_t> ff_obj)
  return result

def __call_python (func not None, ff_obj not None,
                   bint result_is_bool):
  try:
    result = func (ff_obj)
    if result_is_bool:
      result = not not result
  except:
    tb = ''.join (traceback.format_exc ())
    msg = ''.join (traceback.format_exception_only (sys.exc_type, sys.exc_value))
    logWarning (tb.replace('%', '%%'))
    postError('Unhandled exception', msg.replace('%','%%'))
  return result

cdef void __menu_info_func (void *data, void *ff_obj):
  cdef __menu_item_data *py_data = <__menu_item_data *> data
  obj = __ff_obj_to_py (py_data.flag, ff_obj)
  args = (<object> py_data.menu_func, obj, False)
  PyObject_CallObject (__call_python, args)

cdef int __menu_info_check (void *data, void *ff_obj):
  cdef bint enabled = True
  cdef __menu_item_data *py_data = <__menu_item_data *> data
  if py_data.enable_func != <PyObject *> None:
    obj = __ff_obj_to_py (py_data.flag, ff_obj)
    args = (<object> py_data.enable_func, obj, True)
    retval = PyObject_CallObject (__call_python, args)
    enabled = PyObject_IsTrue (retval)
  return enabled

cdef void __registerMenuItem (int flag,
                              object menu_function,
                              object enable_function,
                              object shortcut_string,
                              object submenu_names):
  assert not get_no_windowing_ui ()
  cdef __menu_item_data *py_data
  cdef char **menu_names
  cdef char *shortcut_str
  cdef usermenu.menu_info_data menu_data
  cdef void (*register_menu_item) (usermenu.menu_info_func,
                                   usermenu.menu_info_check,
                                   usermenu.menu_info_data, int,
                                   char *, char **)
  py_data = <__menu_item_data *> xgc.x_gc_malloc (sizeof (__menu_item_data))
  Py_XINCREF (<PyObject *> menu_function)
  Py_XINCREF (<PyObject *> enable_function)
  py_data.flag = flag
  py_data.menu_func = <PyObject *> menu_function
  py_data.enable_func = <PyObject *> enable_function
  menu_names = __submenu_names (submenu_names)
  menu_data = <usermenu.menu_info_data> py_data
  shortcut_str = __shortcut_str (shortcut_string)
  register_menu_item = <void (*) (usermenu.menu_info_func,
                                  usermenu.menu_info_check,
                                  usermenu.menu_info_data, int,
                                  char *, char **)> usermenu.RegisterMenuItem
  register_menu_item (__menu_info_func, __menu_info_check, menu_data,
                      flag, shortcut_str, menu_names)

def register_fontforge_menu_item (window not None,
                                  menu_path not None,
                                  action not None,
                                  enabled = lambda _: True,
                                  shortcut = None):
  cdef int flag
  if not get_no_windowing_ui ():
    flag = __menu_flag (window)
    if flag & usermenu.menu_cv != 0:
      __registerMenuItem (usermenu.menu_cv, action, enabled, shortcut, menu_path)
    if flag & usermenu.menu_fv != 0:
      __registerMenuItem (usermenu.menu_fv, action, enabled, shortcut, menu_path)
