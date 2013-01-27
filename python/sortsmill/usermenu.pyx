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

cdef extern from 'config.h':
  pass

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

cimport sortsmill.cython.usermenu as c_usermenu
cimport sortsmill.cython.guile as scm
from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.xgc as xgc
from sortsmill.cython.const_pointers cimport const_char_ptr, const_char_ptr_ptr
from cpython.ref cimport PyObject, Py_XINCREF, Py_XDECREF
from cpython.object cimport PyObject_IsTrue
from libc.stdint cimport uintptr_t

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()

from . import (views, notices)
import sys
import traceback

cdef bint __scm_is_font_view (SCM obj):
  cdef SCM font_view_p = scm.scm_c_private_ref ("sortsmill usermenu", "font_view_p__")
  return scm.scm_to_bool (scm.scm_call_1 (font_view_p, obj))

cdef bint __scm_is_glyph_view (SCM obj):
  cdef SCM glyph_view_p = scm.scm_c_private_ref ("sortsmill usermenu", "glyph_view_p__")
  return scm.scm_to_bool (scm.scm_call_1 (glyph_view_p, obj))

cdef void *__scm_c_font_view_to_pointer (SCM obj):
  cdef SCM font_view_to_pointer = scm.scm_c_private_ref ("sortsmill usermenu", "font_view_to_pointer__")
  return scm.scm_to_pointer (scm.scm_call_1 (font_view_to_pointer, obj))

cdef void *__scm_c_glyph_view_to_pointer (SCM obj):
  cdef SCM glyph_view_to_pointer = scm.scm_c_private_ref ("sortsmill usermenu", "glyph_view_to_pointer__")
  return scm.scm_to_pointer (scm.scm_call_1 (glyph_view_to_pointer, obj))

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

cdef object __scm_view_to_py_view (SCM scm_view):
  cdef uintptr_t view_addr
  if __scm_is_font_view (scm_view):
    view_addr = <uintptr_t> __scm_c_font_view_to_pointer (scm_view)
    py_view = views.font_view (view_addr)
  elif __scm_is_glyph_view (scm_view):
    view_addr = <uintptr_t> __scm_c_glyph_view_to_pointer (scm_view)
    py_view = views.glyph_view (view_addr)
  else:
    raise ValueError ('expected a Guile font view or glyph view')
  return py_view

cdef SCM __scm_do_action (SCM scm_view, SCM py_action):
  py_view = __scm_view_to_py_view (scm_view)
  action = <object> <PyObject *> scm.scm_to_pointer (py_action)
  retval = call_python_action (action, py_view)
  cdef char *unspecified = "*unspecified*"
  return scm.scm_eval_string (scm.scm_from_latin1_string (unspecified))

cdef SCM __scm_check_enabled (SCM scm_view, SCM py_enabled):
  py_view = __scm_view_to_py_view (scm_view)
  enabled = <object> <PyObject *> scm.scm_to_pointer (py_enabled)
  retval = call_python_enabled (enabled, py_view)
  cdef bint is_enabled = PyObject_IsTrue (retval) 
  return scm.scm_from_bool (is_enabled)

cdef SCM __cython_action_func = scm.scm_c_make_gsubr ("ff-python-do-action", 2, 0, 0, <void *> __scm_do_action)
scm.scm_permanent_object (__cython_action_func)
cdef SCM __cython_enabled_func = scm.scm_c_make_gsubr ("ff-python-check-enabled", 2, 0, 0, <void *> __scm_check_enabled)
scm.scm_permanent_object (__cython_enabled_func)

cdef SCM __create_action_closure (object py_action):
  cdef SCM closure_maker = scm.scm_c_private_ref ("sortsmill usermenu", "closure_maker__")
  cdef SCM py_func = scm.scm_from_pointer (<PyObject *> py_action, NULL)
  return scm.scm_call_2 (closure_maker, __cython_action_func, py_func)

cdef SCM __create_enabled_closure (object py_enabled):
  cdef SCM closure_maker = scm.scm_c_private_ref ("sortsmill usermenu", "closure_maker__")
  cdef SCM py_func = scm.scm_from_pointer (<PyObject *> py_enabled, NULL)
  return scm.scm_call_2 (closure_maker, __cython_enabled_func, py_func)

cdef void __registerMenuItem (int c_window,
                              object action_function,
                              object enable_function,
                              object shortcut,
                              object menu_path):
  assert not get_no_windowing_ui ()

  Py_XINCREF (<PyObject *> action_function)
  Py_XINCREF (<PyObject *> enable_function)
  cdef char **c_menu_path = __c_menu_path (menu_path)
  cdef char *c_shortcut = __c_shortcut (shortcut)
  cdef SCM scm_action = __create_action_closure (action_function)
  cdef SCM scm_enabled = __create_enabled_closure (enable_function)

  scm.scm_gc_protect_object (scm_action)
  scm.scm_gc_protect_object (scm_enabled)

  c_usermenu.register_fontforge_menu_entry (c_window,
                                            <const_char_ptr_ptr> c_menu_path,
                                            scm_action, scm_enabled,                                            
                                            <const_char_ptr> c_shortcut)

#--------------------------------------------------------------------------

def __default_enabled (view):
  return True

def register_fontforge_menu_entry (window not None,
                                   menu_path not None,
                                   action not None,
                                   enabled = None,
                                   shortcut = None):
  if enabled is None:
    enabled = __default_enabled
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
  def enabled_action (v):
    return not not enabled (v)
  return call_python_action (enabled_action, view)

#--------------------------------------------------------------------------
