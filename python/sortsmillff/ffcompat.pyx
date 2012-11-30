# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 by Barry Schwartz
# Based in part on python.c by George Williams, which is
#   Copyright (C) 2007-2012 by George Williams
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# The name of the author may not be used to endorse or promote products
# derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
# WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
# EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
# OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
# ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

include 'config.pxi'

cdef extern from "config.h": pass

cdef extern from "stdbool.h": pass
from libcpp cimport bool

cimport usermenu
cimport xgc
from cpython.ref cimport PyObject, Py_XINCREF
from cpython.object cimport PyObject_CallObject, PyObject_IsTrue

cdef extern from "baseviews.h":
  ctypedef struct FontViewBase:
    pass

cdef extern from "splinefont.h":
  ctypedef struct SplineChar:
    pass
  int hasspiro ()

cdef extern from "ffpython.h":
  PyObject *PyFV_From_FV (FontViewBase *fv)
  PyObject *PyFV_From_FV_I (FontViewBase *fv)
  PyObject *PySC_From_SC (SplineChar *sc)
  PyObject *PySC_From_SC_I (SplineChar *sc)

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()
  void set_no_windowing_ui (bool)

cdef extern from "scripting.h":
  bool get_running_script ()
  void set_running_script (bool)

import sys
import warnings
import traceback

from sortsmillff.legacy.fontforge import (
  layer,
  activeGlyph,
  point,
  preloadCidmap,
  hooks,
  askChoices,
  mathkern,
  private,
  spiroRight,
  getPrefs,
  selection,
  loadEncodingFile,
  setPrefs,
  glyphPen,
  open,
  contour,
  glyph,
  parseTTInstrs,
  activeFontInUI,
  savePrefs,
  activeFont,
  glyphlayerarray,
  saveFilename,
  unParseTTInstrs,
  layerinfo,
  nameFromUnicode,
  defaultOtherSubrs,
  awcontext,
  math,
  unitShape,
  fontlayerarray,
  activeLayer,
  glyphlayerrefarray,
  contouriter,
  postError,
  spiroOpen,
  askString,
  logWarning,
  printSetup,
  loadPrefs,
  layeriter,
  registerImportExport,
  registerGlyphSeparationHook,
  ask,
  loadNamelist,
  fontsInFile,
  unicodeFromName,
  cvtiter,
  glyphlayeriter,
  awglyphIndex,
  spiroCorner,
  awglyph,
  font,
  cvt,
  fonts,
  fontiter,
  readOtherSubrsFile,
  privateiter,
  openFilename,
  postNotice,
  loadNamelistDir,
  spiroG4,
  spiroG2,
  fontlayeriter,
  spiroLeft,
  )

#--------------------------------------------------------------------------

def __windowing_ui (bool setting):
  cdef bool old_setting = not get_no_windowing_ui ()
  if setting is not None:
    set_no_windowing_ui (not setting)
  return old_setting

def __running_script (bool setting):
  cdef bool old_setting = get_running_script ()
  if setting is not None:
    set_running_script (setting)
  return old_setting

#--------------------------------------------------------------------------

__version__ = FF_MODULE_VERSION

def version ():
  warnings.warn ('version() is deprecated; use __version__ instead.',
                 DeprecationWarning)

  # A workaround: return a big value so, in old scripts, checking for
  # a minimal version always succeeds.
  return '99999999'

def loadPlugin (filename):
  warnings.warn ('loadPlugin() is deprecated in {} and does nothing.'
                 .format (PACKAGE_NAME),
                 DeprecationWarning)

def loadPluginDir (dirname):
  warnings.warn ('loadPluginDir() is deprecated in {} and does nothing.'
                 .format (PACKAGE_NAME),
                 DeprecationWarning)

def hasUserInterface ():
  return not get_no_windowing_ui ()

def hasSpiro ():
  return <bool> hasspiro ()

#--------------------------------------------------------------------------
#
# Implementation of registerMenuItem.

IF HAVE_GUI:

  ctypedef struct __menu_item_data:
    int flag
    PyObject *menu_func
    PyObject *enable_func
    PyObject *data

  def __menu_flags (which_window):
    message = 'expected "Glyph" and/or "Font"'
    cdef int flags = 0
    glyph = ["glyph", "char"]
    font = ["font"]
    if isinstance (which_window, str):
      if which_window.lower () in glyph:
        flags |= usermenu.menu_cv
      elif which_window.lower () in font:
        flags |= usermenu.menu_fv
      else:
        raise ValueError (message)
    else:
      try:
        for s in which_window:
          pass
      except:
        raise ValueError (message)
      for s in which_window:
        if s.lower () in glyph:
          flags |= usermenu.menu_cv
        elif s.lower () in font:
          flags |= usermenu.menu_fv
        else:
          raise ValueError (message)
    return flags

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

  cdef PyObject *__ff_obj_to_py (int flag, void *ff_obj):
    assert (flag == usermenu.menu_cv or flag == usermenu.menu_fv)
    cdef PyObject *result = NULL
    if flag == usermenu.menu_cv:
      result = PySC_From_SC_I (<SplineChar *> ff_obj)
    elif flag == usermenu.menu_fv:
      result = PyFV_From_FV_I (<FontViewBase *> ff_obj)
    return result

  def __call_python (func not None, data, ff_obj not None,
                     bool result_is_bool):
    try:
      result = func (data, ff_obj)
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
    cdef PyObject *obj = __ff_obj_to_py (py_data.flag, ff_obj)
    args = (<object> py_data.menu_func, <object> py_data.data,
            <object> obj, False)
    PyObject_CallObject (__call_python, args)

  cdef int __menu_info_check (void *data, void *ff_obj):
    cdef bool enabled = True
    cdef __menu_item_data *py_data = <__menu_item_data *> data
    cdef PyObject *obj
    if py_data.enable_func != <PyObject *> None:
      obj = __ff_obj_to_py (py_data.flag, ff_obj)
      args = (<object> py_data.enable_func, <object> py_data.data,
              <object> obj, True)
      retval = PyObject_CallObject (__call_python, args)
      enabled = PyObject_IsTrue (retval)
    return enabled

  cdef void __registerMenuItem (int flag,
                                object menu_function,
                                object enable_function,
                                object data,
                                object shortcut_string,
                                object submenu_names):
    assert hasUserInterface ()
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
    Py_XINCREF (<PyObject *> data)
    py_data.flag = flag
    py_data.menu_func = <PyObject *> menu_function
    py_data.enable_func = <PyObject *> enable_function
    py_data.data = <PyObject *> data
    menu_names = __submenu_names (submenu_names)
    menu_data = <usermenu.menu_info_data> py_data
    shortcut_str = __shortcut_str (shortcut_string)
    register_menu_item = <void (*) (usermenu.menu_info_func,
                                    usermenu.menu_info_check,
                                    usermenu.menu_info_data, int,
                                    char *, char **)> usermenu.RegisterMenuItem
    register_menu_item (__menu_info_func, __menu_info_check, menu_data,
                        flag, shortcut_str, menu_names)

  def registerMenuItem (menu_function not None,
                        enable_function,
                        data,
                        which_window not None,
                        shortcut_string,
                        submenu_names not None):
    if isinstance (shortcut_string, str):
      warnings.warn ('registerMenuItem with "None" as shortcut_string is deprecated; use the None object instead.')
      shortcut_str = None
    cdef int flags
    if hasUserInterface ():
      flags = __menu_flags (which_window)
      if flags & usermenu.menu_cv != 0:
        __registerMenuItem (usermenu.menu_cv, menu_function, enable_function,
                            data, shortcut_string, submenu_names)
      if flags & usermenu.menu_fv != 0:
        __registerMenuItem (usermenu.menu_fv, menu_function, enable_function,
                            data, shortcut_string, submenu_names)

IF not HAVE_GUI:
  def registerMenuItem (menu_function not None,
                        enable_function,
                        data,
                        which_window not None,
                        shortcut_string,
                        submenu_names not None):
    pass

#--------------------------------------------------------------------------
