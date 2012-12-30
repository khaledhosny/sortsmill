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

include 'sortsmillff/cython/config.pxi'

cdef extern from "config.h":
  pass

cdef extern from "stdbool.h":
  pass
from libcpp cimport bool

cimport sortsmillff.cython.xgc as xgc
from cpython.ref cimport PyObject, Py_XINCREF
from cpython.object cimport PyObject_CallObject, PyObject_IsTrue
from libc.stdint cimport uintptr_t

cdef extern from "baseviews.h":
  ctypedef struct CharViewBase:
    SplineChar *sc
  ctypedef struct FontViewBase:
    pass
  int CVLayer (CharViewBase *cv)

cdef extern from "splinefont.h":
  ctypedef struct SplineChar:
    pass
  cdef enum layer_type:
    ly_all = -2
    ly_grid = -1
    ly_back = 0
    ly_fore = 1
    ly_none = -3
  int hasspiro ()

cdef extern from "activeinui.h":
  FontViewBase *fv_active_in_ui
  CharViewBase *cv_active_in_ui
  SplineChar *sc_active_in_ui   # FIXME: Do we need this anymore?
  int layer_active_in_ui

cdef extern from "ffpython.h":
  object PyFV_From_FV_I (FontViewBase *)
  object PySC_From_CV_I (CharViewBase *)
  FontViewBase *FV_From_PyFV (object)
  CharViewBase *CV_From_PySC (object)

cdef extern from "fontforge.h":
  bool get_no_windowing_ui ()
  void set_no_windowing_ui (bool)
  bool get_running_script ()
  void set_running_script (bool)

import sys
import warnings
import traceback
from . import (fontforge_api, notices)

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
  spiroOpen,
  askString,
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

def logWarning (msg):
  notices.log_fontforge_warning (msg)

def postNotice (win_title, msg):
  notices.post_fontforge_notice (win_title, msg)

def postError (win_title, msg):
  notices.post_fontforge_error (win_title, msg)

#--------------------------------------------------------------------------
#
# Implementation of registerMenuItem in terms of
# usermenu.register_fontforge_menu_entry.

IF HAVE_GUI:
  from sortsmillff.usermenu import register_fontforge_menu_entry

  def __default_enabled (view):
    return True

  def __call_with_glyph_globals_set (func, legacy_obj):

    cdef CharViewBase *cvb = CV_From_PySC (legacy_obj)
#    cdef uintptr_t cvb_uint = <uintptr_t> <void *> cvb
#    py_cvb = fontforge_api.CharViewBase (cvb_uint)
#    cdef SplineChar *sc = <SplineChar *> py_cvb._sc
    cdef SplineChar *sc = cvb.sc

    cv_active_in_ui = cvb
    sc_active_in_ui = sc
    layer_active_in_ui = CVLayer (cvb)

    result = func (legacy_obj)

    cv_active_in_ui = NULL
    sc_active_in_ui = NULL
    layer_active_in_ui = ly_fore

    return result

  def __call_with_font_globals_set (func, legacy_obj):

    cdef FontViewBase *fvb = FV_From_PyFV (legacy_obj)
    cdef uintptr_t fvb_uint = <uintptr_t> <void *> fvb
    py_fvb = fontforge_api.FontViewBase (fvb_uint)
    cdef int active_layer = py_fvb._active_layer

    fv_active_in_ui = fvb
    layer_active_in_ui = active_layer

    result = func (legacy_obj)

    fv_active_in_ui = NULL

    return result

  def registerMenuItem (menu_function not None,
                        enable_function,
                        data,
                        which_window not None,
                        shortcut_string,
                        *submenu_names):

    def canonical_window (window):
      w = window.lower ()
      if w == 'char':
        w = 'glyph'
      if w not in ('glyph', 'font'):
        raise ValueError ("Expected 'glyph' or 'font', but got " + str (w))
      return w

    def convert_glyph_view (gv):
      cdef uintptr_t p = gv.to_internal_type().ptr
      cdef CharViewBase *cvb = <CharViewBase *> p
      return PySC_From_CV_I (cvb)

    def convert_font_view (fv):
      cdef uintptr_t p = fv.to_internal_type().ptr
      cdef FontViewBase *fvb = <FontViewBase *> p
      return PyFV_From_FV_I (fvb)

    if isinstance (which_window, str):
      which_window = (which_window,)
    windows = {canonical_window (w) for w in which_window}

    for w in windows:

      convert = convert_font_view
      call_with_globals_set = __call_with_font_globals_set
      if w == 'glyph':
        convert = convert_glyph_view
        call_with_globals_set = __call_with_glyph_globals_set
      Py_XINCREF (<PyObject *> convert)
      Py_XINCREF (<PyObject *> call_with_globals_set)

      action_inner = lambda obj: menu_function (data, convert (obj))
      Py_XINCREF (<PyObject *> action_inner)

      enabled_inner = __default_enabled
      if enable_function is not None:
        enabled_inner = lambda obj: enable_function (data, convert (obj))
      Py_XINCREF (<PyObject *> enabled_inner)

      action = lambda obj: call_with_globals_set (action_inner, obj)
      enabled = lambda obj: call_with_globals_set (enabled_inner, obj)
      Py_XINCREF (<PyObject *> action)
      Py_XINCREF (<PyObject *> enabled)

      register_fontforge_menu_entry (w, submenu_names, action, enabled,
                                     shortcut_string)

IF not HAVE_GUI:
  def registerMenuItem (menu_function not None,
                        enable_function,
                        data,
                        which_window not None,
                        shortcut_string,
                        *submenu_names):
    pass

#--------------------------------------------------------------------------
