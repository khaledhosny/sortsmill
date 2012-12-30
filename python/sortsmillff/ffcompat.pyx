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
  SplineChar *sc_active_in_ui
  int layer_active_in_ui

cdef extern from "ffpython.h":
  object PyFV_From_FV_I (FontViewBase *)
  object PySC_From_SC_I (SplineChar *)

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

    def call_with_globals_set_for_glyph (func, data, gv):
      cdef uintptr_t cvb = gv.to_internal_type().ptr
      cdef uintptr_t sc = fontforge_api.CharViewBase (cvb)._sc

      # Set globals that are used in the legacy python.c code.
      sc_active_in_ui = <SplineChar *> sc
      layer_active_in_ui = CVLayer (<CharViewBase *> cvb)

      legacy_glyph_obj = PySC_From_SC_I (<SplineChar *> sc)
      result = func (data, legacy_glyph_obj)

      # We are done with those globals.
      sc_active_in_ui = NULL
      layer_active_in_ui = ly_fore

      return result

    def call_with_globals_set_for_font (func, data, fv):
      cdef uintptr_t fvb = fv.to_internal_type().ptr
      cdef int active_layer = fontforge_api.FontViewBase (fvb)._active_layer

      # Set globals that are used in the legacy python.c code.
      fv_active_in_ui = <FontViewBase *> fvb
      layer_active_in_ui = active_layer

      legacy_font_obj = PyFV_From_FV_I (<FontViewBase *> fvb)
      result = func (data, legacy_font_obj)

      # We are done with those globals.
      fv_active_in_ui = NULL

      return result

    if isinstance (which_window, str):
      which_window = (which_window,)
    windows = {canonical_window (w) for w in which_window}

    if enable_function is None:
      enable_function = __default_enabled

    for w in windows:

      call_with_globals_set = call_with_globals_set_for_font
      if w == 'glyph':
        call_with_globals_set = call_with_globals_set_for_glyph

      action = lambda obj: call_with_globals_set (menu_function, data, obj)
      Py_XINCREF (<PyObject *> action)
      enabled = lambda obj: call_with_globals_set (enable_function, data, obj)
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
