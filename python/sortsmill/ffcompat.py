# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 by Barry Schwartz
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

import warnings
import traceback
from . import (pkg_info, guile, notices)

from sortsmill.legacy.fontforge import (
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

__no_windowing_ui_ref = guile.public_ref ('sortsmill ffcompat',
                                          'no_windowing_ui-ref')
__no_windowing_ui_set = guile.public_ref ('sortsmill ffcompat',
                                          'no_windowing_ui-set!')
__running_script_ref = guile.public_ref ('sortsmill ffcompat',
                                         'running_script-ref')
__running_script_set = guile.public_ref ('sortsmill ffcompat',
                                         'running_script-set!')

#--------------------------------------------------------------------------

def __get_no_windowing_ui ():
  guile.pyguile_to_bool (guile.call (__no_windowing_ui_ref))

def __set_no_windowing_ui (setting):
  assert isinstance (setting, bool)
  guile.call (__no_windowing_ui_set, guile.bool_to_pyguile (setting))

def __get_running_script ():
  guile.pyguile_to_bool (guile.call (__running_script_ref))

def __set_running_script (setting):
  assert isinstance (setting, bool)
  guile.call (__running_script_set, guile.bool_to_pyguile (setting))

def __windowing_ui (setting):
  old_setting = not __get_no_windowing_ui ()
  if setting is not None:
    __set_no_windowing_ui (not setting)
  return old_setting

def __running_script (setting):
  old_setting = __get_running_script ()
  if setting is not None:
    __set_running_script (setting)
  return old_setting

#--------------------------------------------------------------------------

__version__ = pkg_info.version

def version ():
  warnings.warn ('version() is deprecated; use __version__ instead.',
                 DeprecationWarning)
  # A workaround: return a big value so, in old scripts, checking for
  # a minimal version always succeeds.
  return '99999999'

def loadPlugin (filename):
  warnings.warn ('loadPlugin() is deprecated in {} and does nothing.'
                 .format (pkg_info.package_name),
                 DeprecationWarning)

def loadPluginDir (dirname):
  warnings.warn ('loadPluginDir() is deprecated in {} and does nothing.'
                 .format (pkg_info.package_name),
                 DeprecationWarning)

def hasUserInterface ():
  return not __get_no_windowing_ui ()

def hasSpiro ():
  return True

#--------------------------------------------------------------------------

def logWarning (msg):
  notices.log_fontforge_warning (msg)

def postNotice (win_title, msg):
  notices.post_fontforge_notice (win_title, msg)

def postError (win_title, msg):
  notices.post_fontforge_error (win_title, msg)

#--------------------------------------------------------------------------

def registerMenuItem (menu_function, enable_function, data,
                      which_window, shortcut_string, *submenu_names):
  assert menu_function is not None
  assert which_window is not None
  if pkg_info.have_gui and not __get_no_windowing_ui ():
    __registerMenuItem = guile.private_ref ('sortsmill usermenu python',
                                            'registerMenuItem')
    if isinstance (which_window, str):
      windows = (which_window,)
    else:
      windows = tuple (which_window)
    menu_path = tuple (submenu_names)
    action = menu_function
    enabled = enable_function
    shortcut = shortcut_string
    guile.call (__registerMenuItem,
                guile.pyobject (action),
                guile.pyobject (enabled),
                guile.pyobject (data),
                guile.pyobject (windows),
                guile.pyobject (shortcut),
                guile.pyobject (menu_path))

#--------------------------------------------------------------------------
