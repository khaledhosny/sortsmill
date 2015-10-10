# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

# Based in part on python.c by George Williams, for which copyright
# and license are as follows:
#
# Copyright (C) 2007-2012 by George Williams
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
from . import (pkg_info, guile, notices, conditions)

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

def __force_unicode (s):
  if isinstance (s, bytes):
    s = s.decode ('UTF-8')
  return s

# FIXME: Centralize this information.
__window_cases = ('font', 'glyph', 'char')

def __acceptable_as_window (w):
  global __window_cases
  if isinstance (w, unicode) or isinstance (w, bytes):
    w = (w,)
  try:
    acceptable = all ([(isinstance (__force_unicode (s), unicode)
                        and __force_unicode (s).lower () in __window_cases)
                       for s in w])
  except:
    acceptable = False
  return acceptable

def __acceptable_as_menu_path (mp):
  try:
    acceptable = all ([isinstance (__force_unicode (s), unicode) for s in mp])
  except:
    acceptable = False
  return acceptable

def __registerMenuItem_preconditions (menu_function, enable_function, data,
                                      which_window, shortcut_string,
                                      *submenu_names):
  result = True
  if not callable (menu_function):
    result = '‘menu_function’ is not callable: {!r}'.format (menu_function)
  elif enable_function is not None and not callable (enable_function):
    result = '‘enable_function’ is not callable: {!r}'.format (enable_function)
  elif not __acceptable_as_window (which_window):
    result = 'invalid ‘which_window’: {!r}'.format (which_window)
  elif shortcut_string is not None and shortcut_string != 'None' \
        and not isinstance (__force_unicode (shortcut_string), unicode):
    result = '‘shortcut_string’ is not a string: {!r}'.format (shortcut_string)
  elif len (submenu_names) == 0:
    result = 'no menu entry name was specified'
  elif not __acceptable_as_menu_path (submenu_names):
    result = 'expected strings for the submenu and menu entry names, but got: {!r}'.format (submenu_names)
  return result

@conditions.pre (__registerMenuItem_preconditions)
def registerMenuItem (menu_function, enable_function, data,
                      which_window, shortcut_string, *submenu_names):
  assert which_window is not None
  if pkg_info.have_gui and not __get_no_windowing_ui ():
    __registerMenuItem = guile.public_ref ('sortsmill usermenu python',
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
