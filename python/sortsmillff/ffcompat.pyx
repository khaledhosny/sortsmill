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

import warnings

from sortsmillff.legacy.fontforge import (
    layer,
    hasSpiro,
    activeGlyph,
    point,
    preloadCidmap,
    hooks,
    askChoices,
    mathkern,
    private,
    loadPlugin,
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
#    version,
    layerinfo,
    nameFromUnicode,
    defaultOtherSubrs,
    awcontext,
    math,
    registerMenuItem,
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
    loadPluginDir,
    cvt,
    fonts,
    fontiter,
    hasUserInterface,
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

__version__ = '{}.{}.{}{}'.format (VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_EXTRA_SHORT)

def version ():
  warnings.warn ('version() is deprecated; use __version__ instead.',
                 DeprecationWarning)

  # A workaround: return a big value so, in old scripts, checking for
  # a minimal version always succeeds.
  return '99999999'

