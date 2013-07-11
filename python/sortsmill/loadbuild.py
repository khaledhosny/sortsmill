# Copyright (C) 2009, 2010, 2011, 2013 Khaled Hosny and Barry Schwartz
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

import fontforge
import os
import spacing_by_anchors
import tools
from os.path import exists
from imp import reload

def build_file_exists(font):
    build_file_name = font.fontname + "_build.py"
    return exists(build_file_name)

def load_build_by_fontname(font, fontname):
    module_name = fontname + "_build"
    module = __import__(module_name)
    reload(module)
    module.build_glyphs(None, font)

def load_build(font):
    load_build_by_fontname(font, font.fontname)

def load_build_thoroughly(font):
    spacing_by_anchors.clear_cached_data(font)
    load_build(font)
    load_build(font)
    load_build(font)
    tools.reautohint(font)

fontforge.registerMenuItem((lambda _, font: load_build(font)),
                           (lambda _, font: build_file_exists(font)),
                           None, "Font", "None", "Build glyphs")

fontforge.registerMenuItem((lambda _, font: load_build_thoroughly(font)),
                           (lambda _, font: build_file_exists(font)),
                           None, "Font", "None", "Thoroughly build glyphs")
