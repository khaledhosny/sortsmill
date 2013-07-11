# -*- coding: utf-8 -*-

# Copyright (C) 2009, 2013 Khaled Hosny and Barry Schwartz
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

def cap_spacing(font, caps_names, expansion):

    advance_widths = {}
    for n in caps_names:
        if n in font:
            w = font[n].width
            if w in advance_widths:
                advance_widths[w].append(n)
            else:
                advance_widths[w] = [n]

    rules = ''
    for w in sorted(advance_widths):
        advance_increase = int(round(expansion * w))
        pos = int(advance_increase / 2)
        glyphs = advance_widths[w]
        rules += '  pos '
        if len(glyphs) == 1:
            rules += '\\' + glyphs[0]
        else:
            rules += '[ '
            for g in glyphs:
                rules += '\\' + g + ' '
            rules += ']'
        rules += ' <' + str(pos) + ' 0 ' + str(advance_increase) + ' 0>;\n'

    return rules
