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

import sortsmill.ffcompat as fontforge

#--------------------------------------------------------------------------

def point_list_to_contour(point_list, is_closed, is_quadratic):
    c = fontforge.contour()
    c.is_quadratic = is_quadratic
    c.closed = is_closed
    for p in point_list:
        c += p
    return c

def make_pin(glyph):
    glyph.preserveLayerAsUndo()
    new_layer = fontforge.layer()
    layer = glyph.layers[glyph.activeLayer]
    for contour in layer:
        points = list(contour)
        for i in range(0, len(points)):
            if points[i].on_curve and points[i].selected:
                if i == 0 and contour.closed and not points[-1].on_curve:
                    points[-1].x = points[0].x
                    points[-1].y = points[0].y
                elif i == len(points) - 1 and contour.closed and not points[0].on_curve:
                    points[0].x = points[-1].x
                    points[0].y = points[-1].y
                if i != len(points) - 1 and not points[i + 1].on_curve:
                    points[i + 1].x = points[i].x
                    points[i + 1].y = points[i].y
                if i != 0 and not points[i - 1].on_curve:
                    points[i - 1].x = points[i].x
                    points[i - 1].y = points[i].y
        c = point_list_to_contour(points, contour.closed, contour.is_quadratic)
        new_layer += c
    glyph.layers[glyph.activeLayer] = new_layer

def points_are_selected(glyph):
    layer = glyph.layers[glyph.activeLayer]
    if layer.is_quadratic:
        return False         # Quadratics are not currently supported.
    for contour in layer:
        for point in contour:
            if point.selected:
                return True
    return False

fontforge.registerMenuItem((lambda _, glyph: make_pin(glyph)),
                           (lambda _, glyph: points_are_selected(glyph)),
                           None, "Glyph", "None",
                           "Turn points into pins")

#--------------------------------------------------------------------------

def reautohint(f):
    for g in f.glyphs():
        if not g.manualHints:
            g.autoHint()

fontforge.registerMenuItem((lambda _, glyph: glyph.autoHint()),
                           (lambda _, glyph: not glyph.manualHints),
                           None, "Glyph", "None",
                           "Autohint")

fontforge.registerMenuItem((lambda _, font: reautohint(font)),
                           None, None, "Font", "None",
                           "Re-autohint autohintable glyphs")

#--------------------------------------------------------------------------
