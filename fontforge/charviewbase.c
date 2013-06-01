#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <baseviews.h>
#include <splinefont.h>

CharViewBase
minimalist_CharViewBase (SplineChar *sc)
{
  // The minimum necessary for use as a glyph-view in Guile: a
  // CharViewBase with the ‘editable’ layer set to the font’s current
  // active layer.

  CharViewBase cvb = {
    .tag = FF_GLYPH_WINDOW,
    .next = NULL,
    .fv = sc->parent->fv,
    .sc = sc,
    .layerheads = {
      [dm_grid] = &sc->parent->grid,
      [dm_back] = &sc->layers[sc->parent->fv->active_layer],
      [dm_fore] = &sc->layers[1]
    },
    .drawmode = dm_back,
    .ft_gridfitwidth = 0,
    .gridfit = NULL,
    .container = NULL
  };
  return cvb;
}
