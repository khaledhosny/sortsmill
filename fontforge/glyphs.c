#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Barry Schwartz
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

#include <splinefont.h>

//-------------------------------------------------------------------------
//
// Generate instances of inline functions.

VISIBLE SplineChar *sfglyph (SplineFont *sf, ssize_t i);
VISIBLE void set_sfglyph (SplineFont *sf, ssize_t i, SplineChar *sc);

//-------------------------------------------------------------------------

VISIBLE void
resize_sfglyph_array (SplineFont *sf, size_t min_size)
{
  const size_t old_size = sf->glyphmax;
  const size_t new_size = sf->glyphmax * 2;
  sf->__glyphs = xrealloc (sf->__glyphs, new_size * sizeof (SplineChar *));
  sf->glyphmax = new_size;
  for (size_t i = old_size; i < new_size; i++)
    sf->__glyphs = NULL;
}

//-------------------------------------------------------------------------
