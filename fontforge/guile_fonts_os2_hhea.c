#include <config.h>             // -*- coding: utf-8 -*-

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

#include <guile_fonts_os2_hhea.h>

// Supporting functions for OS/2 and hhea table modules.

//-------------------------------------------------------------------------

static SCM
stored_considering_offsets (bool stored_is_offset, SCM value,
                            SCM given_is_offset, SCM base)
{
  SCM result;
  if (scm_is_true (given_is_offset))
    {
      if (stored_is_offset)
        result = value;
      else
        result = scm_sum (value, base);
    }
  else
    {
      if (stored_is_offset)
        result = scm_difference (value, base);
      else
        result = value;
    }
  return result;
}

int
stored_int_considering_offsets (bool stored_is_offset, SCM value,
                                SCM given_is_offset, SCM base,
                                SCM rounding_function (SCM))
{
  SCM v = stored_considering_offsets (stored_is_offset, value,
                                      given_is_offset, base);
  return scm_to_int (scm_inexact_to_exact (rounding_function (v)));
}

SCM
value_considering_offsets (SCM result_is_offset, SCM value,
                           bool stored_is_offset, SCM base)
{
  SCM result;
  if (scm_is_true (result_is_offset))
    {
      if (stored_is_offset)
        result = value;
      else
        result = scm_difference (value, base);
    }
  else
    {
      if (stored_is_offset)
        result = scm_sum (value, base);
      else
        result = value;
    }
  return result;
}

static DBounds
sf_bounding_box (SplineFont *sf)
{
  DBounds bbox;
  SplineFontFindBounds (sf, &bbox);
  return bbox;
}

double
sf_ymax (SplineFont *sf)
{
  return sf_bounding_box (sf).maxy;
}

double
sf_ymin (SplineFont *sf)
{
  return sf_bounding_box (sf).miny;
}

//-------------------------------------------------------------------------
