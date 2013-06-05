// -*- coding: utf-8 -*-

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

#ifndef _GUILE_FONTS_OS2_HHEA_H
#define _GUILE_FONTS_OS2_HHEA_H

#include <stdbool.h>
#include <libguile.h>
#include <splinefont.h>

// Supporting functions for OS/2 and hhea table modules.

int stored_int_considering_offsets (bool stored_is_offset, SCM value,
                                    SCM given_is_offset, SCM base,
                                    SCM rounding_function (SCM));
SCM value_considering_offsets (SCM result_is_offset, SCM value,
                               bool stored_is_offset, SCM base);
double sf_ymax (SplineFont *sf);
double sf_ymin (SplineFont *sf);

#endif  // _GUILE_FONTS_OS2_HHEA_H
