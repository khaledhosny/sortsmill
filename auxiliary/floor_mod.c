#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/math/floor_mod.h>
#include <math.h>

// Like fmod() except rounding the quotient @code{a / b} in the
// negative direction instead of towards zero.
//
// The semantics of floor_mod() are similar to those of the ‘mod’ or
// ‘flmod’ operations in R⁶RS Scheme.
VISIBLE double
floor_mod (double a, double b)
{
  return a - b * (floor (a / b));
}
