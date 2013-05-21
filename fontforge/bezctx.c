#include <config.h>

// ppedit - A pattern plate editor for Spiro splines.
// Copyright (C) 2007 Raph Levien
//
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

#include "bezctx.h"

void
bezctx_moveto (bezctx *bc, double x, double y, int is_open)
{
  bc->moveto (bc, x, y, is_open);
}

void
bezctx_lineto (bezctx *bc, double x, double y)
{
  bc->lineto (bc, x, y);
}

void
bezctx_quadto (bezctx *bc, double x1, double y1, double x2, double y2)
{
  bc->quadto (bc, x1, y1, x2, y2);
}

void
bezctx_curveto (bezctx *bc, double x1, double y1, double x2, double y2, double x3, double y3)
{
  bc->curveto (bc, x1, y1, x2, y2, x3, y3);
}

void
bezctx_mark_knot (bezctx *bc, int knot_idx)
{
  if (bc->mark_knot)
    bc->mark_knot (bc, knot_idx);
}
