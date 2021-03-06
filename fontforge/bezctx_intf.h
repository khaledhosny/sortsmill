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

#ifndef  _FF_INTERNAL_BEZCTX_INTF_H
#define _FF_INTERNAL_BEZCTX_INTF_H

typedef struct _bezctx bezctx;

bezctx *new_bezctx (void);

void bezctx_moveto (bezctx * bc, double x, double y, int is_open);

void bezctx_lineto (bezctx * bc, double x, double y);

void bezctx_quadto (bezctx * bc, double x1, double y1, double x2, double y2);

void bezctx_curveto (bezctx * bc, double x1, double y1, double x2, double y2, double x3, double y3);

void bezctx_mark_knot (bezctx * bc, int knot_idx);

#endif //  _FF_INTERNAL_BEZCTX_INTF_H
