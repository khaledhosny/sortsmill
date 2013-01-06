/*
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 3
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.
*/

#ifndef _FF_INTERNAL_SPIRO_H
#define _FF_INTERNAL_SPIRO_H

typedef struct
{
  double x;
  double y;
  char ty;
} spiro_cp;

typedef struct spiro_seg_s spiro_seg;

spiro_seg *run_spiro (const spiro_cp *src, int n);

void free_spiro (spiro_seg *s);

void spiro_to_bpath (const spiro_seg *s, int n, bezctx * bc);

double get_knot_th (const spiro_seg *s, int i);

#endif // _FF_INTERNAL_SPIRO_H
