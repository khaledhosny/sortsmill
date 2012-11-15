#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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

#include <polyspline.h>
#include <pascals_triangle.h>
#include <string.h>
#include <math.h>

int nothing_here_yet = 0;

#if 0				/* FIXME: This is too complex. */

/* Compute A*x, where A is a matrix of the form for conversion between
   scaled Bernstein and Sánchez-Reyes symmetric power bases, and x is
   a vector. Only non-zero, left-side rows of the matrix A are
   actually used. (Thus the rest can optionally be left
   uninitialized.)  */
static void
sbsr_vecmul (unsigned int deg, const double a[deg + 1][deg + 1],
             const double x[deg + 1], double result[deg + 1])
{
  double y[deg + 1];
  unsigned int q = (deg + 1) / 2;

  for (unsigned int i = 0; i < deg; i++)
    y[i] = a[i][0] * x[0];
  y[deg] = 0.0;

  for (unsigned int i = 0; i < deg; i++)
    y[deg - i] = MY_FAST_FMA (a[i][0], x[deg], y[deg - i]);

  for (unsigned int i = 1; i < q; i++)
    for (unsigned int j = i; j < deg - i; j++)
      {
        y[j] = MY_FAST_FMA (a[j][i], x[i], y[j]);
        y[deg - j] = MY_FAST_FMA (a[j][i], x[deg - i], y[deg -j]);
      }

  if ((deg + 1) % 2 != 0)
    y[q] += x[q];

  memcpy (result, y, sizeof y);
}
#endif
