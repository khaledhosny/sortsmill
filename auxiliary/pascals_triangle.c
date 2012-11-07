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

#include <pascals_triangle.h>
#include <xgc.h>
#include <string.h>

static const int row0[] = { 1 };
static const int row1[] = { 1, 1 };
static const int row2[] = { 1, 2, 1 };
static const int row3[] = { 1, 3, 3, 1 };
static const int row4[] = { 1, 4, 6, 4, 1 };
static const int row5[] = { 1, 5, 10, 10, 5, 1 };
static const int row6[] = { 1, 6, 15, 20, 15, 6, 1 };
static const int row7[] = { 1, 7, 21, 35, 35, 21, 7, 1 };
static const int row8[] = { 1, 8, 28, 56, 70, 56, 28, 8, 1 };
static const int row9[] = { 1, 9, 36, 84, 126, 126, 84, 36, 9, 1 };
static const int row10[] = { 1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1 };
static const int row11[] =
  { 1, 11, 55, 165, 330, 462, 462, 330, 165, 55, 11, 1 };
static const int row12[] =
  { 1, 12, 66, 220, 495, 792, 924, 792, 495, 220, 66, 12, 1 };
static const int row13[] =
  { 1, 13, 78, 286, 715, 1287, 1716, 1716, 1287, 715, 286, 78, 13, 1 };

static const int row14[] =
  { 1, 14, 91, 364, 1001, 2002, 3003, 3432, 3003, 2002, 1001, 364, 91, 14,
  1
};

static const int row15[] =
  { 1, 15, 105, 455, 1365, 3003, 5005, 6435, 6435, 5005, 3003, 1365, 455, 105,
  15, 1
};

static const int row16[] =
  { 1, 16, 120, 560, 1820, 4368, 8008, 11440, 12870, 11440, 8008, 4368, 1820,
  560, 120, 16, 1
};

static const int row17[] =
  { 1, 17, 136, 680, 2380, 6188, 12376, 19448, 24310, 24310, 19448, 12376,
  6188, 2380, 680, 136, 17, 1
};

static const int *rows[18] = {
  row0,
  row1,
  row2,
  row3,
  row4,
  row5,
  row6,
  row7,
  row8,
  row9,
  row10,
  row11,
  row12,
  row13,
  row14,
  row15,
  row16,
  row17
};

const int *
pascals_triangle_row (unsigned int n)
{
  const int *result;

  if (n <= 17)
    result = rows[n];
  else
    {
      int *bincoef = x_gc_malloc_atomic ((n + 1) * sizeof (int));
      memcpy (bincoef, row17, 18 * sizeof (int));
      for (unsigned int i = 18; i <= n; i++)
        {
          bincoef[i] = 1;
          for (unsigned int j = i - 1; 0 < j; j--)
            bincoef[j] += bincoef[j - 1];
        }
      result = (const int *) bincoef;
    }
  return result;
}

// Pascal’s triangle rows, but with alternating signs. (These are
// often used to represent powers of 1-t.)
const int *
pascals_triangle_row_altsigns (unsigned int n)
{
  const int *bc = pascals_triangle_row (n);
  int *row = x_gc_malloc_atomic ((n + 1) * sizeof (int));
  for (unsigned int j = 0; j <= n; j++)
    row[j] = (j % 2 == 0) ? bc[j] : -bc[j];
  return (const int *) row;
}
