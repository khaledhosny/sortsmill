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

#include <sortsmillff/linalg.h>

VISIBLE void
_GMP_TYPE (_matrix_swap_rows) (unsigned int m, unsigned int n,
                               _GMP_TYPE (_t) A[m][n],
                               unsigned int i, unsigned int j)
{
  if (i != j)
    for (unsigned int p = 0; p < n; p++)
      _GMP_TYPE (_swap) (A[i][p], A[j][p]);
}

VISIBLE void
_GMP_TYPE (_matrix_swap_columns) (unsigned int m, unsigned int n,
                                  _GMP_TYPE (_t) A[m][n],
                                  unsigned int i, unsigned int j)
{
  if (i != j)
    for (unsigned int p = 0; p < m; p++)
      _GMP_TYPE (_swap) (A[p][i], A[p][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_swap_rowcol) (unsigned int m,
                                 _GMP_TYPE (_t) A[m][m],
                                 unsigned int i, unsigned int j)
{
  for (unsigned int p = 0; p < m; p++)
    _GMP_TYPE (_swap) (A[i][p], A[p][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_transpose_memcpy) (unsigned int m, unsigned int n,
                                      _GMP_TYPE (_t) result[n][m],
                                      _GMP_TYPE (_t) A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_set) (result[j][i], A[i][j]);
}
