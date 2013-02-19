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

#include <sortsmill/gmp_matrix.h>

VISIBLE void
_GMP_TYPE (_matrix_memcpy) (unsigned int m, unsigned int n,
                            _GMP_TYPE (_t) result[m][n], _GMP_TYPE (_t) A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_set) (result[i][j], A[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_swap) (unsigned int m, unsigned int n,
                          _GMP_TYPE (_t) A[m][n], _GMP_TYPE (_t) B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_swap) (A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_memcpy_c90) (unsigned int m, unsigned int n,
                                _GMP_TYPE (_t) result[], _GMP_TYPE (_t) A[])
{
  _GMP_TYPE (_matrix_memcpy) (m, n,
                              (_GMP_ARRAY (m, n)) result,
                              (_GMP_ARRAY (m, n)) A);
}

VISIBLE void
_GMP_TYPE (_matrix_swap_c90) (unsigned int m, unsigned int n,
                              _GMP_TYPE (_t) A[], _GMP_TYPE (_t) B[])
{
  _GMP_TYPE (_matrix_swap) (m, n, (_GMP_ARRAY (m, n)) A, (_GMP_ARRAY (m, n)) B);
}
