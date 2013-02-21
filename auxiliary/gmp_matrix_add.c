#include <config.h>

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

#include <sortsmill/gmp_matrix.h>

VISIBLE void
_GMP_TYPE (_matrix_add) (unsigned int m, unsigned int n,
                         _GMP_TYPE (_t) A[m][n], _GMP_TYPE (_t) B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_add) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_sub) (unsigned int m, unsigned int n,
                         _GMP_TYPE (_t) A[m][n], _GMP_TYPE (_t) B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_sub) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_add_constant) (unsigned int m, unsigned int n,
                                  _GMP_TYPE (_t) A[m][n],
                                  const _GMP_TYPE (_t) x)
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_add) (A[i][j], A[i][j], x);
}

VISIBLE void
_GMP_TYPE (_matrix_add_c90) (unsigned int m, unsigned int n,
                             _GMP_TYPE (_t) A[], _GMP_TYPE (_t) B[])
{
  _GMP_TYPE (_matrix_add) (m, n, (_GMP_MATRIX (m, n)) A,
                           (_GMP_MATRIX (m, n)) B);
}

VISIBLE void
_GMP_TYPE (_matrix_sub_c90) (unsigned int m, unsigned int n,
                             _GMP_TYPE (_t) A[], _GMP_TYPE (_t) B[])
{
  _GMP_TYPE (_matrix_sub) (m, n, (_GMP_MATRIX (m, n)) A,
                           (_GMP_MATRIX (m, n)) B);
}

VISIBLE void
_GMP_TYPE (_matrix_add_constant_c90) (unsigned int m, unsigned int n,
                                      _GMP_TYPE (_t) A[],
                                      const _GMP_TYPE (_t) x)
{
  _GMP_TYPE (_matrix_add_constant) (m, n, (_GMP_MATRIX (m, n)) A, x);
}
