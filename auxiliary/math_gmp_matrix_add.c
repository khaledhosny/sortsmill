#include <config.h>

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

#include <sortsmill/math/gmp_matrix.h>

VISIBLE void
_GMP_TYPE (_matrix_add) (size_t m, size_t n,
                         _GMP_TYPE (_t) A[m][n], _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_add) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_sub) (size_t m, size_t n,
                         _GMP_TYPE (_t) A[m][n], _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_sub) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_add_constant) (size_t m, size_t n,
                                  _GMP_TYPE (_t) A[m][n],
                                  const _GMP_TYPE (_t) x)
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_add) (A[i][j], A[i][j], x);
}
