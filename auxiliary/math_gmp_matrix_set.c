#include <config.h>

// Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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
#include <sortsmill/math/gmp_constants.h>

VISIBLE void
_GMP_TYPE (_matrix_set_all) (size_t m, size_t n,
                             _GMP_TYPE (_t) A[m][n], const _GMP_TYPE (_t) x)
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_set) (A[i][j], x);
}

VISIBLE void
_GMP_TYPE (_matrix_set_zero) (size_t m, size_t n, _GMP_TYPE (_t) A[m][n])
{
  const _GMP_TYPE2 (__, _struct) * zero = _GMP_TYPE (_zero) ();
  _GMP_TYPE (_matrix_set_all) (m, n, A, zero);
}

VISIBLE void
_GMP_TYPE (_matrix_set_identity) (size_t m, size_t n, _GMP_TYPE (_t) A[m][n])
{
  const _GMP_TYPE2 (__, _struct) * zero = _GMP_TYPE (_zero) ();
  const _GMP_TYPE2 (__, _struct) * one = _GMP_TYPE (_one) ();
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_set) (A[i][j], ((i == j) ? one : zero));
}
