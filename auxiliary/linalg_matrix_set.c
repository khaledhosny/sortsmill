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

#include <linalg.h>
#include <gmp_constants.h>

VISIBLE void
_GMP_TYPE (_matrix_set_all) (unsigned int m, unsigned int n,
                             _GMP_TYPE (_t) A[m][n], const _GMP_TYPE (_t) x)
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_set) (A[i][j], x);
}

VISIBLE void
_GMP_TYPE (_matrix_set_zero) (unsigned int m, unsigned int n,
                              _GMP_TYPE (_t) A[m][n])
{
  const _GMP_TYPE2 (__, _struct) * zero = _GMP_TYPE (_zero) ();
  _GMP_TYPE (_matrix_set_all) (m, n, A, zero);
}

VISIBLE void
_GMP_TYPE (_matrix_set_identity) (unsigned int m, unsigned int n,
                                  _GMP_TYPE (_t) A[m][n])
{
  const _GMP_TYPE2 (__, _struct) * zero = _GMP_TYPE (_zero) ();
  const _GMP_TYPE2 (__, _struct) * one = _GMP_TYPE (_one) ();
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_set) (A[i][j], ((i == j) ? one : zero));
}
