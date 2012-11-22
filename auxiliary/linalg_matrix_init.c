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
_GMP_TYPE (_matrix_init) (unsigned int m, unsigned int n,
                          _GMP_TYPE (_t) A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_init) (A[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_clear) (unsigned int m, unsigned int n,
                           _GMP_TYPE (_t) A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      _GMP_TYPE (_clear) (A[i][j]);
}
