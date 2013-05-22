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

#include <sortsmill/math/f64_matrix.h>
#include <assert.h>

//-------------------------------------------------------------------------

// In-place multiplication by a diagonal matrix (represented as a
// vector). @var{Side} says whether the diagonal matrix is on the left
// or the right side of the multiplication.
VISIBLE void
f64_matrix_mul_diagonal (CBLAS_SIDE_t Side, size_t m, size_t n,
                         double A[m][n], double x[(Side == CblasLeft) ? m : n])
{
  assert (Side == CblasLeft || Side == CblasRight);

  if (Side == CblasLeft)
    // Scale the rows of A.
    for (size_t i = 0; i < m; i++)
      for (size_t j = 0; j < n; j++)
        A[i][j] *= x[i];
  else
    // Scale the columns of A.
    for (size_t j = 0; j < n; j++)
      for (size_t i = 0; i < m; i++)
        A[i][j] *= x[j];
}

//-------------------------------------------------------------------------
