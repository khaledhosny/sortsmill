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

//-------------------------------------------------------------------------
//
// Solve triangular linear systems by forward/back substitution.

static void
upper_triangle_no_trans (CBLAS_DIAG_t Diag, unsigned int n,
                         _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) x[n])
{
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (product);

  if (Diag == CblasNonUnit)
    _GMP_TYPE (_div) (x[0], x[0], A[0][0]);
  for (unsigned int i = 1; i < n; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        {
          _GMP_TYPE (_mul) (product, A[i][j], x[j]);
          _GMP_TYPE (_sub) (x[i], x[i], product);
        }
      if (Diag == CblasNonUnit)
        _GMP_TYPE (_div) (x[i], x[i], A[i][i]);
    }

  _GMP_TYPE (_clear) (product);
}

static void
upper_triangle_trans (CBLAS_DIAG_t Diag, unsigned int n,
                      _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) x[n])
{
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (product);

  const unsigned int n1 = n - 1;
  if (Diag == CblasNonUnit)
    _GMP_TYPE (_div) (x[n1], x[n1], A[n1][n1]);
  for (unsigned int ni = 1; ni < n; ni++)
    {
      const unsigned int i = n1 - ni;
      for (unsigned int nj = 0; nj < ni; nj++)
        {
          const unsigned int j = n1 - nj;
          _GMP_TYPE (_mul) (product, A[j][i], x[j]);
          _GMP_TYPE (_sub) (x[i], x[i], product);
        }
      if (Diag == CblasNonUnit)
        _GMP_TYPE (_div) (x[i], x[i], A[i][i]);
    }

  _GMP_TYPE (_clear) (product);
}

static void
lower_triangle_no_trans (CBLAS_DIAG_t Diag, unsigned int n,
                         _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) x[n])
{
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (product);

  const unsigned int n1 = n - 1;
  if (Diag == CblasNonUnit)
    _GMP_TYPE (_div) (x[n1], x[n1], A[n1][n1]);
  for (unsigned int ni = 1; ni < n; ni++)
    {
      const unsigned int i = n1 - ni;
      for (unsigned int nj = 0; nj < ni; nj++)
        {
          const unsigned int j = n1 - nj;
          _GMP_TYPE (_mul) (product, A[i][j], x[j]);
          _GMP_TYPE (_sub) (x[i], x[i], product);
        }
      if (Diag == CblasNonUnit)
        _GMP_TYPE (_div) (x[i], x[i], A[i][i]);
    }

  _GMP_TYPE (_clear) (product);
}

static void
lower_triangle_trans (CBLAS_DIAG_t Diag, unsigned int n,
                      _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) x[n])
{
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (product);

  if (Diag == CblasNonUnit)
    _GMP_TYPE (_div) (x[0], x[0], A[0][0]);
  for (unsigned int i = 1; i < n; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        {
          _GMP_TYPE (_mul) (product, A[j][i], x[j]);
          _GMP_TYPE (_sub) (x[i], x[i], product);
        }
      if (Diag == CblasNonUnit)
        _GMP_TYPE (_div) (x[i], x[i], A[i][i]);
    }

  _GMP_TYPE (_clear) (product);
}

static bool
determinant_is_zero (unsigned int n, _GMP_TYPE (_t) A[n][n])
{
  bool is_zero = false;
  unsigned int i = 0;
  while (!is_zero && i < n)
    {
      is_zero = (_GMP_TYPE (_sgn) (A[i][i]) == 0);
      i++;
    }
  return is_zero;
}

VISIBLE void
_GMP_TYPE (_matrix_trsv) (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                          CBLAS_DIAG_t Diag, unsigned int n,
                          _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) x[n],
                          bool *singular)
{
  assert (0 < n);
  assert (Uplo == CblasLower || Uplo == CblasUpper);
  assert (Diag == CblasNonUnit || Diag == CblasUnit);

  // FIXME: We do not yet support conjugate transposes.
  assert (TransA == CblasNoTrans || TransA == CblasTrans);

  *singular = false;

  if (Diag == CblasNonUnit && determinant_is_zero (n, A))
    {
      *singular = true;
    }
  else if (Uplo == CblasLower)
    {
      if (TransA == CblasNoTrans)
        upper_triangle_no_trans (Diag, n, A, x);
      else
        upper_triangle_trans (Diag, n, A, x);
    }
  else
    {
      if (TransA == CblasNoTrans)
        lower_triangle_no_trans (Diag, n, A, x);
      else
        lower_triangle_trans (Diag, n, A, x);
    }
}

//-------------------------------------------------------------------------
