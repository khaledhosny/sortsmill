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
#include <sortsmillff/gmp_constants.h>

VISIBLE void
_GMP_TYPE (_matrix_scale) (unsigned int m, unsigned int n,
                           _GMP_TYPE (_t) A[m][n], const _GMP_TYPE (_t) x)
{
  if (_GMP_TYPE (_sgn) (x) == 0)
    _GMP_TYPE (_matrix_set_zero) (m, n, A);
  else if (!_GMP_TYPE (_equal) (x, _GMP_TYPE (_one) ()))
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        _GMP_TYPE (_mul) (A[i][j], A[i][j], x);
}

static inline void
_GMP_TYPE (_matmul_alpha1_beta1) (CBLAS_TRANSPOSE_t TransA,
                                  CBLAS_TRANSPOSE_t TransB, unsigned int m,
                                  unsigned int n, unsigned int k,
                                  _GMP_TYPE (_t) _FF_TRANSMATRIX (A, TransA,
                                                                  m, k),
                                  _GMP_TYPE (_t) _FF_TRANSMATRIX (B, TransB,
                                                                  k, n),
                                  _GMP_TYPE (_t) C[m][n])
{
  _GMP_TYPE (_t) product;

  _GMP_TYPE (_init) (product);

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        for (unsigned int p = 0; p < k; p++)
          {
            const unsigned int rA = _trans_row (TransA, i, p);
            const unsigned int cA = _trans_col (TransA, i, p);
            const unsigned int rB = _trans_row (TransB, p, j);
            const unsigned int cB = _trans_col (TransB, p, j);
            _GMP_TYPE (_mul) (product, A[rA][cA], B[rB][cB]);
            _GMP_TYPE (_add) (C[i][j], C[i][j], product);
          }
      }

  _GMP_TYPE (_clear) (product);
}

static inline void
_GMP_TYPE (_matmul_beta1) (CBLAS_TRANSPOSE_t TransA,
                           CBLAS_TRANSPOSE_t TransB, unsigned int m,
                           unsigned int n, unsigned int k,
                           const _GMP_TYPE (_t) alpha,
                           _GMP_TYPE (_t) _FF_TRANSMATRIX (A, TransA, m, k),
                           _GMP_TYPE (_t) _FF_TRANSMATRIX (B, TransB, k, n),
                           _GMP_TYPE (_t) C[m][n])
{
  _GMP_TYPE (_t) product;

  _GMP_TYPE (_init) (product);

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        for (unsigned int p = 0; p < k; p++)
          {
            const unsigned int rA = _trans_row (TransA, i, p);
            const unsigned int cA = _trans_col (TransA, i, p);
            const unsigned int rB = _trans_row (TransB, p, j);
            const unsigned int cB = _trans_col (TransB, p, j);
            _GMP_TYPE (_mul) (product, A[rA][cA], B[rB][cB]);
            _GMP_TYPE (_mul) (product, product, alpha);
            _GMP_TYPE (_add) (C[i][j], C[i][j], product);
          }
      }

  _GMP_TYPE (_clear) (product);
}

VISIBLE void
_GMP_TYPE (_matrix_gemm) (CBLAS_TRANSPOSE_t TransA,
                          CBLAS_TRANSPOSE_t TransB, unsigned int m,
                          unsigned int n, unsigned int k,
                          const _GMP_TYPE (_t) alpha,
                          _GMP_TYPE (_t) _FF_TRANSMATRIX (A, TransA, m, k),
                          _GMP_TYPE (_t) _FF_TRANSMATRIX (B, TransB, k, n),
                          const _GMP_TYPE (_t) beta, _GMP_TYPE (_t) C[m][n])
{
  _GMP_TYPE (_matrix_scale) (m, n, C, beta);
  if (_GMP_TYPE (_sgn) (alpha) != 0)
    {
      if (_GMP_TYPE (_equal) (alpha, _GMP_TYPE (_one) ()))
        _GMP_TYPE (_matmul_alpha1_beta1) (TransA, TransB, m, n, k, A, B, C);
      else
        _GMP_TYPE (_matmul_beta1) (TransA, TransB, m, n, k, alpha, A, B, C);
    }
}
