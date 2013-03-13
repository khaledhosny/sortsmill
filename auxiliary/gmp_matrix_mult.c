#include <config.h>

// Copyright (C) 2012, 2013 Barry Schwartz
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

#include <sortsmill/math/gmp_matrix.h>
#include <sortsmill/math/gmp_constants.h>

VISIBLE void
_GMP_TYPE (_matrix_scale) (size_t m, size_t n,
                           _GMP_TYPE (_t) A[m][n], const _GMP_TYPE (_t) x)
{
  if (_GMP_TYPE (_sgn) (x) == 0)
    _GMP_TYPE (_matrix_set_zero) (m, n, A);
  else if (!_GMP_TYPE (_equal) (x, _GMP_TYPE (_one) ()))
    for (size_t i = 0; i < m; i++)
      for (size_t j = 0; j < n; j++)
        _GMP_TYPE (_mul) (A[i][j], A[i][j], x);
}

VISIBLE void
_GMP_TYPE (_matrix_mul_elements) (size_t m, size_t n,
                                  _GMP_TYPE (_t) A[m][n],
                                  _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_mul) (A[i][j], A[i][j], B[i][j]);
}

#if _GMP_TYPE_MPZ

VISIBLE void
_GMP_TYPE (_matrix_cdiv_q_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  // mpz_cdiv_q rounds towards +∞
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_cdiv_q) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_fdiv_q_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  // mpz_fdiv_q rounds towards −∞
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_fdiv_q) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_tdiv_q_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  // mpz_tdiv_q rounds towards zero (‘truncation’).
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_tdiv_q) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_cdiv_r_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_cdiv_r) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_fdiv_r_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_fdiv_r) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_tdiv_r_elements) (size_t m, size_t n,
                                     _GMP_TYPE (_t) A[m][n],
                                     _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_tdiv_r) (A[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_cdiv_qr_elements) (size_t m, size_t n,
                                      _GMP_TYPE (_t) A[m][n],
                                      _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_cdiv_qr) (A[i][j], B[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_fdiv_qr_elements) (size_t m, size_t n,
                                      _GMP_TYPE (_t) A[m][n],
                                      _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_fdiv_qr) (A[i][j], B[i][j], A[i][j], B[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_tdiv_qr_elements) (size_t m, size_t n,
                                      _GMP_TYPE (_t) A[m][n],
                                      _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_tdiv_qr) (A[i][j], B[i][j], A[i][j], B[i][j]);
}

#else // !_GMP_TYPE_MPZ

VISIBLE void
_GMP_TYPE (_matrix_div_elements) (size_t m, size_t n,
                                  _GMP_TYPE (_t) A[m][n],
                                  _GMP_TYPE (_t) B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_div) (A[i][j], A[i][j], B[i][j]);
}

#endif // !_GMP_TYPE_MPZ

//-------------------------------------------------------------------------
//
// General matrix multiplication.

static inline void
_GMP_TYPE (_matmul_alpha1_beta1) (CBLAS_TRANSPOSE_t TransA,
                                  CBLAS_TRANSPOSE_t TransB, size_t m,
                                  size_t n, size_t k,
                                  _GMP_TYPE (_t) _FF_TRANSMATRIX (A, TransA,
                                                                  m, k),
                                  _GMP_TYPE (_t) _FF_TRANSMATRIX (B, TransB,
                                                                  k, n),
                                  _GMP_TYPE (_t) C[m][n])
{
  _GMP_TYPE (_t) product;

  _GMP_TYPE (_init) (product);

  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        for (size_t p = 0; p < k; p++)
          {
            const size_t rA = _trans_row (TransA, i, p);
            const size_t cA = _trans_col (TransA, i, p);
            const size_t rB = _trans_row (TransB, p, j);
            const size_t cB = _trans_col (TransB, p, j);
            _GMP_TYPE (_mul) (product, A[rA][cA], B[rB][cB]);
            _GMP_TYPE (_add) (C[i][j], C[i][j], product);
          }
      }

  _GMP_TYPE (_clear) (product);
}

static inline void
_GMP_TYPE (_matmul_beta1) (CBLAS_TRANSPOSE_t TransA,
                           CBLAS_TRANSPOSE_t TransB, size_t m,
                           size_t n, size_t k,
                           const _GMP_TYPE (_t) alpha,
                           _GMP_TYPE (_t) _FF_TRANSMATRIX (A, TransA, m, k),
                           _GMP_TYPE (_t) _FF_TRANSMATRIX (B, TransB, k, n),
                           _GMP_TYPE (_t) C[m][n])
{
  _GMP_TYPE (_t) product;

  _GMP_TYPE (_init) (product);

  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        for (size_t p = 0; p < k; p++)
          {
            const size_t rA = _trans_row (TransA, i, p);
            const size_t cA = _trans_col (TransA, i, p);
            const size_t rB = _trans_row (TransB, p, j);
            const size_t cB = _trans_col (TransB, p, j);
            _GMP_TYPE (_mul) (product, A[rA][cA], B[rB][cB]);
            _GMP_TYPE (_mul) (product, product, alpha);
            _GMP_TYPE (_add) (C[i][j], C[i][j], product);
          }
      }

  _GMP_TYPE (_clear) (product);
}

VISIBLE void
_GMP_TYPE (_matrix_gemm) (CBLAS_TRANSPOSE_t TransA,
                          CBLAS_TRANSPOSE_t TransB, size_t m,
                          size_t n, size_t k,
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

//-------------------------------------------------------------------------
//
// Multiplication by a triangular matrix on one or the other side.

static void
_GMP_TYPE (_matrix_trmm_left_lower_notrans) (CBLAS_DIAG_t Diag,
                                             size_t m, size_t n,
                                             _GMP_TYPE (_t) A[m][m],
                                             _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        _GMP_TYPE (_set) (sum, B[m - i - 1][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[m - i - 1][m - i - 1]);
        for (size_t q = 0; q < m - i - 1; q++)
          {
            _GMP_TYPE (_mul) (product, B[q][j], A[m - i - 1][q]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[m - i - 1][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_left_lower_trans) (CBLAS_DIAG_t Diag,
                                           size_t m, size_t n,
                                           _GMP_TYPE (_t) A[m][m],
                                           _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        _GMP_TYPE (_set) (sum, B[i][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[i][i]);
        for (size_t q = i + 1; q < m; q++)
          {
            _GMP_TYPE (_mul) (product, B[q][j], A[q][i]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_left_upper_notrans) (CBLAS_DIAG_t Diag,
                                             size_t m, size_t n,
                                             _GMP_TYPE (_t) A[m][m],
                                             _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        _GMP_TYPE (_set) (sum, B[i][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[i][i]);
        for (size_t q = i + 1; q < m; q++)
          {
            _GMP_TYPE (_mul) (product, B[q][j], A[i][q]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_left_upper_trans) (CBLAS_DIAG_t Diag,
                                           size_t m, size_t n,
                                           _GMP_TYPE (_t) A[m][m],
                                           _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        _GMP_TYPE (_set) (sum, B[m - i - 1][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[m - i - 1][m - i - 1]);
        for (size_t q = 0; q < m - i - 1; q++)
          {
            _GMP_TYPE (_mul) (product, B[q][j], A[q][m - i - 1]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[m - i - 1][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_right_lower_notrans) (CBLAS_DIAG_t Diag,
                                              size_t m, size_t n,
                                              _GMP_TYPE (_t) A[n][n],
                                              _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        _GMP_TYPE (_set) (sum, B[i][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[j][j]);
        for (size_t q = j + 1; q < n; q++)
          {
            _GMP_TYPE (_mul) (product, B[i][q], A[q][j]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_right_lower_trans) (CBLAS_DIAG_t Diag,
                                            size_t m, size_t n,
                                            _GMP_TYPE (_t) A[n][n],
                                            _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        _GMP_TYPE (_set) (sum, B[i][n - j - 1]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[n - j - 1][n - j - 1]);
        for (size_t q = 0; q < n - j - 1; q++)
          {
            _GMP_TYPE (_mul) (product, B[i][q], A[n - j - 1][q]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][n - j - 1], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_right_upper_notrans) (CBLAS_DIAG_t Diag,
                                              size_t m, size_t n,
                                              _GMP_TYPE (_t) A[n][n],
                                              _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        _GMP_TYPE (_set) (sum, B[i][n - j - 1]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[n - j - 1][n - j - 1]);
        for (size_t q = 0; q < n - j - 1; q++)
          {
            _GMP_TYPE (_mul) (product, B[i][q], A[q][n - j - 1]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][n - j - 1], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_right_upper_trans) (CBLAS_DIAG_t Diag,
                                            size_t m, size_t n,
                                            _GMP_TYPE (_t) A[n][n],
                                            _GMP_TYPE (_t) B[m][n])
{
  _GMP_TYPE (_t) sum;
  _GMP_TYPE (_t) product;
  _GMP_TYPE (_init) (sum);
  _GMP_TYPE (_init) (product);
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        _GMP_TYPE (_set) (sum, B[i][j]);
        if (Diag == CblasNonUnit)
          _GMP_TYPE (_mul) (sum, sum, A[j][j]);
        for (size_t q = j + 1; q < n; q++)
          {
            _GMP_TYPE (_mul) (product, B[i][q], A[j][q]);
            _GMP_TYPE (_add) (sum, sum, product);
          }
        _GMP_TYPE (_set) (B[i][j], sum);
      }
  _GMP_TYPE (_clear) (sum);
  _GMP_TYPE (_clear) (product);
}

static void
_GMP_TYPE (_matrix_trmm_left_lower) (CBLAS_TRANSPOSE_t TransA,
                                     CBLAS_DIAG_t Diag, size_t m,
                                     size_t n, _GMP_TYPE (_t) A[m][m],
                                     _GMP_TYPE (_t) B[m][n])
{
  if (TransA == CblasNoTrans)
    _GMP_TYPE (_matrix_trmm_left_lower_notrans) (Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_left_lower_trans) (Diag, m, n, A, B);
}

static void
_GMP_TYPE (_matrix_trmm_left_upper) (CBLAS_TRANSPOSE_t TransA,
                                     CBLAS_DIAG_t Diag, size_t m,
                                     size_t n, _GMP_TYPE (_t) A[m][m],
                                     _GMP_TYPE (_t) B[m][n])
{
  if (TransA == CblasNoTrans)
    _GMP_TYPE (_matrix_trmm_left_upper_notrans) (Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_left_upper_trans) (Diag, m, n, A, B);
}

static void
_GMP_TYPE (_matrix_trmm_right_lower) (CBLAS_TRANSPOSE_t TransA,
                                      CBLAS_DIAG_t Diag, size_t m,
                                      size_t n, _GMP_TYPE (_t) A[n][n],
                                      _GMP_TYPE (_t) B[m][n])
{
  if (TransA == CblasNoTrans)
    _GMP_TYPE (_matrix_trmm_right_lower_notrans) (Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_right_lower_trans) (Diag, m, n, A, B);
}

static void
_GMP_TYPE (_matrix_trmm_right_upper) (CBLAS_TRANSPOSE_t TransA,
                                      CBLAS_DIAG_t Diag, size_t m,
                                      size_t n, _GMP_TYPE (_t) A[n][n],
                                      _GMP_TYPE (_t) B[m][n])
{
  if (TransA == CblasNoTrans)
    _GMP_TYPE (_matrix_trmm_right_upper_notrans) (Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_right_upper_trans) (Diag, m, n, A, B);
}

static void
_GMP_TYPE (_matrix_trmm_left) (CBLAS_UPLO_t Uplo,
                               CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                               size_t m, size_t n,
                               _GMP_TYPE (_t) A[m][m], _GMP_TYPE (_t) B[m][n])
{
  if (Uplo == CblasLower)
    _GMP_TYPE (_matrix_trmm_left_lower) (TransA, Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_left_upper) (TransA, Diag, m, n, A, B);
}

static void
_GMP_TYPE (_matrix_trmm_right) (CBLAS_UPLO_t Uplo,
                                CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                                size_t m, size_t n,
                                _GMP_TYPE (_t) A[n][n], _GMP_TYPE (_t) B[m][n])
{
  if (Uplo == CblasLower)
    _GMP_TYPE (_matrix_trmm_right_lower) (TransA, Diag, m, n, A, B);
  else
    _GMP_TYPE (_matrix_trmm_right_upper) (TransA, Diag, m, n, A, B);
}

VISIBLE void
_GMP_TYPE (_matrix_trmm) (CBLAS_SIDE_t Side, CBLAS_UPLO_t Uplo,
                          CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                          size_t m, size_t n,
                          const _GMP_TYPE (_t) alpha,
                          _GMP_TYPE (_t)
                          A[(Side == CblasLeft) ? m : n][(Side ==
                                                          CblasLeft) ? m : n],
                          _GMP_TYPE (_t) B[m][n])
{
  assert (0 < m);
  assert (0 < n);
  assert (Side == CblasLeft || Side == CblasRight);
  assert (Uplo == CblasLower || Uplo == CblasUpper);
  assert (Diag == CblasNonUnit || Diag == CblasUnit);

  // FIXME: We do not yet support conjugate transposes.
  assert (TransA == CblasNoTrans || TransA == CblasTrans);

  if (_GMP_TYPE (_sgn) (alpha) == 0)
    _GMP_TYPE (_matrix_set_zero) (m, n, B);
  else
    {
      _GMP_TYPE (_matrix_scale) (m, n, B, alpha);
      if (Side == CblasLeft)
        _GMP_TYPE (_matrix_trmm_left) (Uplo, TransA, Diag, m, n, A, B);
      else
        _GMP_TYPE (_matrix_trmm_right) (Uplo, TransA, Diag, m, n, A, B);
    }
}

//-------------------------------------------------------------------------

// In-place multiplication by a diagonal matrix (represented as a
// vector). @var{Side} says whether the diagonal matrix is on the left
// or the right side of the multiplication.
VISIBLE void
_GMP_TYPE (_matrix_mul_diagonal) (CBLAS_SIDE_t Side,
                                  size_t m, size_t n,
                                  _GMP_TYPE (_t) A[m][n],
                                  _GMP_TYPE (_t) x[(Side == CblasLeft) ? m : n])
{
  assert (Side == CblasLeft || Side == CblasRight);

  if (Side == CblasLeft)
    // Scale the rows of A.
    for (size_t i = 0; i < m; i++)
      for (size_t j = 0; j < n; j++)
        _GMP_TYPE (_mul) (A[i][j], A[i][j], x[i]);
  else
    // Scale the columns of A.
    for (size_t j = 0; j < n; j++)
      for (size_t i = 0; i < m; i++)
        _GMP_TYPE (_mul) (A[i][j], A[i][j], x[j]);
}

//-------------------------------------------------------------------------
