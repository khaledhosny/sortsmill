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

#include <sortsmill/math/scm_matrix.h>
#include <assert.h>

VISIBLE void
scm_matrix_set_all (size_t m, size_t n, SCM A[m][n], SCM x)
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = x;
}

VISIBLE void
scm_matrix_set_zero (size_t m, size_t n, SCM A[m][n])
{
  scm_matrix_set_all (m, n, A, scm_from_int (0));
}

VISIBLE void
scm_matrix_set_identity (size_t m, size_t n, SCM A[m][n])
{
  SCM zero = scm_from_int (0);
  SCM one = scm_from_int (1);
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = (i == j) ? one : zero;
}

VISIBLE void
scm_matrix_memcpy (size_t m, size_t n, SCM result[m][n], SCM A[m][n])
{
  memcpy (&result[0][0], &A[0][0], m * n * sizeof (SCM));
}

VISIBLE void
scm_matrix_swap (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        SCM x = A[i][j];
        A[i][j] = B[i][j];
        B[i][j] = x;
      }
}

VISIBLE void
scm_matrix_swap_rows (size_t m, size_t n, SCM A[m][n], size_t i, size_t j)
{
  if (i != j)
    for (size_t p = 0; p < n; p++)
      {
        SCM x = A[i][p];
        A[i][p] = A[j][p];
        A[j][p] = x;
      }
}

VISIBLE void
scm_matrix_swap_columns (size_t m, size_t n, SCM A[m][n], size_t i, size_t j)
{
  if (i != j)
    for (size_t p = 0; p < m; p++)
      {
        SCM x = A[p][i];
        A[p][i] = A[p][j];
        A[p][j] = x;
      }
}

VISIBLE void
scm_matrix_swap_rowcol (size_t m, SCM A[m][m], size_t i, size_t j)
{
  for (size_t p = 0; p < m; p++)
    {
      SCM x = A[i][p];
      A[i][p] = A[p][j];
      A[p][j] = x;
    }
}

VISIBLE void
scm_matrix_transpose_memcpy (size_t m, size_t n, SCM result[n][m], SCM A[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      result[j][i] = A[i][j];
}

VISIBLE void
scm_matrix_scale (size_t m, size_t n, SCM A[m][n], SCM x)
{
  if (scm_is_true (scm_zero_p (x)))
    scm_matrix_set_zero (m, n, A);
  else if (scm_is_false (scm_zero_p (scm_oneminus (x))))
    for (size_t i = 0; i < m; i++)
      for (size_t j = 0; j < n; j++)
        A[i][j] = scm_product (A[i][j], x);
}

//-------------------------------------------------------------------------

VISIBLE void
scm_matrix_mul_elements (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = scm_product (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_div_elements (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = scm_divide (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_add (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = scm_sum (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_sub (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = scm_difference (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_add_constant (size_t m, size_t n, SCM A[m][n], SCM x)
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      A[i][j] = scm_sum (A[i][j], x);
}

#define _FF_SCM_ELEMENTWISE_PRED(NAME, TRUTH, ELEMENT_PRED)     \
  bool                                                          \
  NAME (size_t m, size_t n, SCM A[m][n])                        \
  {                                                             \
    bool result = true;                                         \
    for (size_t i = 0; i < m; i++)                              \
      for (size_t j = 0; j < n; j++)                            \
        {                                                       \
          /* The following is coded purposely to make early  */ \
          /* exit necessary for correct results. Thus we can */ \
          /* more easily detect if the code gets broken.     */ \
          result = TRUTH (ELEMENT_PRED (A[i][j]));              \
          if (result == false)                                  \
            {                                                   \
              i = m - 1;                                        \
              j = n - 1;                                        \
            }                                                   \
        }                                                       \
    return result;                                              \
  }

VISIBLE _FF_SCM_ELEMENTWISE_PRED (scm_matrix_isnull, scm_is_true, scm_zero_p);
VISIBLE _FF_SCM_ELEMENTWISE_PRED (scm_matrix_ispos, scm_is_true,
                                  scm_positive_p);
VISIBLE _FF_SCM_ELEMENTWISE_PRED (scm_matrix_isneg, scm_is_true,
                                  scm_negative_p);
VISIBLE _FF_SCM_ELEMENTWISE_PRED (scm_matrix_isnonneg, scm_is_false,
                                  scm_negative_p);

VISIBLE bool
scm_matrix_equal (size_t m, size_t n, SCM A[m][n], SCM B[m][n])
{
  bool result = true;
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        // The following is coded purposely to make early exit
        // necessary for correct results. Thus we can more easily
        // detect if the code gets broken.
        result = scm_is_true (scm_num_eq_p (A[i][j], B[i][j]));
        if (result == false)
          {
            i = m - 1;
            j = n - 1;
          }
      }
  return result;
}

//-------------------------------------------------------------------------

_GL_ATTRIBUTE_CONST static inline size_t
_trans_row (CBLAS_TRANSPOSE_t trans, size_t i, size_t j)
{
  return (trans == CblasNoTrans) ? i : j;
}

_GL_ATTRIBUTE_CONST static inline size_t
_trans_col (CBLAS_TRANSPOSE_t trans, size_t i, size_t j)
{
  return (trans == CblasNoTrans) ? j : i;
}

//-------------------------------------------------------------------------
//
// General matrix multiplication.

static inline void
scm_matmul_alpha1_beta1 (CBLAS_TRANSPOSE_t TransA,
                         CBLAS_TRANSPOSE_t TransB,
                         size_t m, size_t n, size_t k,
                         SCM _FF_TRANSMATRIX (A, TransA, m, k),
                         SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM C[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        for (size_t p = 0; p < k; p++)
          {
            const size_t rA = _trans_row (TransA, i, p);
            const size_t cA = _trans_col (TransA, i, p);
            const size_t rB = _trans_row (TransB, p, j);
            const size_t cB = _trans_col (TransB, p, j);
            C[i][j] = scm_sum (C[i][j], scm_product (A[rA][cA], B[rB][cB]));
          }
      }
}

static inline void
scm_matmul_beta1 (CBLAS_TRANSPOSE_t TransA,
                  CBLAS_TRANSPOSE_t TransB,
                  size_t m, size_t n, size_t k,
                  const SCM alpha,
                  SCM _FF_TRANSMATRIX (A, TransA, m, k),
                  SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM C[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        for (size_t p = 0; p < k; p++)
          {
            const size_t rA = _trans_row (TransA, i, p);
            const size_t cA = _trans_col (TransA, i, p);
            const size_t rB = _trans_row (TransB, p, j);
            const size_t cB = _trans_col (TransB, p, j);
            C[i][j] = scm_sum (C[i][j],
                               scm_product (scm_product (A[rA][cA], B[rB][cB]),
                                            alpha));
          }
      }
}

VISIBLE void
scm_matrix_gemm (CBLAS_TRANSPOSE_t TransA,
                 CBLAS_TRANSPOSE_t TransB,
                 size_t m, size_t n, size_t k,
                 const SCM alpha,
                 SCM _FF_TRANSMATRIX (A, TransA, m, k),
                 SCM _FF_TRANSMATRIX (B, TransB, k, n),
                 const SCM beta, SCM C[m][n])
{
  // FIXME: We do not yet support conjugate transposes.
  assert (TransA != CblasConjTrans);
  assert (TransB != CblasConjTrans);

  scm_matrix_scale (m, n, C, beta);
  if (scm_is_false (scm_zero_p (alpha)))
    {
      if (scm_is_true (scm_zero_p (scm_oneminus (alpha))))
        scm_matmul_alpha1_beta1 (TransA, TransB, m, n, k, A, B, C);
      else
        scm_matmul_beta1 (TransA, TransB, m, n, k, alpha, A, B, C);
    }
}

//-------------------------------------------------------------------------
//
// Multiplication by a triangular matrix on one or the other side.

static void
scm_matrix_trmm_left_lower_notrans (CBLAS_DIAG_t Diag,
                                    size_t m, size_t n,
                                    SCM A[m][m], SCM B[m][n])
{
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        SCM sum = B[m - i - 1][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[m - i - 1][m - i - 1]);
        for (size_t q = 0; q < m - i - 1; q++)
          sum = scm_sum (sum, scm_product (B[q][j], A[m - i - 1][q]));
        B[m - i - 1][j] = sum;
      }
}

static void
scm_matrix_trmm_left_lower_trans (CBLAS_DIAG_t Diag,
                                  size_t m, size_t n, SCM A[m][m], SCM B[m][n])
{
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        SCM sum = B[i][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[i][i]);
        for (size_t q = i + 1; q < m; q++)
          sum = scm_sum (sum, scm_product (B[q][j], A[q][i]));
        B[i][j] = sum;
      }
}

static void
scm_matrix_trmm_left_upper_notrans (CBLAS_DIAG_t Diag,
                                    size_t m, size_t n,
                                    SCM A[m][m], SCM B[m][n])
{
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        SCM sum = B[i][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[i][i]);
        for (size_t q = i + 1; q < m; q++)
          sum = scm_sum (sum, scm_product (B[q][j], A[i][q]));
        B[i][j] = sum;
      }
}

static void
scm_matrix_trmm_left_upper_trans (CBLAS_DIAG_t Diag,
                                  size_t m, size_t n, SCM A[m][m], SCM B[m][n])
{
  for (size_t j = 0; j < n; j++)
    for (size_t i = 0; i < m; i++)
      {
        SCM sum = B[m - i - 1][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[m - i - 1][m - i - 1]);
        for (size_t q = 0; q < m - i - 1; q++)
          sum = scm_sum (sum, scm_product (B[q][j], A[q][m - i - 1]));
        B[m - i - 1][j] = sum;
      }
}

static void
scm_matrix_trmm_right_lower_notrans (CBLAS_DIAG_t Diag,
                                     size_t m, size_t n,
                                     SCM A[n][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        SCM sum = B[i][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[j][j]);
        for (size_t q = j + 1; q < n; q++)
          sum = scm_sum (sum, scm_product (B[i][q], A[q][j]));
        B[i][j] = sum;
      }
}

static void
scm_matrix_trmm_right_lower_trans (CBLAS_DIAG_t Diag,
                                   size_t m, size_t n, SCM A[n][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        SCM sum = B[i][n - j - 1];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[n - j - 1][n - j - 1]);
        for (size_t q = 0; q < n - j - 1; q++)
          sum = scm_sum (sum, scm_product (B[i][q], A[n - j - 1][q]));
        B[i][n - j - 1] = sum;
      }
}

static void
scm_matrix_trmm_right_upper_notrans (CBLAS_DIAG_t Diag,
                                     size_t m, size_t n,
                                     SCM A[n][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        SCM sum = B[i][n - j - 1];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[n - j - 1][n - j - 1]);
        for (size_t q = 0; q < n - j - 1; q++)
          sum = scm_sum (sum, scm_product (B[i][q], A[q][n - j - 1]));
        B[i][n - j - 1] = sum;
      }
}

static void
scm_matrix_trmm_right_upper_trans (CBLAS_DIAG_t Diag,
                                   size_t m, size_t n, SCM A[n][n], SCM B[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      {
        SCM sum = B[i][j];
        if (Diag == CblasNonUnit)
          sum = scm_product (sum, A[j][j]);
        for (size_t q = j + 1; q < n; q++)
          sum = scm_sum (sum, scm_product (B[i][q], A[j][q]));
        B[i][j] = sum;
      }
}

static void
scm_matrix_trmm_left_lower (CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                            size_t m, size_t n, SCM A[m][m], SCM B[m][n])
{
  if (TransA == CblasNoTrans)
    scm_matrix_trmm_left_lower_notrans (Diag, m, n, A, B);
  else
    scm_matrix_trmm_left_lower_trans (Diag, m, n, A, B);
}

static void
scm_matrix_trmm_left_upper (CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                            size_t m, size_t n, SCM A[m][m], SCM B[m][n])
{
  if (TransA == CblasNoTrans)
    scm_matrix_trmm_left_upper_notrans (Diag, m, n, A, B);
  else
    scm_matrix_trmm_left_upper_trans (Diag, m, n, A, B);
}

static void
scm_matrix_trmm_right_lower (CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                             size_t m, size_t n, SCM A[n][n], SCM B[m][n])
{
  if (TransA == CblasNoTrans)
    scm_matrix_trmm_right_lower_notrans (Diag, m, n, A, B);
  else
    scm_matrix_trmm_right_lower_trans (Diag, m, n, A, B);
}

static void
scm_matrix_trmm_right_upper (CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                             size_t m, size_t n, SCM A[n][n], SCM B[m][n])
{
  if (TransA == CblasNoTrans)
    scm_matrix_trmm_right_upper_notrans (Diag, m, n, A, B);
  else
    scm_matrix_trmm_right_upper_trans (Diag, m, n, A, B);
}

static void
scm_matrix_trmm_left (CBLAS_UPLO_t Uplo,
                      CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                      size_t m, size_t n, SCM A[m][m], SCM B[m][n])
{
  if (Uplo == CblasLower)
    scm_matrix_trmm_left_lower (TransA, Diag, m, n, A, B);
  else
    scm_matrix_trmm_left_upper (TransA, Diag, m, n, A, B);
}

static void
scm_matrix_trmm_right (CBLAS_UPLO_t Uplo,
                       CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                       size_t m, size_t n, SCM A[n][n], SCM B[m][n])
{
  if (Uplo == CblasLower)
    scm_matrix_trmm_right_lower (TransA, Diag, m, n, A, B);
  else
    scm_matrix_trmm_right_upper (TransA, Diag, m, n, A, B);
}

VISIBLE void
scm_matrix_trmm (CBLAS_SIDE_t Side, CBLAS_UPLO_t Uplo,
                 CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                 size_t m, size_t n, SCM alpha,
                 SCM
                 A[(Side == CblasLeft) ? m : n][(Side == CblasLeft) ? m : n],
                 SCM B[m][n])
{
  assert (0 < m);
  assert (0 < n);
  assert (Side == CblasLeft || Side == CblasRight);
  assert (Uplo == CblasLower || Uplo == CblasUpper);
  assert (Diag == CblasNonUnit || Diag == CblasUnit);

  // FIXME: We do not yet support conjugate transposes.
  assert (TransA == CblasNoTrans || TransA == CblasTrans);

  if (scm_is_true (scm_zero_p (alpha)))
    scm_matrix_set_zero (m, n, B);
  else
    {
      scm_matrix_scale (m, n, B, alpha);
      if (Side == CblasLeft)
        scm_matrix_trmm_left (Uplo, TransA, Diag, m, n, A, B);
      else
        scm_matrix_trmm_right (Uplo, TransA, Diag, m, n, A, B);
    }
}

//-------------------------------------------------------------------------
//
// Solve triangular linear systems by forward/back substitution.

static void
upper_triangle_no_trans (CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n])
{
  if (Diag == CblasNonUnit)
    x[0] = scm_divide (x[0], A[0][0]);
  for (size_t i = 1; i < n; i++)
    {
      for (size_t j = 0; j < i; j++)
        x[i] = scm_difference (x[i], scm_product (A[i][j], x[j]));
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
upper_triangle_trans (CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n])
{
  const size_t n1 = n - 1;
  if (Diag == CblasNonUnit)
    x[n1] = scm_divide (x[n1], A[n1][n1]);
  for (size_t ni = 1; ni < n; ni++)
    {
      const size_t i = n1 - ni;
      for (size_t nj = 0; nj < ni; nj++)
        {
          const size_t j = n1 - nj;
          x[i] = scm_difference (x[i], scm_product (A[j][i], x[j]));
        }
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
lower_triangle_no_trans (CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n])
{
  const size_t n1 = n - 1;
  if (Diag == CblasNonUnit)
    x[n1] = scm_divide (x[n1], A[n1][n1]);
  for (size_t ni = 1; ni < n; ni++)
    {
      const size_t i = n1 - ni;
      for (size_t nj = 0; nj < ni; nj++)
        {
          const size_t j = n1 - nj;
          x[i] = scm_difference (x[i], scm_product (A[i][j], x[j]));
        }
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
lower_triangle_trans (CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n])
{
  if (Diag == CblasNonUnit)
    x[0] = scm_divide (x[0], A[0][0]);
  for (size_t i = 1; i < n; i++)
    {
      for (size_t j = 0; j < i; j++)
        x[i] = scm_difference (x[i], scm_product (A[j][i], x[j]));
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

VISIBLE void
scm_matrix_trsv (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                 CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n])
{
  assert (0 < n);
  assert (Uplo == CblasLower || Uplo == CblasUpper);

  // FIXME: We do not yet support conjugate transposes.
  assert (TransA == CblasNoTrans || TransA == CblasTrans);

  if (Uplo == CblasLower)
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

// In-place multiplication by a diagonal matrix (represented as a
// vector). @var{Side} says whether the diagonal matrix is on the left
// or the right side of the multiplication.
VISIBLE void
scm_matrix_mul_diagonal (CBLAS_SIDE_t Side, size_t m, size_t n,
                         SCM A[m][n], SCM x[(Side == CblasLeft) ? m : n])
{
  assert (Side == CblasLeft || Side == CblasRight);

  if (Side == CblasLeft)
    // Scale the rows of A.
    for (size_t i = 0; i < m; i++)
      for (size_t j = 0; j < n; j++)
        A[i][j] = scm_product (A[i][j], x[i]);
  else
    // Scale the columns of A.
    for (size_t j = 0; j < n; j++)
      for (size_t i = 0; i < m; i++)
        A[i][j] = scm_product (A[i][j], x[j]);
}

//-------------------------------------------------------------------------
