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

#include <sortsmill/scm_matrix.h>
#include <assert.h>

VISIBLE void
scm_matrix_set_all (unsigned int m, unsigned int n, SCM A[m][n], SCM x)
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = x;
}

VISIBLE void
scm_matrix_set_zero (unsigned int m, unsigned int n, SCM A[m][n])
{
  scm_matrix_set_all (m, n, A, scm_from_int (0));
}

VISIBLE void
scm_matrix_set_identity (unsigned int m, unsigned int n, SCM A[m][n])
{
  SCM zero = scm_from_int (0);
  SCM one = scm_from_int (1);
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = (i == j) ? one : zero;
}

VISIBLE void
scm_matrix_memcpy (unsigned int m, unsigned int n,
                   SCM result[m][n], SCM A[m][n])
{
  memcpy (&result[0][0], &A[0][0], m * n * sizeof (SCM));
}

VISIBLE void
scm_matrix_swap (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        SCM x = A[i][j];
        A[i][j] = B[i][j];
        B[i][j] = x;
      }
}

VISIBLE void
scm_matrix_swap_rows (unsigned int m, unsigned int n,
                      SCM A[m][n], unsigned int i, unsigned int j)
{
  if (i != j)
    for (unsigned int p = 0; p < n; p++)
      {
        SCM x = A[i][p];
        A[i][p] = A[j][p];
        A[j][p] = x;
      }
}

VISIBLE void
scm_matrix_swap_columns (unsigned int m, unsigned int n,
                         SCM A[m][n], unsigned int i, unsigned int j)
{
  if (i != j)
    for (unsigned int p = 0; p < m; p++)
      {
        SCM x = A[p][i];
        A[p][i] = A[p][j];
        A[p][j] = x;
      }
}

VISIBLE void
scm_matrix_swap_rowcol (unsigned int m,
                        SCM A[m][m], unsigned int i, unsigned int j)
{
  for (unsigned int p = 0; p < m; p++)
    {
      SCM x = A[i][p];
      A[i][p] = A[p][j];
      A[p][j] = x;
    }
}

VISIBLE void
scm_matrix_transpose_memcpy (unsigned int m, unsigned int n,
                             SCM result[n][m], SCM A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      result[j][i] = A[i][j];
}

VISIBLE void
scm_matrix_scale (unsigned int m, unsigned int n, SCM A[m][n], SCM x)
{
  if (scm_is_true (scm_zero_p (x)))
    scm_matrix_set_zero (m, n, A);
  else if (scm_is_false (scm_zero_p (scm_oneminus (x))))
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        A[i][j] = scm_product (A[i][j], x);
}

_GL_ATTRIBUTE_CONST static inline unsigned int
_trans_row (CBLAS_TRANSPOSE_t trans, unsigned int i, unsigned int j)
{
  return (trans == CblasNoTrans) ? i : j;
}

_GL_ATTRIBUTE_CONST static inline unsigned int
_trans_col (CBLAS_TRANSPOSE_t trans, unsigned int i, unsigned int j)
{
  return (trans == CblasNoTrans) ? j : i;
}

static inline void
scm_matmul_alpha1_beta1 (CBLAS_TRANSPOSE_t TransA,
                         CBLAS_TRANSPOSE_t TransB,
                         unsigned int m, unsigned int n, unsigned int k,
                         SCM _FF_TRANSMATRIX (A, TransA, m, k),
                         SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM C[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        for (unsigned int p = 0; p < k; p++)
          {
            const unsigned int rA = _trans_row (TransA, i, p);
            const unsigned int cA = _trans_col (TransA, i, p);
            const unsigned int rB = _trans_row (TransB, p, j);
            const unsigned int cB = _trans_col (TransB, p, j);
            C[i][j] = scm_sum (C[i][j], scm_product (A[rA][cA], B[rB][cB]));
          }
      }
}

static inline void
scm_matmul_beta1 (CBLAS_TRANSPOSE_t TransA,
                  CBLAS_TRANSPOSE_t TransB,
                  unsigned int m, unsigned int n, unsigned int k,
                  const SCM alpha,
                  SCM _FF_TRANSMATRIX (A, TransA, m, k),
                  SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM C[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        for (unsigned int p = 0; p < k; p++)
          {
            const unsigned int rA = _trans_row (TransA, i, p);
            const unsigned int cA = _trans_col (TransA, i, p);
            const unsigned int rB = _trans_row (TransB, p, j);
            const unsigned int cB = _trans_col (TransB, p, j);
            C[i][j] = scm_sum (C[i][j],
                               scm_product (scm_product (A[rA][cA], B[rB][cB]),
                                            alpha));
          }
      }
}

VISIBLE void
scm_matrix_gemm (CBLAS_TRANSPOSE_t TransA,
                 CBLAS_TRANSPOSE_t TransB,
                 unsigned int m, unsigned int n, unsigned int k,
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

VISIBLE void
scm_matrix_mul_elements (unsigned int m, unsigned int n,
                         SCM A[m][n], SCM B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = scm_product (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_div_elements (unsigned int m, unsigned int n,
                         SCM A[m][n], SCM B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = scm_divide (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_add (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = scm_sum (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_sub (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = scm_difference (A[i][j], B[i][j]);
}

VISIBLE void
scm_matrix_add_constant (unsigned int m, unsigned int n, SCM A[m][n], SCM x)
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      A[i][j] = scm_sum (A[i][j], x);
}

#define _FF_SCM_ELEMENTWISE_PRED(NAME, TRUTH, ELEMENT_PRED)     \
  bool                                                          \
  NAME (unsigned int m, unsigned int n, SCM A[m][n])            \
  {                                                             \
    bool result = true;                                         \
    for (unsigned int i = 0; i < m; i++)                        \
      for (unsigned int j = 0; j < n; j++)                      \
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
scm_matrix_equal (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n])
{
  bool result = true;
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
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
//
// Solve triangular linear systems by forward/back substitution.

static void
upper_triangle_no_trans (CBLAS_DIAG_t Diag,
                         unsigned int n, SCM A[n][n], SCM x[n])
{
  if (Diag == CblasNonUnit)
    x[0] = scm_divide (x[0], A[0][0]);
  for (unsigned int i = 1; i < n; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        x[i] = scm_difference (x[i], scm_product (A[i][j], x[j]));
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
upper_triangle_trans (CBLAS_DIAG_t Diag, unsigned int n, SCM A[n][n], SCM x[n])
{
  const unsigned int n1 = n - 1;
  if (Diag == CblasNonUnit)
    x[n1] = scm_divide (x[n1], A[n1][n1]);
  for (unsigned int ni = 1; ni < n; ni++)
    {
      const unsigned int i = n1 - ni;
      for (unsigned int nj = 0; nj < ni; nj++)
        {
          const unsigned int j = n1 - nj;
          x[i] = scm_difference (x[i], scm_product (A[j][i], x[j]));
        }
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
lower_triangle_no_trans (CBLAS_DIAG_t Diag,
                         unsigned int n, SCM A[n][n], SCM x[n])
{
  const unsigned int n1 = n - 1;
  if (Diag == CblasNonUnit)
    x[n1] = scm_divide (x[n1], A[n1][n1]);
  for (unsigned int ni = 1; ni < n; ni++)
    {
      const unsigned int i = n1 - ni;
      for (unsigned int nj = 0; nj < ni; nj++)
        {
          const unsigned int j = n1 - nj;
          x[i] = scm_difference (x[i], scm_product (A[i][j], x[j]));
        }
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

static void
lower_triangle_trans (CBLAS_DIAG_t Diag, unsigned int n, SCM A[n][n], SCM x[n])
{
  if (Diag == CblasNonUnit)
    x[0] = scm_divide (x[0], A[0][0]);
  for (unsigned int i = 1; i < n; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        x[i] = scm_difference (x[i], scm_product (A[j][i], x[j]));
      if (Diag == CblasNonUnit)
        x[i] = scm_divide (x[i], A[i][i]);
    }
}

VISIBLE void
scm_matrix_trsv (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                 CBLAS_DIAG_t Diag, unsigned int n, SCM A[n][n], SCM x[n])
{
  assert (0 < n);
  assert (Uplo == CblasLower || Uplo == CblasUpper);

  // FIXME: We do not yet support conjugate transposes.
  assert (TransA == CblasNoTrans || TransA == CblasTrans);

  if (Uplo == CblasLower)
    if (TransA == CblasNoTrans)
      upper_triangle_no_trans (Diag, n, A, x);
    else
      upper_triangle_trans (Diag, n, A, x);
  else if (TransA == CblasNoTrans)
    lower_triangle_no_trans (Diag, n, A, x);
  else
    lower_triangle_trans (Diag, n, A, x);
}

//-------------------------------------------------------------------------