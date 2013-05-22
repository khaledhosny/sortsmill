/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_MATH_SCM_MATRIX_H
#define _SORTSMILL_MATH_SCM_MATRIX_H

#include <libguile.h>
#include <sortsmill/c_version.h>
#include <sortsmill/math/transmatrix.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#if _FF_C99_OR_GREATER

void scm_matrix_set_all (size_t m, size_t n, SCM A[m][n], SCM x);
void scm_matrix_set_zero (size_t m, size_t n, SCM A[m][n]);
void scm_matrix_set_identity (size_t m, size_t n, SCM A[m][n]);

void scm_matrix_memcpy (size_t m, size_t n, SCM result[m][n], SCM A[m][n]);
void scm_matrix_swap (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);

/* Row and column swapping, in place. */
void scm_matrix_swap_rows (size_t m, size_t n, SCM A[m][n], size_t i, size_t j);
void scm_matrix_swap_columns (size_t m, size_t n, SCM A[m][n],
                              size_t i, size_t j);
void scm_matrix_swap_rowcol (size_t m, SCM A[m][m], size_t i, size_t j);

/* Matrix transposition, not in place. */
void scm_matrix_transpose_memcpy (size_t m, size_t n,
                                  SCM result[n][m], SCM A[m][n]);

/* Matrix scaling, in place. */
void scm_matrix_scale (size_t m, size_t n, SCM A[m][n], SCM x);

void scm_matrix_mul_elements (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);
void scm_matrix_div_elements (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);

void scm_matrix_add (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);
void scm_matrix_sub (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);
void scm_matrix_add_constant (size_t m, size_t n, SCM A[m][n], SCM x);

bool scm_matrix_isnull (size_t m, size_t n, SCM A[m][n]);
bool scm_matrix_ispos (size_t m, size_t n, SCM A[m][n]);
bool scm_matrix_isneg (size_t m, size_t n, SCM A[m][n]);
bool scm_matrix_isnonneg (size_t m, size_t n, SCM A[m][n]);
bool scm_matrix_equal (size_t m, size_t n, SCM A[m][n], SCM B[m][n]);

/* Multiply by a diagonal matrix that is represented by a vector. */
void scm_matrix_mul_diagonal (CBLAS_SIDE_t Side,
                              size_t m, size_t n,
                              SCM A[m][n], SCM x[(Side == CblasLeft) ? m : n]);

/* General matrix multiplication; based on BLAS Level 3 xGEMM.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
scm_matrix_gemm (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                 size_t m, size_t n, size_t k,
                 SCM alpha,
                 SCM _FF_TRANSMATRIX (A, TransA, m, k),
                 SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM beta, SCM C[m][n]);

/* Triangular matrix multiplication; based on BLAS Level 3 xTRMM. */
void
scm_matrix_trmm (CBLAS_SIDE_t Side, CBLAS_UPLO_t Uplo,
                 CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                 size_t m, size_t n, SCM alpha,
                 SCM
                 A[(Side == CblasLeft) ? m : n][(Side == CblasLeft) ? m : n],
                 SCM B[m][n]);

/* Solve triangular systems; based on BLAS Level 2 xTRSV. */
void scm_matrix_trsv (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                      CBLAS_DIAG_t Diag, size_t n, SCM A[n][n], SCM x[n]);

/* LU decomposition. The permutation vectors here are size_t arrays
   for compatibility with gsl_permutation. */
void scm_linalg_LU_decomp (size_t n, SCM A[n][n], size_t p[n], int *signum);
void scm_linalg_LU_solve (size_t n, SCM LU[n][n], size_t p[n],
                          SCM b[n], SCM x[n]);
void scm_linalg_LU_svx (size_t n, SCM LU[n][n], size_t p[n], SCM x[n]);
void scm_linalg_LU_refine (size_t n, SCM A[n][n], SCM LU[n][n],
                           size_t p[n], SCM b[n], SCM x[n], SCM residual[n]);
void scm_linalg_LU_invert (size_t n, SCM LU[n][n], size_t p[n],
                           SCM inverse[n][n]);
SCM scm_linalg_LU_det (size_t n, SCM LU[n][n], int signum);
SCM scm_linalg_LU_lndet (size_t n, SCM LU[n][n]);
int scm_linalg_LU_sgndet (size_t n, SCM LU[n][n], int signum);

#endif /* _FF_C99_OR_GREATER */

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_SCM_MATRIX_H */
