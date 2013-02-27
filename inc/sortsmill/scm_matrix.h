/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_SCM_MATRIX_H
#define _SORTSMILL_SCM_MATRIX_H

#include <libguile.h>
#include <sortsmill/c_version.h>
#include <sortsmill/transmatrix.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#if _FF_C99_OR_GREATER

void scm_matrix_set_all (unsigned int m, unsigned int n, SCM A[m][n], SCM x);
void scm_matrix_set_zero (unsigned int m, unsigned int n, SCM A[m][n]);
void scm_matrix_set_identity (unsigned int m, unsigned int n, SCM A[m][n]);

void scm_matrix_memcpy (unsigned int m, unsigned int n,
                        SCM result[m][n], SCM A[m][n]);
void scm_matrix_swap (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n]);

/* Row and column swapping, in place. */
void scm_matrix_swap_rows (unsigned int m, unsigned int n, SCM A[m][n],
                           unsigned int i, unsigned int j);
void scm_matrix_swap_columns (unsigned int m, unsigned int n, SCM A[m][n],
                              unsigned int i, unsigned int j);
void scm_matrix_swap_rowcol (unsigned int m, SCM A[m][m],
                             unsigned int i, unsigned int j);

/* Matrix transposition, not in place. */
void scm_matrix_transpose_memcpy (unsigned int m, unsigned int n,
                                  SCM result[n][m], SCM A[m][n]);

/* Matrix scaling, in place. */
void scm_matrix_scale (unsigned int m, unsigned int n, SCM A[m][n], SCM x);

/* General matrix multiplication.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
scm_matrix_gemm (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                 unsigned int m, unsigned int n, unsigned int k,
                 SCM alpha,
                 SCM _FF_TRANSMATRIX (A, TransA, m, k),
                 SCM _FF_TRANSMATRIX (B, TransB, k, n), SCM beta, SCM C[m][n]);

void scm_matrix_mul_elements (unsigned int m, unsigned int n,
                              SCM A[m][n], SCM B[m][n]);
void scm_matrix_div_elements (unsigned int m, unsigned int n,
                              SCM A[m][n], SCM B[m][n]);

void scm_matrix_add (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n]);
void scm_matrix_sub (unsigned int m, unsigned int n, SCM A[m][n], SCM B[m][n]);
void scm_matrix_add_constant (unsigned int m, unsigned int n,
                              SCM A[m][n], SCM x);

bool scm_matrix_isnull (unsigned int m, unsigned int n, SCM A[m][n]);
bool scm_matrix_ispos (unsigned int m, unsigned int n, SCM A[m][n]);
bool scm_matrix_isneg (unsigned int m, unsigned int n, SCM A[m][n]);
bool scm_matrix_isnonneg (unsigned int m, unsigned int n, SCM A[m][n]);
bool scm_matrix_equal (unsigned int m, unsigned int n, SCM A[m][n],
                       SCM B[m][n]);

/* Solve triangular systems. */
void scm_matrix_trsv (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                      CBLAS_DIAG_t Diag, unsigned int n, SCM A[n][n], SCM x[n]);

/* LU decomposition. */
void scm_linalg_LU_decomp (unsigned int n, SCM A[n][n],
                           unsigned int permutation[n], int *signum);
void scm_linalg_LU_solve (unsigned int n, SCM LU[n][n], unsigned int p[n],
                          SCM b[n], SCM x[n]);
void scm_linalg_LU_svx (unsigned int n, SCM LU[n][n],
                        unsigned int permutation[n], SCM x[n]);
void scm_linalg_LU_refine (unsigned int n, SCM A[n][n], SCM LU[n][n],
                           unsigned int permutation[n], SCM b[n], SCM x[n],
                           SCM residual[n]);
void scm_linalg_LU_invert (unsigned int n, SCM LU[n][n],
                           unsigned int permutation[n], SCM inverse[n][n]);
SCM scm_linalg_LU_det (unsigned int n, SCM LU[n][n], int signum);
SCM scm_linalg_LU_lndet (unsigned int n, SCM LU[n][n]);
int scm_linalg_LU_sgndet (unsigned int n, SCM LU[n][n], int signum);

#endif /* _FF_C99_OR_GREATER */

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_SCM_MATRIX_H */
