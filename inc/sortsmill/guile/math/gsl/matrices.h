/*
 * Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_MATH_GSL_MATRICES_H
#define _SORTSMILL_GUILE_MATH_GSL_MATRICES_H

#include <libguile.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <sortsmill/guile/arrays.h>
#include <sortsmill/c_version.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void assert_rank_1_or_2_array (SCM who, SCM array);
void assert_c_rank_1_or_2_array (const char *who, SCM array,
                                 scm_t_array_handle *handlep);
void scm_array_handle_get_vector_dim_and_stride (const char *who,
                                                 SCM vector,
                                                 scm_t_array_handle *handlep,
                                                 size_t *dim, ssize_t *stride);

gsl_vector_const_view scm_gsl_vector_const_view_array_handle (SCM array,
                                                              scm_t_array_handle
                                                              *handlep);
gsl_vector_view scm_gsl_vector_view_array_handle (SCM array,
                                                  scm_t_array_handle *handlep);

gsl_matrix_const_view scm_gsl_matrix_const_view_array_handle (SCM array,
                                                              scm_t_array_handle
                                                              *handlep);
gsl_matrix_view scm_gsl_matrix_view_array_handle (SCM array,
                                                  scm_t_array_handle *handlep);

SCM scm_gsl_vector_to_f64vector (const gsl_vector *v, int low_index);
SCM scm_gsl_matrix_to_f64matrix (const gsl_matrix *m, int low_index);

#if _FF_C99_OR_GREATER

void scm_array_handle_to_mpz_matrix (SCM array, scm_t_array_handle *handlep,
                                     size_t m, size_t n, mpz_t A[m][n]);
void scm_array_handle_to_mpq_matrix (SCM array, scm_t_array_handle *handlep,
                                     size_t m, size_t n, mpq_t A[m][n]);
void scm_array_handle_to_scm_matrix (SCM array, scm_t_array_handle *handlep,
                                     size_t m, size_t n, SCM A[m][n]);

void scm_array_handle_to_transposed_mpz_matrix (SCM array,
                                                scm_t_array_handle *handlep,
                                                size_t m, size_t n,
                                                mpz_t A[n][m]);
void scm_array_handle_to_transposed_mpq_matrix (SCM array,
                                                scm_t_array_handle *handlep,
                                                size_t m, size_t n,
                                                mpq_t A[n][m]);
void scm_array_handle_to_transposed_scm_matrix (SCM array,
                                                scm_t_array_handle *handlep,
                                                size_t m, size_t n,
                                                SCM A[n][m]);

SCM scm_from_mpz_matrix (size_t m, size_t n, mpz_t A[m][n]);
SCM scm_from_mpq_matrix (size_t m, size_t n, mpq_t A[m][n]);
SCM scm_from_scm_matrix (size_t m, size_t n, SCM A[m][n]);

SCM scm_from_transposed_mpz_matrix (size_t m, size_t n, mpz_t A[n][m]);
SCM scm_from_transposed_mpq_matrix (size_t m, size_t n, mpq_t A[n][m]);
SCM scm_from_transposed_scm_matrix (size_t m, size_t n, SCM A[n][m]);

#endif /* _FF_C99_OR_GREATER */

/* Unlike their f64 counterpart, the following three routines accept
   row or column matrices as vectors. FIXME: Make the f64 counterpart
   accept them, too. */
void scm_array_handle_to_mpz_vector (SCM array, scm_t_array_handle *handlep,
                                     size_t n, mpz_t A[n]);
void scm_array_handle_to_mpq_vector (SCM array, scm_t_array_handle *handlep,
                                     size_t n, mpq_t A[n]);
void scm_array_handle_to_scm_vector (SCM array, scm_t_array_handle *handlep,
                                     size_t n, SCM A[n]);

SCM scm_from_mpz_vector (size_t n, mpz_t v[n]);
SCM scm_from_mpq_vector (size_t n, mpq_t v[n]);
SCM scm_from_scm_vector (size_t n, SCM v[n]);

size_t scm_matrix_dim1 (scm_t_array_handle *handlep);
size_t scm_matrix_dim2 (scm_t_array_handle *handlep);

SCM scm_gsl_matrix_scale (SCM A, SCM x);
SCM scm_gsl_mpz_matrix_scale (SCM A, SCM x);
SCM scm_gsl_mpq_matrix_scale (SCM A, SCM x);
SCM scm_gsl_scm_matrix_scale (SCM A, SCM x);

SCM scm_gsl_blas_dgemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                        SCM beta, SCM C);
SCM scm_gsl_mpz_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                      SCM beta, SCM C);
SCM scm_gsl_mpq_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                      SCM beta, SCM C);
SCM scm_gsl_scm_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                      SCM beta, SCM C);

SCM scm_gsl_blas_dtrmm (SCM Side, SCM Uplo, SCM TransA, SCM Diag, SCM alpha,
                        SCM A, SCM B);
SCM scm_gsl_mpz_trmm (SCM Side, SCM Uplo, SCM TransA, SCM Diag, SCM alpha,
                      SCM A, SCM B);
SCM scm_gsl_mpq_trmm (SCM Side, SCM Uplo, SCM TransA, SCM Diag, SCM alpha,
                      SCM A, SCM B);
SCM scm_gsl_scm_trmm (SCM Side, SCM Uplo, SCM TransA, SCM Diag, SCM alpha,
                      SCM A, SCM B);

SCM scm_gsl_matrix_add (SCM A, SCM B);
SCM scm_gsl_mpz_matrix_add (SCM A, SCM B);
SCM scm_gsl_mpq_matrix_add (SCM A, SCM B);
SCM scm_gsl_scm_matrix_add (SCM A, SCM B);

SCM scm_gsl_matrix_sub (SCM A, SCM B);
SCM scm_gsl_mpz_matrix_sub (SCM A, SCM B);
SCM scm_gsl_mpq_matrix_sub (SCM A, SCM B);
SCM scm_gsl_scm_matrix_sub (SCM A, SCM B);

SCM scm_gsl_matrix_mul_elements (SCM A, SCM B);
SCM scm_gsl_mpz_matrix_mul_elements (SCM A, SCM B);
SCM scm_gsl_mpq_matrix_mul_elements (SCM A, SCM B);
SCM scm_gsl_scm_matrix_mul_elements (SCM A, SCM B);

SCM scm_gsl_matrix_div_elements (SCM A, SCM B);
SCM scm_gsl_mpq_matrix_div_elements (SCM A, SCM B);
SCM scm_gsl_scm_matrix_div_elements (SCM A, SCM B);

bool scm_c_gsl_matrix_isnull (SCM);
SCM scm_gsl_matrix_isnull_p (SCM);
bool scm_c_gsl_mpz_matrix_isnull (SCM);
SCM scm_gsl_mpz_matrix_isnull_p (SCM);
bool scm_c_gsl_mpq_matrix_isnull (SCM);
SCM scm_gsl_mpq_matrix_isnull_p (SCM);
bool scm_c_gsl_scm_matrix_isnull (SCM);
SCM scm_gsl_scm_matrix_isnull_p (SCM);

bool scm_c_gsl_matrix_isneg (SCM);
SCM scm_gsl_matrix_isneg_p (SCM);
bool scm_c_gsl_mpz_matrix_isneg (SCM);
SCM scm_gsl_mpz_matrix_isneg_p (SCM);
bool scm_c_gsl_mpq_matrix_isneg (SCM);
SCM scm_gsl_mpq_matrix_isneg_p (SCM);
bool scm_c_gsl_scm_matrix_isneg (SCM);
SCM scm_gsl_scm_matrix_isneg_p (SCM);

bool scm_c_gsl_matrix_ispos (SCM);
SCM scm_gsl_matrix_ispos_p (SCM);
bool scm_c_gsl_mpz_matrix_ispos (SCM);
SCM scm_gsl_mpz_matrix_ispos_p (SCM);
bool scm_c_gsl_mpq_matrix_ispos (SCM);
SCM scm_gsl_mpq_matrix_ispos_p (SCM);
bool scm_c_gsl_scm_matrix_ispos (SCM);
SCM scm_gsl_scm_matrix_ispos_p (SCM);

bool scm_c_gsl_matrix_isnonneg (SCM);
SCM scm_gsl_matrix_isnonneg_p (SCM);
bool scm_c_gsl_mpz_matrix_isnonneg (SCM);
SCM scm_gsl_mpz_matrix_isnonneg_p (SCM);
bool scm_c_gsl_mpq_matrix_isnonneg (SCM);
SCM scm_gsl_mpq_matrix_isnonneg_p (SCM);
bool scm_c_gsl_scm_matrix_isnonneg (SCM);
SCM scm_gsl_scm_matrix_isnonneg_p (SCM);

bool scm_c_gsl_matrix_equal (SCM, SCM);
SCM scm_gsl_matrix_equal_p (SCM, SCM);
bool scm_c_gsl_mpz_matrix_equal (SCM, SCM);
SCM scm_gsl_mpz_matrix_equal_p (SCM, SCM);
bool scm_c_gsl_mpq_matrix_equal (SCM, SCM);
SCM scm_gsl_mpq_matrix_equal_p (SCM, SCM);
bool scm_c_gsl_scm_matrix_equal (SCM, SCM);
SCM scm_gsl_scm_matrix_equal_p (SCM, SCM);

SCM scm_gsl_linalg_SVD_golub_reinsch (SCM);
SCM scm_gsl_linalg_SVD_modified_golub_reinsch (SCM);
SCM scm_gsl_linalg_SVD_jacobi (SCM);
SCM scm_gsl_linalg_SVD_solve (SCM U, SCM S, SCM V, SCM B);

SCM scm_gsl_linalg_LU_decomp (SCM A);
SCM scm_gsl_linalg_LU_solve (SCM LU, SCM permutation, SCM B);
SCM scm_gsl_linalg_LU_invert (SCM LU, SCM permutation);

SCM scm_gsl_mpq_linalg_LU_decomp (SCM A);
SCM scm_gsl_mpq_linalg_LU_decomp_fast_pivot (SCM A);
SCM scm_gsl_mpq_linalg_LU_solve (SCM LU, SCM permutation, SCM B);
SCM scm_gsl_mpq_linalg_LU_invert (SCM LU, SCM permutation);

SCM scm_gsl_scm_linalg_LU_decomp (SCM A);
SCM scm_gsl_scm_linalg_LU_solve (SCM LU, SCM permutation, SCM B);
SCM scm_gsl_scm_linalg_LU_invert (SCM LU, SCM permutation);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_GSL_MATRICES_H */
