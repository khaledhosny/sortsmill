/* -*- coding: utf-8 -*- Some of the comments below are in UTF-8. */
/*
 * Copyright (C) 2012, 2013 Barry Schwartz
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

#ifndef _SORTSMILL_GMP_MATRIX_H
#define _SORTSMILL_GMP_MATRIX_H

/*
 * FIXME FIXME FIXME: We need tests for the C90 versions.
 */

#include <gmp.h>
#include <gsl/gsl_blas.h>
#include <libguile.h>
#include <sortsmill/c_version.h>

#define _FF_TRANSMATRIX(A,T,I,J) A[((T) == CblasNoTrans ? (I) : (J))][((T) == CblasNoTrans ? (J) : (I))]
#define _FF_TRANSMATRIX_CAST(TYPE,T,I,J) (TYPE (*)[(unsigned int) ((T) == CblasNoTrans ? (J) : (I))])

/*-------------------------------------------------------------------------
 *
 * GMP init and clear of the matrix entries.
 */

void scm_dynwind_mpz_matrix_unwind_handler (void *);
void scm_dynwind_mpq_matrix_unwind_handler (void *);

#if _FF_C99_OR_GREATER

void mpz_matrix_init (unsigned int m, unsigned int n, mpz_t A[m][n]);
void mpz_matrix_clear (unsigned int m, unsigned int n, mpz_t A[m][n]);
void scm_dynwind_mpz_matrix_clear (unsigned int m, unsigned int n,
                                   mpz_t A[m][n]);

void mpq_matrix_init (unsigned int m, unsigned int n, mpq_t A[m][n]);
void mpq_matrix_clear (unsigned int m, unsigned int n, mpq_t A[m][n]);
void scm_dynwind_mpq_matrix_clear (unsigned int m, unsigned int n,
                                   mpq_t A[m][n]);

#endif /* _FF_C99_OR_GREATER */

void mpz_matrix_init_c90 (unsigned int m, unsigned int n, mpz_t A[]);
void mpz_matrix_clear_c90 (unsigned int m, unsigned int n, mpz_t A[]);
void scm_dynwind_mpz_matrix_clear_c90 (unsigned int m, unsigned int n,
                                       mpz_t A[]);

void mpq_matrix_init_c90 (unsigned int m, unsigned int n, mpq_t A[]);
void mpq_matrix_clear_c90 (unsigned int m, unsigned int n, mpq_t A[]);
void scm_dynwind_mpq_matrix_clear_c90 (unsigned int m, unsigned int n,
                                       mpq_t A[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

void mpz_matrix_set_all (unsigned int m, unsigned int n, mpz_t A[m][n],
                         const mpz_t x);
void mpz_matrix_set_zero (unsigned int m, unsigned int n, mpz_t A[m][n]);
void mpz_matrix_set_identity (unsigned int m, unsigned int n, mpz_t A[m][n]);

void mpq_matrix_set_all (unsigned int m, unsigned int n, mpq_t A[m][n],
                         const mpq_t x);
void mpq_matrix_set_zero (unsigned int m, unsigned int n, mpq_t A[m][n]);
void mpq_matrix_set_identity (unsigned int m, unsigned int n, mpq_t A[m][n]);

#endif /* _FF_C99_OR_GREATER */

void mpz_matrix_set_all_c90 (unsigned int m, unsigned int n, mpz_t A[],
                             const mpz_t x);
void mpz_matrix_set_zero_c90 (unsigned int m, unsigned int n, mpz_t A[]);
void mpz_matrix_set_identity_c90 (unsigned int m, unsigned int n, mpz_t A[]);

void mpq_matrix_set_all_c90 (unsigned int m, unsigned int n, mpq_t A[],
                             const mpq_t x);
void mpq_matrix_set_zero_c90 (unsigned int m, unsigned int n, mpq_t A[]);
void mpq_matrix_set_identity_c90 (unsigned int m, unsigned int n, mpq_t A[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

void mpz_matrix_memcpy (unsigned int m, unsigned int n,
                        mpz_t result[m][n], mpz_t A[m][n]);
void mpz_matrix_swap (unsigned int m, unsigned int n,
                      mpz_t A[m][n], mpz_t B[m][n]);

void mpq_matrix_memcpy (unsigned int m, unsigned int n,
                        mpq_t result[m][n], mpq_t A[m][n]);
void mpq_matrix_swap (unsigned int m, unsigned int n,
                      mpq_t A[m][n], mpq_t B[m][n]);

#endif /* _FF_C99_OR_GREATER */

void mpz_matrix_memcpy_c90 (unsigned int m, unsigned int n,
                            mpz_t result[], mpz_t A[]);
void mpz_matrix_swap_c90 (unsigned int m, unsigned int n, mpz_t A[], mpz_t B[]);

void mpq_matrix_memcpy_c90 (unsigned int m, unsigned int n,
                            mpq_t result[], mpq_t A[]);
void mpq_matrix_swap_c90 (unsigned int m, unsigned int n, mpq_t A[], mpq_t B[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

/* Row and column swapping, in place. */
void mpz_matrix_swap_rows (unsigned int m, unsigned int n, mpz_t A[m][n],
                           unsigned int i, unsigned int j);
void mpz_matrix_swap_columns (unsigned int m, unsigned int n, mpz_t A[m][n],
                              unsigned int i, unsigned int j);
void mpz_matrix_swap_rowcol (unsigned int m, mpz_t A[m][m],
                             unsigned int i, unsigned int j);

/* Matrix transposition, not in place. */
void mpz_matrix_transpose_memcpy (unsigned int m, unsigned int n,
                                  mpz_t result[n][m], mpz_t A[m][n]);

/* Row and column swapping, in place. */
void mpq_matrix_swap_rows (unsigned int m, unsigned int n, mpq_t A[m][n],
                           unsigned int i, unsigned int j);
void mpq_matrix_swap_columns (unsigned int m, unsigned int n, mpq_t A[m][n],
                              unsigned int i, unsigned int j);
void mpq_matrix_swap_rowcol (unsigned int m, mpq_t A[m][m],
                             unsigned int i, unsigned int j);

/* Matrix transposition, not in place. */
void mpq_matrix_transpose_memcpy (unsigned int m, unsigned int n,
                                  mpq_t result[n][m], mpq_t A[m][n]);

#endif /* _FF_C99_OR_GREATER */

/* Row and column swapping, in place. */
void mpz_matrix_swap_rows_c90 (unsigned int m, unsigned int n, mpz_t A[],
                               unsigned int i, unsigned int j);
void mpz_matrix_swap_columns_c90 (unsigned int m, unsigned int n, mpz_t A[],
                                  unsigned int i, unsigned int j);
void mpz_matrix_swap_rowcol_c90 (unsigned int m, mpz_t A[],
                                 unsigned int i, unsigned int j);

/* Matrix transposition, not in place. */
void mpz_matrix_transpose_memcpy_c90 (unsigned int m, unsigned int n,
                                      mpz_t result[], mpz_t A[]);

/* Row and column swapping, in place. */
void mpq_matrix_swap_rows_c90 (unsigned int m, unsigned int n, mpq_t A[],
                               unsigned int i, unsigned int j);
void mpq_matrix_swap_columns_c90 (unsigned int m, unsigned int n, mpq_t A[],
                                  unsigned int i, unsigned int j);
void mpq_matrix_swap_rowcol_c90 (unsigned int m, mpq_t A[],
                                 unsigned int i, unsigned int j);

/* Matrix transposition, not in place. */
void mpq_matrix_transpose_memcpy_c90 (unsigned int m, unsigned int n,
                                      mpq_t result[], mpq_t A[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

/* Matrix scaling, in place. */
void mpz_matrix_scale (unsigned int m, unsigned int n, mpz_t A[m][n],
                       const mpz_t x);

/* General matrix multiplication.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
mpz_matrix_gemm (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                 unsigned int m, unsigned int n, unsigned int k,
                 const mpz_t alpha,
                 mpz_t _FF_TRANSMATRIX (A, TransA, m, k),
                 mpz_t _FF_TRANSMATRIX (B, TransB, k, n),
                 const mpz_t beta, mpz_t C[m][n]);

/* Matrix scaling, in place. */
void
mpq_matrix_scale (unsigned int m, unsigned int n, mpq_t A[m][n], const mpq_t x);

/* General matrix multiplication.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
mpq_matrix_gemm (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                 unsigned int m, unsigned int n, unsigned int k,
                 const mpq_t alpha,
                 mpq_t _FF_TRANSMATRIX (A, TransA, m, k),
                 mpq_t _FF_TRANSMATRIX (B, TransB, k, n),
                 const mpq_t beta, mpq_t C[m][n]);

#endif /* _FF_C99_OR_GREATER */

/* Matrix scaling, in place. */
void mpz_matrix_scale_c90 (unsigned int m, unsigned int n, mpz_t A[],
                           const mpz_t x);

/* General matrix multiplication.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
mpz_matrix_gemm_c90 (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                     unsigned int m, unsigned int n, unsigned int k,
                     const mpz_t alpha, mpz_t A[], mpz_t B[], const mpz_t beta,
                     mpz_t C[]);

/* Matrix scaling, in place. */
void
mpq_matrix_scale_c90 (unsigned int m, unsigned int n, mpq_t A[], const mpq_t x);

/* General matrix multiplication.
   See http://en.wikipedia.org/wiki/General_Matrix_Multiply */
void
mpq_matrix_gemm_c90 (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                     unsigned int m, unsigned int n, unsigned int k,
                     const mpq_t alpha, mpq_t A[], mpq_t B[], const mpq_t beta,
                     mpq_t C[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

/* In the following operations, the result replaces A. */
void mpz_matrix_mul_elements (unsigned int m, unsigned int n,
                              mpz_t A[m][n], mpz_t B[m][n]);
/* ‘cdiv’ rounds towards +∞ (‘ceiling’). */
void mpz_matrix_cdiv_q_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
/* ‘fdiv’ rounds towards −∞ (‘floor’). */
void mpz_matrix_fdiv_q_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
/* ‘tdiv’ rounds towards zero (‘truncate’). */
void mpz_matrix_tdiv_q_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_cdiv_r_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_fdiv_r_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_tdiv_r_elements (unsigned int m, unsigned int n,
                                 mpz_t A[m][n], mpz_t B[m][n]);
void mpq_matrix_mul_elements (unsigned int m, unsigned int n,
                              mpq_t A[m][n], mpq_t B[m][n]);
void mpq_matrix_div_elements (unsigned int m, unsigned int n,
                              mpq_t A[m][n], mpq_t B[m][n]);

/* In the following operations, the quotient replaces A and the
   remainder replaces B. */
void mpz_matrix_cdiv_qr_elements (unsigned int m, unsigned int n,
                                  mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_fdiv_qr_elements (unsigned int m, unsigned int n,
                                  mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_tdiv_qr_elements (unsigned int m, unsigned int n,
                                  mpz_t A[m][n], mpz_t B[m][n]);

#endif /* _FF_C99_OR_GREATER */

void mpz_matrix_mul_elements_c90 (unsigned int m, unsigned int n,
                                  mpz_t A[], mpz_t B[]);
void mpz_matrix_cdiv_q_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_fdiv_q_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_tdiv_q_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_cdiv_r_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_fdiv_r_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_tdiv_r_elements_c90 (unsigned int m, unsigned int n,
                                     mpz_t A[], mpz_t B[]);
void mpz_matrix_cdiv_qr_elements_c90 (unsigned int m, unsigned int n,
                                      mpz_t A[], mpz_t B[]);
void mpz_matrix_fdiv_qr_elements_c90 (unsigned int m, unsigned int n,
                                      mpz_t A[], mpz_t B[]);
void mpz_matrix_tdiv_qr_elements_c90 (unsigned int m, unsigned int n,
                                      mpz_t A[], mpz_t B[]);
void mpq_matrix_mul_elements_c90 (unsigned int m, unsigned int n,
                                  mpq_t A[], mpq_t B[]);
void mpq_matrix_div_elements_c90 (unsigned int m, unsigned int n,
                                  mpq_t A[], mpq_t B[]);

/*-----------------------------------------------------------------------*/

#if _FF_C99_OR_GREATER

/* Addition/subtraction in-place. The result replaces A. */
void mpz_matrix_add (unsigned int m, unsigned int n,
                     mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_sub (unsigned int m, unsigned int n,
                     mpz_t A[m][n], mpz_t B[m][n]);
void mpz_matrix_add_constant (unsigned int m, unsigned int n,
                              mpz_t A[m][n], const mpz_t x);
void mpq_matrix_add (unsigned int m, unsigned int n,
                     mpq_t A[m][n], mpq_t B[m][n]);
void mpq_matrix_sub (unsigned int m, unsigned int n,
                     mpq_t A[m][n], mpq_t B[m][n]);
void mpq_matrix_add_constant (unsigned int m, unsigned int n,
                              mpq_t A[m][n], const mpq_t x);

#endif /* _FF_C99_OR_GREATER */

void mpz_matrix_add_c90 (unsigned int m, unsigned int n, mpz_t A[], mpz_t B[]);
void mpz_matrix_sub_c90 (unsigned int m, unsigned int n, mpz_t A[], mpz_t B[]);
void mpz_matrix_add_constant_c90 (unsigned int m, unsigned int n, mpz_t A[],
                                  const mpz_t x);
void mpq_matrix_add_c90 (unsigned int m, unsigned int n, mpq_t A[], mpq_t B[]);
void mpq_matrix_sub_c90 (unsigned int m, unsigned int n, mpq_t A[], mpq_t B[]);
void mpq_matrix_add_constant_c90 (unsigned int m, unsigned int n, mpq_t A[],
                                  const mpq_t x);

/*-----------------------------------------------------------------------*/

#endif /* _SORTSMILL_GMP_MATRIX_H */
