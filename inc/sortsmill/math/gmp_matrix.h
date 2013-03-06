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

#ifndef _SORTSMILL_MATH_GMP_MATRIX_H
#define _SORTSMILL_MATH_GMP_MATRIX_H

#include <gmp.h>
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

void scm_dynwind_mpz_matrix_unwind_handler (void *);
void scm_dynwind_mpq_matrix_unwind_handler (void *);

#if _FF_C99_OR_GREATER

/*-----------------------------------------------------------------------*/
/*
 * GMP init and clear of the matrix entries.
 */

void mpz_matrix_init (unsigned int m, unsigned int n, mpz_t A[m][n]);
void mpz_matrix_clear (unsigned int m, unsigned int n, mpz_t A[m][n]);
void scm_dynwind_mpz_matrix_clear (unsigned int m, unsigned int n,
                                   mpz_t A[m][n]);

void mpq_matrix_init (unsigned int m, unsigned int n, mpq_t A[m][n]);
void mpq_matrix_clear (unsigned int m, unsigned int n, mpq_t A[m][n]);
void scm_dynwind_mpq_matrix_clear (unsigned int m, unsigned int n,
                                   mpq_t A[m][n]);

static inline void
mpz_vector_init (unsigned int n, mpz_t v[n])
{
  mpz_matrix_init (1, n, (mpz_t (*)[(unsigned int)n]) &v[0]);
}

static inline void
mpz_vector_clear (unsigned int n, mpz_t v[n])
{
  mpz_matrix_clear (1, n, (mpz_t (*)[(unsigned int)n]) &v[0]);
}

static inline void
scm_dynwind_mpz_vector_clear (unsigned int n, mpz_t v[n])
{
  scm_dynwind_mpz_matrix_clear (1, n, (mpz_t (*)[(unsigned int)n]) &v[0]);
}

static inline void
mpq_vector_init (unsigned int n, mpq_t v[n])
{
  mpq_matrix_init (1, n, (mpq_t (*)[(unsigned int)n]) &v[0]);
}

static inline void
mpq_vector_clear (unsigned int n, mpq_t v[n])
{
  mpq_matrix_clear (1, n, (mpq_t (*)[(unsigned int)n]) &v[0]);
}

static inline void
scm_dynwind_mpq_vector_clear (unsigned int n, mpq_t v[n])
{
  scm_dynwind_mpq_matrix_clear (1, n, (mpq_t (*)[(unsigned int)n]) &v[0]);
}

/*-----------------------------------------------------------------------*/

void mpz_matrix_set_all (unsigned int m, unsigned int n, mpz_t A[m][n],
                         const mpz_t x);
void mpz_matrix_set_zero (unsigned int m, unsigned int n, mpz_t A[m][n]);
void mpz_matrix_set_identity (unsigned int m, unsigned int n, mpz_t A[m][n]);

void mpq_matrix_set_all (unsigned int m, unsigned int n, mpq_t A[m][n],
                         const mpq_t x);
void mpq_matrix_set_zero (unsigned int m, unsigned int n, mpq_t A[m][n]);
void mpq_matrix_set_identity (unsigned int m, unsigned int n, mpq_t A[m][n]);

/*-----------------------------------------------------------------------*/

void mpz_matrix_memcpy (unsigned int m, unsigned int n,
                        mpz_t result[m][n], mpz_t A[m][n]);
void mpz_matrix_swap (unsigned int m, unsigned int n,
                      mpz_t A[m][n], mpz_t B[m][n]);

void mpq_matrix_memcpy (unsigned int m, unsigned int n,
                        mpq_t result[m][n], mpq_t A[m][n]);
void mpq_matrix_swap (unsigned int m, unsigned int n,
                      mpq_t A[m][n], mpq_t B[m][n]);

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

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

/* Matrix multiplication by a triangular matrix on one or the other
   side. */
void
mpz_matrix_trmm (CBLAS_SIDE_t Side, CBLAS_UPLO_t Uplo,
                 CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                 unsigned int m, unsigned int n, const mpz_t alpha,
                 mpz_t
                 A[(Side == CblasLeft) ? m : n][(Side == CblasLeft) ? m : n],
                 mpz_t B[m][n]);

/* Multiplication by a diagonal matrix (represented as a vector) on
   one or the other side. */
void mpz_matrix_mul_diagonal (CBLAS_SIDE_t Side,
                              unsigned int m, unsigned int n,
                              mpz_t A[m][n],
                              mpz_t x[(Side == CblasLeft) ? m : n]);


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

/* Matrix multiplication by a triangular matrix on one or the other
   side. */
void
mpq_matrix_trmm (CBLAS_SIDE_t Side, CBLAS_UPLO_t Uplo,
                 CBLAS_TRANSPOSE_t TransA, CBLAS_DIAG_t Diag,
                 unsigned int m, unsigned int n, const mpq_t alpha,
                 mpq_t
                 A[(Side == CblasLeft) ? m : n][(Side == CblasLeft) ? m : n],
                 mpq_t B[m][n]);

/* Multiplication by a diagonal matrix (represented as a vector) on
   one or the other side. */
void mpq_matrix_mul_diagonal (CBLAS_SIDE_t Side,
                              unsigned int m, unsigned int n,
                              mpq_t A[m][n],
                              mpq_t x[(Side == CblasLeft) ? m : n]);

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

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

/*-----------------------------------------------------------------------*/

bool mpz_matrix_isnull (unsigned int m, unsigned int n, mpz_t A[m][n]);
bool mpz_matrix_ispos (unsigned int m, unsigned int n, mpz_t A[m][n]);
bool mpz_matrix_isneg (unsigned int m, unsigned int n, mpz_t A[m][n]);
bool mpz_matrix_isnonneg (unsigned int m, unsigned int n, mpz_t A[m][n]);
bool mpz_matrix_equal (unsigned int m, unsigned int n, mpz_t A[m][n],
                       mpz_t B[m][n]);

bool mpq_matrix_isnull (unsigned int m, unsigned int n, mpq_t A[m][n]);
bool mpq_matrix_ispos (unsigned int m, unsigned int n, mpq_t A[m][n]);
bool mpq_matrix_isneg (unsigned int m, unsigned int n, mpq_t A[m][n]);
bool mpq_matrix_isnonneg (unsigned int m, unsigned int n, mpq_t A[m][n]);
bool mpq_matrix_equal (unsigned int m, unsigned int n, mpq_t A[m][n],
                       mpq_t B[m][n]);

/*-----------------------------------------------------------------------*/

void mpq_matrix_trsv (CBLAS_UPLO_t Uplo, CBLAS_TRANSPOSE_t TransA,
                      CBLAS_DIAG_t Diag,
                      unsigned int n, mpq_t A[n][n], mpq_t x[n],
                      bool *singular);

/*-----------------------------------------------------------------------*/
/* LU decomposition. */

/* The permutation vectors here are size_t arrays for compatibility
   with gsl_permutation. */
void mpq_linalg_LU_decomp (unsigned int n, mpq_t A[n][n],
                           size_t p[n], int *signum);
void mpq_linalg_LU_decomp_fast_pivot (unsigned int n, mpq_t A[n][n],
                                      size_t p[n], int *signum);
void mpq_linalg_LU_solve (unsigned int n, mpq_t LU[n][n], size_t p[n],
                          mpq_t b[n], mpq_t x[n], bool *singular);
void mpq_linalg_LU_svx (unsigned int n, mpq_t LU[n][n], size_t p[n], mpq_t x[n],
                        bool *singular);
void mpq_linalg_LU_invert (unsigned int n, mpq_t LU[n][n], size_t p[n],
                           mpq_t inverse[n][n], bool *singular);
void mpq_linalg_LU_det (unsigned int n, mpq_t LU[n][n], int signum, mpq_t det);
int mpq_linalg_LU_sgndet (unsigned int n, mpq_t LU[n][n], int signum);

/*-----------------------------------------------------------------------*/

#endif /* _FF_C99_OR_GREATER */

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_GMP_MATRIX_H */
