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

#ifndef _SORTSMILL_GUILE_MATH_GSL_MATRICES_H
#define _SORTSMILL_GUILE_MATH_GSL_MATRICES_H

#include <libguile.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <sortsmill/guile/arrays.h>
#include <sortsmill/c_version.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* FIXME: Review these ‘exception’ functions for relevance and, in any
   case, try not to include them here or make them VISIBLE. */
void exception__array_has_no_elements (const char *who, SCM irritants);
void exception__expected_array_of_rank_1 (const char *who, SCM irritants);
void exception__expected_array_of_rank_2 (const char *who, SCM irritants);
void exception__expected_array_of_rank_1_or_2 (const char *who, SCM irritants);
void exception__layout_incompatible_with_gsl (const char *who, SCM irritants);
void exception__unexpected_array_type (const char *who, SCM a);

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
                                     unsigned int m, unsigned int n,
                                     mpz_t A[m][n]);
void scm_array_handle_to_mpq_matrix (SCM array, scm_t_array_handle *handlep,
                                     unsigned int m, unsigned int n,
                                     mpq_t A[m][n]);

SCM scm_from_mpz_matrix (unsigned int m, unsigned int n, mpz_t A[m][n]);
SCM scm_from_mpq_matrix (unsigned int m, unsigned int n, mpq_t A[m][n]);

#endif /* _FF_C99_OR_GREATER */

SCM scm_gsl_blas_dgemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                        SCM beta, SCM C);
SCM scm_gsl_mpz_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                      SCM beta, SCM C);
SCM scm_gsl_mpq_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                      SCM beta, SCM C);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_GSL_MATRICES_H */
