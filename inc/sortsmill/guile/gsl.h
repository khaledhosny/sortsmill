/*
 * Copyright (C) 2012 Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_GSL_H
#define _SORTSMILL_GUILE_GSL_H

#include <libguile.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <sortsmill/guile/arrays.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_gsl_errno_to_symbol (SCM errval);
SCM scm_c_gsl_errno_to_symbol (int errval);
SCM scm_raise_gsl_error (SCM arguments);
void scm_gsl_error_handler_for_raising_a_gsl_error (const char *reason,
                                                    const char *file,
                                                    int line, int gsl_errno);

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

SCM scm_gsl_blas_dgemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                        SCM beta, SCM C);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_GSL_H */
