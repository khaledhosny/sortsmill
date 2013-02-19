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

#ifndef _SORTSMILL_GUILE_MATRICES_H
#define _SORTSMILL_GUILE_MATRICES_H

#include <libguile.h>
#include <gsl/gsl_matrix.h>
#include <sortsmill/guile/gsl.h>
#include <sortsmill/guile/arrays.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* FIXME: Put these in a more Guile-array specific module, and include
   it here. */
void scm_array_handle_unwind_handler (void *handlep);
void scm_dynwind_array_handle_release (scm_t_array_handle *handlep);

gsl_vector_const_view
scm_gsl_vector_const_view_array_handle (scm_t_array_handle *handlep);
gsl_vector_view scm_gsl_vector_view_array_handle (scm_t_array_handle *handlep);

gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle (scm_t_array_handle *handlep);
gsl_matrix_view scm_gsl_matrix_view_array_handle (scm_t_array_handle *handlep);

SCM scm_gsl_vector_to_f64vector (const gsl_vector *v, int low_index);
SCM scm_gsl_matrix_to_f64matrix (const gsl_matrix *m, int low_index);

SCM scm_f64matrix_f64matrix_mult (SCM m1, SCM m2);
SCM scm_f64matrix_f64matrix_add (SCM m1, SCM m2);
SCM scm_f64matrix_f64matrix_sub (SCM m1, SCM m2);

SCM scm_f64matrix_svd_golub_reinsch (SCM m);
SCM scm_f64matrix_svd_modified_golub_reinsch (SCM m);
SCM scm_f64matrix_svd_jacobi (SCM m);
SCM scm_f64matrix_svd_solve_vector (SCM U, SCM S, SCM V, SCM x_transpose,
                                    SCM b_transpose);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATRICES_H */
