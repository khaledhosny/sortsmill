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

#ifndef _SORTSMILL_GUILE_MATRICES_H
#define _SORTSMILL_GUILE_MATRICES_H

#include <libguile.h>
#include <gsl/gsl_matrix.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* FIXME: Put scm_c_gsl_error in a more general GSL module. */
SCM scm_c_gsl_error (int errval, const char *who, SCM irritants);

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

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATRICES_H */
