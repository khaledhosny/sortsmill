// Copyright (C) 2012 Barry Schwartz
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

#ifndef _SORTSMILLFF_GUILE_GSL_H
#define _SORTSMILLFF_GUILE_GSL_H

#include <libguile.h>
#include <gsl/gsl_matrix.h>

gsl_matrix_const_view scm_gsl_matrix_const_view_array_handle (scm_t_array_handle *handlep);
gsl_matrix_view scm_gsl_matrix_view_array_handle (scm_t_array_handle *handlep);
SCM scm_gsl_matrix_to_array_f64 (const gsl_matrix *m, int low_index);

SCM scm_array_f64_matrix_mult (SCM m1, SCM m2);
SCM scm_array_f64_matrix_add (SCM m1, SCM m2);
SCM scm_array_f64_matrix_sub (SCM m1, SCM m2);

#endif /* _SORTSMILLFF_GUILE_GSL_H */
