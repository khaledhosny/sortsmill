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

#ifndef _SORTSMILL_GUILE_MATH_MATRICES_H
#define _SORTSMILL_GUILE_MATH_MATRICES_H

#include <stdbool.h>
#include <libguile.h>
#include <gsl/gsl_matrix.h>
#include <sortsmill/guile/math/gsl.h>
#include <sortsmill/guile/arrays.h>
#include <sortsmill/gmp_matrix.h>
#include <sortsmill/c_version.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*----------------------------------------------------------------------------*/

// FIXME: Move this to GSL module, and rename it. But do not delete
// this .h file or its .c file, because I expect to use them for other things.
//
SCM scm_f64matrix_svd_solve_vector (SCM U, SCM S, SCM V, SCM x_transpose,
                                    SCM b_transpose);

/*----------------------------------------------------------------------------*/

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MATRICES_H */
