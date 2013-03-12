/* -*- coding: utf-8 -*-
 *
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

#ifndef _SORTSMILL_MATH_POLYSPLINE_ADD_H
#define _SORTSMILL_MATH_POLYSPLINE_ADD_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Addition of polynomial coefficients. Guaranteed safe for in-place
   calculation. */
void add_f64_splines (size_t degree,
                      ssize_t stride1, const double *spline1,
                      ssize_t stride2, const double *spline2,
                      ssize_t result_stride, double *result);
void add_scm_splines (size_t degree,
                      ssize_t stride1, const SCM *spline1,
                      ssize_t stride2, const SCM *spline2,
                      ssize_t result_stride, SCM *result);
SCM scm_add_f64_splines (SCM spline1, SCM spline2);
SCM scm_add_scm_splines (SCM spline1, SCM spline2);

/* Subtraction of polynomial coefficients. Guaranteed safe for
   in-place calculation. */
void sub_f64_splines (size_t degree,
                      ssize_t stride1, const double *spline1,
                      ssize_t stride2, const double *spline2,
                      ssize_t result_stride, double *result);
void sub_scm_splines (size_t degree,
                      ssize_t stride1, const SCM *spline1,
                      ssize_t stride2, const SCM *spline2,
                      ssize_t result_stride, SCM *result);
SCM scm_sub_f64_splines (SCM spline1, SCM spline2);
SCM scm_sub_scm_splines (SCM spline1, SCM spline2);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_ADD_H */
