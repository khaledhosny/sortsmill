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

#ifndef _SORTSMILL_MATH_POLYSPLINE_ELEV_H
#define _SORTSMILL_MATH_POLYSPLINE_ELEV_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Degree elevation of polynomials in monomial form. Guaranteed safe
   for in-place calculation. */
void elev_f64_mono (size_t new_degree,
                    size_t degree, ssize_t stride, const double *spline,
                    ssize_t result_stride, double *result);
void elev_scm_mono (size_t new_degree,
                    size_t degree, ssize_t stride, const SCM *spline,
                    ssize_t result_stride, SCM *result);
SCM scm_elev_f64_mono (SCM new_degree, SCM spline);
SCM scm_elev_scm_mono (SCM new_degree, SCM spline);

/* Degree elevation of polynomials in Bernstein form. Guaranteed safe
   for in-place calculation. */
void elev_f64_bern (size_t new_degree,
                    size_t degree, ssize_t stride, const double *spline,
                    ssize_t result_stride, double *result);
void elev_scm_bern (size_t new_degree,
                    size_t degree, ssize_t stride, const SCM *spline,
                    ssize_t result_stride, SCM *result);
SCM scm_elev_f64_bern (SCM new_degree, SCM spline);
SCM scm_elev_scm_bern (SCM new_degree, SCM spline);

/* Degree elevation of polynomials in scaled Bernstein
   form. Guaranteed safe for in-place calculation. */
void elev_f64_sbern (size_t new_degree,
                     size_t degree, ssize_t stride, const double *spline,
                     ssize_t result_stride, double *result);
void elev_scm_sbern (size_t new_degree,
                     size_t degree, ssize_t stride, const SCM *spline,
                     ssize_t result_stride, SCM *result);
SCM scm_elev_f64_sbern (SCM new_degree, SCM spline);
SCM scm_elev_scm_sbern (SCM new_degree, SCM spline);

/* Degree elevation of polynomials in s-power form. Guaranteed safe
   for in-place calculation. */
void elev_f64_spower (size_t new_degree,
                      size_t degree, ssize_t stride, const double *spline,
                      ssize_t result_stride, double *result);
void elev_scm_spower (size_t new_degree,
                      size_t degree, ssize_t stride, const SCM *spline,
                      ssize_t result_stride, SCM *result);
SCM scm_elev_f64_spower (SCM new_degree, SCM spline);
SCM scm_elev_scm_spower (SCM new_degree, SCM spline);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_ELEV_H */
