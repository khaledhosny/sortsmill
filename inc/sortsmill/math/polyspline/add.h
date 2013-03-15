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

/* Addition of polynomial coefficients, with automatic elevation to
   the higher degree. Guaranteed safe for in-place calculation. */
void add_f64_mono (size_t degree1, ssize_t stride1, const double *spline1,
                   size_t degree2, ssize_t stride2, const double *spline2,
                   ssize_t result_stride, double *result);
void add_scm_mono (size_t degree1, ssize_t stride1, const SCM *spline1,
                   size_t degree2, ssize_t stride2, const SCM *spline2,
                   ssize_t result_stride, SCM *result);
SCM scm_add_f64_mono (SCM spline1, SCM spline2);
SCM scm_add_scm_mono (SCM spline1, SCM spline2);
void add_f64_bern (size_t degree1, ssize_t stride1, const double *spline1,
                   size_t degree2, ssize_t stride2, const double *spline2,
                   ssize_t result_stride, double *result);
void add_scm_bern (size_t degree1, ssize_t stride1, const SCM *spline1,
                   size_t degree2, ssize_t stride2, const SCM *spline2,
                   ssize_t result_stride, SCM *result);
SCM scm_add_f64_bern (SCM spline1, SCM spline2);
SCM scm_add_scm_bern (SCM spline1, SCM spline2);
void add_f64_sbern (size_t degree1, ssize_t stride1, const double *spline1,
                    size_t degree2, ssize_t stride2, const double *spline2,
                    ssize_t result_stride, double *result);
void add_scm_sbern (size_t degree1, ssize_t stride1, const SCM *spline1,
                    size_t degree2, ssize_t stride2, const SCM *spline2,
                    ssize_t result_stride, SCM *result);
SCM scm_add_f64_sbern (SCM spline1, SCM spline2);
SCM scm_add_scm_sbern (SCM spline1, SCM spline2);
void add_f64_spower (size_t degree1, ssize_t stride1, const double *spline1,
                     size_t degree2, ssize_t stride2, const double *spline2,
                     ssize_t result_stride, double *result);
void add_scm_spower (size_t degree1, ssize_t stride1, const SCM *spline1,
                     size_t degree2, ssize_t stride2, const SCM *spline2,
                     ssize_t result_stride, SCM *result);
SCM scm_add_f64_spower (SCM spline1, SCM spline2);
SCM scm_add_scm_spower (SCM spline1, SCM spline2);

/* Weighted addition of polynomial coefficients. Guaranteed safe for
   in-place calculation. */
void weighted_add_f64_splines (size_t degree,
                               double w1,
                               ssize_t stride1, const double *spline1,
                               double w2,
                               ssize_t stride2, const double *spline2,
                               ssize_t result_stride, double *result);
void weighted_add_scm_splines (size_t degree,
                               SCM w1, ssize_t stride1, const SCM *spline1,
                               SCM w2, ssize_t stride2, const SCM *spline2,
                               ssize_t result_stride, SCM *result);

/* Compute the polynomial 1 − a(t), given polynomial a(t). Guaranteed
   safe for in-place calculation. */
void one_minus_f64_mono (size_t degree, ssize_t stride, const double *spline,
                         ssize_t result_stride, double *result);
void one_minus_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                         ssize_t result_stride, SCM *result);
void one_minus_f64_bern (size_t degree, ssize_t stride, const double *spline,
                         ssize_t result_stride, double *result);
void one_minus_scm_bern (size_t degree, ssize_t stride, const SCM *spline,
                         ssize_t result_stride, SCM *result);
void one_minus_f64_sbern (size_t degree, ssize_t stride, const double *spline,
                          ssize_t result_stride, double *result);
void one_minus_scm_sbern (size_t degree, ssize_t stride, const SCM *spline,
                          ssize_t result_stride, SCM *result);
void one_minus_f64_spower (size_t degree, ssize_t stride, const double *spline,
                           ssize_t result_stride, double *result);
void one_minus_scm_spower (size_t degree, ssize_t stride, const SCM *spline,
                           ssize_t result_stride, SCM *result);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_ADD_H */
