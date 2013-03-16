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

#ifndef _SORTSMILL_MATH_POLYSPLINE_SUBDIV_H
#define _SORTSMILL_MATH_POLYSPLINE_SUBDIV_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Subdivision of polynomials in monomial form. Guaranteed safe for
   in-place calculation. */
void subdiv_f64_mono (size_t degree, ssize_t stride, const double *spline,
                      double t,
                      ssize_t stride_a, double *a, ssize_t stride_b, double *b);
void subdiv_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                      SCM t,
                      ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b);
SCM scm_subdiv_f64_mono (SCM vector, SCM t);
SCM scm_subdiv_scm_mono (SCM vector, SCM t);

/* Subdivision of polynomials in Bernstein form. Guaranteed safe for
   in-place calculation. */
void subdiv_f64_bern (size_t degree, ssize_t stride, const double *spline,
                      double t,
                      ssize_t stride_a, double *a, ssize_t stride_b, double *b);
void subdiv_scm_bern (size_t degree, ssize_t stride, const SCM *spline,
                      SCM t,
                      ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b);
SCM scm_subdiv_f64_bern (SCM vector, SCM t);
SCM scm_subdiv_scm_bern (SCM vector, SCM t);

/* Subdivision of polynomials in scaled Bernstein form. Guaranteed
   safe for in-place calculation. */
void subdiv_f64_sbern (size_t degree, ssize_t stride, const double *spline,
                       double t,
                       ssize_t stride_a, double *a,
                       ssize_t stride_b, double *b);
void subdiv_scm_sbern (size_t degree, ssize_t stride, const SCM *spline,
                       SCM t,
                       ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b);
SCM scm_subdiv_f64_sbern (SCM vector, SCM t);
SCM scm_subdiv_scm_sbern (SCM vector, SCM t);

/* Subdivision of polynomials in Sánchez-Reyes s-power
   form. Guaranteed safe for in-place calculation. */
void subdiv_f64_spower (size_t degree, ssize_t stride, const double *spline,
                        double t,
                        ssize_t stride_a, double *a,
                        ssize_t stride_b, double *b);
void subdiv_scm_spower (size_t degree, ssize_t stride, const SCM *spline,
                        SCM t,
                        ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b);
SCM scm_subdiv_f64_spower (SCM vector, SCM t);
SCM scm_subdiv_scm_spower (SCM vector, SCM t);

/* Extract the (directed) portion of a spline between t1 and t2. */
void portion_f64_mono (size_t degree, ssize_t stride, const double *spline,
                       double t1, double t2,
                       ssize_t result_stride, double *result);
void portion_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                       SCM t1, SCM t2, ssize_t result_stride, SCM *result);
SCM scm_portion_f64_mono (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_mono (SCM vector, SCM t1, SCM t2);
/* */
void portion_f64_bern_de_casteljau (size_t degree, ssize_t stride,
                                    const double *spline, double t1, double t2,
                                    ssize_t result_stride, double *result);
void portion_scm_bern_de_casteljau (size_t degree, ssize_t stride,
                                    const SCM *spline, SCM t1, SCM t2,
                                    ssize_t result_stride, SCM *result);
SCM scm_portion_f64_bern_de_casteljau (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_bern_de_casteljau (SCM vector, SCM t1, SCM t2);
/* */
void portion_f64_bern_horner (size_t degree, ssize_t stride,
                              const double *spline, double t1, double t2,
                              ssize_t result_stride, double *result);
void portion_scm_bern_horner (size_t degree, ssize_t stride, const SCM *spline,
                              SCM t1, SCM t2, ssize_t result_stride,
                              SCM *result);
SCM scm_portion_f64_bern_horner (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_bern_horner (SCM vector, SCM t1, SCM t2);
/* */
void portion_f64_sbern_de_casteljau (size_t degree, ssize_t stride,
                                     const double *spline, double t1, double t2,
                                     ssize_t result_stride, double *result);
void portion_scm_sbern_de_casteljau (size_t degree, ssize_t stride,
                                     const SCM *spline, SCM t1, SCM t2,
                                     ssize_t result_stride, SCM *result);
SCM scm_portion_f64_sbern_de_casteljau (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_sbern_de_casteljau (SCM vector, SCM t1, SCM t2);
/* */
void portion_f64_sbern_horner (size_t degree, ssize_t stride,
                               const double *spline, double t1, double t2,
                               ssize_t result_stride, double *result);
void portion_scm_sbern_horner (size_t degree, ssize_t stride, const SCM *spline,
                               SCM t1, SCM t2, ssize_t result_stride,
                               SCM *result);
SCM scm_portion_f64_sbern_horner (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_sbern_horner (SCM vector, SCM t1, SCM t2);
/* */
void portion_f64_spower (size_t degree, ssize_t stride, const double *spline,
                         double t1, double t2,
                         ssize_t result_stride, double *result);
void portion_scm_spower (size_t degree, ssize_t stride, const SCM *spline,
                         SCM t1, SCM t2, ssize_t result_stride, SCM *result);
SCM scm_portion_f64_spower (SCM vector, SCM t1, SCM t2);
SCM scm_portion_scm_spower (SCM vector, SCM t1, SCM t2);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_SUBDIV_H */
