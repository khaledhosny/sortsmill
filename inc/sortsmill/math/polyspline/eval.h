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

#ifndef _SORTSMILL_MATH_POLYSPLINE_EVAL_H
#define _SORTSMILL_MATH_POLYSPLINE_EVAL_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Polynomial evaluation in the monomial basis. */
double eval_f64_mono (unsigned int degree, int stride,
                      const double *spline, double t);
SCM scm_c_eval_mono (unsigned int degree, int stride, const SCM *spline, SCM t);
SCM scm_eval_f64_mono (SCM vector, SCM t);
SCM scm_eval_scm_mono (SCM vector, SCM t);

/* ‘Fast’ evaluation of polynomials in Bernstein form, by the
   algorithm of Schumaker and Volk. */
double eval_f64_bern_schumaker_volk (unsigned int deg, int stride,
                                     const double *spline, double t);
SCM scm_c_eval_bern_schumaker_volk (unsigned int deg, int stride,
                                    const SCM *spline, SCM t);
SCM scm_eval_f64_bern_schumaker_volk (SCM splines, SCM t);
SCM scm_eval_scm_bern_schumaker_volk (SCM splines, SCM t);

/* ‘Fast’ evaluation of polynomials in scaled Bernstein form, by the
   algorithm of Schumaker and Volk. */
double eval_f64_sbern_schumaker_volk (unsigned int deg, int stride,
                                      const double *spline, double t);
SCM scm_c_eval_sbern_schumaker_volk (unsigned int deg, int stride,
                                     const SCM *spline, SCM t);
SCM scm_eval_f64_sbern_schumaker_volk (SCM vector, SCM t);
SCM scm_eval_scm_sbern_schumaker_volk (SCM vector, SCM t);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_EVAL_H */
