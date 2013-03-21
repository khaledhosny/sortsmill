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

#ifndef _SORTSMILL_MATH_POLYSPLINE_ROOTS_H
#define _SORTSMILL_MATH_POLYSPLINE_ROOTS_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Count sign variations, as defined at
   http://en.wikipedia.org/wiki/Vincent%27s_theorem#Sign_variation
   This information is used in many root-isolation algorithms. */
size_t sign_variations_f64 (size_t degree, ssize_t stride,
                            const double *spline);
size_t sign_variations_scm (size_t degree, ssize_t stride, const SCM *spline);
SCM scm_sign_variations_f64 (SCM spline);
SCM scm_sign_variations_scm (SCM spline);

/* Budan’s 0_1 roots test for polynomials in the monomial basis. See
   http://en.wikipedia.org/wiki/Budan%27s_theorem#Early_applications_of_Budan.27s_theorem */
size_t budan_0_1_scm_mono (size_t degree, ssize_t stride, const SCM *spline);
SCM scm_budan_0_1_scm_mono (SCM spline);

/* Isolate roots of a polynomial in the closed interval [0,1]. */
SCM isolate_roots_scm_mono (size_t degree, ssize_t stride, const SCM *poly);
SCM scm_isolate_roots_scm_mono (SCM poly);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_ROOTS_H */
