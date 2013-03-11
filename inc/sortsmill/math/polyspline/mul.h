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

#ifndef _SORTSMILL_MATH_POLYSPLINE_MUL_H
#define _SORTSMILL_MATH_POLYSPLINE_MUL_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Multiplication of polynomials in monomial form. (Not guaranteed
   safe for in-place calculation.) */
void mul_f64_mono (unsigned int degree1, int stride1, const double *spline1,
                   unsigned int degree2, int stride2, const double *spline2,
                   double *result);
SCM scm_mul_f64_mono (SCM vector, SCM t);

/* Multiplication of polynomials in Bernstein form. (Not guaranteed
   safe for in-place calculation.) */
void mul_f64_bern (unsigned int degree1, int stride1, const double *spline1,
                   unsigned int degree2, int stride2, const double *spline2,
                   double *result);
SCM scm_mul_f64_bern (SCM vector, SCM t);

/* Multiplication of polynomials in scaled Bernstein form. (Not
   guaranteed safe for in-place calculation.) */
void mul_f64_sbern (unsigned int degree1, int stride1, const double *spline1,
                    unsigned int degree2, int stride2, const double *spline2,
                    double *result);
SCM scm_mul_f64_sbern (SCM vector, SCM t);

/* Multiplication of polynomials in s-power form. (Not guaranteed safe
   for in-place calculation.) */

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_MUL_H */
