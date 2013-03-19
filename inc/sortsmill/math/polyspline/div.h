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

#ifndef _SORTSMILL_MATH_POLYSPLINE_DIV_H
#define _SORTSMILL_MATH_POLYSPLINE_DIV_H

#include <gmp.h>
#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Division of polynomials in monomial form. Guaranteed safe for
   in-place calculation. */
void div_f64_mono (size_t degree1, ssize_t stride1, const double *spline1,
                   size_t degree2, ssize_t stride2, const double *spline2,
                   size_t *degree_q, ssize_t stride_q, double *quotient,
                   size_t *degree_r, ssize_t stride_r, double *remainder,
                   bool *division_by_zero);
void div_scm_mono (size_t degree1, ssize_t stride1, const SCM *spline1,
                   size_t degree2, ssize_t stride2, const SCM *spline2,
                   size_t *degree_q, ssize_t stride_q, SCM *quotient,
                   size_t *degree_r, ssize_t stride_r, SCM *remainder,
                   bool *division_by_zero);
SCM scm_div_f64_mono (SCM spline1, SCM spline2);
SCM scm_div_scm_mono (SCM spline1, SCM spline2);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_DIV_H */
