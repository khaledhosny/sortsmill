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

#ifndef _SORTSMILL_MATH_POLYSPLINE_DERIV_H
#define _SORTSMILL_MATH_POLYSPLINE_DERIV_H

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*
 * Formal derivatives of polynomials. The operations are guaranteed
 * safe for in-place computation.
 */

void deriv_f64_mono (size_t degree, ssize_t stride, const double *spline,
                     ssize_t deriv_stride, double *deriv);
void deriv_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                     ssize_t deriv_stride, SCM *deriv);
SCM scm_deriv_f64_mono (SCM spline);
SCM scm_deriv_scm_mono (SCM spline);

void deriv_f64_bern (size_t degree, ssize_t stride, const double *spline,
                     ssize_t deriv_stride, double *deriv);
void deriv_scm_bern (size_t degree, ssize_t stride, const SCM *spline,
                     ssize_t deriv_stride, SCM *deriv);
SCM scm_deriv_f64_bern (SCM spline);
SCM scm_deriv_scm_bern (SCM spline);

void deriv_f64_sbern (size_t degree, ssize_t stride, const double *spline,
                      ssize_t deriv_stride, double *deriv);
void deriv_scm_sbern (size_t degree, ssize_t stride, const SCM *spline,
                      ssize_t deriv_stride, SCM *deriv);
SCM scm_deriv_f64_sbern (SCM spline);
SCM scm_deriv_scm_sbern (SCM spline);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_DERIV_H */
