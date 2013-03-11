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

/* Subdivision of polynomials in Bernstein form. (Not guaranteed safe
   for in-place calculation.) */
void subdiv_f64_bern (unsigned int degree, int stride, const double *spline,
                      double t, double *a, double *b);
void scm_c_subdiv_bern (unsigned int degree, int stride, const SCM *spline,
                        SCM t, SCM *a, SCM *b);
SCM scm_subdiv_f64_bern (SCM vector, SCM t);
SCM scm_subdiv_scm_bern (SCM vector, SCM t);

/* Subdivision of polynomials in scaled Bernstein form. (Not
   guaranteed safe for in-place calculation.) */
void subdiv_f64_sbern (unsigned int degree, int stride, const double *spline,
                       double t, double *a, double *b);
void scm_c_subdiv_sbern (unsigned int degree, int stride, const SCM *spline,
                         SCM t, SCM *a, SCM *b);
SCM scm_subdiv_f64_sbern (SCM vector, SCM t);
SCM scm_subdiv_scm_sbern (SCM vector, SCM t);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_SUBDIV_H */
