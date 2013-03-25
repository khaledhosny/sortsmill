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

#ifndef _SORTSMILL_MATH_POLYSPLINE_REDUCE_H
#define _SORTSMILL_MATH_POLYSPLINE_REDUCE_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Find the minimum degree (the actual degree) of a polynomial. */
size_t min_degree_f64_mono (size_t degree, ssize_t stride, const double *poly);
size_t min_degree_scm_mono (size_t degree, ssize_t stride, const SCM *poly);
size_t scm_c_min_degree_f64_mono (SCM poly);
size_t scm_c_min_degree_scm_mono (SCM poly);
SCM scm_min_degree_f64_mono (SCM poly);
SCM scm_min_degree_scm_mono (SCM poly);
size_t min_degree_f64_spower (size_t degree, ssize_t stride,
                              const double *poly);
size_t min_degree_scm_spower (size_t degree, ssize_t stride, const SCM *poly);
size_t scm_c_min_degree_f64_spower (SCM poly);
size_t scm_c_min_degree_scm_spower (SCM poly);
SCM scm_min_degree_f64_spower (SCM poly);
SCM scm_min_degree_scm_spower (SCM poly);

/* Reduce the degree of a polynomial. The value of @var{new_degree}
   must be less than or equal the old degree. These routines are
   guaranteed safe for in-place computation. */
void reduce_degree_f64_mono (size_t old_degree, ssize_t stride,
                             const double *poly, size_t new_degree,
                             ssize_t result_stride, double *result);
void reduce_degree_scm_mono (size_t old_degree, ssize_t stride,
                             const SCM *poly, size_t new_degree,
                             ssize_t result_stride, SCM *result);
void reduce_degree_f64_spower (size_t old_degree, ssize_t stride,
                               const double *poly, size_t new_degree,
                               ssize_t result_stride, double *result);
void reduce_degree_scm_spower (size_t old_degree, ssize_t stride,
                               const SCM *poly, size_t new_degree,
                               ssize_t result_stride, SCM *result);
SCM scm_c_reduce_degree_f64_mono (SCM poly, size_t new_degree);
SCM scm_c_reduce_degree_scm_mono (SCM poly, size_t new_degree);
SCM scm_reduce_degree_f64_mono (SCM poly, SCM new_degree);
SCM scm_reduce_degree_scm_mono (SCM poly, SCM new_degree);
SCM scm_c_reduce_degree_f64_spower (SCM poly, size_t new_degree);
SCM scm_c_reduce_degree_scm_spower (SCM poly, size_t new_degree);
SCM scm_reduce_degree_f64_spower (SCM poly, SCM new_degree);
SCM scm_reduce_degree_scm_spower (SCM poly, SCM new_degree);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_REDUCE_H */
