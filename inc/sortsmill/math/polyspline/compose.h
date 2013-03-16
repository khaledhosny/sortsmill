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

#ifndef _SORTSMILL_MATH_POLYSPLINE_COMPOSE_H
#define _SORTSMILL_MATH_POLYSPLINE_COMPOSE_H

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
 * Compute the composition of two polynomial splines:
 *
 *    c(t) = (b ∘ a)(t) = b(a(t))
 *
 * The result c is a polynomial whose degree (in general) is the
 * product of the degrees of a and b.
 *
 * The routines are safe for in-place computation.
 */

void compose_f64_mono (size_t degree_a, ssize_t stride_a, const double *a,
                       size_t degree_b, ssize_t stride_b, const double *b,
                       ssize_t stride_c, double *c);
void compose_scm_mono (size_t degree_a, ssize_t stride_a, const SCM *a,
                       size_t degree_b, ssize_t stride_b, const SCM *b,
                       ssize_t stride_c, SCM *c);
SCM scm_compose_f64_mono (SCM a, SCM b);
SCM scm_compose_scm_mono (SCM a, SCM b);

void compose_f64_bern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                    const double *a, size_t degree_b,
                                    ssize_t stride_b, const double *b,
                                    ssize_t stride_c, double *c);
void compose_scm_bern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                    const SCM *a, size_t degree_b,
                                    ssize_t stride_b, const SCM *b,
                                    ssize_t stride_c, SCM *c);
SCM scm_compose_f64_bern_de_casteljau (SCM a, SCM b);
SCM scm_compose_scm_bern_de_casteljau (SCM a, SCM b);

void compose_f64_bern_horner (size_t degree_a, ssize_t stride_a,
                              const double *a, size_t degree_b,
                              ssize_t stride_b, const double *b,
                              ssize_t stride_c, double *c);
void compose_scm_bern_horner (size_t degree_a, ssize_t stride_a, const SCM *a,
                              size_t degree_b, ssize_t stride_b, const SCM *b,
                              ssize_t stride_c, SCM *c);
SCM scm_compose_f64_bern_horner (SCM a, SCM b);
SCM scm_compose_scm_bern_horner (SCM a, SCM b);

void compose_f64_sbern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                     const double *a, size_t degree_b,
                                     ssize_t stride_b, const double *b,
                                     ssize_t stride_c, double *c);
void compose_scm_sbern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                     const SCM *a, size_t degree_b,
                                     ssize_t stride_b, const SCM *b,
                                     ssize_t stride_c, SCM *c);
SCM scm_compose_f64_sbern_de_casteljau (SCM a, SCM b);
SCM scm_compose_scm_sbern_de_casteljau (SCM a, SCM b);

void compose_f64_sbern_horner (size_t degree_a, ssize_t stride_a,
                               const double *a, size_t degree_b,
                               ssize_t stride_b, const double *b,
                               ssize_t stride_c, double *c);
void compose_scm_sbern_horner (size_t degree_a, ssize_t stride_a, const SCM *a,
                               size_t degree_b, ssize_t stride_b, const SCM *b,
                               ssize_t stride_c, SCM *c);
SCM scm_compose_f64_sbern_horner (SCM a, SCM b);
SCM scm_compose_scm_sbern_horner (SCM a, SCM b);

void compose_f64_spower (size_t degree_a, ssize_t stride_a, const double *a,
                         size_t degree_b, ssize_t stride_b, const double *b,
                         ssize_t stride_c, double *c);
void compose_scm_spower (size_t degree_a, ssize_t stride_a, const SCM *a,
                         size_t degree_b, ssize_t stride_b, const SCM *b,
                         ssize_t stride_c, SCM *c);
SCM scm_compose_f64_spower (SCM a, SCM b);
SCM scm_compose_scm_spower (SCM a, SCM b);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_COMPOSE_H */
