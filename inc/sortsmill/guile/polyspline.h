/*
 * Copyright (C) 2012, 2013 Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_POLYSPLINE_H
#define _SORTSMILL_GUILE_POLYSPLINE_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_f64vector_binomial_coefficients (SCM degree);
SCM scm_f64vector_binomial_coefficients_altsigns (SCM degree);
SCM scm_vector_binomial_coefficients (SCM degree);
SCM scm_vector_binomial_coefficients_altsigns (SCM degree);

SCM scm_f64vector_sbern_basis_in_mono (SCM degree);
SCM scm_f64vector_mono_basis_in_sbern (SCM degree);
SCM scm_f64vector_sbern_basis_in_sbern (SCM degree);
SCM scm_f64vector_spower_basis_in_sbern (SCM degree);
SCM scm_vector_sbern_basis_in_mono (SCM degree);
SCM scm_vector_mono_basis_in_sbern (SCM degree);
SCM scm_vector_sbern_basis_in_sbern (SCM degree);
SCM scm_vector_spower_basis_in_sbern (SCM degree);

SCM scm_f64vector_sbern_to_bern (SCM spline);
SCM scm_f64vector_bern_to_sbern (SCM spline);
SCM scm_f64vector_sbern_to_mono (SCM spline);
SCM scm_f64vector_mono_to_sbern (SCM spline);
SCM scm_f64vector_bern_to_mono (SCM spline);
SCM scm_f64vector_mono_to_bern (SCM spline);

SCM scm_f64vector_eval_sbern (SCM spline, SCM t);
SCM scm_f64vector_eval_bern (SCM spline, SCM t);
SCM scm_f64vector_evaldc_sbern (SCM spline, SCM t);
SCM scm_f64vector_evaldc_bern (SCM spline, SCM t);
SCM scm_f64vector_eval_mono (SCM spline, SCM t);

SCM scm_f64vector_subdiv_sbern (SCM spline, SCM t);
SCM scm_f64vector_subdiv_bern (SCM spline, SCM t);

SCM scm_f64vector_mul_sbern (SCM spline1, SCM spline2);
SCM scm_f64vector_mul_bern (SCM spline1, SCM spline2);
SCM scm_f64vector_mul_mono (SCM spline1, SCM spline2);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_POLYSPLINE_H */
