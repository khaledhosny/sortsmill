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

#ifndef _SORTSMILL_MATH_POLYSPLINE_H
#define _SORTSMILL_MATH_POLYSPLINE_H

/* FIXME: Existing tests may be deficient in testing non-unit and
   negative strides in the routines included below. */
#include <sortsmill/math/polyspline/add.h>
#include <sortsmill/math/polyspline/bases.h>
#include <sortsmill/math/polyspline/compose.h>
#include <sortsmill/math/polyspline/elev.h>
#include <sortsmill/math/polyspline/eval.h>
#include <sortsmill/math/polyspline/mul.h>
#include <sortsmill/math/polyspline/subdiv.h>

/***************************************************************************/
/* FIXME: The stuff below eventually is to be replaced by the stuff above. */
/***************************************************************************/

#include <stdlib.h>
#include <gmp.h>
#include <sortsmill/attributes.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*-----------------------------------------------------------------------*/
/*
 * FIXME: Document these. The transformation matrices have the basis
 * vectors as their rows (or columns, if accessed from Fortran).
 */

/* FIXME: Consider getting rid of these. */
const double *fl_binomial_coefficients (unsigned int degree);
const double *fl_binomial_coefficients_altsigns (unsigned int degree);

/* FIXME: Get rid of these. */
__mpz_struct *mpz_binomial_coefficients (unsigned int degree);
__mpz_struct *mpz_binomial_coefficients_altsigns (unsigned int degree);
void free_mpz_binomial_coefficients (unsigned int degree, __mpz_struct *);

/* FIXME: Get rid of these. */
__mpq_struct *mpq_binomial_coefficients (unsigned int degree);
__mpq_struct *mpq_binomial_coefficients_altsigns (unsigned int degree);
void free_mpq_binomial_coefficients (unsigned int degree, __mpq_struct *);

const double *f64_sbern_basis_in_mono (unsigned int degree);
const double *f64_mono_basis_in_sbern (unsigned int degree);
const double *f64_sbern_basis_in_spower (unsigned int degree);
const double *f64_spower_basis_in_sbern (unsigned int degree);

/* FIXME: Get rid of these. */
__mpz_struct *mpz_sbern_basis_in_mono (unsigned int degree);
__mpz_struct *mpz_mono_basis_in_sbern (unsigned int degree);
__mpz_struct *mpz_sbern_basis_in_spower (unsigned int degree);
__mpz_struct *mpz_spower_basis_in_sbern (unsigned int degree);
void free_mpz_transformation_matrix (unsigned int degree, __mpz_struct *);

/* FIXME: Get rid of these. */
__mpq_struct *mpq_sbern_basis_in_mono (unsigned int degree);
__mpq_struct *mpq_mono_basis_in_sbern (unsigned int degree);
__mpq_struct *mpq_sbern_basis_in_spower (unsigned int degree);
__mpq_struct *mpq_spower_basis_in_sbern (unsigned int degree);
void free_mpq_transformation_matrix (unsigned int degree, __mpq_struct *);

/*-----------------------------------------------------------------------*/

/*
 * vis--
 * vis-- @deftypefun void f64_sbern_to_bern (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from scaled Bernstein to Bernstein basis.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_sbern_to_bern (unsigned int deg, const double *from, double *to,
                        size_t num_splines);

/*
 * vis--
 * vis-- @deftypefun void f64_bern_to_sbern (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from Bernstein to scaled Bernstein basis.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_bern_to_sbern (unsigned int deg, const double *from, double *to,
                        size_t num_splines);

/*
 * vis--
 * vis-- @deftypefun void f64_sbern_to_mono (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from scaled Bernstein to monomial basis.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_sbern_to_mono (unsigned int deg, const double *from, double *to,
                        size_t num_splines);

/*
 * vis--
 * vis-- @deftypefun void f64_mono_to_sbern (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from monomial basis to scaled Bernstein.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_mono_to_sbern (unsigned int deg, const double *from, double *to,
                        size_t num_splines);

/*
 * vis--
 * vis-- @deftypefun void f64_bern_to_sbern (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from Bernstein to monomial basis.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_bern_to_mono (unsigned int deg, const double *from, double *to,
                       size_t num_splines);

/*
 * vis--
 * vis-- @deftypefun void f64_mono_to_bern (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
 * vis--
 * vis-- Convert a spline from monomial basis to Bernstein.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_mono_to_bern (unsigned int deg, const double *from, double *to,
                       size_t num_splines);

/*
 * XvisX--
 * XvisX-- @deftypefun double f64_eval_sbern (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
 * XvisX--
 * XvisX-- Evaluate a spline in scaled Bernstein basis, using the
 * XvisX-- algorithm of Schumaker and Volk.
 * XvisX--
 * XvisX-- @end deftypefun
 * XvisX--
 */
/*_FF_ATTRIBUTE_PURE double f64_eval_sbern (unsigned int deg,
  const double *spline, double t);*/

/*
 * XvisX--
 * XvisX-- @deftypefun double f64_eval_bern (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
 * XvisX--
 * XvisX-- Evaluate a spline in Bernstein basis, using the
 * XvisX-- algorithm of Schumaker and Volk.
 * XvisX--
 * XvisX-- @end deftypefun
 * XvisX--
 */
/*_FF_ATTRIBUTE_PURE double f64_eval_bern (unsigned int deg,
  const double *spline, double t);*/

/*
 * XvisX--
 * XvisX-- @deftypefun double f64_eval_sbern (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
 * XvisX--
 * XvisX-- Evaluate a spline in scaled Bernstein basis, using the
 * XvisX-- algorithm of De~Casteljau.
 * XvisX--
 * XvisX-- (De~Casteljau’s generally is the most stable algorithm, but
 * XvisX-- can be expensive.)
 * XvisX--
 * XvisX-- @end deftypefun
 * XvisX--
 */
/*_FF_ATTRIBUTE_PURE double f64_evaldc_sbern (unsigned int deg,
  const double *spline, double t);*/

/*
 * XvisX--
 * XvisX-- @deftypefun double f64_eval_bern (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
 * XvisX--
 * XvisX-- Evaluate a spline in Bernstein basis, using the
 * XvisX-- algorithm of De~Casteljau.
 * XvisX--
 * XvisX-- (De~Casteljau’s generally is the most stable algorithm, but
 * XvisX-- can be expensive.)
 * XvisX--
 * XvisX-- @end deftypefun
 * XvisX--
 */
/*_FF_ATTRIBUTE_PURE double f64_evaldc_bern (unsigned int deg,
  const double *spline, double t);*/

/*
 * XvisX--
 * XvisX-- @deftypefun double f64_eval_mono (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
 * XvisX--
 * XvisX-- Evaluate a spline in monomial basis.
 * XvisX--
 * XvisX-- @end deftypefun
 * XvisX--
 */
/*_FF_ATTRIBUTE_PURE double f64_eval_mono (unsigned int deg,
  const double *spline, double t);*/

/*
 * vis--
 * vis-- @deftypefun void f64_subdiv_sbern (unsigned int @var{deg}, const double *@var{spline}, double @var{t}, double *@var{a}, double *@var{b});
 * vis--
 * vis-- Subdivide a spline in scaled Bernstein basis, using the
 * vis-- algorithm of De~Casteljau.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_subdiv_sbern (unsigned int deg, const double *spline, double t,
                       double *a, double *b);

/*
 * vis--
 * vis-- @deftypefun void f64_subdiv_bern (unsigned int @var{deg}, const double *@var{spline}, double @var{t}, double *@var{a}, double *@var{b});
 * vis--
 * vis-- Subdivide a spline in Bernstein basis, using the
 * vis-- algorithm of De~Casteljau.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void f64_subdiv_bern (unsigned int deg, const double *spline, double t,
                      double *a, double *b);

/* FIXME: The stuff below is entirely undocumented so far. */

void f64_mul_sbern (unsigned int deg1, const double *spline1,
                    unsigned int deg2, const double *spline2, double *result);
void f64_mul_bern (unsigned int deg1, const double *spline1,
                   unsigned int deg2, const double *spline2, double *result);
void f64_mul_mono (unsigned int deg1, const double *spline1,
                   unsigned int deg2, const double *spline2, double *result);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_H */
