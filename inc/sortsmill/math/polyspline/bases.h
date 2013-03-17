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

#ifndef _SORTSMILL_MATH_POLYSPLINE_BASES_H
#define _SORTSMILL_MATH_POLYSPLINE_BASES_H

/*
 * Polynomial bases.
 *
 *
 * Assuming cubic splines, the general formula is
 *
 *    x(t) = x₀b₀(t) + x₁b₁(t) + x₂b₂(t) + x₃b₃(t)
 *    y(t) = y₀b₀(t) + y₁b₁(t) + y₂b₂(t) + y₃b₃(t)
 *
 * for 0 ≤ t ≤ 1, where b₀(t), b₁(t), b₂(t), b₃(t) are basis
 * polynomials of degree 3 or less. Font outlines usually are made out
 * of such polynomial splines of degree 2 or 3. (Other representations
 * that are in use, such as rational splines and clothoids, are not
 * dealt with here.)
 *
 *
 * Monomial (or power) basis.
 *
 * The usual notation for polynomials, but numerically sensitive.
 *
 *    b₀(t) = 1
 *    b₁(t) = t
 *    b₂(t) = t²
 *    b₃(t) = t³
 *
 *
 * Bernstein (Bézier) basis.
 *
 * The coefficients of the spline in this basis are equal to the
 * coordinates of the ‘Bézier control points’. This basis is very
 * stable numerically.
 *
 *    b₀(t) = (1 − t)³
 *    b₁(t) = 3t(1 − t)²
 *    b₂(t) = 3t²(1 − t)
 *    b₃(t) = t³
 *
 * The constants (1, 3, 3, 1) are the binomial coefficients C(n,k),
 * where in this case n = 3.
 *
 * References.
 *
 * http://en.wikipedia.org/wiki/Bernstein_polynomial
 *
 *
 * Scaled (or modified) Bernstein basis.
 *
 * The Bernstein basis, but with the binomial coefficients moved out
 * of the basis polynomials. This variant of the Bernstein basis is
 * more convenient for many calculations.
 *
 *    b₀(t) = (1 − t)³
 *    b₁(t) = t(1 − t)²
 *    b₂(t) = t²(1 − t)
 *    b₃(t) = t³
 *
 *
 * References.
 *
 * J. Sánchez-Reyes, ‘Algebraic manipulation in the Bernstein form
 * made simple via convolutions’, Computer-Aided Design 35 (2003)
 * 959-967.
 *
 *
 * Sánchez-Reyes (symmetric power, s-power) basis.
 *
 * This is a more complicated basis that I will not bother to write
 * out here. It is numerically reasonably stable but at the same time
 * has many of the advantages of the ordinary monomial basis. Two
 * important examples: (1) polynomial degree can be reduced by simple
 * truncation; (2) analytic functions can be expanded in a Taylor-like
 * series.
 *
 * References.
 *
 * J. Sánchez-Reyes, ‘The symmetric analogue of the polynomial power
 * basis’, ACM Transactions on Graphics, vol 16 no 3, July 1997,
 * 319-357.
 *
 * J. Sánchez-Reyes, ‘Applications of the polynomial s-power basis in
 * geometry processing’, ACM Transactions on Graphics, vol 19 no 1,
 * January 2000, 27-55.
 *
 */

#include <gmp.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* Returns an identity matrix. */
void mpq_coefficients_mono_to_mono (size_t degree,
                                    mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_mono_to_mono (size_t degree);
SCM scm_coefficients_mono_to_mono (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   monomial basis to Bernstein basis. */
void mpq_coefficients_mono_to_bern (size_t degree,
                                    mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_mono_to_bern (size_t degree);
SCM scm_coefficients_mono_to_bern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   monomial basis to scaled Bernstein basis. */
void mpq_coefficients_mono_to_sbern (size_t degree,
                                     mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_mono_to_sbern (size_t degree);
SCM scm_coefficients_mono_to_sbern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   monomial basis to Sánchez-Reyes s-power basis. */
void mpq_coefficients_mono_to_spower (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_mono_to_spower (size_t degree);
SCM scm_coefficients_mono_to_spower (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Bernstein basis to monomial basis. */
void mpq_coefficients_bern_to_mono (size_t degree,
                                    mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_bern_to_mono (size_t degree);
SCM scm_coefficients_bern_to_mono (SCM degree);

/* Returns an identity matrix. */
void mpq_coefficients_bern_to_bern (size_t degree,
                                    mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_bern_to_bern (size_t degree);
SCM scm_coefficients_bern_to_bern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Bernstein basis to scaled Bernstein basis. */
void mpq_coefficients_bern_to_sbern (size_t degree,
                                     mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_bern_to_sbern (size_t degree);
SCM scm_coefficients_bern_to_sbern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Bernstein basis to Sánchez-Reyes s-power basis. */
void mpq_coefficients_bern_to_spower (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_bern_to_spower (size_t degree);
SCM scm_coefficients_bern_to_spower (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   scaled Bernstein basis to monomial basis. */
void mpq_coefficients_sbern_to_mono (size_t degree,
                                     mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_sbern_to_mono (size_t degree);
SCM scm_coefficients_sbern_to_mono (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   scaled Bernstein basis to Bernstein basis. */
void mpq_coefficients_sbern_to_bern (size_t degree,
                                     mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_sbern_to_bern (size_t degree);
SCM scm_coefficients_sbern_to_bern (SCM degree);

/* Returns an identity matrix. */
void mpq_coefficients_sbern_to_sbern (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_sbern_to_sbern (size_t degree);
SCM scm_coefficients_sbern_to_sbern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   scaled Bernstein basis to Sánchez-Reyes s-power basis. */
void mpq_coefficients_sbern_to_spower (size_t degree,
                                       mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_sbern_to_spower (size_t degree);
SCM scm_coefficients_sbern_to_spower (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Sánchez-Reyes s-power basis to monomial basis. */
void mpq_coefficients_spower_to_mono (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_spower_to_mono (size_t degree);
SCM scm_coefficients_spower_to_mono (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Sánchez-Reyes s-power basis to Bernstein basis. */
void mpq_coefficients_spower_to_bern (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_spower_to_bern (size_t degree);
SCM scm_coefficients_spower_to_bern (SCM degree);

/* Multiply a row vector by this matrix to convert coefficients from
   Sánchez-Reyes s-power basis to scaled Bernstein basis. */
void mpq_coefficients_spower_to_sbern (size_t degree,
                                       mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_spower_to_sbern (size_t degree);
SCM scm_coefficients_spower_to_sbern (SCM degree);

/* Returns an identity matrix. */
void mpq_coefficients_spower_to_spower (size_t degree,
                                        mpq_t T[degree + 1][degree + 1]);
SCM scm_c_coefficients_spower_to_spower (size_t degree);
SCM scm_coefficients_spower_to_spower (SCM degree);

/* Change basis. Guaranteed safe for in-place computation. */
void change_basis_f64_mono_to_mono (size_t degree,
                                    ssize_t stride, const double *spline,
                                    ssize_t result_stride, double *result);
void change_basis_f64_mono_to_bern (size_t degree,
                                    ssize_t stride, const double *spline,
                                    ssize_t result_stride, double *result);
void change_basis_f64_mono_to_sbern (size_t degree,
                                     ssize_t stride, const double *spline,
                                     ssize_t result_stride, double *result);
void change_basis_f64_mono_to_spower (size_t degree,
                                      ssize_t stride, const double *spline,
                                      ssize_t result_stride, double *result);
void change_basis_f64_bern_to_mono (size_t degree,
                                    ssize_t stride, const double *spline,
                                    ssize_t result_stride, double *result);
void change_basis_f64_bern_to_bern (size_t degree,
                                    ssize_t stride, const double *spline,
                                    ssize_t result_stride, double *result);
void change_basis_f64_bern_to_sbern (size_t degree,
                                     ssize_t stride, const double *spline,
                                     ssize_t result_stride, double *result);
void change_basis_f64_bern_to_spower (size_t degree,
                                      ssize_t stride, const double *spline,
                                      ssize_t result_stride, double *result);
void change_basis_f64_sbern_to_mono (size_t degree,
                                     ssize_t stride, const double *spline,
                                     ssize_t result_stride, double *result);
void change_basis_f64_sbern_to_bern (size_t degree,
                                     ssize_t stride, const double *spline,
                                     ssize_t result_stride, double *result);
void change_basis_f64_sbern_to_sbern (size_t degree,
                                      ssize_t stride, const double *spline,
                                      ssize_t result_stride, double *result);
void change_basis_f64_sbern_to_spower (size_t degree,
                                       ssize_t stride, const double *spline,
                                       ssize_t result_stride, double *result);
void change_basis_f64_spower_to_mono (size_t degree,
                                      ssize_t stride, const double *spline,
                                      ssize_t result_stride, double *result);
void change_basis_f64_spower_to_bern (size_t degree,
                                      ssize_t stride, const double *spline,
                                      ssize_t result_stride, double *result);
void change_basis_f64_spower_to_sbern (size_t degree,
                                       ssize_t stride, const double *spline,
                                       ssize_t result_stride, double *result);
void change_basis_f64_spower_to_spower (size_t degree,
                                        ssize_t stride, const double *spline,
                                        ssize_t result_stride, double *result);

/* Construct the polynomial that is equal to the constant one. */
void one_f64_mono (size_t degree, ssize_t stride, double *one);
void one_scm_mono (size_t degree, ssize_t stride, SCM *one);
void one_f64_bern (size_t degree, ssize_t stride, double *one);
void one_scm_bern (size_t degree, ssize_t stride, SCM *one);
void one_f64_sbern (size_t degree, ssize_t stride, double *one);
void one_scm_sbern (size_t degree, ssize_t stride, SCM *one);
void one_f64_spower (size_t degree, ssize_t stride, double *one);
void one_scm_spower (size_t degree, ssize_t stride, SCM *one);

/* Construct the polynomial that is equal to a constant number. */
void constant_f64_mono (double w, size_t degree, ssize_t stride,
                        double *spline);
void constant_scm_mono (SCM w, size_t degree, ssize_t stride, SCM *spline);
void constant_f64_bern (double w, size_t degree, ssize_t stride,
                        double *spline);
void constant_scm_bern (SCM w, size_t degree, ssize_t stride, SCM *spline);
void constant_f64_sbern (double w, size_t degree, ssize_t stride,
                         double *spline);
void constant_scm_sbern (SCM w, size_t degree, ssize_t stride, SCM *spline);
void constant_f64_spower (double w, size_t degree, ssize_t stride,
                          double *spline);
void constant_scm_spower (SCM w, size_t degree, ssize_t stride, SCM *spline);

/* Construct an s-power spline from its symmetric halves, truncating
   them if necessary to fit the specified degree. (Thus, thanks to the
   properties of s-power splines, these routines can be used for
   degree reduction.) */
void unsplit_f64_spower (size_t degree,
                         ssize_t stride0, const double *a0,
                         ssize_t stride1, const double *a1,
                         ssize_t result_stride, double *result);
void unsplit_scm_spower (size_t degree,
                         ssize_t stride0, const SCM *a0,
                         ssize_t stride1, const SCM *a1,
                         ssize_t result_stride, SCM *result);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_BASES_H */
