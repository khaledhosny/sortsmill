#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/math.h>
#include <sortsmill/guile.h>
#include <sortsmill/core.h>

//-------------------------------------------------------------------------

VISIBLE void
compose_f64_mono (size_t degree_a, ssize_t stride_a, const double *a,
                  size_t degree_b, ssize_t stride_b, const double *b,
                  ssize_t stride_c, double *c)
{
  // Horner’s rule with polynomial values.

  const size_t degree_c = degree_a * degree_b;
  double _c[degree_c + 1];

  _c[0] = b[stride_b * (ssize_t) degree_b];
  for (size_t i = 1; i <= degree_b; i++)
    {
      mul_f64_mono (degree_a, stride_a, a, (i - 1) * degree_a, 1, _c, 1, _c);
      _c[0] += b[stride_b * (degree_b - i)];
    }

  copy_f64_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

VISIBLE void
compose_scm_mono (size_t degree_a, ssize_t stride_a, const SCM *a,
                  size_t degree_b, ssize_t stride_b, const SCM *b,
                  ssize_t stride_c, SCM *c)
{
  // Horner’s rule with polynomial values.

  const size_t degree_c = degree_a * degree_b;
  SCM _c[degree_c + 1];

  _c[0] = b[stride_b * (ssize_t) degree_b];
  for (size_t i = 1; i <= degree_b; i++)
    {
      mul_scm_mono (degree_a, stride_a, a, (i - 1) * degree_a, 1, _c, 1, _c);
      _c[0] = scm_sum (_c[0], b[stride_b * (degree_b - i)]);
    }

  copy_scm_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

VISIBLE void
compose_f64_bern_de_casteljau (size_t degree_a, ssize_t stride_a,
                               const double *a, size_t degree_b,
                               ssize_t stride_b, const double *b,
                               ssize_t stride_c, double *c)
{
  // De Casteljau’s algorithm with polynomial values.

  // We could get by with a lot less work space, but instead we are
  // going for ease and clarity of programming. In practice the arrays
  // are not going to be very large, anyway.

  double one_minus_a[degree_a + 1];
  one_minus_f64_bern (degree_a, stride_a, a, 1, one_minus_a);

  double _c[degree_b + 1][degree_a * degree_b + 1];
  double temp1[degree_a * degree_b + 1];
  double temp2[degree_a * degree_b + 1];

  for (size_t i = 0; i <= degree_b; i++)
    _c[i][0] = b[stride_b * (ssize_t) i];

  for (size_t i = 0; i < degree_b; i++)
    for (size_t j = 0; j < degree_b - i; j++)
      {
        mul_f64_bern (degree_a, 1, one_minus_a,
                      i * degree_a, 1, _c[j], 1, temp1);
        mul_f64_bern (degree_a, stride_a, a,
                      i * degree_a, 1, _c[j + 1], 1, temp2);
        add_f64_splines ((i + 1) * degree_a, 1, temp1, 1, temp2, 1, _c[j]);
      }

  copy_f64_with_strides (stride_c, c, 1, _c[0], degree_a * degree_b + 1);
}

VISIBLE void
compose_scm_bern_de_casteljau (size_t degree_a, ssize_t stride_a,
                               const SCM *a, size_t degree_b,
                               ssize_t stride_b, const SCM *b,
                               ssize_t stride_c, SCM *c)
{
  // De Casteljau’s algorithm with polynomial values.

  // We could get by with a lot less work space, but instead we are
  // going for ease and clarity of programming. In practice the arrays
  // are not going to be very large, anyway.

  SCM one_minus_a[degree_a + 1];
  one_minus_scm_bern (degree_a, stride_a, a, 1, one_minus_a);

  SCM _c[degree_b + 1][degree_a * degree_b + 1];
  SCM temp1[degree_a * degree_b + 1];
  SCM temp2[degree_a * degree_b + 1];

  for (size_t i = 0; i <= degree_b; i++)
    _c[i][0] = b[stride_b * (ssize_t) i];

  for (size_t i = 0; i < degree_b; i++)
    for (size_t j = 0; j < degree_b - i; j++)
      {
        mul_scm_bern (degree_a, 1, one_minus_a,
                      i * degree_a, 1, _c[j], 1, temp1);
        mul_scm_bern (degree_a, stride_a, a,
                      i * degree_a, 1, _c[j + 1], 1, temp2);
        add_scm_splines ((i + 1) * degree_a, 1, temp1, 1, temp2, 1, _c[j]);
      }

  copy_scm_with_strides (stride_c, c, 1, _c[0], degree_a * degree_b + 1);
}

VISIBLE void
compose_f64_bern_horner (size_t degree_a, ssize_t stride_a, const double *a,
                         size_t degree_b, ssize_t stride_b, const double *b,
                         ssize_t stride_c, double *c)
{
  // Transform the problem to scaled Bernstein basis.

  double _a[degree_a + 1];
  double _b[degree_b + 1];
  double _c[degree_a * degree_b + 1];

  copy_f64_with_strides (1, _a, stride_a, a, degree_a + 1);
  for (size_t i = 1; i < degree_a; i++)
    _a[i] *= bincoef (degree_a, i);

  copy_f64_with_strides (1, _b, stride_b, b, degree_b + 1);
  for (size_t i = 1; i < degree_b; i++)
    _b[i] *= bincoef (degree_b, i);

  compose_f64_sbern_horner (degree_a, 1, _a, degree_b, 1, _b, 1, _c);

  for (size_t i = 1; i < degree_a * degree_b; i++)
    _c[i] /= bincoef (degree_a * degree_b, i);
  copy_f64_with_strides (stride_c, c, 1, _c, degree_a * degree_b + 1);
}

VISIBLE void
compose_scm_bern_horner (size_t degree_a, ssize_t stride_a, const SCM *a,
                         size_t degree_b, ssize_t stride_b, const SCM *b,
                         ssize_t stride_c, SCM *c)
{
  // Transform the problem to scaled Bernstein basis.

  SCM _a[degree_a + 1];
  SCM _b[degree_b + 1];
  SCM _c[degree_a * degree_b + 1];

  copy_scm_with_strides (1, _a, stride_a, a, degree_a + 1);
  for (size_t i = 1; i < degree_a; i++)
    _a[i] = scm_product (_a[i], scm_c_bincoef (degree_a, i));

  copy_scm_with_strides (1, _b, stride_b, b, degree_b + 1);
  for (size_t i = 1; i < degree_b; i++)
    _b[i] = scm_product (_b[i], scm_c_bincoef (degree_b, i));

  compose_scm_sbern_horner (degree_a, 1, _a, degree_b, 1, _b, 1, _c);

  for (size_t i = 1; i < degree_a * degree_b; i++)
    _c[i] = scm_divide (_c[i], scm_c_bincoef (degree_a * degree_b, i));
  copy_scm_with_strides (stride_c, c, 1, _c, degree_a * degree_b + 1);
}

VISIBLE void
compose_f64_sbern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                const double *a, size_t degree_b,
                                ssize_t stride_b, const double *b,
                                ssize_t stride_c, double *c)
{
  // De Casteljau’s algorithm with polynomial values.

  // We could get by with a lot less work space, but instead we are
  // going for ease and clarity of programming. In practice the arrays
  // are not going to be very large, anyway.

  double one_minus_a[degree_a + 1];
  one_minus_f64_sbern (degree_a, stride_a, a, 1, one_minus_a);

  double _c[degree_b + 1][degree_a * degree_b + 1];
  double temp1[degree_a * degree_b + 1];
  double temp2[degree_a * degree_b + 1];

  for (size_t i = 0; i <= degree_b; i++)
    _c[i][0] = b[stride_b * (ssize_t) i] / bincoef (degree_b, i);

  for (size_t i = 0; i < degree_b; i++)
    for (size_t j = 0; j < degree_b - i; j++)
      {
        mul_f64_sbern (degree_a, 1, one_minus_a,
                       i * degree_a, 1, _c[j], 1, temp1);
        mul_f64_sbern (degree_a, stride_a, a,
                       i * degree_a, 1, _c[j + 1], 1, temp2);
        add_f64_splines ((i + 1) * degree_a, 1, temp1, 1, temp2, 1, _c[j]);
      }

  copy_f64_with_strides (stride_c, c, 1, _c[0], degree_a * degree_b + 1);
}

VISIBLE void
compose_scm_sbern_de_casteljau (size_t degree_a, ssize_t stride_a,
                                const SCM *a, size_t degree_b,
                                ssize_t stride_b, const SCM *b,
                                ssize_t stride_c, SCM *c)
{
  // De Casteljau’s algorithm with polynomial values.

  // We could get by with a lot less work space, but instead we are
  // going for ease and clarity of programming. In practice the arrays
  // are not going to be very large, anyway.

  SCM one_minus_a[degree_a + 1];
  one_minus_scm_sbern (degree_a, stride_a, a, 1, one_minus_a);

  SCM _c[degree_b + 1][degree_a * degree_b + 1];
  SCM temp1[degree_a * degree_b + 1];
  SCM temp2[degree_a * degree_b + 1];

  for (size_t i = 0; i <= degree_b; i++)
    _c[i][0] =
      scm_divide (b[stride_b * (ssize_t) i], scm_c_bincoef (degree_b, i));

  for (size_t i = 0; i < degree_b; i++)
    for (size_t j = 0; j < degree_b - i; j++)
      {
        mul_scm_sbern (degree_a, 1, one_minus_a,
                       i * degree_a, 1, _c[j], 1, temp1);
        mul_scm_sbern (degree_a, stride_a, a,
                       i * degree_a, 1, _c[j + 1], 1, temp2);
        add_scm_splines ((i + 1) * degree_a, 1, temp1, 1, temp2, 1, _c[j]);
      }

  copy_scm_with_strides (stride_c, c, 1, _c[0], degree_a * degree_b + 1);
}

VISIBLE void
compose_f64_sbern_horner (size_t degree_a, ssize_t stride_a, const double *a,
                          size_t degree_b, ssize_t stride_b, const double *b,
                          ssize_t stride_c, double *c)
{
  // Use a simple modified Horner scheme, with polynomial values. The
  // approach is to treat b as a ‘polynomial’ with coefficients
  //
  //    [1 − a(t)]ⁿ⋅b₀
  //    [1 − a(t)]ⁿ⁻¹⋅b₁
  //    [1 − a(t)]ⁿ⁻²⋅b₂
  //    [1 − a(t)]ⁿ⁻³⋅b₃
  //         ⋮
  //    [1 − a(t)]⁰⋅bₙ
  //
  // and evaluate that ‘polynomial’ by Horner’s rule, using polynomial
  // multiplication and polynomial addition.

  double one_minus_a[degree_a + 1];
  one_minus_f64_sbern (degree_a, stride_a, a, 1, one_minus_a);

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  double power_of_one_minus_a[degree_c + 1];
  memcpy (power_of_one_minus_a, one_minus_a, (degree_a + 1) * sizeof (double));

  double accumulator[degree_c + 1];
  accumulator[0] = b[stride_b * (ssize_t) degree_b];

  double new_term[degree_c + 1];
  for (size_t i = 1; i <= degree_b; i++)
    {
      mul_f64_sbern ((i - 1) * degree_a, 1, accumulator,
                     degree_a, stride_a, a, 1, accumulator);

      memcpy (new_term, power_of_one_minus_a,
              (i * degree_a + 1) * sizeof (double));
      for (size_t j = 0; j <= i * degree_a; j++)
        new_term[j] *= b[stride_b * (ssize_t) (degree_b - i)];

      add_f64_splines (i * degree_a, 1, accumulator, 1, new_term, 1,
                       accumulator);

      if (i < degree_b)
        mul_f64_sbern (i * degree_a, 1, power_of_one_minus_a,
                       degree_a, 1, one_minus_a, 1, power_of_one_minus_a);
    }

  copy_f64_with_strides (stride_c, c, 1, accumulator, degree_c + 1);
}

VISIBLE void
compose_scm_sbern_horner (size_t degree_a, ssize_t stride_a, const SCM *a,
                          size_t degree_b, ssize_t stride_b, const SCM *b,
                          ssize_t stride_c, SCM *c)
{
  // Use a simple modified Horner scheme, with polynomial values. The
  // approach is to treat b as a ‘polynomial’ with coefficients
  //
  //    [1 − a(t)]ⁿ⋅b₀
  //    [1 − a(t)]ⁿ⁻¹⋅b₁
  //    [1 − a(t)]ⁿ⁻²⋅b₂
  //    [1 − a(t)]ⁿ⁻³⋅b₃
  //         ⋮
  //    [1 − a(t)]⁰⋅bₙ
  //
  // and evaluate that ‘polynomial’ by Horner’s rule, using polynomial
  // multiplication and polynomial addition.
  //
  // (De Casteljau’s algorithm also would work but is expensive.  The
  // Schumaker-Volk algorithm would require rational functions.)

  SCM one_minus_a[degree_a + 1];
  one_minus_scm_sbern (degree_a, stride_a, a, 1, one_minus_a);

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  SCM power_of_one_minus_a[degree_c + 1];
  memcpy (power_of_one_minus_a, one_minus_a, (degree_a + 1) * sizeof (SCM));

  SCM accumulator[degree_c + 1];
  accumulator[0] = b[stride_b * (ssize_t) degree_b];

  SCM new_term[degree_c + 1];
  for (size_t i = 1; i <= degree_b; i++)
    {
      mul_scm_sbern ((i - 1) * degree_a, 1, accumulator,
                     degree_a, stride_a, a, 1, accumulator);

      memcpy (new_term, power_of_one_minus_a,
              (i * degree_a + 1) * sizeof (SCM));
      for (size_t j = 0; j <= i * degree_a; j++)
        new_term[j] =
          scm_product (new_term[j], b[stride_b * (ssize_t) (degree_b - i)]);

      add_scm_splines (i * degree_a, 1, accumulator, 1, new_term, 1,
                       accumulator);

      if (i < degree_b)
        mul_scm_sbern (i * degree_a, 1, power_of_one_minus_a,
                       degree_a, 1, one_minus_a, 1, power_of_one_minus_a);
    }

  copy_scm_with_strides (stride_c, c, 1, accumulator, degree_c + 1);
}

static void
compose_f64_spower_odd_degree (size_t degree_a,
                               ssize_t stride_a, const double *a,
                               size_t degree_b,
                               ssize_t stride_b, const double *b,
                               ssize_t stride_c, double *c)
{
  // Use Horner’s rule to compute
  //
  //    c(t) = b(a(t)) = b₀(t) + s(t)[b₁(t) + s(t)[b₂(t) + s(t)[b₃(t) + ⋯]]]
  //
  // where
  //
  //    s(t) = [1 − a(t)]⋅a(t)
  //    bₖ(t) = [1 − a(t)]⋅b⁰ₖ + a(t)⋅b¹ₖ
  //
  // and (b⁰ₖ, b¹ₖ) are the symmetric coefficient pairs of b.
  //

  double one_minus_a[degree_a + 1];
  one_minus_f64_spower (degree_a, stride_a, a, 1, one_minus_a);

  double s[2 * degree_a + 1];
  mul_f64_spower (degree_a, 1, one_minus_a, degree_a, stride_a, a, 1, s);

  // qb = the degree of a symmetric half of b.
  const size_t qb = degree_b / 2;

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  double _c[degree_c + 1];
  weighted_add_f64_splines (degree_a,
                            b[stride_b * (ssize_t) qb], 1, one_minus_a,
                            b[stride_b * (ssize_t) (degree_b - qb)], stride_a,
                            a, 1, _c);

  double bk[degree_a + 1];
  for (size_t k = 1; k <= qb; k++)
    {
      mul_f64_spower ((2 * k - 1) * degree_a, 1, _c, 2 * degree_a, 1, s, 1, _c);
      weighted_add_f64_splines (degree_a,
                                b[stride_b * (ssize_t) (qb - k)], 1,
                                one_minus_a,
                                b[stride_b * (ssize_t) (degree_b - qb + k)],
                                stride_a, a, 1, bk);
      add_f64_spower ((2 * k + 1) * degree_a, 1, _c, degree_a, 1, bk, 1, _c);
    }

  copy_f64_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

static void
compose_f64_spower_even_degree (size_t degree_a,
                                ssize_t stride_a, const double *a,
                                size_t degree_b,
                                ssize_t stride_b, const double *b,
                                ssize_t stride_c, double *c)
{
  // Use Horner’s rule to compute
  //
  //    c(t) = b(a(t)) = b₀(t) + s(t)[b₁(t) + s(t)[b₂(t) + s(t)[b₃(t) + ⋯]]]
  //
  // where
  //
  //    s(t) = [1 − a(t)]⋅a(t)
  //    bₖ(t) = [1 − a(t)]⋅b⁰ₖ + a(t)⋅b¹ₖ
  //
  // and (b⁰ₖ, b¹ₖ) are the symmetric coefficient pairs of b.
  //

  double one_minus_a[degree_a + 1];
  one_minus_f64_spower (degree_a, stride_a, a, 1, one_minus_a);

  double s[2 * degree_a + 1];
  mul_f64_spower (degree_a, 1, one_minus_a, degree_a, stride_a, a, 1, s);

  // qb = the degree of a symmetric half of b.
  const size_t qb = degree_b / 2;

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  double _c[degree_c + 1];
  _c[0] = b[stride_b * (ssize_t) qb];

  double bk[degree_a + 1];
  for (size_t k = 1; k <= qb; k++)
    {
      mul_f64_spower (2 * (k - 1) * degree_a, 1, _c, 2 * degree_a, 1, s, 1, _c);
      weighted_add_f64_splines (degree_a,
                                b[stride_b * (ssize_t) (qb - k)], 1,
                                one_minus_a,
                                b[stride_b * (ssize_t) (degree_b - qb + k)],
                                stride_a, a, 1, bk);
      add_f64_spower (2 * k * degree_a, 1, _c, degree_a, 1, bk, 1, _c);
    }

  copy_f64_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

VISIBLE void
compose_f64_spower (size_t degree_a, ssize_t stride_a, const double *a,
                    size_t degree_b, ssize_t stride_b, const double *b,
                    ssize_t stride_c, double *c)
{
  if (degree_b % 2 == 1)
    compose_f64_spower_odd_degree (degree_a, stride_a, a,
                                   degree_b, stride_b, b, stride_c, c);
  else
    compose_f64_spower_even_degree (degree_a, stride_a, a,
                                    degree_b, stride_b, b, stride_c, c);
}

static void
compose_scm_spower_odd_degree (size_t degree_a,
                               ssize_t stride_a, const SCM *a,
                               size_t degree_b,
                               ssize_t stride_b, const SCM *b,
                               ssize_t stride_c, SCM *c)
{
  // Use Horner’s rule to compute
  //
  //    c(t) = b(a(t)) = b₀(t) + s(t)[b₁(t) + s(t)[b₂(t) + s(t)[b₃(t) + ⋯]]]
  //
  // where
  //
  //    s(t) = [1 − a(t)]⋅a(t)
  //    bₖ(t) = [1 − a(t)]⋅b⁰ₖ + a(t)⋅b¹ₖ
  //
  // and (b⁰ₖ, b¹ₖ) are the symmetric coefficient pairs of b.
  //

  SCM one_minus_a[degree_a + 1];
  one_minus_scm_spower (degree_a, stride_a, a, 1, one_minus_a);

  SCM s[2 * degree_a + 1];
  mul_scm_spower (degree_a, 1, one_minus_a, degree_a, stride_a, a, 1, s);

  // qb = the degree of a symmetric half of b.
  const size_t qb = degree_b / 2;

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  SCM _c[degree_c + 1];
  weighted_add_scm_splines (degree_a,
                            b[stride_b * (ssize_t) qb], 1, one_minus_a,
                            b[stride_b * (ssize_t) (degree_b - qb)], stride_a,
                            a, 1, _c);

  SCM bk[degree_a + 1];
  for (size_t k = 1; k <= qb; k++)
    {
      mul_scm_spower ((2 * k - 1) * degree_a, 1, _c, 2 * degree_a, 1, s, 1, _c);
      weighted_add_scm_splines (degree_a,
                                b[stride_b * (ssize_t) (qb - k)], 1,
                                one_minus_a,
                                b[stride_b * (ssize_t) (degree_b - qb + k)],
                                stride_a, a, 1, bk);
      add_scm_spower ((2 * k + 1) * degree_a, 1, _c, degree_a, 1, bk, 1, _c);
    }

  copy_scm_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

static void
compose_scm_spower_even_degree (size_t degree_a,
                                ssize_t stride_a, const SCM *a,
                                size_t degree_b,
                                ssize_t stride_b, const SCM *b,
                                ssize_t stride_c, SCM *c)
{
  // Use Horner’s rule to compute
  //
  //    c(t) = b(a(t)) = b₀(t) + s(t)[b₁(t) + s(t)[b₂(t) + s(t)[b₃(t) + ⋯]]]
  //
  // where
  //
  //    s(t) = [1 − a(t)]⋅a(t)
  //    bₖ(t) = [1 − a(t)]⋅b⁰ₖ + a(t)⋅b¹ₖ
  //
  // and (b⁰ₖ, b¹ₖ) are the symmetric coefficient pairs of b.
  //

  SCM one_minus_a[degree_a + 1];
  one_minus_scm_spower (degree_a, stride_a, a, 1, one_minus_a);

  SCM s[2 * degree_a + 1];
  mul_scm_spower (degree_a, 1, one_minus_a, degree_a, stride_a, a, 1, s);

  // qb = the degree of a symmetric half of b.
  const size_t qb = degree_b / 2;

  // degree_c = the degree of the result.
  const size_t degree_c = degree_a * degree_b;

  SCM _c[degree_c + 1];
  _c[0] = b[stride_b * (ssize_t) qb];

  SCM bk[degree_a + 1];
  for (size_t k = 1; k <= qb; k++)
    {
      mul_scm_spower (2 * (k - 1) * degree_a, 1, _c, 2 * degree_a, 1, s, 1, _c);
      weighted_add_scm_splines (degree_a,
                                b[stride_b * (ssize_t) (qb - k)], 1,
                                one_minus_a,
                                b[stride_b * (ssize_t) (degree_b - qb + k)],
                                stride_a, a, 1, bk);
      add_scm_spower (2 * k * degree_a, 1, _c, degree_a, 1, bk, 1, _c);
    }

  copy_scm_with_strides (stride_c, c, 1, _c, degree_c + 1);
}

VISIBLE void
compose_scm_spower (size_t degree_a, ssize_t stride_a, const SCM *a,
                    size_t degree_b, ssize_t stride_b, const SCM *b,
                    ssize_t stride_c, SCM *c)
{
  if (degree_b % 2 == 1)
    compose_scm_spower_odd_degree (degree_a, stride_a, a,
                                   degree_b, stride_b, b, stride_c, c);
  else
    compose_scm_spower_even_degree (degree_a, stride_a, a,
                                    degree_b, stride_b, b, stride_c, c);
}

//-------------------------------------------------------------------------

static SCM
scm_compose_f64_spline (const char *who,
                        void compose_f64_spline (size_t degree_a,
                                                 ssize_t stride_a,
                                                 const double *aa,
                                                 size_t degree_b,
                                                 ssize_t stride_b,
                                                 const double *bb,
                                                 ssize_t stride_c,
                                                 double *cc), SCM a, SCM b)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, a, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, a, &handle1,
                                              &dim1, &stride1);
  const double *_a = scm_array_handle_f64_elements (&handle1);

  scm_array_get_handle (b, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, b, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, b, &handle2,
                                              &dim2, &stride2);
  const double *_b = scm_array_handle_f64_elements (&handle2);

  size_t dim = (dim1 - 1) * (dim2 - 1) + 1;

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_from_size_t (dim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);

  compose_f64_spline (dim1 - 1, stride1, _a, dim2 - 1, stride2, _b, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_compose_f64_mono (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_mono",
                                 compose_f64_mono, a, b);
}

VISIBLE SCM
scm_compose_f64_bern_de_casteljau (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_bern_de_casteljau",
                                 compose_f64_bern_de_casteljau, a, b);
}

VISIBLE SCM
scm_compose_f64_bern_horner (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_bern_horner",
                                 compose_f64_bern_horner, a, b);
}

VISIBLE SCM
scm_compose_f64_sbern_de_casteljau (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_sbern_de_casteljau",
                                 compose_f64_sbern_de_casteljau, a, b);
}

VISIBLE SCM
scm_compose_f64_sbern_horner (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_sbern_horner",
                                 compose_f64_sbern_horner, a, b);
}

VISIBLE SCM
scm_compose_f64_spower (SCM a, SCM b)
{
  return scm_compose_f64_spline ("scm_compose_f64_spower",
                                 compose_f64_spower, a, b);
}

//-------------------------------------------------------------------------

static SCM
scm_compose_scm_spline (const char *who,
                        void compose_scm_spline (size_t degree_a,
                                                 ssize_t stride_a,
                                                 const SCM *aa,
                                                 size_t degree_b,
                                                 ssize_t stride_b,
                                                 const SCM *bb,
                                                 ssize_t stride_c,
                                                 SCM *cc), SCM a, SCM b)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, a, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, a, &handle1,
                                              &dim1, &stride1);
  const SCM *_a = scm_array_handle_elements (&handle1);

  scm_array_get_handle (b, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, b, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, b, &handle2,
                                              &dim2, &stride2);
  const SCM *_b = scm_array_handle_elements (&handle2);

  size_t dim = (dim1 - 1) * (dim2 - 1) + 1;

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_from_size_t (dim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);

  compose_scm_spline (dim1 - 1, stride1, _a, dim2 - 1, stride2, _b, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_compose_scm_mono (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_mono",
                                 compose_scm_mono, a, b);
}

VISIBLE SCM
scm_compose_scm_bern_de_casteljau (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_bern_de_casteljau",
                                 compose_scm_bern_de_casteljau, a, b);
}

VISIBLE SCM
scm_compose_scm_bern_horner (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_bern_horner",
                                 compose_scm_bern_horner, a, b);
}

VISIBLE SCM
scm_compose_scm_sbern_de_casteljau (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_sbern_de_casteljau",
                                 compose_scm_sbern_de_casteljau, a, b);
}

VISIBLE SCM
scm_compose_scm_sbern_horner (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_sbern_horner",
                                 compose_scm_sbern_horner, a, b);
}

VISIBLE SCM
scm_compose_scm_spower (SCM a, SCM b)
{
  return scm_compose_scm_spline ("scm_compose_scm_spower",
                                 compose_scm_spower, a, b);
}

//-------------------------------------------------------------------------

void init_math_polyspline_compose (void);

VISIBLE void
init_math_polyspline_compose (void)
{
  scm_c_define_gsubr ("poly:compose-f64-mono", 2, 0, 0, scm_compose_f64_mono);
  scm_c_define_gsubr ("poly:compose-scm-mono", 2, 0, 0, scm_compose_scm_mono);

  scm_c_define_gsubr ("poly:compose-f64-bern-de-casteljau", 2, 0, 0,
                      scm_compose_f64_bern_de_casteljau);
  scm_c_define_gsubr ("poly:compose-scm-bern-de-casteljau", 2, 0, 0,
                      scm_compose_scm_bern_de_casteljau);

  scm_c_define_gsubr ("poly:compose-f64-bern-horner", 2, 0, 0,
                      scm_compose_f64_bern_horner);
  scm_c_define_gsubr ("poly:compose-scm-bern-horner", 2, 0, 0,
                      scm_compose_scm_bern_horner);

  scm_c_define_gsubr ("poly:compose-f64-sbern-de-casteljau", 2, 0, 0,
                      scm_compose_f64_sbern_de_casteljau);
  scm_c_define_gsubr ("poly:compose-scm-sbern-de-casteljau", 2, 0, 0,
                      scm_compose_scm_sbern_de_casteljau);

  scm_c_define_gsubr ("poly:compose-f64-sbern-horner", 2, 0, 0,
                      scm_compose_f64_sbern_horner);
  scm_c_define_gsubr ("poly:compose-scm-sbern-horner", 2, 0, 0,
                      scm_compose_scm_sbern_horner);

  scm_c_define_gsubr ("poly:compose-f64-spower", 2, 0, 0,
                      scm_compose_f64_spower);
  scm_c_define_gsubr ("poly:compose-scm-spower", 2, 0, 0,
                      scm_compose_scm_spower);
}

//-------------------------------------------------------------------------
