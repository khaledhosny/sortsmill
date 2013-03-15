#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 by Barry Schwartz
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/math.h>
#include <sortsmill/guile.h>
#include <sortsmill/copy_with_strides.h>

//-------------------------------------------------------------------------

static void
f64_convolve (size_t degree1, ssize_t stride1, const double *poly1,
              size_t degree2, ssize_t stride2, const double *poly2,
              ssize_t result_stride, double *result)
{
  // This is just the ‘naïve’ algorithm (no Karatsuba, FFT, etc.).

  const size_t degree = degree1 + degree2;
  double buffer[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = 0.0;
  for (size_t j = 0; j <= degree2; j++)
    for (size_t i = 0; i <= degree1; i++)
      buffer[j + i] +=
        poly2[stride2 * (ssize_t) j] * poly1[stride1 * (ssize_t) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

static void
scm_convolve (size_t degree1, ssize_t stride1, const SCM *poly1,
              size_t degree2, ssize_t stride2, const SCM *poly2,
              ssize_t result_stride, SCM *result)
{
  // This is just the ‘naïve’ algorithm (no Karatsuba, FFT, etc.).

  const SCM zero = scm_from_uint (0);

  const size_t degree = degree1 + degree2;
  SCM buffer[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = zero;
  for (size_t j = 0; j <= degree2; j++)
    for (size_t i = 0; i <= degree1; i++)
      buffer[j + i] = scm_sum (buffer[j + i],
                               scm_product (poly2[stride2 * (ssize_t) j],
                                            poly1[stride1 * (ssize_t) i]));
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

//-------------------------------------------------------------------------

VISIBLE void
mul_f64_mono (size_t degree1, ssize_t stride1, const double *spline1,
              size_t degree2, ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  f64_convolve (degree1, stride1, spline1, degree2, stride2, spline2,
                result_stride, result);
}

VISIBLE void
mul_scm_mono (size_t degree1, ssize_t stride1, const SCM *spline1,
              size_t degree2, ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  scm_convolve (degree1, stride1, spline1, degree2, stride2, spline2,
                result_stride, result);
}

VISIBLE void
mul_f64_bern (size_t degree1, ssize_t stride1, const double *spline1,
              size_t degree2, ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  const size_t degree = degree1 + degree2;
  double buffer[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = 0.0;
  for (size_t j = 0; j <= degree2; j++)
    for (size_t i = 0; i <= degree1; i++)
      buffer[j + i] +=
        (bincoef (degree2, j) * spline2[stride2 * (ssize_t) j]) *
        (bincoef (degree1, i) * spline1[stride1 * (ssize_t) i]);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] /= bincoef (degree, i);
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
mul_scm_bern (size_t degree1, ssize_t stride1, const SCM *spline1,
              size_t degree2, ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  const SCM zero = scm_from_uint (0);

  scm_dynwind_begin (0);

  mpz_t zCi;
  mpz_init (zCi);
  scm_dynwind_mpz_clear (zCi);

  mpz_t zCj;
  mpz_init (zCj);
  scm_dynwind_mpz_clear (zCj);

  const size_t degree = degree1 + degree2;
  SCM buffer[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = zero;
  for (size_t j = 0; j <= degree2; j++)
    {
      mpz_bincoef_ui (zCj, degree2, j);
      const SCM Cj = scm_from_mpz (zCj);
      for (size_t i = 0; i <= degree1; i++)
        {
          mpz_bincoef_ui (zCi, degree1, i);
          const SCM Ci = scm_from_mpz (zCi);
          const SCM tempj = scm_product (Cj, spline2[stride2 * (ssize_t) j]);
          const SCM tempi = scm_product (Ci, spline1[stride1 * (ssize_t) i]);
          buffer[j + i] = scm_sum (buffer[j + i], scm_product (tempj, tempi));
        }
    }
  for (size_t i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (zCi, degree, i);
      const SCM Ci = scm_from_mpz (zCi);
      buffer[i] = scm_divide (buffer[i], Ci);
    }

  scm_dynwind_end ();

  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
mul_f64_sbern (size_t degree1, ssize_t stride1, const double *spline1,
               size_t degree2, ssize_t stride2, const double *spline2,
               ssize_t result_stride, double *result)
{
  f64_convolve (degree1, stride1, spline1, degree2, stride2, spline2,
                result_stride, result);
}

VISIBLE void
mul_scm_sbern (size_t degree1, ssize_t stride1, const SCM *spline1,
               size_t degree2, ssize_t stride2, const SCM *spline2,
               ssize_t result_stride, SCM *result)
{
  scm_convolve (degree1, stride1, spline1, degree2, stride2, spline2,
                result_stride, result);
}

VISIBLE void
mul_f64_spower (size_t degree1, ssize_t stride1, const double *spline1,
                size_t degree2, ssize_t stride2, const double *spline2,
                ssize_t result_stride, double *result)
{
  // See J. Sánchez-Reyes, ‘Applications of the polynomial s-power
  // basis in geometry processing’, ACM Transactions on Graphics, vol
  // 19 no 1, January 2000, page 35. We compute the symmetric halves
  // of the product by the formula
  //
  //    c⁰ = a⁰∗b⁰ − shift₁(Δa∗Δb)
  //    c¹ = a¹∗b¹ − shift₁(Δa∗Δb)
  //
  // where ∗ is the convolution operator, shift₁ is a shift upwards in
  // degree by one (setting the least-degree term to zero), and
  //
  //    Δa = a¹ − a⁰
  //    Δb = b¹ − b⁰
  //

  // q1 = degree of a symmetric half of spline1.
  // q2 = degree of a symmetric half of spline2.
  const size_t q1 = degree1 / 2;
  const size_t q2 = degree2 / 2;

  // Compute Δa∗Δb.
  double DaDb[q1 + q2 + 1];
  {
    double delta1[q1 + 1];
    double delta2[q2 + 1];
    sub_f64_splines (q1, -stride1, &spline1[stride1 * (ssize_t) degree1],
                     stride1, spline1, 1, delta1);
    sub_f64_splines (q2, -stride2, &spline2[stride2 * (ssize_t) degree2],
                     stride2, spline2, 1, delta2);
    f64_convolve (q1, 1, delta1, q2, 1, delta2, 1, DaDb);
  }

  // Compute a⁰∗b⁰ − shift₁(Δa∗Δb).
  double c0[q1 + q2 + 2];
  f64_convolve (q1, stride1, spline1, q2, stride2, spline2, 1, c0);
  for (size_t i = 1; i <= q1 + q2; i++)
    c0[i] -= DaDb[i - 1];
  c0[q1 + q2 + 1] = -DaDb[q1 + q2];

  // Compute a¹∗b¹ − shift₁(Δa∗Δb).
  double c1[q1 + q2 + 2];
  f64_convolve (q1, -stride1, &spline1[stride1 * (ssize_t) degree1],
                q2, -stride2, &spline2[stride2 * (ssize_t) degree2], 1, c1);
  for (size_t i = 1; i <= q1 + q2; i++)
    c1[i] -= DaDb[i - 1];
  c1[q1 + q2 + 1] = -DaDb[q1 + q2];

  // Now put it all together.
  unsplit_f64_spower (degree1 + degree2, 1, c0, 1, c1, result_stride, result);
}

VISIBLE void
mul_scm_spower (size_t degree1, ssize_t stride1, const SCM *spline1,
                size_t degree2, ssize_t stride2, const SCM *spline2,
                ssize_t result_stride, SCM *result)
{
  // See J. Sánchez-Reyes, ‘Applications of the polynomial s-power
  // basis in geometry processing’, ACM Transactions on Graphics, vol
  // 19 no 1, January 2000, page 35. We compute the symmetric halves
  // of the product by the formula
  //
  //    c⁰ = a⁰∗b⁰ − shift₁(Δa∗Δb)
  //    c¹ = a¹∗b¹ − shift₁(Δa∗Δb)
  //
  // where ∗ is the convolution operator, shift₁ is a shift upwards in
  // degree by one (setting the least-degree term to zero), and
  //
  //    Δa = a¹ − a⁰
  //    Δb = b¹ − b⁰
  //

  const SCM zero = scm_from_uint (0);

  // q1 = degree of a symmetric half of spline1.
  // q2 = degree of a symmetric half of spline2.
  const size_t q1 = degree1 / 2;
  const size_t q2 = degree2 / 2;

  // Compute Δa∗Δb.
  SCM DaDb[q1 + q2 + 1];
  {
    SCM delta1[q1 + 1];
    SCM delta2[q2 + 1];
    sub_scm_splines (q1, -stride1, &spline1[stride1 * (ssize_t) degree1],
                     stride1, spline1, 1, delta1);
    sub_scm_splines (q2, -stride2, &spline2[stride2 * (ssize_t) degree2],
                     stride2, spline2, 1, delta2);
    scm_convolve (q1, 1, delta1, q2, 1, delta2, 1, DaDb);
  }

  // Compute a⁰∗b⁰ − shift₁(Δa∗Δb).
  SCM c0[q1 + q2 + 2];
  scm_convolve (q1, stride1, spline1, q2, stride2, spline2, 1, c0);
  for (size_t i = 1; i <= q1 + q2; i++)
    c0[i] = scm_difference (c0[i], DaDb[i - 1]);
  c0[q1 + q2 + 1] = scm_difference (zero, DaDb[q1 + q2]);

  // Compute a¹∗b¹ − shift₁(Δa∗Δb).
  SCM c1[q1 + q2 + 2];
  scm_convolve (q1, -stride1, &spline1[stride1 * (ssize_t) degree1],
                q2, -stride2, &spline2[stride2 * (ssize_t) degree2], 1, c1);
  for (size_t i = 1; i <= q1 + q2; i++)
    c1[i] = scm_difference (c1[i], DaDb[i - 1]);
  c1[q1 + q2 + 1] = scm_difference (zero, DaDb[q1 + q2]);

  // Now put it all together.
  unsplit_scm_spower (degree1 + degree2, 1, c0, 1, c1, result_stride, result);
}

//-------------------------------------------------------------------------

static SCM
scm_mul_f64_spline (const char *who,
                    void mul_f64_spline (size_t degree1,
                                         ssize_t stride1, const double *spline1,
                                         size_t degree2,
                                         ssize_t stride2, const double *spline2,
                                         ssize_t result_stride, double *result),
                    SCM spline1, SCM spline2)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline1, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, spline1, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1, &handle1,
                                              &dim1, &stride1);
  const double *_spline1 = scm_array_handle_f64_elements (&handle1);

  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2, &handle2,
                                              &dim2, &stride2);
  const double *_spline2 = scm_array_handle_f64_elements (&handle2);

  size_t dim = dim1 + dim2 - 1;

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                             scm_from_size_t
                                                             (dim))));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);

  mul_f64_spline (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2,
                  1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_mul_f64_mono (SCM spline1, SCM spline2)
{
  return scm_mul_f64_spline ("scm_mul_f64_mono", mul_f64_mono,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_f64_bern (SCM spline1, SCM spline2)
{
  return scm_mul_f64_spline ("scm_mul_f64_bern", mul_f64_bern,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_f64_sbern (SCM spline1, SCM spline2)
{
  return scm_mul_f64_spline ("scm_mul_f64_sbern", mul_f64_sbern,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_f64_spower (SCM spline1, SCM spline2)
{
  return scm_mul_f64_spline ("scm_mul_f64_spower", mul_f64_spower,
                             spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_mul_scm_spline (const char *who,
                    void mul_scm_spline (size_t degree1,
                                         ssize_t stride1, const SCM *spline1,
                                         size_t degree2,
                                         ssize_t stride2, const SCM *spline2,
                                         ssize_t result_stride, SCM *result),
                    SCM spline1, SCM spline2)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline1, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, spline1, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1, &handle1,
                                              &dim1, &stride1);
  const SCM *_spline1 = scm_array_handle_elements (&handle1);

  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2, &handle2,
                                              &dim2, &stride2);
  const SCM *_spline2 = scm_array_handle_elements (&handle2);

  size_t dim = dim1 + dim2 - 1;

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                       scm_from_size_t (dim))));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);

  mul_scm_spline (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2,
                  1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_mul_scm_mono (SCM spline1, SCM spline2)
{
  return scm_mul_scm_spline ("scm_mul_scm_mono", mul_scm_mono,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_scm_bern (SCM spline1, SCM spline2)
{
  return scm_mul_scm_spline ("scm_mul_scm_bern", mul_scm_bern,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_scm_sbern (SCM spline1, SCM spline2)
{
  return scm_mul_scm_spline ("scm_mul_scm_sbern", mul_scm_sbern,
                             spline1, spline2);
}

VISIBLE SCM
scm_mul_scm_spower (SCM spline1, SCM spline2)
{
  return scm_mul_scm_spline ("scm_mul_scm_spower", mul_scm_spower,
                             spline1, spline2);
}

//-------------------------------------------------------------------------

void init_math_polyspline_mul (void);

VISIBLE void
init_math_polyspline_mul (void)
{
  scm_c_define_gsubr ("poly:mul-f64-mono", 2, 0, 0, scm_mul_f64_mono);
  scm_c_define_gsubr ("poly:mul-scm-mono", 2, 0, 0, scm_mul_scm_mono);

  scm_c_define_gsubr ("poly:mul-f64-bern", 2, 0, 0, scm_mul_f64_bern);
  scm_c_define_gsubr ("poly:mul-scm-bern", 2, 0, 0, scm_mul_scm_bern);

  scm_c_define_gsubr ("poly:mul-f64-sbern", 2, 0, 0, scm_mul_f64_sbern);
  scm_c_define_gsubr ("poly:mul-scm-sbern", 2, 0, 0, scm_mul_scm_sbern);

  scm_c_define_gsubr ("poly:mul-f64-spower", 2, 0, 0, scm_mul_f64_spower);
  scm_c_define_gsubr ("poly:mul-scm-spower", 2, 0, 0, scm_mul_scm_spower);
}

//-------------------------------------------------------------------------
