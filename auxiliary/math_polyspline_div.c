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
#include <basics.h>
#include <intl.h>
#include <assert.h>

//-------------------------------------------------------------------------

// Return the actual (minimum) degree of a polynomial in monomial
// form, or -1 if the polynomial equals zero identically. If degree <
// 0 then the routine returns that value unchanged.
static ssize_t
f64_mono_min_degree (size_t degree, const double *poly)
{
  ssize_t d = degree;
  while (0 <= d && poly[d] == 0.0)
    d--;
  return d;
}

// Return the actual (minimum) degree of a polynomial in monomial
// form, or -1 if the polynomial equals zero identically. If degree <
// 0 then the routine returns that value unchanged.
static ssize_t
scm_mono_min_degree (size_t degree, const SCM *poly)
{
  ssize_t d = degree;
  while (0 <= d && scm_is_true (scm_zero_p (poly[d])))
    d--;
  return d;
}

static void
f64_fill (size_t degree, double *poly, double fill)
{
  for (size_t i = 0; i <= degree; i++)
    poly[i] = fill;
}

static void
scm_fill (size_t degree, SCM *poly, SCM fill)
{
  for (size_t i = 0; i <= degree; i++)
    poly[i] = fill;
}

//-------------------------------------------------------------------------

VISIBLE void
div_f64_mono (size_t degree1, ssize_t stride1, const double *spline1,
              size_t degree2, ssize_t stride2, const double *spline2,
              size_t *degree_q, ssize_t stride_q, double *quotient,
              size_t *degree_r, ssize_t stride_r, double *remainder,
              bool *division_by_zero)
{
  // See http://en.wikipedia.org/wiki/Polynomial_long_division and
  // http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials#Euclidean_division

  double denom[degree2 + 1];
  copy_f64_with_strides (1, denom, stride2, spline2, degree2 + 1);
  const ssize_t deg_denom = f64_mono_min_degree (degree2, denom);

  *division_by_zero = (deg_denom < 0);

  if (!*division_by_zero)
    {
      double q[degree1 + 1];
      double r[degree1 + 1];

      f64_fill (degree1, q, 0.0);
      copy_f64_with_strides (1, r, stride1, spline1, degree1 + 1);

      ssize_t deg_r = f64_mono_min_degree (degree1, r);
      *degree_q = sszmax (0, deg_r - deg_denom);

      while (deg_denom <= deg_r)
        {
          q[deg_r - deg_denom] = r[deg_r] / denom[deg_denom];

          r[deg_r] = 0.0;
          for (size_t i = 1; i <= deg_denom; i++)
            r[deg_r - i] -= q[deg_r - deg_denom] * denom[deg_denom - i];

          deg_r = f64_mono_min_degree (degree1, r);
        }

      *degree_r = sszmax (0, deg_r);
      copy_f64_with_strides (stride_q, quotient, 1, q, degree1 + 1);
      copy_f64_with_strides (stride_r, remainder, 1, r, degree1 + 1);
    }
}

VISIBLE void
div_scm_mono (size_t degree1, ssize_t stride1, const SCM *spline1,
              size_t degree2, ssize_t stride2, const SCM *spline2,
              size_t *degree_q, ssize_t stride_q, SCM *quotient,
              size_t *degree_r, ssize_t stride_r, SCM *remainder,
              bool *division_by_zero)
{
  // See http://en.wikipedia.org/wiki/Polynomial_long_division and
  // http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials#Euclidean_division

  SCM denom[degree2 + 1];
  copy_scm_with_strides (1, denom, stride2, spline2, degree2 + 1);
  const ssize_t deg_denom = scm_mono_min_degree (degree2, denom);

  *division_by_zero = (deg_denom < 0);

  if (!*division_by_zero)
    {
      SCM q[degree1 + 1];
      SCM r[degree1 + 1];

      const SCM zero = scm_from_int (0);

      scm_fill (degree1, q, zero);
      copy_scm_with_strides (1, r, stride1, spline1, degree1 + 1);

      ssize_t deg_r = scm_mono_min_degree (degree1, r);
      *degree_q = sszmax (0, deg_r - deg_denom);

      while (deg_denom <= deg_r)
        {
          q[deg_r - deg_denom] = scm_divide (r[deg_r], denom[deg_denom]);

          r[deg_r] = zero;
          for (size_t i = 1; i <= deg_denom; i++)
            r[deg_r - i] =
              scm_difference (r[deg_r - i],
                              scm_product (q[deg_r - deg_denom],
                                           denom[deg_denom - i]));

          deg_r = scm_mono_min_degree (degree1, r);
        }

      *degree_r = sszmax (0, deg_r);
      copy_scm_with_strides (stride_q, quotient, 1, q, degree1 + 1);
      copy_scm_with_strides (stride_r, remainder, 1, r, degree1 + 1);
    }
}

//-------------------------------------------------------------------------

static void
gcd_recursion_f64 (ssize_t degree, double *a, double *b, double *result)
{
  const ssize_t deg_a = f64_mono_min_degree (degree, a);
  const ssize_t deg_b = f64_mono_min_degree (degree, b);
  if (0 <= deg_b)
    {
      double q[degree + 1];
      size_t deg_q;
      size_t deg_r;
      bool division_by_zero;
      div_f64_mono (sszmax (0, deg_a), 1, a, deg_b, 1, b,
                    &deg_q, 1, q, &deg_r, 1, a, &division_by_zero);
      assert (!division_by_zero);
      gcd_recursion_f64 (deg_b, b, a, result);
    }
  else if (0 <= deg_a)
    {
      // Make the result monic.
      for (size_t i = 0; i < deg_a; i++)
        a[i] /= a[deg_a];
      a[deg_a] = 1.0;
      memmove (result, a, (degree + 1) * sizeof (double));
    }
}

static void
gcd_recursion_scm (ssize_t degree, SCM *a, SCM *b, SCM *result)
{
  const ssize_t deg_a = scm_mono_min_degree (degree, a);
  const ssize_t deg_b = scm_mono_min_degree (degree, b);
  if (0 <= deg_b)
    {
      SCM q[degree + 1];
      size_t deg_q;
      size_t deg_r;
      bool division_by_zero;
      div_scm_mono (sszmax (0, deg_a), 1, a, deg_b, 1, b,
                    &deg_q, 1, q, &deg_r, 1, a, &division_by_zero);
      assert (!division_by_zero);
      gcd_recursion_scm (deg_b, b, a, result);
    }
  else if (0 <= deg_a)
    {
      // Make the result monic.
      for (size_t i = 0; i < deg_a; i++)
        a[i] = scm_divide (a[i], a[deg_a]);
      a[deg_a] = scm_from_int (1);
      memmove (result, a, (degree + 1) * sizeof (SCM));
    }
}

VISIBLE void
gcd_f64_mono (size_t degree1, ssize_t stride1, const double *spline1,
              size_t degree2, ssize_t stride2, const double *spline2,
              size_t *degree, ssize_t stride, double *gcd)
{
  // See
  // http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials

  double poly1[degree1 + 1];
  copy_f64_with_strides (1, poly1, stride1, spline1, degree1 + 1);

  double poly2[degree2 + 1];
  copy_f64_with_strides (1, poly2, stride2, spline2, degree2 + 1);

  size_t degree_max = szmax (degree1, degree2);

  double a[degree_max + 1];
  f64_fill (degree_max, a, 0.0);
  memcpy (a, poly1, (degree1 + 1) * sizeof (double));

  double b[degree_max + 1];
  f64_fill (degree_max, b, 0.0);
  memcpy (b, poly2, (degree2 + 1) * sizeof (double));

  gcd_recursion_f64 (degree_max, a, b, a);

  *degree = sszmax (0, f64_mono_min_degree (degree_max, a));
  copy_f64_with_strides (stride, gcd, 1, a, degree_max + 1);
}

VISIBLE void
gcd_scm_mono (size_t degree1, ssize_t stride1, const SCM *spline1,
              size_t degree2, ssize_t stride2, const SCM *spline2,
              size_t *degree, ssize_t stride, SCM *gcd)
{
  // See
  // http://en.wikipedia.org/wiki/Greatest_common_divisor_of_two_polynomials

  const SCM zero = scm_from_int (0);

  SCM poly1[degree1 + 1];
  copy_scm_with_strides (1, poly1, stride1, spline1, degree1 + 1);

  SCM poly2[degree2 + 1];
  copy_scm_with_strides (1, poly2, stride2, spline2, degree2 + 1);

  size_t degree_max = szmax (degree1, degree2);

  SCM a[degree_max + 1];
  scm_fill (degree_max, a, zero);
  memcpy (a, poly1, (degree1 + 1) * sizeof (SCM));

  SCM b[degree_max + 1];
  scm_fill (degree_max, b, zero);
  memcpy (b, poly2, (degree2 + 1) * sizeof (SCM));

  gcd_recursion_scm (degree_max, a, b, a);

  *degree = sszmax (0, scm_mono_min_degree (degree_max, a));
  copy_scm_with_strides (stride, gcd, 1, a, degree_max + 1);
}

//-------------------------------------------------------------------------

static SCM
scm_div_f64_spline (const char *who,
                    void div (size_t degree1, ssize_t stride1,
                              const double *spline1, size_t degree2,
                              ssize_t stride2, const double *spline2,
                              size_t *degree_q, ssize_t stride_q,
                              double *quotient, size_t *degree_r,
                              ssize_t stride_r, double *remainder,
                              bool *division_by_zero), SCM spline1, SCM spline2)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle_q;
  scm_t_array_handle handle_r;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline1, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, spline1, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1,
                                              &handle1, &dim1, &stride1);
  const double *_spline1 = scm_array_handle_f64_elements (&handle1);

  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2,
                                              &handle2, &dim2, &stride2);
  const double *_spline2 = scm_array_handle_f64_elements (&handle2);

  double q[dim1];
  double r[dim1];
  size_t deg_q;
  size_t deg_r;
  bool division_by_zero;
  div (dim1 - 1, stride1, _spline1, dim2 - 1,
       stride2, _spline2, &deg_q, 1, q, &deg_r, 1, r, &division_by_zero);

  if (division_by_zero)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("polynomial division by zero")),
        rnrs_make_irritants_condition (scm_list_2 (spline1, spline2))));

  SCM quotient = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                       scm_list_1 (scm_list_2
                                                   (scm_from_uint (1),
                                                    scm_from_size_t
                                                    (deg_q + 1))));
  scm_array_get_handle (quotient, &handle_q);
  scm_dynwind_array_handle_release (&handle_q);
  double *_quotient = scm_array_handle_f64_writable_elements (&handle_q);
  memcpy (_quotient, q, (deg_q + 1) * sizeof (double));

  SCM remainder = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                        scm_list_1 (scm_list_2
                                                    (scm_from_uint (1),
                                                     scm_from_size_t
                                                     (deg_r + 1))));
  scm_array_get_handle (remainder, &handle_r);
  scm_dynwind_array_handle_release (&handle_r);
  double *_remainder = scm_array_handle_f64_writable_elements (&handle_r);
  memcpy (_remainder, r, (deg_r + 1) * sizeof (double));

  scm_dynwind_end ();

  SCM values[2] = { quotient, remainder };
  return scm_c_values (values, 2);
}

VISIBLE SCM
scm_div_f64_mono (SCM spline1, SCM spline2)
{
  return scm_div_f64_spline ("scm_div_f64_mono",
                             div_f64_mono, spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_div_scm_spline (const char *who,
                    void div (size_t degree1, ssize_t stride1,
                              const SCM *spline1, size_t degree2,
                              ssize_t stride2, const SCM *spline2,
                              size_t *degree_q, ssize_t stride_q,
                              SCM *quotient, size_t *degree_r,
                              ssize_t stride_r, SCM *remainder,
                              bool *division_by_zero), SCM spline1, SCM spline2)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle_q;
  scm_t_array_handle handle_r;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline1, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, spline1, &handle1);

  size_t dim1;
  ssize_t stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1,
                                              &handle1, &dim1, &stride1);
  const SCM *_spline1 = scm_array_handle_elements (&handle1);
  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  size_t dim2;
  ssize_t stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2,
                                              &handle2, &dim2, &stride2);
  const SCM *_spline2 = scm_array_handle_elements (&handle2);

  SCM q[dim1];
  SCM r[dim1];
  size_t deg_q;
  size_t deg_r;
  bool division_by_zero;
  div (dim1 - 1, stride1, _spline1, dim2 - 1,
       stride2, _spline2, &deg_q, 1, q, &deg_r, 1, r, &division_by_zero);

  if (division_by_zero)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("polynomial division by zero")),
        rnrs_make_irritants_condition (scm_list_2 (spline1, spline2))));

  SCM quotient_bounds =
    scm_list_1 (scm_list_2 (scm_from_uint (1), scm_from_size_t (deg_q + 1)));
  SCM quotient = scm_make_array (SCM_UNSPECIFIED, quotient_bounds);
  scm_array_get_handle (quotient, &handle_q);
  scm_dynwind_array_handle_release (&handle_q);
  SCM *_quotient = scm_array_handle_writable_elements (&handle_q);
  memcpy (_quotient, q, (deg_q + 1) * sizeof (SCM));

  SCM remainder_bounds =
    scm_list_1 (scm_list_2 (scm_from_uint (1), scm_from_size_t (deg_r + 1)));
  SCM remainder = scm_make_array (SCM_UNSPECIFIED, remainder_bounds);
  scm_array_get_handle (remainder, &handle_r);
  scm_dynwind_array_handle_release (&handle_r);
  SCM *_remainder = scm_array_handle_writable_elements (&handle_r);
  memcpy (_remainder, r, (deg_r + 1) * sizeof (SCM));

  scm_dynwind_end ();

  SCM values[2] = { quotient, remainder };
  return scm_c_values (values, 2);
}

VISIBLE SCM
scm_div_scm_mono (SCM spline1, SCM spline2)
{
  return scm_div_scm_spline ("scm_div_scm_mono",
                             div_scm_mono, spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_gcd_f64_spline (const char *who,
                    void gcd (size_t degree1, ssize_t stride1,
                              const double *spline1, size_t degree2,
                              ssize_t stride2, const double *spline2,
                              size_t *degree, ssize_t stride, double *gcd),
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

  double gcd_poly[szmax (dim1, dim2)];
  size_t deg;
  gcd (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2, &deg, 1,
       gcd_poly);

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                             scm_from_size_t
                                                             (deg + 1))));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);
  memcpy (_result, gcd_poly, (deg + 1) * sizeof (double));

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gcd_f64_mono (SCM spline1, SCM spline2)
{
  return scm_gcd_f64_spline ("scm_gcd_f64_mono",
                             gcd_f64_mono, spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_gcd_scm_spline (const char *who,
                    void gcd (size_t degree1, ssize_t stride1,
                              const SCM *spline1, size_t degree2,
                              ssize_t stride2, const SCM *spline2,
                              size_t *degree, ssize_t stride, SCM *gcd),
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

  SCM gcd_poly[szmax (dim1, dim2)];
  size_t deg;
  gcd (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2, &deg, 1,
       gcd_poly);

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                       scm_from_size_t
                                                       (deg + 1))));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);
  memcpy (_result, gcd_poly, (deg + 1) * sizeof (SCM));

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gcd_scm_mono (SCM spline1, SCM spline2)
{
  return scm_gcd_scm_spline ("scm_gcd_scm_mono",
                             gcd_scm_mono, spline1, spline2);
}

//-------------------------------------------------------------------------

void init_math_polyspline_div (void);
VISIBLE void
init_math_polyspline_div (void)
{
  scm_c_define_gsubr ("poly:div-f64-mono", 2, 0, 0, scm_div_f64_mono);
  scm_c_define_gsubr ("poly:div-scm-mono", 2, 0, 0, scm_div_scm_mono);
  //scm_c_define_gsubr ("poly:div-f64-spower", 2, 0, 0, scm_div_f64_spower);
  //scm_c_define_gsubr ("poly:div-scm-spower", 2, 0, 0, scm_div_scm_spower);

  scm_c_define_gsubr ("poly:gcd-f64-mono", 2, 0, 0, scm_gcd_f64_mono);
  scm_c_define_gsubr ("poly:gcd-scm-mono", 2, 0, 0, scm_gcd_scm_mono);
  //scm_c_define_gsubr ("poly:gcd-f64-spower", 2, 0, 0, scm_gcd_f64_spower);
  //scm_c_define_gsubr ("poly:gcd-scm-spower", 2, 0, 0, scm_gcd_scm_spower);
}

//-------------------------------------------------------------------------
