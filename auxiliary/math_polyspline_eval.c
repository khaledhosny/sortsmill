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

// Do not use ‘floating-point multiply and add’, so we should get the
// same results with f64matrix and general matrix. But keep these
// macros as documentation.
#undef MY_FAST_FMA

#ifndef MY_FAST_FMA
#ifdef FP_FAST_FMA
#define MY_FAST_FMA fma
#else
#define MY_FAST_FMA(x, y, z) ((x) * (y) + (z))
#endif
#endif

//-------------------------------------------------------------------------

VISIBLE double
eval_f64_mono (size_t degree, ssize_t stride, const double *spline, double t)
{
  // Horner’s rule.
  double x = spline[stride * (ssize_t) degree];
  for (size_t i = 1; i <= degree; i++)
    x = MY_FAST_FMA (x, t, spline[stride * (ssize_t) (degree - i)]);
  return x;
}

VISIBLE SCM
eval_scm_mono (size_t degree, ssize_t stride, const SCM *spline, SCM t)
{
  // Horner’s rule.
  SCM x = spline[stride * (ssize_t) degree];
  for (size_t i = 1; i <= degree; i++)
    x = scm_sum (scm_product (x, t), spline[stride * (ssize_t) (degree - i)]);
  return x;
}

VISIBLE double
eval_f64_sbern_schumaker_volk (size_t degree, ssize_t stride,
                               const double *spline, double t)
{
  double v;

  const double s = 1.0 - t;

  if (t <= 0.5)
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      double u = t / s;
      v = spline[stride * (ssize_t) degree];
      for (size_t i = 1; i <= degree; i++)
        v = MY_FAST_FMA (v, u, spline[stride * (ssize_t) (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      double power = s;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  else
    {
      // Horner form in the variable @var{u} = @var{s} / @var{t}.
      double u = s / t;
      v = spline[0];
      for (size_t i = 1; i <= degree; i++)
        v = MY_FAST_FMA (v, u, spline[stride * (ssize_t) i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      double power = t;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  return v;
}

VISIBLE double
eval_f64_bern_schumaker_volk (size_t degree, ssize_t stride,
                              const double *spline, double t)
{
  double v;

  const double s = 1.0 - t;

  if (t <= 0.5)
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      double u = t / s;
      v = spline[stride * (ssize_t) degree];
      for (size_t i = 1; i <= degree; i++)
        v =
          MY_FAST_FMA (v, u,
                       bincoef (degree, degree - i) *
                       spline[stride * (ssize_t) (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      double power = s;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  else
    {
      // Horner form in the variable @var{u} = @var{s} / @var{t}.
      double u = s / t;
      v = spline[0];
      for (size_t i = 1; i <= degree; i++)
        v =
          MY_FAST_FMA (v, u,
                       bincoef (degree, i) * spline[stride * (ssize_t) i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      double power = t;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  return v;
}

VISIBLE SCM
eval_scm_sbern_schumaker_volk (size_t degree, ssize_t stride,
                               const SCM *spline, SCM t)
{
  SCM v;

  const SCM one = scm_from_int (1);

  const SCM s = scm_difference (one, t);

  if (scm_is_true (scm_leq_p (scm_sum (t, t), one)))
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      SCM u = scm_divide (t, s);
      v = spline[stride * (ssize_t) degree];
      for (size_t i = 1; i <= degree; i++)
        v =
          scm_sum (scm_product (v, u), spline[stride * (ssize_t) (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      SCM power = s;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v = scm_product (v, power);
          i >>= 1;
          if (i != 0)
            power = scm_product (power, power);
        }
    }
  else
    {
      // Horner form in the variable @var{u} = @var{s} / @var{t}.
      SCM u = scm_divide (s, t);
      v = spline[0];
      for (size_t i = 1; i <= degree; i++)
        v = scm_sum (scm_product (v, u), spline[stride * (ssize_t) i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      SCM power = t;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v = scm_product (v, power);
          i >>= 1;
          if (i != 0)
            power = scm_product (power, power);
        }
    }
  return v;
}

VISIBLE SCM
eval_scm_bern_schumaker_volk (size_t degree, ssize_t stride,
                              const SCM *spline, SCM t)
{
  SCM v;

  const SCM one = scm_from_int (1);

  const SCM s = scm_difference (one, t);

  if (scm_is_true (scm_leq_p (scm_sum (t, t), one)))
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      SCM u = scm_divide (t, s);
      v = spline[stride * (ssize_t) degree];
      for (size_t i = 1; i <= degree; i++)
        v = scm_sum (scm_product (v, u),
                     scm_product (scm_c_bincoef (degree, degree - i),
                                  spline[stride * (ssize_t) (degree - i)]));

      // Multiply by @var{s} raised to the power @var{degree}.
      SCM power = s;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v = scm_product (v, power);
          i >>= 1;
          if (i != 0)
            power = scm_product (power, power);
        }
    }
  else
    {
      // Horner form in the variable @var{u} = @var{s} / @var{t}.
      SCM u = scm_divide (s, t);
      v = spline[0];
      for (size_t i = 1; i <= degree; i++)
        v = scm_sum (scm_product (v, u),
                     scm_product (scm_c_bincoef (degree, i),
                                  spline[stride * (ssize_t) i]));

      // Multiply by @var{t} raised to the power @var{degree}.
      SCM power = t;
      size_t i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v = scm_product (v, power);
          i >>= 1;
          if (i != 0)
            power = scm_product (power, power);
        }
    }

  return v;
}

VISIBLE double
eval_f64_sbern_de_casteljau (size_t degree, ssize_t stride,
                             const double *spline, double t)
{
  const double t1 = 1.0 - t;

  double b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    b[i] = spline[stride * (ssize_t) i] / bincoef (degree, i);
  for (size_t i = 0; i < degree; i++)
    for (size_t j = 0; j < degree - i; j++)
      b[j] = t1 * b[j] + t * b[j + 1];
  return b[0];
}

VISIBLE double
eval_f64_bern_de_casteljau (size_t degree, ssize_t stride,
                            const double *spline, double t)
{
  const double t1 = 1.0 - t;

  double b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    b[i] = spline[stride * (ssize_t) i];
  for (size_t i = 0; i < degree; i++)
    for (size_t j = 0; j < degree - i; j++)
      b[j] = t1 * b[j] + t * b[j + 1];
  return b[0];
}

VISIBLE SCM
eval_scm_sbern_de_casteljau (size_t degree, ssize_t stride,
                             const SCM *spline, SCM t)
{
  const SCM t1 = scm_difference (scm_from_int (1), t);

  SCM b[degree + 1];

  for (size_t i = 0; i <= degree; i++)
    b[i] = scm_divide (spline[stride * (ssize_t) i], scm_c_bincoef (degree, i));

  for (size_t i = 0; i < degree; i++)
    for (size_t j = 0; j < degree - i; j++)
      b[j] = scm_sum (scm_product (t1, b[j]), scm_product (t, b[j + 1]));

  return b[0];
}

VISIBLE SCM
eval_scm_bern_de_casteljau (size_t degree, ssize_t stride,
                            const SCM *spline, SCM t)
{
  const SCM t1 = scm_difference (scm_from_int (1), t);

  SCM b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    b[i] = spline[stride * (ssize_t) i];
  for (size_t i = 0; i < degree; i++)
    for (size_t j = 0; j < degree - i; j++)
      b[j] = scm_sum (scm_product (t1, b[j]), scm_product (t, b[j + 1]));
  return b[0];
}

VISIBLE double
eval_f64_spower (size_t degree, ssize_t stride, const double *spline, double t)
{
  // Compute a convex combination of two shorter polynomials in
  //
  //    s = t(1 − t)
  //
  // The Sánchez-Reyes coefficients are the monomial coefficients of
  // the two shorter polynomials in s.

  const double t1 = 1.0 - t;
  const double s = t * t1;

  const double left = eval_f64_mono (degree / 2, stride, spline, s);
  const double right =
    eval_f64_mono (degree / 2, -stride, &spline[stride * (ssize_t) degree], s);

  return (t1 * left + t * right);
}

VISIBLE SCM
eval_scm_spower (size_t degree, ssize_t stride, const SCM *spline, SCM t)
{
  // Compute a convex combination of two shorter polynomials in
  //
  //    s = t(1 − t)
  //
  // The Sánchez-Reyes coefficients are the monomial coefficients of
  // the two shorter polynomials in s.

  const SCM t1 = scm_difference (scm_from_uint (1), t);
  const SCM s = scm_product (t, t1);

  const SCM left = eval_scm_mono (degree / 2, stride, spline, s);
  const SCM right =
    eval_scm_mono (degree / 2, -stride, &spline[stride * (ssize_t) degree],
                   s);

  return scm_sum (scm_product (t1, left), scm_product (t, right));
}

//-------------------------------------------------------------------------

static SCM
scm_eval_f64_spline (const char *who,
                     double (*eval_f64_spline) (size_t degree, ssize_t stride,
                                                const double *spline, double t),
                     SCM vector, SCM t)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, vector, &handle,
                                              &dim, &stride);
  const double *spline = scm_array_handle_f64_elements (&handle);

  double value = eval_f64_spline (dim - 1, stride, spline, scm_to_double (t));

  scm_dynwind_end ();

  return scm_from_double (value);
}

VISIBLE SCM
scm_eval_f64_mono (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_mono", eval_f64_mono, vector, t);
}

VISIBLE SCM
scm_eval_f64_bern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_bern_schumaker_volk",
                              eval_f64_bern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_f64_bern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_bern_de_casteljau",
                              eval_f64_bern_de_casteljau, vector, t);
}

VISIBLE SCM
scm_eval_f64_sbern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_sbern_schumaker_volk",
                              eval_f64_sbern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_f64_sbern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_sbern_de_casteljau",
                              eval_f64_sbern_de_casteljau, vector, t);
}

VISIBLE SCM
scm_eval_f64_spower (SCM vector, SCM t)
{
  return scm_eval_f64_spline ("scm_eval_f64_spower",
                              eval_f64_spower, vector, t);
}

//-------------------------------------------------------------------------

static SCM
scm_eval_scm_spline (const char *who,
                     SCM (*eval_scm_spline) (size_t degree, ssize_t stride,
                                             const SCM *spline, SCM t),
                     SCM vector, SCM t)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, vector, &handle,
                                              &dim, &stride);
  const SCM *spline = scm_array_handle_elements (&handle);

  SCM value = eval_scm_spline (dim - 1, stride, spline, t);

  scm_dynwind_end ();

  return value;
}

VISIBLE SCM
scm_eval_scm_mono (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_mono", eval_scm_mono, vector, t);
}

VISIBLE SCM
scm_eval_scm_bern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_bern_schumaker_volk",
                              eval_scm_bern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_scm_bern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_bern_de_casteljau",
                              eval_scm_bern_de_casteljau, vector, t);
}

VISIBLE SCM
scm_eval_scm_sbern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_sbern_schumaker_volk",
                              eval_scm_sbern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_scm_sbern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_sbern_de_casteljau",
                              eval_scm_sbern_de_casteljau, vector, t);
}

VISIBLE SCM
scm_eval_scm_spower (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_spower", eval_scm_spower, vector,
                              t);
}

//-------------------------------------------------------------------------

void init_math_polyspline_eval (void);

VISIBLE void
init_math_polyspline_eval (void)
{
  scm_c_define_gsubr ("poly:eval-f64-mono", 2, 0, 0, scm_eval_f64_mono);
  scm_c_define_gsubr ("poly:eval-scm-mono", 2, 0, 0, scm_eval_scm_mono);

  scm_c_define_gsubr ("poly:eval-f64-bern-schumaker-volk", 2, 0, 0,
                      scm_eval_f64_bern_schumaker_volk);
  scm_c_define_gsubr ("poly:eval-scm-bern-schumaker-volk", 2, 0, 0,
                      scm_eval_scm_bern_schumaker_volk);

  scm_c_define_gsubr ("poly:eval-f64-bern-de-casteljau", 2, 0, 0,
                      scm_eval_f64_bern_de_casteljau);
  scm_c_define_gsubr ("poly:eval-scm-bern-de-casteljau", 2, 0, 0,
                      scm_eval_scm_bern_de_casteljau);

  scm_c_define_gsubr ("poly:eval-f64-sbern-schumaker-volk", 2, 0, 0,
                      scm_eval_f64_sbern_schumaker_volk);
  scm_c_define_gsubr ("poly:eval-scm-sbern-schumaker-volk", 2, 0, 0,
                      scm_eval_scm_sbern_schumaker_volk);

  scm_c_define_gsubr ("poly:eval-f64-sbern-de-casteljau", 2, 0, 0,
                      scm_eval_f64_sbern_de_casteljau);
  scm_c_define_gsubr ("poly:eval-scm-sbern-de-casteljau", 2, 0, 0,
                      scm_eval_scm_sbern_de_casteljau);

  scm_c_define_gsubr ("poly:eval-f64-spower", 2, 0, 0, scm_eval_f64_spower);
  scm_c_define_gsubr ("poly:eval-scm-spower", 2, 0, 0, scm_eval_scm_spower);
}

//-------------------------------------------------------------------------
