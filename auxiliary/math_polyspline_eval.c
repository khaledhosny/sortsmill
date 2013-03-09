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
eval_f64_mono (unsigned int degree, int stride, const double *spline, double t)
{
  // Horner’s rule.
  double x = spline[stride * degree];
  for (int i = 1; i <= degree; i++)
    x = MY_FAST_FMA (x, t, spline[stride * (degree - i)]);
  return x;
}

VISIBLE SCM
scm_c_eval_mono (unsigned int degree, int stride, const SCM *spline, SCM t)
{
  // Horner’s rule.
  SCM x = spline[stride * degree];
  for (int i = 1; i <= degree; i++)
    x = scm_sum (scm_product (x, t), spline[stride * (degree - i)]);
  return x;
}

VISIBLE double
eval_f64_sbern_schumaker_volk (unsigned int degree, int stride,
                               const double *spline, double t)
{
  double v;

  const double s = 1.0 - t;

  if (t <= 0.5)
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      double u = t / s;
      v = spline[stride * degree];
      for (unsigned int i = 1; i <= degree; i++)
        v = MY_FAST_FMA (v, u, spline[stride * (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      double power = s;
      unsigned int i = degree;
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
      for (unsigned int i = 1; i <= degree; i++)
        v = MY_FAST_FMA (v, u, spline[stride * i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      double power = t;
      unsigned int i = degree;
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
eval_f64_bern_schumaker_volk (unsigned int degree, int stride,
                              const double *spline, double t)
{
  double v;

  const double s = 1.0 - t;

  if (t <= 0.5)
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      double u = t / s;
      v = spline[stride * degree];
      for (unsigned int i = 1; i <= degree; i++)
        v =
          MY_FAST_FMA (v, u,
                       bincoef (degree,
                                degree - i) * spline[stride * (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      double power = s;
      unsigned int i = degree;
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
      for (unsigned int i = 1; i <= degree; i++)
        v = MY_FAST_FMA (v, u, bincoef (degree, i) * spline[stride * i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      double power = t;
      unsigned int i = degree;
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
scm_c_eval_sbern_schumaker_volk (unsigned int degree, int stride,
                                 const SCM *spline, SCM t)
{
  SCM v;

  const SCM one = scm_from_int (1);

  const SCM s = scm_difference (one, t);

  if (scm_is_true (scm_leq_p (scm_sum (t, t), one)))
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      SCM u = scm_divide (t, s);
      v = spline[stride * degree];
      for (unsigned int i = 1; i <= degree; i++)
        v = scm_sum (scm_product (v, u), spline[stride * (degree - i)]);

      // Multiply by @var{s} raised to the power @var{degree}.
      SCM power = s;
      unsigned int i = degree;
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
      for (unsigned int i = 1; i <= degree; i++)
        v = scm_sum (scm_product (v, u), spline[stride * i]);

      // Multiply by @var{t} raised to the power @var{degree}.
      SCM power = t;
      unsigned int i = degree;
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
scm_c_eval_bern_schumaker_volk (unsigned int degree, int stride,
                                const SCM *spline, SCM t)
{
  SCM v;

  scm_dynwind_begin (0);

  mpz_t C;
  mpz_init (C);
  scm_dynwind_mpz_clear (C);

  const SCM one = scm_from_int (1);

  const SCM s = scm_difference (one, t);

  if (scm_is_true (scm_leq_p (scm_sum (t, t), one)))
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      SCM u = scm_divide (t, s);
      v = spline[stride * degree];
      for (unsigned int i = 1; i <= degree; i++)
        {
          mpz_bincoef_ui (C, degree, degree - i);
          v = scm_sum (scm_product (v, u),
                       scm_product (scm_from_mpz (C),
                                    spline[stride * (degree - i)]));
        }

      // Multiply by @var{s} raised to the power @var{degree}.
      SCM power = s;
      unsigned int i = degree;
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
      for (unsigned int i = 1; i <= degree; i++)
        {
          mpz_bincoef_ui (C, degree, i);
          v =
            scm_sum (scm_product (v, u),
                     scm_product (scm_from_mpz (C), spline[stride * i]));
        }

      // Multiply by @var{t} raised to the power @var{degree}.
      SCM power = t;
      unsigned int i = degree;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v = scm_product (v, power);
          i >>= 1;
          if (i != 0)
            power = scm_product (power, power);
        }
    }

  scm_dynwind_end ();
  return v;
}

VISIBLE double
eval_f64_sbern_de_casteljau (unsigned int degree, int stride,
                             const double *spline, double t)
{
  double b[degree + 1];
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * i] / bincoef (degree, i);
  for (unsigned int i = 0; i < degree; i++)
    for (unsigned int j = 0; j < degree; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

VISIBLE double
eval_f64_bern_de_casteljau (unsigned int degree, int stride,
                            const double *spline, double t)
{
  double b[degree + 1];
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * i];
  for (unsigned int i = 0; i < degree; i++)
    for (unsigned int j = 0; j < degree; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

VISIBLE SCM
scm_c_eval_sbern_de_casteljau (unsigned int degree, int stride,
                               const SCM *spline, SCM t)
{
  scm_dynwind_begin (0);

  mpz_t C;
  mpz_init (C);
  scm_dynwind_mpz_clear (C);

  SCM b[degree + 1];
  for (unsigned int i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      b[i] = scm_divide (spline[stride * i], scm_from_mpz (C));
    }

  scm_dynwind_end ();

  for (unsigned int i = 0; i < degree; i++)
    for (unsigned int j = 0; j < degree; j++)
      b[j] = scm_sum (b[j], scm_product (t, scm_difference (b[j + 1], b[j])));

  return b[0];
}

VISIBLE SCM
scm_c_eval_bern_de_casteljau (unsigned int degree, int stride,
                              const SCM *spline, SCM t)
{
  SCM b[degree + 1];
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * i];
  for (unsigned int i = 0; i < degree; i++)
    for (unsigned int j = 0; j < degree; j++)
      b[j] = scm_sum (b[j], scm_product (t, scm_difference (b[j + 1], b[j])));
  return b[0];
}

//-------------------------------------------------------------------------

static SCM
scm_eval_f64_spline (const char *who,
                     double (*eval_f64_spline) (unsigned int degree, int stride,
                                                const double *spline, double t),
                     SCM vector, SCM t)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  const size_t rank = scm_array_handle_rank (&handle);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  const double *spline = scm_array_handle_f64_elements (&handle);

  unsigned int degree;
  int stride;

  if (rank == 1 || dims[1].ubnd == dims[1].lbnd)
    {
      // A vector or a column matrix
      degree = dims[0].ubnd - dims[0].lbnd;
      stride = dims[0].inc;
    }
  else if (dims[0].ubnd == dims[0].lbnd)
    {
      // A row matrix.
      degree = dims[1].ubnd - dims[1].lbnd;
      stride = dims[1].inc;
    }
  else
    exception__expected_a_vector (who, scm_list_1 (vector));

  double value = eval_f64_spline (degree, stride, spline, scm_to_double (t));

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

//-------------------------------------------------------------------------

static SCM
scm_eval_scm_spline (const char *who,
                     SCM (*scm_c_eval_spline) (unsigned int degree, int stride,
                                               const SCM *spline, SCM t),
                     SCM vector, SCM t)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  const size_t rank = scm_array_handle_rank (&handle);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  const SCM *spline = scm_array_handle_elements (&handle);

  unsigned int degree;
  int stride;

  if (rank == 1 || dims[1].ubnd == dims[1].lbnd)
    {
      // A vector or a column matrix
      degree = dims[0].ubnd - dims[0].lbnd;
      stride = dims[0].inc;
    }
  else if (dims[0].ubnd == dims[0].lbnd)
    {
      // A row matrix.
      degree = dims[1].ubnd - dims[1].lbnd;
      stride = dims[1].inc;
    }
  else
    exception__expected_a_vector (who, scm_list_1 (vector));

  SCM value = scm_c_eval_spline (degree, stride, spline, t);

  scm_dynwind_end ();

  return value;
}

VISIBLE SCM
scm_eval_scm_mono (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_mono", scm_c_eval_mono, vector, t);
}

VISIBLE SCM
scm_eval_scm_bern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_bern_schumaker_volk",
                              scm_c_eval_bern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_scm_bern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_bern_de_casteljau",
                              scm_c_eval_bern_de_casteljau, vector, t);
}

VISIBLE SCM
scm_eval_scm_sbern_schumaker_volk (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_sbern_schumaker_volk",
                              scm_c_eval_sbern_schumaker_volk, vector, t);
}

VISIBLE SCM
scm_eval_scm_sbern_de_casteljau (SCM vector, SCM t)
{
  return scm_eval_scm_spline ("scm_eval_scm_sbern_de_casteljau",
                              scm_c_eval_sbern_de_casteljau, vector, t);
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

}

//-------------------------------------------------------------------------
