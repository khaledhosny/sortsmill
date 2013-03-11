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

//-------------------------------------------------------------------------

static void
f64_convolve (unsigned int degree1, int stride1, const double *poly1,
              unsigned int degree2, int stride2, const double *poly2,
              double *result)
{
  // This is just the ‘naïve’ algorithm (no Karatsuba, FFT, etc.).

  const int degree = degree1 + degree2;
  for (unsigned int i = 0; i <= degree; i++)
    result[i] = 0.0;
  for (unsigned int j = 0; j <= degree2; j++)
    for (unsigned int i = 0; i <= degree1; i++)
      result[j + i] += poly2[stride2 * (int) j] * poly1[stride1 * (int) i];
}

//-------------------------------------------------------------------------

VISIBLE void
mul_f64_mono (unsigned int degree1, int stride1, const double *spline1,
              unsigned int degree2, int stride2, const double *spline2,
              double *result)
{
  f64_convolve (degree1, stride1, spline1, degree2, stride2, spline2, result);
}

VISIBLE void
mul_f64_bern (unsigned int degree1, int stride1, const double *spline1,
              unsigned int degree2, int stride2, const double *spline2,
              double *result)
{
  const int degree = degree1 + degree2;
  for (unsigned int i = 0; i <= degree; i++)
    result[i] = 0.0;
  for (unsigned int j = 0; j <= degree2; j++)
    for (unsigned int i = 0; i <= degree1; i++)
      result[j + i] +=
        (bincoef (degree2, j) * spline2[stride2 * (int) j]) *
        (bincoef (degree1, i) * spline1[stride1 * (int) i]);
  for (unsigned int i = 0; i <= degree; i++)
    result[i] /= bincoef (degree, i);
}

VISIBLE void
mul_f64_sbern (unsigned int degree1, int stride1, const double *spline1,
               unsigned int degree2, int stride2, const double *spline2,
               double *result)
{
  f64_convolve (degree1, stride1, spline1, degree2, stride2, spline2, result);
}

//-------------------------------------------------------------------------

static SCM
scm_mul_f64_spline (const char *who,
                    void mul_f64_spline (unsigned int degree1, int stride1,
                                         const double *spline1,
                                         unsigned int degree2, int stride2,
                                         const double *spline2, double *result),
                    SCM spline1, SCM spline2)
{
  scm_t_array_handle handle1;
  scm_t_array_handle handle2;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline1, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  assert_c_rank_1_or_2_array (who, spline1, &handle1);

  unsigned int dim1;
  int stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1, &handle1,
                                              &dim1, &stride1);
  const double *_spline1 = scm_array_handle_f64_elements (&handle1);

  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  unsigned int dim2;
  int stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2, &handle2,
                                              &dim2, &stride2);
  const double *_spline2 = scm_array_handle_f64_elements (&handle2);

  unsigned int dim = dim1 + dim2 - 1;

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_2 (scm_from_uint (1),
                                                 scm_from_uint (dim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);

  mul_f64_spline (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2,
                  _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_mul_f64_mono (SCM vector, SCM t)
{
  return scm_mul_f64_spline ("scm_mul_f64_mono", mul_f64_mono, vector, t);
}

VISIBLE SCM
scm_mul_f64_bern (SCM vector, SCM t)
{
  return scm_mul_f64_spline ("scm_mul_f64_bern", mul_f64_bern, vector, t);
}

VISIBLE SCM
scm_mul_f64_sbern (SCM vector, SCM t)
{
  return scm_mul_f64_spline ("scm_mul_f64_sbern", mul_f64_sbern, vector, t);
}

//-------------------------------------------------------------------------

/*
static SCM
scm_subdiv_scm_spline (const char *who,
                       void scm_c_subdiv_spline (unsigned int degree,
                                                 int stride, const SCM *spline,
                                                 SCM t, SCM *a, SCM *b),
                       SCM vector, SCM t)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  unsigned int dim;
  int stride;
  scm_array_handle_get_vector_dim_and_stride (who, vector, &handle,
                                              &dim, &stride);
  const SCM *spline = scm_array_handle_elements (&handle);

  SCM values[2];

  values[0] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_2 (scm_from_uint (1),
                                          scm_from_uint (dim)));
  scm_array_get_handle (values[0], &handle_a);
  scm_dynwind_array_handle_release (&handle_a);
  SCM *a = scm_array_handle_writable_elements (&handle_a);

  values[1] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_2 (scm_from_uint (1),
                                          scm_from_uint (dim)));
  scm_array_get_handle (values[1], &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  SCM *b = scm_array_handle_writable_elements (&handle_b);

  scm_c_subdiv_spline (dim - 1, stride, spline, t, a, b);

  scm_dynwind_end ();

  return scm_c_values (values, 2);
}

VISIBLE SCM
scm_subdiv_scm_bern (SCM vector, SCM t)
{
  return scm_subdiv_scm_spline ("scm_subdiv_scm_bern",
                                scm_c_subdiv_bern, vector, t);
}

VISIBLE SCM
scm_subdiv_scm_sbern (SCM vector, SCM t)
{
  return scm_subdiv_scm_spline ("scm_subdiv_scm_sbern",
                                scm_c_subdiv_sbern, vector, t);
}
*/

//-------------------------------------------------------------------------

void init_math_polyspline_mul (void);

VISIBLE void
init_math_polyspline_mul (void)
{
  scm_c_define_gsubr ("poly:mul-f64-mono", 2, 0, 0, scm_mul_f64_mono);
  //  scm_c_define_gsubr ("poly:mul-scm-mono", 2, 0, 0, scm_mul_scm_mono);

  scm_c_define_gsubr ("poly:mul-f64-bern", 2, 0, 0, scm_mul_f64_bern);
  //  scm_c_define_gsubr ("poly:mul-scm-bern", 2, 0, 0, scm_mul_scm_bern);

  scm_c_define_gsubr ("poly:mul-f64-sbern", 2, 0, 0, scm_mul_f64_sbern);
  //  scm_c_define_gsubr ("poly:mul-scm-sbern", 2, 0, 0, scm_mul_scm_sbern);

  //  scm_c_define_gsubr ("poly:mul-f64-spower", 2, 0, 0, scm_mul_f64_spower);
  //  scm_c_define_gsubr ("poly:mul-scm-spower", 2, 0, 0, scm_mul_scm_spower);
}

//-------------------------------------------------------------------------
