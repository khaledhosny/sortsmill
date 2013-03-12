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

VISIBLE void
subdiv_f64_bern (size_t degree, ssize_t stride, const double *spline,
                 double t,
                 ssize_t stride_a, double *a, ssize_t stride_b, double *b)
{
  double _a[degree + 1];
  double _b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    _b[i] = spline[stride * (ssize_t) i];
  for (size_t i = 0; i < degree; i++)
    {
      _a[i] = _b[0];
      for (size_t j = 0; j < degree - i; j++)
        _b[j] += t * (_b[j + 1] - _b[j]);
    }
  _a[degree] = _b[0];
  copy_f64_with_strides (stride_a, a, 1, _a, degree + 1);
  copy_f64_with_strides (stride_b, b, 1, _b, degree + 1);
}

VISIBLE void
scm_c_subdiv_bern (size_t degree, ssize_t stride, const SCM *spline,
                   SCM t, ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b)
{
  SCM _a[degree + 1];
  SCM _b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    _b[i] = spline[stride * (ssize_t) i];
  for (size_t i = 0; i < degree; i++)
    {
      _a[i] = _b[0];
      for (size_t j = 0; j < degree - i; j++)
        _b[j] =
          scm_sum (_b[j], scm_product (t, scm_difference (_b[j + 1], _b[j])));
    }
  _a[degree] = _b[0];
  copy_scm_with_strides (stride_a, a, 1, _a, degree + 1);
  copy_scm_with_strides (stride_b, b, 1, _b, degree + 1);
}

VISIBLE void
subdiv_f64_sbern (size_t degree, ssize_t stride, const double *spline,
                  double t,
                  ssize_t stride_a, double *a, ssize_t stride_b, double *b)
{
  double _a[degree + 1];
  double _b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    _b[i] = spline[stride * (ssize_t) i] / bincoef (degree, i);
  for (size_t i = 0; i < degree; i++)
    {
      _a[i] = _b[0];
      for (size_t j = 0; j < degree - i; j++)
        _b[j] += t * (_b[j + 1] - _b[j]);
    }
  _a[degree] = _b[0];
  for (size_t i = 0; i <= degree; i++)
    {
      double C = bincoef (degree, i);
      _a[i] *= C;
      _b[i] *= C;
    }
  copy_f64_with_strides (stride_a, a, 1, _a, degree + 1);
  copy_f64_with_strides (stride_b, b, 1, _b, degree + 1);
}


VISIBLE void
scm_c_subdiv_sbern (size_t degree, ssize_t stride, const SCM *spline,
                    SCM t, ssize_t stride_a, SCM *a, ssize_t stride_b, SCM *b)
{
  scm_dynwind_begin (0);

  mpz_t C;
  mpz_init (C);
  scm_dynwind_mpz_clear (C);

  SCM _a[degree + 1];
  SCM _b[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      _b[i] = scm_divide (spline[stride * (ssize_t) i], scm_from_mpz (C));
    }
  for (size_t i = 0; i < degree; i++)
    {
      _a[i] = _b[0];
      for (size_t j = 0; j < degree - i; j++)
        _b[j] =
          scm_sum (_b[j], scm_product (t, scm_difference (_b[j + 1], _b[j])));
    }
  _a[degree] = _b[0];
  for (size_t i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      SCM _C = scm_from_mpz (C);
      _a[i] = scm_product (_a[i], _C);
      _b[i] = scm_product (_b[i], _C);
    }
  copy_scm_with_strides (stride_a, a, 1, _a, degree + 1);
  copy_scm_with_strides (stride_b, b, 1, _b, degree + 1);

  scm_dynwind_end ();
}

//-------------------------------------------------------------------------

static SCM
scm_subdiv_f64_spline (const char *who,
                       void subdiv_f64_spline (size_t degree, ssize_t stride,
                                               const double *spline,
                                               double t,
                                               ssize_t stride_a, double *a,
                                               ssize_t stride_b, double *b),
                       SCM vector, SCM t)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, vector, &handle,
                                              &dim, &stride);
  const double *spline = scm_array_handle_f64_elements (&handle);

  SCM values[2];

  values[0] = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                    scm_list_1 (scm_list_2
                                                (scm_from_uint (1),
                                                 scm_from_size_t (dim))));
  scm_array_get_handle (values[0], &handle_a);
  scm_dynwind_array_handle_release (&handle_a);
  double *a = scm_array_handle_f64_writable_elements (&handle_a);

  values[1] = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                    scm_list_1 (scm_list_2
                                                (scm_from_uint (1),
                                                 scm_from_size_t (dim))));
  scm_array_get_handle (values[1], &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  double *b = scm_array_handle_f64_writable_elements (&handle_b);

  subdiv_f64_spline (dim - 1, stride, spline, scm_to_double (t), 1, a, 1, b);

  scm_dynwind_end ();

  return scm_c_values (values, 2);
}

VISIBLE SCM
scm_subdiv_f64_bern (SCM vector, SCM t)
{
  return scm_subdiv_f64_spline ("scm_subdiv_f64_bern",
                                subdiv_f64_bern, vector, t);
}

VISIBLE SCM
scm_subdiv_f64_sbern (SCM vector, SCM t)
{
  return scm_subdiv_f64_spline ("scm_subdiv_f64_sbern",
                                subdiv_f64_sbern, vector, t);
}

//-------------------------------------------------------------------------

static SCM
scm_subdiv_scm_spline (const char *who,
                       void scm_c_subdiv_spline (size_t degree,
                                                 ssize_t stride,
                                                 const SCM *spline, SCM t,
                                                 ssize_t stride_a, SCM *a,
                                                 ssize_t stride_b, SCM *b),
                       SCM vector, SCM t)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_dynwind_begin (0);

  scm_array_get_handle (vector, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, vector, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, vector, &handle,
                                              &dim, &stride);
  const SCM *spline = scm_array_handle_elements (&handle);

  SCM values[2];

  values[0] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_1 (scm_list_2
                                          (scm_from_uint (1),
                                           scm_from_size_t (dim))));
  scm_array_get_handle (values[0], &handle_a);
  scm_dynwind_array_handle_release (&handle_a);
  SCM *a = scm_array_handle_writable_elements (&handle_a);

  values[1] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_1 (scm_list_2
                                          (scm_from_uint (1),
                                           scm_from_size_t (dim))));
  scm_array_get_handle (values[1], &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  SCM *b = scm_array_handle_writable_elements (&handle_b);

  scm_c_subdiv_spline (dim - 1, stride, spline, t, 1, a, 1, b);

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

//-------------------------------------------------------------------------

void init_math_polyspline_subdiv (void);

VISIBLE void
init_math_polyspline_subdiv (void)
{
  //  scm_c_define_gsubr ("poly:subdiv-f64-mono", 2, 0, 0, scm_subdiv_f64_mono);
  //  scm_c_define_gsubr ("poly:subdiv-scm-mono", 2, 0, 0, scm_subdiv_scm_mono);

  scm_c_define_gsubr ("poly:subdiv-f64-bern", 2, 0, 0, scm_subdiv_f64_bern);
  scm_c_define_gsubr ("poly:subdiv-scm-bern", 2, 0, 0, scm_subdiv_scm_bern);

  scm_c_define_gsubr ("poly:subdiv-f64-sbern", 2, 0, 0, scm_subdiv_f64_sbern);
  scm_c_define_gsubr ("poly:subdiv-scm-sbern", 2, 0, 0, scm_subdiv_scm_sbern);

  //  scm_c_define_gsubr ("poly:subdiv-f64-spower", 2, 0, 0, scm_subdiv_f64_spower);
  //  scm_c_define_gsubr ("poly:subdiv-scm-spower", 2, 0, 0, scm_subdiv_scm_spower);
}

//-------------------------------------------------------------------------
