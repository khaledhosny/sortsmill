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

VISIBLE void
deriv_f64_mono (size_t degree, ssize_t stride, const double *spline,
                ssize_t deriv_stride, double *deriv)
{
  if (degree == 0)
    deriv[0] = 0.0;
  else
    {
      deriv[0] = spline[stride];
      for (size_t i = 2; i <= degree; i++)
        deriv[deriv_stride * (ssize_t) (i - 1)] =
          i * spline[stride * (ssize_t) i];
    }
}

VISIBLE void
deriv_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                ssize_t deriv_stride, SCM *deriv)
{
  if (degree == 0)
    deriv[0] = scm_from_int (0);
  else
    {
      deriv[0] = spline[stride];
      for (size_t i = 2; i <= degree; i++)
        deriv[deriv_stride * (ssize_t) (i - 1)] =
          scm_product (scm_from_size_t (i), spline[stride * (ssize_t) i]);
    }
}

//-------------------------------------------------------------------------

static SCM
scm_deriv_f64_spline (const char *who,
                      void deriv (size_t degree, ssize_t stride,
                                  const double *spline, ssize_t deriv_stride,
                                  double *deriv), SCM spline)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle1;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const double *_spline = scm_array_handle_f64_elements (&handle);

  const size_t degree = dim - 1;
  const size_t deriv_degree = (degree == 0) ? (size_t) 0 : degree - 1;

  SCM bounds =
    scm_list_1 (scm_list_2
                (scm_from_uint (1), scm_from_size_t (deriv_degree + 1)));
  SCM result =
    scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED, bounds);

  scm_array_get_handle (result, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  double *_result = scm_array_handle_f64_writable_elements (&handle1);

  deriv (degree, stride, _spline, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_deriv_f64_mono (SCM spline)
{
  return scm_deriv_f64_spline ("scm_deriv_f64_mono", deriv_f64_mono, spline);
}

//-------------------------------------------------------------------------

static SCM
scm_deriv_scm_spline (const char *who,
                      void deriv (size_t degree, ssize_t stride,
                                  const SCM *spline, ssize_t deriv_stride,
                                  SCM *deriv), SCM spline)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle1;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  const size_t degree = dim - 1;
  const size_t deriv_degree = (degree == 0) ? (size_t) 0 : degree - 1;

  SCM bounds =
    scm_list_1 (scm_list_2
                (scm_from_uint (1), scm_from_size_t (deriv_degree + 1)));
  SCM result = scm_make_array (SCM_UNSPECIFIED, bounds);
  scm_array_get_handle (result, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  SCM *_result = scm_array_handle_writable_elements (&handle1);

  deriv (degree, stride, _spline, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_deriv_scm_mono (SCM spline)
{
  return scm_deriv_scm_spline ("scm_deriv_scm_mono", deriv_scm_mono, spline);
}

//-------------------------------------------------------------------------

void init_math_polyspline_deriv (void);

VISIBLE void
init_math_polyspline_deriv (void)
{
  scm_c_define_gsubr ("poly:deriv-f64-mono", 1, 0, 0, scm_deriv_f64_mono);
  scm_c_define_gsubr ("poly:deriv-scm-mono", 1, 0, 0, scm_deriv_scm_mono);
}

//-------------------------------------------------------------------------
