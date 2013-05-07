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
#include <intl.h>
#include <assert.h>

//-------------------------------------------------------------------------

VISIBLE void
elev_f64_mono (size_t new_degree,
               size_t degree, ssize_t stride, const double *spline,
               ssize_t result_stride, double *result)
{
  assert (degree <= new_degree);

  // Degree elevation in the monomial basis is simply padding on the
  // right with zeros.

  double buffer[new_degree + 1];
  copy_f64_with_strides (1, buffer, stride, spline, degree + 1);
  for (size_t i = degree + 1; i <= new_degree; i++)
    buffer[i] = 0.0;
  copy_f64_with_strides (result_stride, result, 1, buffer, new_degree + 1);
}

VISIBLE void
elev_scm_mono (size_t new_degree,
               size_t degree, ssize_t stride, const SCM *spline,
               ssize_t result_stride, SCM *result)
{
  assert (degree <= new_degree);

  const SCM zero = scm_from_int (0);

  // Degree elevation in the monomial basis is simply padding on the
  // right with zeros.

  SCM buffer[new_degree + 1];
  copy_scm_with_strides (1, buffer, stride, spline, degree + 1);
  for (size_t i = degree + 1; i <= new_degree; i++)
    buffer[i] = zero;
  copy_scm_with_strides (result_stride, result, 1, buffer, new_degree + 1);
}

VISIBLE void
elev_f64_bern (size_t new_degree,
               size_t degree, ssize_t stride, const double *spline,
               ssize_t result_stride, double *result)
{
  assert (degree <= new_degree);

  // Multiply by a polynomial that is identically equal to one.

  double one[(new_degree - degree) + 1];
  one_f64_bern (new_degree - degree, 1, one);
  mul_f64_bern (new_degree - degree, 1, one, degree, stride, spline,
                result_stride, result);
}

VISIBLE void
elev_scm_bern (size_t new_degree,
               size_t degree, ssize_t stride, const SCM *spline,
               ssize_t result_stride, SCM *result)
{
  assert (degree <= new_degree);

  // Multiply by a polynomial that is identically equal to one.

  SCM one[(new_degree - degree) + 1];
  one_scm_bern (new_degree - degree, 1, one);
  mul_scm_bern (new_degree - degree, 1, one, degree, stride, spline,
                result_stride, result);
}

VISIBLE void
elev_f64_sbern (size_t new_degree,
                size_t degree, ssize_t stride, const double *spline,
                ssize_t result_stride, double *result)
{
  assert (degree <= new_degree);

  // Multiply by a polynomial that is identically equal to one.

  double one[(new_degree - degree) + 1];
  one_f64_sbern (new_degree - degree, 1, one);
  mul_f64_sbern (new_degree - degree, 1, one, degree, stride, spline,
                 result_stride, result);
}

VISIBLE void
elev_scm_sbern (size_t new_degree,
                size_t degree, ssize_t stride, const SCM *spline,
                ssize_t result_stride, SCM *result)
{
  assert (degree <= new_degree);

  // Multiply by a polynomial that is identically equal to one.

  SCM one[(new_degree - degree) + 1];
  one_scm_sbern (new_degree - degree, 1, one);
  mul_scm_sbern (new_degree - degree, 1, one, degree, stride, spline,
                 result_stride, result);
}


VISIBLE void
elev_f64_spower (size_t new_degree,
                 size_t degree, ssize_t stride, const double *spline,
                 ssize_t result_stride, double *result)
{
  assert (degree <= new_degree);

  // To elevate degree in the s-power basis one can simply pad the
  // symmetric halves with zeros.

  double buffer[new_degree + 1];
  copy_f64_with_strides (1, buffer, stride, spline, degree / 2 + 1);
  copy_f64_with_strides (-1, &buffer[new_degree],
                         -stride, &spline[stride * (ssize_t) degree],
                         degree / 2 + 1);
  for (size_t i = degree / 2 + 1; i < new_degree - degree / 2; i++)
    buffer[i] = 0.0;
  copy_f64_with_strides (result_stride, result, 1, buffer, new_degree + 1);
}

VISIBLE void
elev_scm_spower (size_t new_degree,
                 size_t degree, ssize_t stride, const SCM *spline,
                 ssize_t result_stride, SCM *result)
{
  assert (degree <= new_degree);

  // To elevate degree in the s-power basis one can simply pad the
  // symmetric halves with zeros.

  const SCM zero = scm_from_int (0);

  SCM buffer[new_degree + 1];
  copy_scm_with_strides (1, buffer, stride, spline, degree / 2 + 1);
  copy_scm_with_strides (-1, &buffer[new_degree],
                         -stride, &spline[stride * (ssize_t) degree],
                         degree / 2 + 1);
  for (size_t i = degree / 2 + 1; i < new_degree - degree / 2; i++)
    buffer[i] = zero;
  copy_scm_with_strides (result_stride, result, 1, buffer, new_degree + 1);
}

//-------------------------------------------------------------------------

static void
the_new_degree_is_not_an_elevation (const char *who, SCM new_degree,
                                    SCM old_degree, SCM spline)
{
  const char *localized_message =
    _("the new degree ~a is less than the old degree ~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_2 (new_degree, old_degree));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (new_degree, spline))));
}

//-------------------------------------------------------------------------

static SCM
scm_elev_f64_spline (const char *who,
                     void elev_f64_spline (size_t new_degree,
                                           size_t degree,
                                           ssize_t stride,
                                           const double *spline,
                                           ssize_t result_stride,
                                           double *result),
                     SCM new_degree, SCM spline)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle2;

  scm_dynwind_begin (0);

  const size_t _new_degree = scm_to_size_t (new_degree);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const double *_spline = scm_array_handle_f64_elements (&handle);

  if (_new_degree < dim - 1)
    the_new_degree_is_not_an_elevation (who, new_degree,
                                        scm_from_size_t (dim - 1), spline);

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_oneplus (new_degree)));
  scm_array_get_handle (result, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  double *_result = scm_array_handle_f64_writable_elements (&handle2);

  elev_f64_spline (_new_degree, dim - 1, stride, _spline, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_elev_f64_mono (SCM new_degree, SCM spline)
{
  return scm_elev_f64_spline ("scm_elev_f64_mono", elev_f64_mono,
                              new_degree, spline);
}

VISIBLE SCM
scm_elev_f64_bern (SCM new_degree, SCM spline)
{
  return scm_elev_f64_spline ("scm_elev_f64_bern", elev_f64_bern,
                              new_degree, spline);
}

VISIBLE SCM
scm_elev_f64_sbern (SCM new_degree, SCM spline)
{
  return scm_elev_f64_spline ("scm_elev_f64_sbern", elev_f64_sbern,
                              new_degree, spline);
}

VISIBLE SCM
scm_elev_f64_spower (SCM new_degree, SCM spline)
{
  return scm_elev_f64_spline ("scm_elev_f64_spower", elev_f64_spower,
                              new_degree, spline);
}

//-------------------------------------------------------------------------

static SCM
scm_elev_scm_spline (const char *who,
                     void elev_scm_spline (size_t new_degree,
                                           size_t degree,
                                           ssize_t stride,
                                           const SCM *spline,
                                           ssize_t result_stride,
                                           SCM *result),
                     SCM new_degree, SCM spline)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle2;

  scm_dynwind_begin (0);

  const size_t _new_degree = scm_to_size_t (new_degree);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  if (_new_degree < dim - 1)
    the_new_degree_is_not_an_elevation (who, new_degree,
                                        scm_from_size_t (dim - 1), spline);

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_oneplus (new_degree)));
  scm_array_get_handle (result, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  SCM *_result = scm_array_handle_writable_elements (&handle2);

  elev_scm_spline (_new_degree, dim - 1, stride, _spline, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_elev_scm_mono (SCM new_degree, SCM spline)
{
  return scm_elev_scm_spline ("scm_elev_scm_mono", elev_scm_mono,
                              new_degree, spline);
}

VISIBLE SCM
scm_elev_scm_bern (SCM new_degree, SCM spline)
{
  return scm_elev_scm_spline ("scm_elev_scm_bern", elev_scm_bern,
                              new_degree, spline);
}

VISIBLE SCM
scm_elev_scm_sbern (SCM new_degree, SCM spline)
{
  return scm_elev_scm_spline ("scm_elev_scm_sbern", elev_scm_sbern,
                              new_degree, spline);
}


VISIBLE SCM
scm_elev_scm_spower (SCM new_degree, SCM spline)
{
  return scm_elev_scm_spline ("scm_elev_scm_spower", elev_scm_spower,
                              new_degree, spline);
}

//-------------------------------------------------------------------------

void init_math_polyspline_elev (void);

VISIBLE void
init_math_polyspline_elev (void)
{
  scm_c_define_gsubr ("poly:elev-f64-mono", 2, 0, 0, scm_elev_f64_mono);
  scm_c_define_gsubr ("poly:elev-scm-mono", 2, 0, 0, scm_elev_scm_mono);

  scm_c_define_gsubr ("poly:elev-f64-bern", 2, 0, 0, scm_elev_f64_bern);
  scm_c_define_gsubr ("poly:elev-scm-bern", 2, 0, 0, scm_elev_scm_bern);

  scm_c_define_gsubr ("poly:elev-f64-sbern", 2, 0, 0, scm_elev_f64_sbern);
  scm_c_define_gsubr ("poly:elev-scm-sbern", 2, 0, 0, scm_elev_scm_sbern);

  scm_c_define_gsubr ("poly:elev-f64-spower", 2, 0, 0, scm_elev_f64_spower);
  scm_c_define_gsubr ("poly:elev-scm-spower", 2, 0, 0, scm_elev_scm_spower);
}

//-------------------------------------------------------------------------
