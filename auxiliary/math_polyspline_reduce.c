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

VISIBLE size_t
min_degree_f64_mono (size_t degree, ssize_t stride, const double *poly)
{
  while (0 < degree && poly[stride * (ssize_t) degree] == 0.0)
    degree--;
  return degree;
}

VISIBLE size_t
min_degree_scm_mono (size_t degree, ssize_t stride, const SCM *poly)
{
  while (0 < degree
         && scm_is_true (scm_zero_p (poly[stride * (ssize_t) degree])))
    degree--;
  return degree;
}

VISIBLE size_t
min_degree_f64_spower (size_t degree, ssize_t stride, const double *poly)
{
  size_t d2 = degree / 2;
  while (0 < d2 && poly[stride * (ssize_t) d2] == 0.0
         && poly[stride * (ssize_t) (degree - d2)] == 0.0)
    d2--;
  size_t result = 2 * d2;
  if (poly[stride * (ssize_t) d2] != poly[stride * (ssize_t) (degree - d2)])
    result++;
  return result;
}

VISIBLE size_t
min_degree_scm_spower (size_t degree, ssize_t stride, const SCM *poly)
{
  size_t d2 = degree / 2;
  while (0 < d2 && scm_is_true (scm_zero_p (poly[stride * (ssize_t) d2]))
         && scm_is_true (scm_zero_p (poly[stride * (ssize_t) (degree - d2)])))
    d2--;
  size_t result = 2 * d2;
  if (scm_is_false (scm_num_eq_p (poly[stride * (ssize_t) d2],
                                  poly[stride * (ssize_t) (degree - d2)])))
    result++;
  return result;
}

//-------------------------------------------------------------------------

static size_t
scm_c_min_degree_f64 (const char *who,
                      size_t (*min_degree)
                      (size_t degree, ssize_t stride, const double *p),
                      SCM poly)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (poly, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, poly, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, poly, &handle,
                                              &dim, &stride);
  const double *_poly = scm_array_handle_f64_elements (&handle);

  size_t min_deg = min_degree (dim - 1, stride, _poly);

  scm_dynwind_end ();

  return min_deg;
}

static size_t
scm_c_min_degree_scm (const char *who,
                      size_t (*min_degree)
                      (size_t degree, ssize_t stride, const SCM *p), SCM poly)
{
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (poly, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, poly, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, poly, &handle,
                                              &dim, &stride);
  const SCM *_poly = scm_array_handle_elements (&handle);

  size_t min_deg = min_degree (dim - 1, stride, _poly);

  scm_dynwind_end ();

  return min_deg;
}

VISIBLE size_t
scm_c_min_degree_f64_mono (SCM poly)
{
  return scm_c_min_degree_f64 ("scm_c_min_degree_f64_mono",
                               min_degree_f64_mono, poly);
}

VISIBLE SCM
scm_min_degree_f64_mono (SCM poly)
{
  return scm_from_size_t (scm_c_min_degree_f64_mono (poly));
}

VISIBLE size_t
scm_c_min_degree_scm_mono (SCM poly)
{
  return scm_c_min_degree_scm ("scm_c_min_degree_scm_mono",
                               min_degree_scm_mono, poly);
}

VISIBLE SCM
scm_min_degree_scm_mono (SCM poly)
{
  return scm_from_size_t (scm_c_min_degree_scm_mono (poly));
}

VISIBLE size_t
scm_c_min_degree_f64_spower (SCM poly)
{
  return scm_c_min_degree_f64 ("scm_c_min_degree_f64_spower",
                               min_degree_f64_spower, poly);
}

VISIBLE SCM
scm_min_degree_f64_spower (SCM poly)
{
  return scm_from_size_t (scm_c_min_degree_f64_spower (poly));
}

VISIBLE size_t
scm_c_min_degree_scm_spower (SCM poly)
{
  return scm_c_min_degree_scm ("scm_c_min_degree_scm_spower",
                               min_degree_scm_spower, poly);
}

VISIBLE SCM
scm_min_degree_scm_spower (SCM poly)
{
  return scm_from_size_t (scm_c_min_degree_scm_spower (poly));
}

//-------------------------------------------------------------------------

VISIBLE void
reduce_degree_f64_mono (size_t old_degree, ssize_t stride, const double *poly,
                        size_t new_degree, ssize_t result_stride,
                        double *result)
{
  assert (new_degree <= old_degree);

  double buffer[new_degree + 1];
  for (size_t i = 0; i <= new_degree; i++)
    {
      buffer[i] = *poly;
      poly += stride;
    }
  for (size_t i = 0; i <= new_degree; i++)
    {
      *result = buffer[i];
      result += result_stride;
    }
}

VISIBLE void
reduce_degree_scm_mono (size_t old_degree, ssize_t stride, const SCM *poly,
                        size_t new_degree, ssize_t result_stride, SCM *result)
{
  assert (new_degree <= old_degree);

  SCM buffer[new_degree + 1];
  for (size_t i = 0; i <= new_degree; i++)
    {
      buffer[i] = *poly;
      poly += stride;
    }
  for (size_t i = 0; i <= new_degree; i++)
    {
      *result = buffer[i];
      result += result_stride;
    }
}

VISIBLE void
reduce_degree_f64_spower (size_t old_degree, ssize_t stride, const double *poly,
                          size_t new_degree, ssize_t result_stride,
                          double *result)
{
  assert (new_degree <= old_degree);

  double buffer[old_degree + 1];
  copy_f64_with_strides (1, buffer, stride, poly, old_degree + 1);
  unsplit_f64_spower (new_degree, 1, buffer, -1, &buffer[old_degree],
                      result_stride, result);
}

VISIBLE void
reduce_degree_scm_spower (size_t old_degree, ssize_t stride, const SCM *poly,
                          size_t new_degree, ssize_t result_stride, SCM *result)
{
  assert (new_degree <= old_degree);

  SCM buffer[old_degree + 1];
  copy_scm_with_strides (1, buffer, stride, poly, old_degree + 1);
  unsplit_scm_spower (new_degree, 1, buffer, -1, &buffer[old_degree],
                      result_stride, result);
}

//-------------------------------------------------------------------------

static SCM
scm_c_reduce_degree_f64 (const char *who,
                         void (*reduce) (size_t old_degree, ssize_t stride,
                                         const double *poly, size_t new_degree,
                                         ssize_t result_stride, double *result),
                         SCM poly, size_t new_degree)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle1;

  scm_dynwind_begin (0);

  scm_array_get_handle (poly, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, poly, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, poly, &handle,
                                              &dim, &stride);
  const double *_poly = scm_array_handle_f64_elements (&handle);

  if (dim <= new_degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition
        (_("the new degree must be less than or equal to the old degree")),
        rnrs_make_irritants_condition
        (scm_list_2 (poly, scm_from_size_t (new_degree)))));

  SCM bounds = scm_list_1 (scm_list_2 (scm_from_uint (1),
                                       scm_from_size_t (new_degree + 1)));
  SCM result =
    scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED, bounds);
  scm_array_get_handle (result, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  double *_result = scm_array_handle_f64_writable_elements (&handle1);

  reduce (dim - 1, stride, _poly, new_degree,
          scm_array_handle_dims (&handle1)[0].inc, _result);

  scm_dynwind_end ();

  return result;
}

static SCM
scm_c_reduce_degree_scm (const char *who,
                         void (*reduce) (size_t old_degree, ssize_t stride,
                                         const SCM *poly, size_t new_degree,
                                         ssize_t result_stride, SCM *result),
                         SCM poly, size_t new_degree)
{
  scm_t_array_handle handle;
  scm_t_array_handle handle1;

  scm_dynwind_begin (0);

  scm_array_get_handle (poly, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, poly, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, poly, &handle,
                                              &dim, &stride);
  const SCM *_poly = scm_array_handle_elements (&handle);

  if (dim <= new_degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition
        (_("the new degree must be less than or equal to the old degree")),
        rnrs_make_irritants_condition
        (scm_list_2 (poly, scm_from_size_t (new_degree)))));

  SCM bounds = scm_list_1 (scm_list_2 (scm_from_uint (1),
                                       scm_from_size_t (new_degree + 1)));
  SCM result = scm_make_array (SCM_UNSPECIFIED, bounds);
  scm_array_get_handle (result, &handle1);
  scm_dynwind_array_handle_release (&handle1);
  SCM *_result = scm_array_handle_writable_elements (&handle1);

  reduce (dim - 1, stride, _poly, new_degree,
          scm_array_handle_dims (&handle1)[0].inc, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_c_reduce_degree_f64_mono (SCM poly, size_t new_degree)
{
  return scm_c_reduce_degree_f64 ("scm_c_reduce_degree_f64_mono",
                                  reduce_degree_f64_mono, poly, new_degree);
}

VISIBLE SCM
scm_reduce_degree_f64_mono (SCM poly, SCM new_degree)
{
  return scm_c_reduce_degree_f64_mono (poly, scm_to_size_t (new_degree));
}

VISIBLE SCM
scm_c_reduce_degree_f64_spower (SCM poly, size_t new_degree)
{
  return scm_c_reduce_degree_f64 ("scm_c_reduce_degree_f64_spower",
                                  reduce_degree_f64_spower, poly, new_degree);
}

VISIBLE SCM
scm_reduce_degree_f64_spower (SCM poly, SCM new_degree)
{
  return scm_c_reduce_degree_f64_spower (poly, scm_to_size_t (new_degree));
}

VISIBLE SCM
scm_c_reduce_degree_scm_mono (SCM poly, size_t new_degree)
{
  return scm_c_reduce_degree_scm ("scm_c_reduce_degree_scm_mono",
                                  reduce_degree_scm_mono, poly, new_degree);
}

VISIBLE SCM
scm_reduce_degree_scm_mono (SCM poly, SCM new_degree)
{
  return scm_c_reduce_degree_scm_mono (poly, scm_to_size_t (new_degree));
}

VISIBLE SCM
scm_c_reduce_degree_scm_spower (SCM poly, size_t new_degree)
{
  return scm_c_reduce_degree_scm ("scm_c_reduce_degree_scm_spower",
                                  reduce_degree_scm_spower, poly, new_degree);
}

VISIBLE SCM
scm_reduce_degree_scm_spower (SCM poly, SCM new_degree)
{
  return scm_c_reduce_degree_scm_spower (poly, scm_to_size_t (new_degree));
}

//-------------------------------------------------------------------------

void init_math_polyspline_reduce (void);

VISIBLE void
init_math_polyspline_reduce (void)
{
  scm_c_define_gsubr ("poly:min-degree-f64-mono", 1, 0, 0,
                      scm_min_degree_f64_mono);
  scm_c_define_gsubr ("poly:min-degree-scm-mono", 1, 0, 0,
                      scm_min_degree_scm_mono);
  scm_c_define_gsubr ("poly:min-degree-f64-spower", 1, 0, 0,
                      scm_min_degree_f64_spower);
  scm_c_define_gsubr ("poly:min-degree-scm-spower", 1, 0, 0,
                      scm_min_degree_scm_spower);

  scm_c_define_gsubr ("poly:reduce-degree-f64-mono", 2, 0, 0,
                      scm_reduce_degree_f64_mono);
  scm_c_define_gsubr ("poly:reduce-degree-f64-spower", 2, 0, 0,
                      scm_reduce_degree_f64_spower);
  scm_c_define_gsubr ("poly:reduce-degree-scm-mono", 2, 0, 0,
                      scm_reduce_degree_scm_mono);
  scm_c_define_gsubr ("poly:reduce-degree-scm-spower", 2, 0, 0,
                      scm_reduce_degree_scm_spower);
}

//-------------------------------------------------------------------------
