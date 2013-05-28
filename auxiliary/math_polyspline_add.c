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
#include <sortsmill/copy_with_strides.h>
#include <intl.h>

static size_t
max_size (size_t size1, size_t size2)
{
  return (size1 < size2) ? size2 : size1;
}

//-------------------------------------------------------------------------

VISIBLE void
add_f64_splines (size_t degree,
                 ssize_t stride1, const double *spline1,
                 ssize_t stride2, const double *spline2,
                 ssize_t result_stride, double *result)
{
  double buffer[degree + 1];
  copy_f64_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] += spline2[stride2 * (ssize_t) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
add_scm_splines (size_t degree,
                 ssize_t stride1, const SCM *spline1,
                 ssize_t stride2, const SCM *spline2,
                 ssize_t result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  copy_scm_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = scm_sum (buffer[i], spline2[stride2 * (ssize_t) i]);
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
sub_f64_splines (size_t degree,
                 ssize_t stride1, const double *spline1,
                 ssize_t stride2, const double *spline2,
                 ssize_t result_stride, double *result)
{
  double buffer[degree + 1];
  copy_f64_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] -= spline2[stride2 * (ssize_t) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
sub_scm_splines (size_t degree,
                 ssize_t stride1, const SCM *spline1,
                 ssize_t stride2, const SCM *spline2,
                 ssize_t result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  copy_scm_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = scm_difference (buffer[i], spline2[stride2 * (ssize_t) i]);
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
weighted_add_f64_splines (size_t degree,
                          double w1, ssize_t stride1, const double *spline1,
                          double w2, ssize_t stride2, const double *spline2,
                          ssize_t result_stride, double *result)
{
  double buffer[degree + 1];
  copy_f64_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = w1 * buffer[i] + w2 * spline2[stride2 * (ssize_t) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
weighted_add_scm_splines (size_t degree,
                          SCM w1, ssize_t stride1, const SCM *spline1,
                          SCM w2, ssize_t stride2, const SCM *spline2,
                          ssize_t result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  copy_scm_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (size_t i = 0; i <= degree; i++)
    buffer[i] = scm_sum (scm_product (w1, buffer[i]),
                         scm_product (w2, spline2[stride2 * (ssize_t) i]));
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

//-------------------------------------------------------------------------

static void
add_f64_with_degree_elevation (void
                               elevate (size_t new_degree, size_t degree,
                                        ssize_t stride, const double *spline,
                                        ssize_t result_stride, double *result),
                               size_t degree1, ssize_t stride1,
                               const double *spline1, size_t degree2,
                               ssize_t stride2, const double *spline2,
                               ssize_t result_stride, double *result)
{
  if (degree1 < degree2)
    add_f64_with_degree_elevation (elevate, degree2, stride2, spline2,
                                   degree1, stride1, spline1,
                                   result_stride, result);
  else
    {
      double buffer[degree1 + 1];
      elevate (degree1, degree2, stride2, spline2, 1, buffer);
      add_f64_splines (degree1, stride1, spline1, 1, buffer,
                       result_stride, result);
    }
}

static void
add_scm_with_degree_elevation (void
                               elevate (size_t new_degree, size_t degree,
                                        ssize_t stride, const SCM *spline,
                                        ssize_t result_stride, SCM *result),
                               size_t degree1, ssize_t stride1,
                               const SCM *spline1, size_t degree2,
                               ssize_t stride2, const SCM *spline2,
                               ssize_t result_stride, SCM *result)
{
  if (degree1 < degree2)
    add_scm_with_degree_elevation (elevate, degree2, stride2, spline2,
                                   degree1, stride1, spline1,
                                   result_stride, result);
  else
    {
      SCM buffer[degree1 + 1];
      elevate (degree1, degree2, stride2, spline2, 1, buffer);
      add_scm_splines (degree1, stride1, spline1, 1, buffer,
                       result_stride, result);
    }
}

VISIBLE void
add_f64_mono (size_t degree1, ssize_t stride1,
              const double *spline1, size_t degree2,
              ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  if (degree1 < degree2)
    add_f64_mono (degree2, stride2, spline2,
                  degree1, stride1, spline1, result_stride, result);
  else
    {
      // In the monomial basis one can simply add the terms of like
      // degree.

      double buffer[degree1 + 1];
      copy_f64_with_strides (1, buffer, stride1, spline1, degree1 + 1);
      add_f64_splines (degree2, 1, buffer, stride2, spline2, 1, buffer);
      copy_f64_with_strides (result_stride, result, 1, buffer, degree1 + 1);
    }
}

VISIBLE void
add_scm_mono (size_t degree1, ssize_t stride1,
              const SCM *spline1, size_t degree2,
              ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  if (degree1 < degree2)
    add_scm_mono (degree2, stride2, spline2,
                  degree1, stride1, spline1, result_stride, result);
  else
    {
      // In the monomial basis one can simply add the terms of like
      // degree.

      SCM buffer[degree1 + 1];
      copy_scm_with_strides (1, buffer, stride1, spline1, degree1 + 1);
      add_scm_splines (degree2, 1, buffer, stride2, spline2, 1, buffer);
      copy_scm_with_strides (result_stride, result, 1, buffer, degree1 + 1);
    }
}

VISIBLE void
add_f64_bern (size_t degree1, ssize_t stride1,
              const double *spline1, size_t degree2,
              ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  add_f64_with_degree_elevation (elev_f64_bern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
add_scm_bern (size_t degree1, ssize_t stride1,
              const SCM *spline1, size_t degree2,
              ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  add_scm_with_degree_elevation (elev_scm_bern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
add_f64_sbern (size_t degree1, ssize_t stride1,
               const double *spline1, size_t degree2,
               ssize_t stride2, const double *spline2,
               ssize_t result_stride, double *result)
{
  add_f64_with_degree_elevation (elev_f64_sbern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
add_scm_sbern (size_t degree1, ssize_t stride1,
               const SCM *spline1, size_t degree2,
               ssize_t stride2, const SCM *spline2,
               ssize_t result_stride, SCM *result)
{
  add_scm_with_degree_elevation (elev_scm_sbern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
add_f64_spower (size_t degree1, ssize_t stride1,
                const double *spline1, size_t degree2,
                ssize_t stride2, const double *spline2,
                ssize_t result_stride, double *result)
{
  add_f64_with_degree_elevation (elev_f64_spower,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
add_scm_spower (size_t degree1, ssize_t stride1,
                const SCM *spline1, size_t degree2,
                ssize_t stride2, const SCM *spline2,
                ssize_t result_stride, SCM *result)
{
  add_scm_with_degree_elevation (elev_scm_spower,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

//-------------------------------------------------------------------------

static void
sub_f64_with_degree_elevation (void
                               add (size_t, ssize_t, const double *, size_t,
                                    ssize_t, const double *, ssize_t, double *),
                               size_t degree1, ssize_t stride1,
                               const double *spline1, size_t degree2,
                               ssize_t stride2, const double *spline2,
                               ssize_t result_stride, double *result)
{
  double _spline2[degree2 + 1];
  copy_f64_with_strides (1, _spline2, stride2, spline2, degree2 + 1);
  for (size_t i = 0; i <= degree2; i++)
    _spline2[i] = -_spline2[i];
  add (degree1, stride1, spline1, degree2, 1, _spline2, result_stride, result);
}

static void
sub_scm_with_degree_elevation (void
                               add (size_t, ssize_t, const SCM *, size_t,
                                    ssize_t, const SCM *, ssize_t, SCM *),
                               size_t degree1, ssize_t stride1,
                               const SCM *spline1, size_t degree2,
                               ssize_t stride2, const SCM *spline2,
                               ssize_t result_stride, SCM *result)
{
  const SCM zero = scm_from_int (0);

  SCM _spline2[degree2 + 1];
  copy_scm_with_strides (1, _spline2, stride2, spline2, degree2 + 1);
  for (size_t i = 0; i <= degree2; i++)
    _spline2[i] = scm_difference (zero, _spline2[i]);
  add (degree1, stride1, spline1, degree2, 1, _spline2, result_stride, result);
}

VISIBLE void
sub_f64_mono (size_t degree1, ssize_t stride1,
              const double *spline1, size_t degree2,
              ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  sub_f64_with_degree_elevation (add_f64_mono,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_scm_mono (size_t degree1, ssize_t stride1,
              const SCM *spline1, size_t degree2,
              ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  sub_scm_with_degree_elevation (add_scm_mono,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_f64_bern (size_t degree1, ssize_t stride1,
              const double *spline1, size_t degree2,
              ssize_t stride2, const double *spline2,
              ssize_t result_stride, double *result)
{
  sub_f64_with_degree_elevation (add_f64_bern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_scm_bern (size_t degree1, ssize_t stride1,
              const SCM *spline1, size_t degree2,
              ssize_t stride2, const SCM *spline2,
              ssize_t result_stride, SCM *result)
{
  sub_scm_with_degree_elevation (add_scm_bern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_f64_sbern (size_t degree1, ssize_t stride1,
               const double *spline1, size_t degree2,
               ssize_t stride2, const double *spline2,
               ssize_t result_stride, double *result)
{
  sub_f64_with_degree_elevation (add_f64_sbern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_scm_sbern (size_t degree1, ssize_t stride1,
               const SCM *spline1, size_t degree2,
               ssize_t stride2, const SCM *spline2,
               ssize_t result_stride, SCM *result)
{
  sub_scm_with_degree_elevation (add_scm_sbern,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_f64_spower (size_t degree1, ssize_t stride1,
                const double *spline1, size_t degree2,
                ssize_t stride2, const double *spline2,
                ssize_t result_stride, double *result)
{
  sub_f64_with_degree_elevation (add_f64_spower,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

VISIBLE void
sub_scm_spower (size_t degree1, ssize_t stride1,
                const SCM *spline1, size_t degree2,
                ssize_t stride2, const SCM *spline2,
                ssize_t result_stride, SCM *result)
{
  sub_scm_with_degree_elevation (add_scm_spower,
                                 degree1, stride1, spline1,
                                 degree2, stride2, spline2,
                                 result_stride, result);
}

//-------------------------------------------------------------------------

static void
one_minus_f64_spline (void one_f64_spline (size_t degree, ssize_t stride,
                                           double *one),
                      size_t degree, ssize_t stride, const double *spline,
                      ssize_t result_stride, double *result)
{
  double one[degree + 1];
  one_f64_spline (degree, 1, one);
  sub_f64_splines (degree, 1, one, stride, spline, result_stride, result);
}

static void
one_minus_scm_spline (void one_scm_spline (size_t degree, ssize_t stride,
                                           SCM *one),
                      size_t degree, ssize_t stride, const SCM *spline,
                      ssize_t result_stride, SCM *result)
{
  SCM one[degree + 1];
  one_scm_spline (degree, 1, one);
  sub_scm_splines (degree, 1, one, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_f64_mono (size_t degree, ssize_t stride, const double *spline,
                    ssize_t result_stride, double *result)
{
  one_minus_f64_spline (one_f64_mono,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_scm_mono (size_t degree, ssize_t stride, const SCM *spline,
                    ssize_t result_stride, SCM *result)
{
  one_minus_scm_spline (one_scm_mono,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_f64_bern (size_t degree, ssize_t stride, const double *spline,
                    ssize_t result_stride, double *result)
{
  one_minus_f64_spline (one_f64_bern,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_scm_bern (size_t degree, ssize_t stride, const SCM *spline,
                    ssize_t result_stride, SCM *result)
{
  one_minus_scm_spline (one_scm_bern,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_f64_sbern (size_t degree, ssize_t stride, const double *spline,
                     ssize_t result_stride, double *result)
{
  one_minus_f64_spline (one_f64_sbern,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_scm_sbern (size_t degree, ssize_t stride, const SCM *spline,
                     ssize_t result_stride, SCM *result)
{
  one_minus_scm_spline (one_scm_sbern,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_f64_spower (size_t degree, ssize_t stride, const double *spline,
                      ssize_t result_stride, double *result)
{
  one_minus_f64_spline (one_f64_spower,
                        degree, stride, spline, result_stride, result);
}

VISIBLE void
one_minus_scm_spower (size_t degree, ssize_t stride, const SCM *spline,
                      ssize_t result_stride, SCM *result)
{
  one_minus_scm_spline (one_scm_spower,
                        degree, stride, spline, result_stride, result);
}

//-------------------------------------------------------------------------

static void
splines_nonconformable_for_addition (const char *who, SCM spline1, SCM spline2)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition
      (_("a spline cannot be added to a spline of different degree")),
      rnrs_make_irritants_condition (scm_list_2 (spline1, spline2))));
}

static void
splines_nonconformable_for_subtraction (const char *who,
                                        SCM spline1, SCM spline2)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition
      (_("a spline cannot be subtracted from a spline of different degree")),
      rnrs_make_irritants_condition (scm_list_2 (spline1, spline2))));
}

//-------------------------------------------------------------------------

static SCM
scm_op_f64_splines (const char *who,
                    void op_f64 (size_t degree,
                                 ssize_t stride1, const double *spline1,
                                 ssize_t stride2, const double *spline2,
                                 ssize_t result_stride, double *result),
                    void splines_nonconformable (const char *who,
                                                 SCM spline1, SCM spline2),
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

  if (dim1 != dim2)
    splines_nonconformable (who, spline1, spline2);

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_from_size_t (dim1)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);

  op_f64 (dim1 - 1, stride1, _spline1, stride2, _spline2, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_add_f64_splines (SCM spline1, SCM spline2)
{
  return scm_op_f64_splines ("scm_add_f64_splines", add_f64_splines,
                             splines_nonconformable_for_addition,
                             spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_splines (SCM spline1, SCM spline2)
{
  return scm_op_f64_splines ("scm_sub_f64_splines", sub_f64_splines,
                             splines_nonconformable_for_subtraction,
                             spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_op_scm_splines (const char *who,
                    void op_scm (size_t degree,
                                 ssize_t stride1, const SCM *spline1,
                                 ssize_t stride2, const SCM *spline2,
                                 ssize_t result_stride, SCM *result),
                    void splines_nonconformable (const char *who,
                                                 SCM spline1, SCM spline2),
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

  if (dim1 != dim2)
    splines_nonconformable (who, spline1, spline2);

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_from_size_t (dim1)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);

  op_scm (dim1 - 1, stride1, _spline1, stride2, _spline2, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_add_scm_splines (SCM spline1, SCM spline2)
{
  return scm_op_scm_splines ("scm_add_scm_splines", add_scm_splines,
                             splines_nonconformable_for_addition,
                             spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_splines (SCM spline1, SCM spline2)
{
  return scm_op_scm_splines ("scm_sub_scm_splines", sub_scm_splines,
                             splines_nonconformable_for_subtraction,
                             spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_op_f64 (const char *who,
            void op_f64 (size_t degree1, ssize_t stride1, const double *spline1,
                         size_t degree2, ssize_t stride2, const double *spline2,
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

  const size_t dim = max_size (dim1, dim2);
  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_from_size_t (dim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  double *_result = scm_array_handle_f64_writable_elements (&handle);

  op_f64 (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_add_f64_mono (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_add_f64_mono", add_f64_mono, spline1, spline2);
}

VISIBLE SCM
scm_add_f64_bern (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_add_f64_bern", add_f64_bern, spline1, spline2);
}

VISIBLE SCM
scm_add_f64_sbern (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_add_f64_sbern", add_f64_sbern, spline1, spline2);
}

VISIBLE SCM
scm_add_f64_spower (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_add_f64_spower", add_f64_spower, spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_mono (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_sub_f64_mono", sub_f64_mono, spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_bern (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_sub_f64_bern", sub_f64_bern, spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_sbern (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_sub_f64_sbern", sub_f64_sbern, spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_spower (SCM spline1, SCM spline2)
{
  return scm_op_f64 ("scm_sub_f64_spower", sub_f64_spower, spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_op_scm (const char *who,
            void op_scm (size_t degree1, ssize_t stride1, const SCM *spline1,
                         size_t degree2, ssize_t stride2, const SCM *spline2,
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

  const size_t dim = max_size (dim1, dim2);
  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_from_size_t (dim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);

  op_scm (dim1 - 1, stride1, _spline1, dim2 - 1, stride2, _spline2, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_add_scm_mono (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_add_scm_mono", add_scm_mono, spline1, spline2);
}

VISIBLE SCM
scm_add_scm_bern (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_add_scm_bern", add_scm_bern, spline1, spline2);
}

VISIBLE SCM
scm_add_scm_sbern (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_add_scm_sbern", add_scm_sbern, spline1, spline2);
}

VISIBLE SCM
scm_add_scm_spower (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_add_scm_spower", add_scm_spower, spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_mono (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_sub_scm_mono", sub_scm_mono, spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_bern (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_sub_scm_bern", sub_scm_bern, spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_sbern (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_sub_scm_sbern", sub_scm_sbern, spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_spower (SCM spline1, SCM spline2)
{
  return scm_op_scm ("scm_sub_scm_spower", sub_scm_spower, spline1, spline2);
}

//-------------------------------------------------------------------------

void init_math_polyspline_add (void);

VISIBLE void
init_math_polyspline_add (void)
{
  scm_c_define_gsubr ("poly:add-f64-splines", 2, 0, 0, scm_add_f64_splines);
  scm_c_define_gsubr ("poly:add-scm-splines", 2, 0, 0, scm_add_scm_splines);

  scm_c_define_gsubr ("poly:sub-f64-splines", 2, 0, 0, scm_sub_f64_splines);
  scm_c_define_gsubr ("poly:sub-scm-splines", 2, 0, 0, scm_sub_scm_splines);

  scm_c_define_gsubr ("poly:add-f64-mono", 2, 0, 0, scm_add_f64_mono);
  scm_c_define_gsubr ("poly:add-scm-mono", 2, 0, 0, scm_add_scm_mono);

  scm_c_define_gsubr ("poly:add-f64-bern", 2, 0, 0, scm_add_f64_bern);
  scm_c_define_gsubr ("poly:add-scm-bern", 2, 0, 0, scm_add_scm_bern);

  scm_c_define_gsubr ("poly:add-f64-sbern", 2, 0, 0, scm_add_f64_sbern);
  scm_c_define_gsubr ("poly:add-scm-sbern", 2, 0, 0, scm_add_scm_sbern);

  scm_c_define_gsubr ("poly:add-f64-spower", 2, 0, 0, scm_add_f64_spower);
  scm_c_define_gsubr ("poly:add-scm-spower", 2, 0, 0, scm_add_scm_spower);

  scm_c_define_gsubr ("poly:sub-f64-mono", 2, 0, 0, scm_sub_f64_mono);
  scm_c_define_gsubr ("poly:sub-scm-mono", 2, 0, 0, scm_sub_scm_mono);

  scm_c_define_gsubr ("poly:sub-f64-bern", 2, 0, 0, scm_sub_f64_bern);
  scm_c_define_gsubr ("poly:sub-scm-bern", 2, 0, 0, scm_sub_scm_bern);

  scm_c_define_gsubr ("poly:sub-f64-sbern", 2, 0, 0, scm_sub_f64_sbern);
  scm_c_define_gsubr ("poly:sub-scm-sbern", 2, 0, 0, scm_sub_scm_sbern);

  scm_c_define_gsubr ("poly:sub-f64-spower", 2, 0, 0, scm_sub_f64_spower);
  scm_c_define_gsubr ("poly:sub-scm-spower", 2, 0, 0, scm_sub_scm_spower);
}

//-------------------------------------------------------------------------
