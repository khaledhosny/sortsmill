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

//-------------------------------------------------------------------------

VISIBLE void
add_f64_splines (unsigned int degree,
                 int stride1, const double *spline1,
                 int stride2, const double *spline2,
                 int result_stride, double *result)
{
  double buffer[degree + 1];
  copy_f64_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    buffer[i] += spline2[stride2 * (int) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
add_scm_splines (unsigned int degree,
                 int stride1, const SCM *spline1,
                 int stride2, const SCM *spline2,
                 int result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  copy_scm_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    buffer[i] = scm_sum (buffer[i], spline2[stride2 * (int) i]);
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
sub_f64_splines (unsigned int degree,
                 int stride1, const double *spline1,
                 int stride2, const double *spline2,
                 int result_stride, double *result)
{
  double buffer[degree + 1];
  copy_f64_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    buffer[i] -= spline2[stride2 * (int) i];
  copy_f64_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE void
sub_scm_splines (unsigned int degree,
                 int stride1, const SCM *spline1,
                 int stride2, const SCM *spline2,
                 int result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  copy_scm_with_strides (1, buffer, stride1, spline1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    buffer[i] = scm_difference (buffer[i], spline2[stride2 * (int) i]);
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
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
scm_op_f64_spline (const char *who,
                   void op_f64 (unsigned int degree,
                                int stride1, const double *spline1,
                                int stride2, const double *spline2,
                                int result_stride, double *result),
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

  if (dim1 != dim2)
    splines_nonconformable (who, spline1, spline2);

  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                             scm_from_uint
                                                             (dim1))));
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
  return scm_op_f64_spline ("scm_add_f64_splines", add_f64_splines,
                            splines_nonconformable_for_addition,
                            spline1, spline2);
}

VISIBLE SCM
scm_sub_f64_splines (SCM spline1, SCM spline2)
{
  return scm_op_f64_spline ("scm_sub_f64_splines", sub_f64_splines,
                            splines_nonconformable_for_subtraction,
                            spline1, spline2);
}

//-------------------------------------------------------------------------

static SCM
scm_op_scm_spline (const char *who,
                   void op_scm (unsigned int degree,
                                int stride1, const SCM *spline1,
                                int stride2, const SCM *spline2,
                                int result_stride, SCM *result),
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

  unsigned int dim1;
  int stride1;
  scm_array_handle_get_vector_dim_and_stride (who, spline1, &handle1,
                                              &dim1, &stride1);
  const SCM *_spline1 = scm_array_handle_elements (&handle1);

  scm_array_get_handle (spline2, &handle2);
  scm_dynwind_array_handle_release (&handle2);
  assert_c_rank_1_or_2_array (who, spline2, &handle2);

  unsigned int dim2;
  int stride2;
  scm_array_handle_get_vector_dim_and_stride (who, spline2, &handle2,
                                              &dim2, &stride2);
  const SCM *_spline2 = scm_array_handle_elements (&handle2);

  if (dim1 != dim2)
    splines_nonconformable (who, spline1, spline2);

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_list_2 (scm_from_uint (1),
                                                       scm_from_uint (dim1))));
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
  return scm_op_scm_spline ("scm_add_scm_splines", add_scm_splines,
                            splines_nonconformable_for_addition,
                            spline1, spline2);
}

VISIBLE SCM
scm_sub_scm_splines (SCM spline1, SCM spline2)
{
  return scm_op_scm_spline ("scm_sub_scm_splines", sub_scm_splines,
                            splines_nonconformable_for_subtraction,
                            spline1, spline2);
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
}

//-------------------------------------------------------------------------
