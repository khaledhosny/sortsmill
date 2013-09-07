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
#include <sortsmill/core.h>
#include <intl.h>

//-------------------------------------------------------------------------

VISIBLE void
plug_xy_into_line_scm_mono (size_t degree,
                            ssize_t xstride, const SCM *xspline,
                            ssize_t ystride, const SCM *yspline,
                            SCM C, SCM A, SCM B,
                            ssize_t result_stride, SCM *result)
{
  SCM buffer[degree + 1];
  for (size_t i = 0; i <= degree; i++)
    {
      buffer[i] = scm_sum (scm_product (A, *xspline),
                           scm_product (B, *yspline));
      xspline += xstride;
      yspline += ystride;
    }
  buffer[0] = scm_sum (buffer[0], C);
  copy_scm_with_strides (result_stride, result, 1, buffer, degree + 1);
}

VISIBLE SCM
scm_plug_xy_into_line_scm_mono (SCM xspline, SCM yspline, SCM implicit_eq)
{
  const char *who = "scm_plug_xy_into_line_scm_mono";

  scm_t_array_handle xhandle;
  scm_t_array_handle yhandle;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (xspline, &xhandle);
  scm_dynwind_array_handle_release (&xhandle);
  assert_c_rank_1_or_2_array (who, xspline, &xhandle);

  size_t xdim;
  ssize_t xstride;
  scm_array_handle_get_vector_dim_and_stride (who, xspline, &xhandle,
                                              &xdim, &xstride);
  const SCM *_xspline = scm_array_handle_elements (&xhandle);

  scm_array_get_handle (yspline, &yhandle);
  scm_dynwind_array_handle_release (&yhandle);
  assert_c_rank_1_or_2_array (who, yspline, &yhandle);

  size_t ydim;
  ssize_t ystride;
  scm_array_handle_get_vector_dim_and_stride (who, yspline, &yhandle,
                                              &ydim, &ystride);
  const SCM *_yspline = scm_array_handle_elements (&yhandle);

  // FIXME: This check should be made reusable.
  if (xdim != ydim)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("the components of the "
                                         "given parametric spline "
                                         "are of different degree")),
        rnrs_make_irritants_condition (scm_list_2 (xspline, yspline))));

  SCM result = scm_make_array (SCM_UNSPECIFIED,
                               scm_list_1 (scm_from_size_t (xdim)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);
  SCM *_result = scm_array_handle_writable_elements (&handle);

  SCM p = implicit_eq;
  scm_c_assert_list_does_not_end_here (who, implicit_eq, p);
  scm_c_assert_can_be_list_link (who, implicit_eq, p);
  SCM C = SCM_CAR (p);
  p = SCM_CDR (p);
  scm_c_assert_list_does_not_end_here (who, implicit_eq, p);
  scm_c_assert_can_be_list_link (who, implicit_eq, p);
  SCM A = SCM_CAR (p);
  p = SCM_CDR (p);
  scm_c_assert_list_does_not_end_here (who, implicit_eq, p);
  scm_c_assert_can_be_list_link (who, implicit_eq, p);
  SCM B = SCM_CAR (p);
  p = SCM_CDR (p);
  scm_c_assert_list_ends_here (who, implicit_eq, p);

  plug_xy_into_line_scm_mono (xdim - 1, xstride, _xspline,
                              ystride, _yspline, C, A, B, 1, _result);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_intersect_with_line_mono (SCM xsplines, SCM ysplines, SCM implicit_eq,
                              SCM t0, SCM t1)
{
  const char *who = "scm_intersect_with_line_mono";

  if (SCM_UNBNDP (t0))
    t0 = scm_from_int (0);
  if (SCM_UNBNDP (t1))
    t1 = scm_from_int (1);

  const size_t m = scm_c_matrix_row_count (xsplines);
  const size_t m1 = scm_c_matrix_row_count (ysplines);
  // FIXME: Make this check reusable.
  if (m != m1)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("the numbers of x and y splines "
                                         "do not match")),
        rnrs_make_irritants_condition (scm_list_2 (xsplines, ysplines))));

  SCM intersections = SCM_EOL;
  for (size_t i = 0; i < m; i++)
    {
      SCM poly =
        scm_plug_xy_into_line_scm_mono (scm_c_matrix_0row (xsplines, i),
                                        scm_c_matrix_0row (ysplines, i),
                                        implicit_eq);
      intersections = scm_cons (scm_find_roots_scm_mono (poly, t0, t1),
                                intersections);
    }
  return scm_reverse (intersections);
}

//-------------------------------------------------------------------------

void init_math_polyspline_intersection (void);

VISIBLE void
init_math_polyspline_intersection (void)
{
  scm_c_define_gsubr ("poly:plug-xy-into-line-scm-mono", 3, 0, 0,
                      scm_plug_xy_into_line_scm_mono);
  scm_c_define_gsubr ("poly:intersect-with-line-mono", 3, 2, 0,
                      scm_intersect_with_line_mono);
}

//-------------------------------------------------------------------------
