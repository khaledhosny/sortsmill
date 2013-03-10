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

#include <sortsmill/math/polyspline/subdiv.h>
#include <sortsmill/math.h>
#include <sortsmill/guile.h>

VISIBLE void
subdiv_f64_bern (unsigned int degree, int stride, const double *spline,
                 double t, double *a, double *b)
{
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * (int) i];
  for (unsigned int i = 0; i < degree; i++)
    {
      a[i] = b[0];
      for (unsigned int j = 0; j < degree; j++)
        b[j] += t * (b[j + 1] - b[j]);
    }
  a[degree] = b[0];
}

VISIBLE void
subdiv_f64_sbern (unsigned int degree, int stride, const double *spline,
                  double t, double *a, double *b)
{
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * (int) i] / bincoef (degree, i);
  for (unsigned int i = 0; i < degree; i++)
    {
      a[i] = b[0];
      for (unsigned int j = 0; j < degree; j++)
        b[j] += t * (b[j + 1] - b[j]);
    }
  a[degree] = b[0];
  for (unsigned int i = 0; i <= degree; i++)
    {
      double C = bincoef (degree, i);
      a[i] *= C;
      b[i] *= C;
    }
}

//-------------------------------------------------------------------------

static SCM
scm_subdiv_f64_spline (const char *who,
                       void subdiv_f64_spline (unsigned int degree, int stride,
                                               const double *spline,
                                               double t, double *a, double *b),
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

  // FIXME: Make this section reusable.
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

  double *a = scm_malloc ((degree + 1) * sizeof (double));
  double *b = scm_malloc ((degree + 1) * sizeof (double));

  subdiv_f64_spline (degree, stride, spline, scm_to_double (t), a, b);

  scm_dynwind_end ();

  // FIXME: These calls can be made reusable: ‘one-based’ can have a C
  // function made for it.
  SCM values[2];
  values[0] = scm_call_1 (scm_c_public_ref ("sortsmill", "one-based"),
                          scm_take_f64vector (a, degree + 1));
  values[1] = scm_call_1 (scm_c_public_ref ("sortsmill", "one-based"),
                          scm_take_f64vector (b, degree + 1));

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

void init_math_polyspline_subdiv (void);

VISIBLE void
init_math_polyspline_subdiv (void)
{
  //  scm_c_define_gsubr ("poly:subdiv-f64-mono", 2, 0, 0, scm_subdiv_f64_mono);
  //  scm_c_define_gsubr ("poly:subdiv-scm-mono", 2, 0, 0, scm_subdiv_scm_mono);

  scm_c_define_gsubr ("poly:subdiv-f64-bern", 2, 0, 0, scm_subdiv_f64_bern);
  //  scm_c_define_gsubr ("poly:subdiv-scm-bern", 2, 0, 0, scm_subdiv_scm_bern);

  scm_c_define_gsubr ("poly:subdiv-f64-sbern", 2, 0, 0, scm_subdiv_f64_sbern);
  //  scm_c_define_gsubr ("poly:subdiv-scm-sbern", 2, 0, 0, scm_subdiv_scm_sbern);

  //  scm_c_define_gsubr ("poly:subdiv-f64-spower", 2, 0, 0, scm_subdiv_f64_spower);
  //  scm_c_define_gsubr ("poly:subdiv-scm-spower", 2, 0, 0, scm_subdiv_scm_spower);
}

//-------------------------------------------------------------------------
