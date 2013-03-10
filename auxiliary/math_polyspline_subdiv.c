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
      for (unsigned int j = 0; j < degree - i; j++)
        b[j] += t * (b[j + 1] - b[j]);
    }
  a[degree] = b[0];
}

VISIBLE void
scm_c_subdiv_bern (unsigned int degree, int stride, const SCM *spline,
                   SCM t, SCM *a, SCM *b)
{
  for (unsigned int i = 0; i <= degree; i++)
    b[i] = spline[stride * (int) i];
  for (unsigned int i = 0; i < degree; i++)
    {
      a[i] = b[0];
      for (unsigned int j = 0; j < degree - i; j++)
        b[j] = scm_sum (b[j], scm_product (t, scm_difference (b[j + 1], b[j])));
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
      for (unsigned int j = 0; j < degree - i; j++)
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


VISIBLE void
scm_c_subdiv_sbern (unsigned int degree, int stride, const SCM *spline,
                    SCM t, SCM *a, SCM *b)
{
  scm_dynwind_begin (0);

  mpz_t C;
  mpz_init (C);
  scm_dynwind_mpz_clear (C);

  for (unsigned int i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      b[i] = scm_divide (spline[stride * (int) i], scm_from_mpz (C));
    }
  for (unsigned int i = 0; i < degree; i++)
    {
      a[i] = b[0];
      for (unsigned int j = 0; j < degree - i; j++)
        b[j] = scm_sum (b[j], scm_product (t, scm_difference (b[j + 1], b[j])));
    }
  a[degree] = b[0];
  for (unsigned int i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      SCM _C = scm_from_mpz (C);
      a[i] = scm_product (a[i], _C);
      b[i] = scm_product (b[i], _C);
    }

  scm_dynwind_end ();
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
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

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

  SCM values[2];

  values[0] = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                    scm_list_2 (scm_from_uint (1),
                                                scm_from_uint (degree + 1)));
  scm_array_get_handle (values[0], &handle_a);
  scm_dynwind_array_handle_release (&handle_a);
  double *a = scm_array_handle_f64_writable_elements (&handle_a);

  values[1] = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                    scm_list_2 (scm_from_uint (1),
                                                scm_from_uint (degree + 1)));
  scm_array_get_handle (values[1], &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  double *b = scm_array_handle_f64_writable_elements (&handle_b);

  subdiv_f64_spline (degree, stride, spline, scm_to_double (t), a, b);

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

  const size_t rank = scm_array_handle_rank (&handle);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  const SCM *spline = scm_array_handle_elements (&handle);

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

  SCM values[2];

  values[0] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_2 (scm_from_uint (1),
                                          scm_from_uint (degree + 1)));
  scm_array_get_handle (values[0], &handle_a);
  scm_dynwind_array_handle_release (&handle_a);
  SCM *a = scm_array_handle_writable_elements (&handle_a);

  values[1] = scm_make_array (SCM_UNSPECIFIED,
                              scm_list_2 (scm_from_uint (1),
                                          scm_from_uint (degree + 1)));
  scm_array_get_handle (values[1], &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  SCM *b = scm_array_handle_writable_elements (&handle_b);

  scm_c_subdiv_spline (degree, stride, spline, t, a, b);

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
