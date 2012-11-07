#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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

#include <libguile.h>
#include <bernstein.h>
#include <xalloc.h>

VISIBLE void init_guile_sortsmillff_bernstein (void);

// FIXME: Move these to a header file.
VISIBLE SCM scm_f64vector_sbern_to_bern (SCM spline);
VISIBLE SCM scm_f64vector_bern_to_sbern (SCM spline);
VISIBLE SCM scm_f64vector_eval_sbern (SCM spline, SCM t);
VISIBLE SCM scm_f64vector_eval_bern (SCM spline, SCM t);
VISIBLE SCM scm_f64vector_evaldc_sbern (SCM spline, SCM t);
VISIBLE SCM scm_f64vector_evaldc_bern (SCM spline, SCM t);

SCM
scm_f64vector_sbern_to_bern (SCM spline)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double *b = xmalloc (len * sizeof (double));
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  sbern_to_bern_double (len - 1, b, b);
  SCM bern = scm_take_f64vector (b, len);
  scm_array_handle_release (&handle);
  return bern;
}

SCM
scm_f64vector_bern_to_sbern (SCM spline)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double *b = xmalloc (len * sizeof (double));
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  bern_to_sbern_double (len - 1, b, b);
  SCM sbern = scm_take_f64vector (b, len);
  scm_array_handle_release (&handle);
  return sbern;
}

SCM
scm_f64vector_eval_sbern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double b[len];
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  double v = eval_sbern_double (len - 1, b, scm_to_double (t));
  scm_array_handle_release (&handle);
  return scm_from_double (v);
}

SCM
scm_f64vector_eval_bern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double b[len];
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  double v = eval_bern_double (len - 1, b, scm_to_double (t));
  scm_array_handle_release (&handle);
  return scm_from_double (v);
}

SCM
scm_f64vector_evaldc_sbern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double b[len];
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  double v = evaldc_sbern_double (len - 1, b, scm_to_double (t));
  scm_array_handle_release (&handle);
  return scm_from_double (v);
}

SCM
scm_f64vector_evaldc_bern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double b[len];
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  double v = evaldc_bern_double (len - 1, b, scm_to_double (t));
  scm_array_handle_release (&handle);
  return scm_from_double (v);
}

void
init_guile_sortsmillff_bernstein (void)
{
  scm_c_define_gsubr ("f64vector-sbern->bern", 1, 0, 0,
                      scm_f64vector_sbern_to_bern);
  scm_c_define_gsubr ("f64vector-bern->sbern", 1, 0, 0,
                      scm_f64vector_bern_to_sbern);
  scm_c_define_gsubr ("f64vector-eval-sbern", 2, 0, 0,
                      scm_f64vector_eval_sbern);
  scm_c_define_gsubr ("f64vector-eval-bern", 2, 0, 0,
                      scm_f64vector_eval_bern);
  scm_c_define_gsubr ("f64vector-evaldc-sbern", 2, 0, 0,
                      scm_f64vector_evaldc_sbern);
  scm_c_define_gsubr ("f64vector-evaldc-bern", 2, 0, 0,
                      scm_f64vector_evaldc_bern);
  //  scm_c_define_gsubr("subdiv-sbern-double", 5, 0, 0, scm_subdiv_sbern_double);
  //  scm_c_define_gsubr("subdiv-bern-double", 5, 0, 0, scm_subdiv_bern_double);
}
