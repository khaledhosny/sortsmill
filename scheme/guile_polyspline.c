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

#include <sortsmillff/guile/polyspline.h>
#include <sortsmillff/polyspline.h>
#include <xalloc.h>

void init_guile_sortsmillff_polyspline (void);

static SCM
change_basis (SCM spline,
              void (*changer) (unsigned int, const double *, double *,
			       size_t))
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double *b = xmalloc (len * sizeof (double));
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  scm_array_handle_release (&handle);
  changer (len - 1, b, b, 1);
  SCM result = scm_take_f64vector (b, len);
  return result;
}

VISIBLE SCM
scm_f64vector_sbern_to_bern (SCM spline)
{
  return change_basis (spline, fl_sbern_to_bern);
}

VISIBLE SCM
scm_f64vector_bern_to_sbern (SCM spline)
{
  return change_basis (spline, fl_bern_to_sbern);
}

VISIBLE SCM
scm_f64vector_sbern_to_mono (SCM spline)
{
  return change_basis (spline, fl_sbern_to_mono);
}

VISIBLE SCM
scm_f64vector_mono_to_sbern (SCM spline)
{
  return change_basis (spline, fl_mono_to_sbern);
}

VISIBLE SCM
scm_f64vector_bern_to_mono (SCM spline)
{
  return change_basis (spline, fl_bern_to_mono);
}

VISIBLE SCM
scm_f64vector_mono_to_bern (SCM spline)
{
  return change_basis (spline, fl_mono_to_bern);
}

static SCM
evaluate (SCM spline, SCM t,
          double (*evaluator) (unsigned int, const double *, double))
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double b[len];
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  scm_array_handle_release (&handle);
  double v = evaluator (len - 1, b, scm_to_double (t));
  return scm_from_double (v);
}

VISIBLE SCM
scm_f64vector_eval_sbern (SCM spline, SCM t)
{
  return evaluate (spline, t, fl_eval_sbern);
}

VISIBLE SCM
scm_f64vector_eval_bern (SCM spline, SCM t)
{
  return evaluate (spline, t, fl_eval_bern);
}

VISIBLE SCM
scm_f64vector_evaldc_sbern (SCM spline, SCM t)
{
  return evaluate (spline, t, fl_evaldc_sbern);
}

VISIBLE SCM
scm_f64vector_evaldc_bern (SCM spline, SCM t)
{
  return evaluate (spline, t, fl_evaldc_bern);
}

VISIBLE SCM
scm_f64vector_eval_mono (SCM spline, SCM t)
{
  return evaluate (spline, t, fl_eval_mono);
}

VISIBLE SCM
scm_f64vector_subdiv_sbern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double *b = xmalloc (len * sizeof (double));
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  scm_array_handle_release (&handle);
  double *a = xmalloc (len * sizeof (double));
  fl_subdiv_sbern (len - 1, b, scm_to_double (t), a, b);
  SCM new_splines[2];
  new_splines[0] = scm_take_f64vector (a, len);
  new_splines[1] = scm_take_f64vector (b, len);
  return scm_c_values (new_splines, 2);
}

VISIBLE SCM
scm_f64vector_subdiv_bern (SCM spline, SCM t)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;
  const double *elem = scm_f64vector_elements (spline, &handle, &len, &inc);
  double *b = xmalloc (len * sizeof (double));
  for (unsigned int i = 0; i < len; i++)
    b[i] = elem[inc * i];
  scm_array_handle_release (&handle);
  double *a = xmalloc (len * sizeof (double));
  fl_subdiv_bern (len - 1, b, scm_to_double (t), a, b);
  SCM new_splines[2];
  new_splines[0] = scm_take_f64vector (a, len);
  new_splines[1] = scm_take_f64vector (b, len);
  return scm_c_values (new_splines, 2);
}

VISIBLE void
init_guile_sortsmillff_polyspline (void)
{
  scm_c_define_gsubr ("f64vector-sbern->bern", 1, 0, 0,
                      scm_f64vector_sbern_to_bern);
  scm_c_define_gsubr ("f64vector-bern->sbern", 1, 0, 0,
                      scm_f64vector_bern_to_sbern);
  scm_c_define_gsubr ("f64vector-sbern->mono", 1, 0, 0,
                      scm_f64vector_sbern_to_mono);
  scm_c_define_gsubr ("f64vector-mono->sbern", 1, 0, 0,
                      scm_f64vector_mono_to_sbern);
  scm_c_define_gsubr ("f64vector-bern->mono", 1, 0, 0,
                      scm_f64vector_bern_to_mono);
  scm_c_define_gsubr ("f64vector-mono->bern", 1, 0, 0,
                      scm_f64vector_mono_to_bern);

  scm_c_define_gsubr ("f64vector-eval-sbern", 2, 0, 0,
                      scm_f64vector_eval_sbern);
  scm_c_define_gsubr ("f64vector-eval-bern", 2, 0, 0,
                      scm_f64vector_eval_bern);

  scm_c_define_gsubr ("f64vector-evaldc-sbern", 2, 0, 0,
                      scm_f64vector_evaldc_sbern);
  scm_c_define_gsubr ("f64vector-evaldc-bern", 2, 0, 0,
                      scm_f64vector_evaldc_bern);

  scm_c_define_gsubr ("f64vector-eval-mono", 2, 0, 0,
                      scm_f64vector_eval_mono);

  scm_c_define_gsubr ("f64vector-subdiv-sbern", 2, 0, 0,
                      scm_f64vector_subdiv_sbern);
  scm_c_define_gsubr ("f64vector-subdiv-bern", 2, 0, 0,
                      scm_f64vector_subdiv_bern);
}