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

static const char *my_module = "sortsmill math polyspline roots";

//-------------------------------------------------------------------------

static inline int
sign_f64 (double x)
{
  int sign;
  if (x < 0)
    sign = -1;
  else if (x == 0)
    sign = 0;
  else
    sign = 1;
  return sign;
}

static inline int
sign_scm (SCM x)
{
  int sign;
  if (scm_is_true (scm_negative_p (x)))
    sign = -1;
  else if (scm_is_true (scm_zero_p (x)))
    sign = 0;
  else
    sign = 1;
  return sign;
}

VISIBLE size_t
sign_variations_f64 (size_t degree, ssize_t stride, const double *spline)
{
  size_t count = 0;
  int old_sign = sign_f64 (spline[0]);
  for (size_t i = 1; i <= degree; i++)
    {
      const int new_sign = sign_f64 (spline[stride * (ssize_t) i]);
      if (old_sign * new_sign < 0)
        {
          old_sign = new_sign;
          count++;
        }
      else if (old_sign == 0)
        old_sign = new_sign;
    }
  return count;
}

VISIBLE size_t
sign_variations_scm (size_t degree, ssize_t stride, const SCM *spline)
{
  size_t count = 0;
  int old_sign = sign_scm (spline[0]);
  for (size_t i = 1; i <= degree; i++)
    {
      const int new_sign = sign_scm (spline[stride * (ssize_t) i]);
      if (old_sign * new_sign < 0)
        {
          old_sign = new_sign;
          count++;
        }
      else if (old_sign == 0)
        old_sign = new_sign;
    }
  return count;
}

VISIBLE SCM
scm_sign_variations_f64 (SCM spline)
{
  const char *who = "scm_sign_variations_f64";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const double *_spline = scm_array_handle_f64_elements (&handle);

  size_t count = sign_variations_f64 (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

VISIBLE SCM
scm_sign_variations_scm (SCM spline)
{
  const char *who = "scm_sign_variations_scm";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  size_t count = sign_variations_scm (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

//-------------------------------------------------------------------------

VISIBLE size_t
budan_0_1_scm_mono (size_t degree, ssize_t stride, const SCM *spline)
{
  // Budan’s 0_1 roots test. See
  // http://en.wikipedia.org/wiki/Budan%27s_theorem#Early_applications_of_Budan.27s_theorem
  //
  // Here we use the binomial expansion
  //
  //    (1 + x)ⁿ = ∑ᵢC(n,i)xⁱ for i = 0,1,...,n
  //
  // to simplify and speed things up.

  size_t count = 0;

  if (degree != 0)
    {
      SCM p[degree + 1];

      p[0] = *spline;
      for (size_t i = 1; i < degree; i++)
        p[i] = scm_product (scm_c_bincoef (degree, i), *spline);
      p[degree] = *spline;

      for (size_t k = 1; k < degree; k++)
        {
          spline += stride;
          p[0] = scm_sum (p[0], *spline);
          for (size_t i = 1; i < degree - k; i++)
            p[i] = scm_sum (p[i], scm_product (scm_c_bincoef (degree - k, i),
                                               *spline));
          p[degree - k] = scm_sum (p[degree - k], *spline);
        }

      spline += stride;
      p[0] = scm_sum (p[0], *spline);

      count = sign_variations_scm (degree, 1, p);
    }

  return count;
}

VISIBLE SCM
scm_budan_0_1_scm_mono (SCM spline)
{
  const char *who = "scm_budan_0_1_scm_mono";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  size_t count = budan_0_1_scm_mono (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

//-------------------------------------------------------------------------
//
// The Vincent-Collins-Akritas method for isolating roots of a
// square-free polynomial (with coefficients given in the ordinary
// monomial basis) in the open interval (0,1). See
// http://en.wikipedia.org/wiki/Vincent%27s_theorem

static void
p_0_mid_to_p_mid_1 (size_t degree, const SCM *p_0_mid, SCM *p_mid_1)
{
  // Here we use the binomial expansion
  //
  //    (1 + x)ⁿ = ∑ᵢC(n,i)xⁱ for i = 0,1,...,n
  //
  // to simplify and speed things up.

  p_mid_1[0] = p_0_mid[degree];
  for (size_t i = 1; i < degree; i++)
    p_mid_1[i] = scm_product (scm_c_bincoef (degree, i), p_0_mid[degree]);
  p_mid_1[degree] = p_0_mid[degree];

  for (size_t k = 1; k < degree; k++)
    {
      p_mid_1[0] = scm_sum (p_mid_1[0], p_0_mid[degree - k]);
      for (size_t i = 1; i < degree - k; i++)
        p_mid_1[i] = scm_sum (p_mid_1[i],
                              scm_product (scm_c_bincoef (degree - k, i),
                                           p_0_mid[degree - k]));
      p_mid_1[degree - k] = scm_sum (p_mid_1[degree - k], p_0_mid[degree - k]);
    }

  p_mid_1[0] = scm_sum (p_mid_1[0], p_0_mid[0]);
}

static SCM
vca_recursion_scm (size_t degree, ssize_t stride, const SCM *p, SCM a, SCM b)
{
  SCM intervals = SCM_EOL;

  size_t var = budan_0_1_scm_mono (degree, stride, p);
  switch (var)
    {
    case 0:
      // Do nothing here; leave @var{intervals} an empty list.
      break;

    case 1:
      intervals = scm_cons (scm_cons (a, b), intervals);
      break;

    default:
      {
        SCM p_0_mid[degree + 1];
        SCM p_mid_1[degree + 1];

        SCM pow2 = scm_from_int (1);
        p_0_mid[degree] = p[stride * (ssize_t) degree];
        for (size_t i = 1; i <= degree; i++)
          {
            pow2 = scm_sum (pow2, pow2);
            p_0_mid[degree - i] =
              scm_product (pow2, p[stride * (ssize_t) (degree - i)]);
          }

        p_0_mid_to_p_mid_1 (degree, p_0_mid, p_mid_1);

        SCM one = scm_from_int (1);
        SCM two = scm_from_int (2);
        SCM one_half = scm_divide (one, two);

        SCM mid = scm_divide (scm_sum (a, b), two);

        SCM p_mid = eval_scm_mono (degree, stride, p, one_half);

        if (scm_is_true (scm_zero_p (p_mid)))
          // The midpoint is a root.
          intervals = scm_cons (scm_cons (mid, mid), intervals);

        SCM left_intervals = vca_recursion_scm (degree, 1, p_0_mid, a, mid);
        SCM right_intervals = vca_recursion_scm (degree, 1, p_mid_1, mid, b);

        intervals =
          scm_append (scm_list_3 (left_intervals, intervals, right_intervals));
      }
      break;
    }

  return intervals;
}

VISIBLE SCM
isolate_roots_scm_mono (size_t degree, ssize_t stride, const SCM *poly,
                        SCM a, SCM b)
{
  SCM zero = scm_from_int (0);
  SCM one = scm_from_int (1);

  if (SCM_UNBNDP (a))
    a = zero;
  if (SCM_UNBNDP (b))
    b = one;

  SCM p[degree + 1];
  if (scm_is_true (scm_zero_p (a)) && scm_is_true (scm_num_eq_p (b, one)))
    copy_scm_with_strides (1, p, stride, poly, degree + 1);
  else
    portion_scm_mono (degree, stride, poly, a, b, 1, p);

  // Isolate roots in (a,b).
  SCM intervals = vca_recursion_scm (degree, 1, p, a, b);

  // Is @var{b} a root?
  SCM p1 = eval_scm_mono (degree, 1, poly, b);
  if (scm_is_true (scm_zero_p (p1)))
    intervals = scm_append (scm_list_2 (intervals,
                                        scm_list_1 (scm_cons (b, b))));

  // Is @var{a} a root?
  SCM p0 = eval_scm_mono (degree, 1, poly, a);
  if (scm_is_true (scm_zero_p (p0)))
    intervals = scm_cons (scm_cons (a, a), intervals);

  return intervals;
}

VISIBLE SCM
scm_isolate_roots_scm_mono (SCM poly, SCM a, SCM b)
{
  const char *who = "scm_isolate_roots_scm_mono";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (poly, &handle);
  scm_dynwind_array_handle_release (&handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, poly, &handle,
                                              &dim, &stride);
  const SCM *_poly = scm_array_handle_elements (&handle);

  SCM intervals = isolate_roots_scm_mono (dim - 1, stride, _poly, a, b);

  scm_dynwind_end ();

  return intervals;
}

//-------------------------------------------------------------------------

typedef double _f64_evaluator_t (size_t degree, ssize_t stride,
                                 const double *spline, double t);

typedef struct
{
  _f64_evaluator_t *eval;
  size_t degree;
  ssize_t stride;
  const double *spline;
} _f64_evaluator_and_spline_t;

static double
f64_eval (double t, void *eval_struct)
{
  _f64_evaluator_and_spline_t *e = (_f64_evaluator_and_spline_t *) eval_struct;
  return e->eval (e->degree, e->stride, e->spline, t);
}

VISIBLE void
find_bracketed_root_f64 (_f64_evaluator_t * eval,
                         size_t degree, ssize_t stride,
                         const double *spline, double a, double b,
                         double tolerance, double epsilon, double *root,
                         int *err, unsigned int *iter_no)
{
  _f64_evaluator_and_spline_t e = {
    .eval = eval,.degree = degree,.stride = stride,.spline = spline
  };
  brentroot (-1, tolerance, epsilon, a, b, f64_eval, &e, root, err, iter_no);
  if (*err != 0)
    *root = nan ("");
}

static SCM
scm_find_bracketed_root_f64 (const char *who,
                             _f64_evaluator_t * eval, SCM spline,
                             SCM a, SCM b, SCM tolerance, SCM epsilon)
{
  scm_t_array_handle handle;

  double tol = (SCM_UNBNDP (tolerance)) ? -1.0 : scm_to_double (tolerance);
  double eps = (SCM_UNBNDP (epsilon)) ? -1.0 : scm_to_double (epsilon);

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle, &dim,
                                              &stride);
  const double *_spline = scm_array_handle_f64_elements (&handle);

  double root;
  int err;
  unsigned int iter_no;
  find_bracketed_root_f64 (eval, dim - 1, stride, _spline, scm_to_double (a),
                           scm_to_double (b), tol, eps, &root, &err, &iter_no);

  scm_dynwind_end ();

  SCM values[3] = {
    ((err == 0) ? scm_from_double (root) : SCM_BOOL_F),
    scm_from_int (err),
    scm_from_uint (iter_no)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_find_bracketed_root_f64_mono (SCM spline, SCM a, SCM b,
                                  SCM tolerance, SCM epsilon)
{
  return scm_find_bracketed_root_f64 ("scm_find_bracketed_root_f64_mono",
                                      eval_f64_mono, spline, a, b,
                                      tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_f64_bern_schumaker_volk (SCM spline, SCM a, SCM b,
                                                 SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_f64
    ("scm_find_bracketed_root_f64_bern_schumaker_volk",
     eval_f64_bern_schumaker_volk, spline, a, b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_f64_bern_de_casteljau (SCM spline, SCM a, SCM b,
                                               SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_f64
    ("scm_find_bracketed_root_f64_bern_de_casteljau",
     eval_f64_bern_de_casteljau, spline, a, b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_f64_sbern_schumaker_volk (SCM spline, SCM a, SCM b,
                                                  SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_f64
    ("scm_find_bracketed_root_f64_sbern_schumaker_volk",
     eval_f64_sbern_schumaker_volk, spline, a, b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_f64_sbern_de_casteljau (SCM spline, SCM a, SCM b,
                                                SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_f64
    ("scm_find_bracketed_root_f64_sbern_de_casteljau",
     eval_f64_sbern_de_casteljau, spline, a, b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_f64_spower (SCM spline, SCM a, SCM b, SCM tolerance,
                                    SCM epsilon)
{
  return scm_find_bracketed_root_f64 ("scm_find_bracketed_root_f64_spower",
                                      eval_f64_spower, spline, a, b,
                                      tolerance, epsilon);
}

//-------------------------------------------------------------------------

typedef SCM _scm_evaluator_t (size_t degree, ssize_t stride,
                              const SCM *spline, SCM t);

typedef struct
{
  _scm_evaluator_t *eval;
  size_t degree;
  ssize_t stride;
  const SCM *spline;
} _scm_evaluator_and_spline_t;

static void
mpq_eval (mpq_t value, mpq_t t, void *eval_struct)
{
  _scm_evaluator_and_spline_t *e = (_scm_evaluator_and_spline_t *) eval_struct;
  scm_to_mpq (e->eval (e->degree, e->stride, e->spline, scm_from_mpq (t)),
              value);
}

VISIBLE void
find_bracketed_root_scm_exact (_scm_evaluator_t *eval,
                               size_t degree, ssize_t stride,
                               const SCM *spline, SCM a, SCM b,
                               SCM tolerance, SCM epsilon, SCM *root,
                               int *err, unsigned int *iter_no)
{
  scm_dynwind_begin (0);

  mpq_t t1;
  mpq_init (t1);
  scm_dynwind_mpq_clear (t1);
  scm_to_mpq (a, t1);

  mpq_t t2;
  mpq_init (t2);
  scm_dynwind_mpq_clear (t2);
  scm_to_mpq (b, t2);

  mpq_t tol;
  mpq_init (tol);
  scm_dynwind_mpq_clear (tol);
  scm_to_mpq (tolerance, tol);

  mpq_t eps;
  mpq_init (eps);
  scm_dynwind_mpq_clear (eps);
  scm_to_mpq (epsilon, eps);

  mpq_t t;
  mpq_init (t);
  scm_dynwind_mpq_clear (t);

  _scm_evaluator_and_spline_t e = {
    .eval = eval,
    .degree = degree,
    .stride = stride,
    .spline = spline
  };
  mpq_brentroot (-1, tol, eps, t1, t2, mpq_eval, &e, t, err, iter_no);

  *root = (*err == 0) ? scm_from_mpq (t) : SCM_BOOL_F;

  scm_dynwind_end ();
}

static SCM
scm_find_bracketed_root_scm_exact (const char *who,
                                   _scm_evaluator_t *eval, SCM spline,
                                   SCM a, SCM b, SCM tolerance, SCM epsilon)
{
  scm_t_array_handle handle;

  SCM tol = (SCM_UNBNDP (tolerance)) ? scm_from_int (-1) : tolerance;
  SCM eps = (SCM_UNBNDP (epsilon)) ? scm_from_int (-1) : epsilon;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  SCM root;
  int err;
  unsigned int iter_no;
  find_bracketed_root_scm_exact (eval, dim - 1, stride, _spline, a, b,
                                 tol, eps, &root, &err, &iter_no);

  scm_dynwind_end ();

  SCM values[3] = {
    root, scm_from_int (err), scm_from_uint (iter_no)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_find_bracketed_root_scm_mono_exact (SCM spline, SCM a, SCM b,
                                        SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_scm_exact ("scm_find_bracketed_root_scm_mono_exact",
                                       eval_scm_mono, spline, a, b, tolerance,
                                       epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_scm_bern_exact (SCM spline, SCM a, SCM b,
                                        SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_scm_exact ("scm_find_bracketed_root_scm_bern_exact",
                                       eval_scm_bern_schumaker_volk, spline, a,
                                       b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_scm_sbern_exact (SCM spline, SCM a, SCM b,
                                         SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_scm_exact
    ("scm_find_bracketed_root_scm_sbern_exact", eval_scm_sbern_schumaker_volk,
     spline, a, b, tolerance, epsilon);
}

VISIBLE SCM
scm_find_bracketed_root_scm_spower_exact (SCM spline, SCM a, SCM b,
                                          SCM tolerance, SCM epsilon)
{
  return
    scm_find_bracketed_root_scm_exact
    ("scm_find_bracketed_root_scm_spower_exact", eval_scm_spower, spline, a, b,
     tolerance, epsilon);
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_find_roots_scm_mono (SCM poly, SCM a, SCM b)
{
  return scm_call_3 (scm_c_public_ref (my_module, "poly:find-roots-scm-mono"),
                     poly, a, b);
}

//-------------------------------------------------------------------------

void init_math_polyspline_roots (void);

VISIBLE void
init_math_polyspline_roots (void)
{
  scm_c_define_gsubr ("poly:sign-variations-f64", 1, 0, 0,
                      scm_sign_variations_f64);
  scm_c_define_gsubr ("poly:sign-variations-scm", 1, 0, 0,
                      scm_sign_variations_scm);

  scm_c_define_gsubr ("poly:budan-0_1-scm-mono", 1, 0, 0,
                      scm_budan_0_1_scm_mono);

  scm_c_define_gsubr ("poly:isolate-roots-scm-mono", 1, 2, 0,
                      scm_isolate_roots_scm_mono);

  scm_c_define_gsubr ("poly:find-bracketed-root-f64-mono", 3, 2, 0,
                      scm_find_bracketed_root_f64_mono);
  scm_c_define_gsubr ("poly:find-bracketed-root-f64-bern-schumaker-volk", 3, 2,
                      0, scm_find_bracketed_root_f64_bern_schumaker_volk);
  scm_c_define_gsubr ("poly:find-bracketed-root-f64-bern-de-casteljau", 3, 2, 0,
                      scm_find_bracketed_root_f64_bern_de_casteljau);
  scm_c_define_gsubr ("poly:find-bracketed-root-f64-sbern-schumaker-volk", 3, 2,
                      0, scm_find_bracketed_root_f64_sbern_schumaker_volk);
  scm_c_define_gsubr ("poly:find-bracketed-root-f64-sbern-de-casteljau", 3, 2,
                      0, scm_find_bracketed_root_f64_sbern_de_casteljau);
  scm_c_define_gsubr ("poly:find-bracketed-root-f64-spower", 3, 2, 0,
                      scm_find_bracketed_root_f64_spower);
  scm_c_define_gsubr ("poly:find-bracketed-root-scm-mono-exact", 3, 2, 0,
                      scm_find_bracketed_root_scm_mono_exact);
  scm_c_define_gsubr ("poly:find-bracketed-root-scm-bern-exact", 3, 2, 0,
                      scm_find_bracketed_root_scm_bern_exact);
  scm_c_define_gsubr ("poly:find-bracketed-root-scm-sbern-exact", 3, 2, 0,
                      scm_find_bracketed_root_scm_sbern_exact);
  scm_c_define_gsubr ("poly:find-bracketed-root-scm-spower-exact", 3, 2, 0,
                      scm_find_bracketed_root_scm_spower_exact);
}

//-------------------------------------------------------------------------
