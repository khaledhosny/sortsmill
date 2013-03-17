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

#include <sortsmill/guile.h>
#include <sortsmill/math.h>
#include <math.h>
#include <gmp.h>

void init_guile_sortsmill_math_brentroot (void);

//-------------------------------------------------------------------------

static double
call_func (double x, void *func_p)
{
  SCM func = *(SCM *) func_p;
  return scm_to_double (scm_call_1 (func, scm_from_double (x)));
}

VISIBLE SCM
scm_f64_brentroot (SCM max_iters, SCM tol, SCM epsilon, SCM t1, SCM t2,
                   SCM func)
{
  double root;
  int err;
  unsigned int iter_no;
  SCM result[3];

  int c_max_iters = scm_to_int (max_iters);
  double c_tol = scm_to_double (tol);
  double c_epsilon = scm_to_double (epsilon);
  double c_t1 = scm_to_double (t1);
  double c_t2 = scm_to_double (t2);

  root = nan ("");
  brentroot (c_max_iters, c_tol, c_epsilon, c_t1, c_t2, call_func, &func, &root,
             &err, &iter_no);

  result[0] = (err == 0) ? scm_from_double (root) : SCM_BOOL_F;
  result[1] = scm_from_int (err);
  result[2] = scm_from_uint (iter_no);

  return scm_c_values (result, 3);
}

//-------------------------------------------------------------------------

static void
call_qfunc (mpq_t result, mpq_t x, void *func_p)
{
  SCM func = *(SCM *) func_p;
  SCM value = scm_call_1 (func, scm_from_mpq (x));
  scm_to_mpq (scm_inexact_to_exact (value), result);
}

VISIBLE SCM
scm_mpq_brentroot (SCM max_iters, SCM tol, SCM epsilon, SCM t1, SCM t2,
                   SCM func)
{
  mpq_t root;
  mpq_t c_tol;
  mpq_t c_t1;
  mpq_t c_t2;
  mpq_t c_epsilon;
  int err;
  unsigned int iter_no;
  SCM result[3];

  scm_dynwind_begin (0);

  mpq_init (root);
  scm_dynwind_mpq_clear (root);

  mpq_init (c_tol);
  scm_dynwind_mpq_clear (c_tol);

  mpq_init (c_t1);
  scm_dynwind_mpq_clear (c_t1);

  mpq_init (c_t2);
  scm_dynwind_mpq_clear (c_t2);

  mpq_init (c_epsilon);
  scm_dynwind_mpq_clear (c_epsilon);

  int c_max_iters = scm_to_int (max_iters);

  scm_to_mpq (scm_inexact_to_exact (tol), c_tol);
  scm_to_mpq (scm_inexact_to_exact (t1), c_t1);
  scm_to_mpq (scm_inexact_to_exact (t2), c_t2);
  scm_to_mpq (scm_inexact_to_exact (epsilon), c_epsilon);

  mpq_brentroot (c_max_iters, c_tol, c_epsilon, c_t1, c_t2, call_qfunc, &func,
                 root, &err, &iter_no);

  result[0] = (err == 0) ? scm_from_mpq (root) : SCM_BOOL_F;
  result[1] = scm_from_int (err);
  result[2] = scm_from_uint (iter_no);

  scm_dynwind_end ();

  return scm_c_values (result, 3);
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmill_math_brentroot (void)
{
  scm_c_define_gsubr ("f64-brentroot", 6, 0, 0, scm_f64_brentroot);
  scm_c_define_gsubr ("mpq-brentroot", 6, 0, 0, scm_mpq_brentroot);
}

//-------------------------------------------------------------------------
