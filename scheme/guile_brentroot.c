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

#include <sortsmillff/guile/brentroot.h>
#include <sortsmillff/brentroot.h>
#include <sortsmillff/qbrentroot.h>
#include <math.h>
#include <gmp.h>

void init_guile_sortsmillff_brentroot (void);

//-------------------------------------------------------------------------

static double
call_func (double x, void *func_p)
{
  SCM func = *(SCM *) func_p;
  return scm_to_double (scm_call_1 (func, scm_from_double (x)));
}

VISIBLE SCM
scm_f64_brentroot (SCM max_iters, SCM tol, SCM t1, SCM t2, SCM func)
{
  double root;
  int err;
  unsigned int iter_no;
  SCM result[3];

  int c_max_iters = scm_to_int (max_iters);
  double c_tol = scm_to_double (tol);
  double c_t1 = scm_to_double (t1);
  double c_t2 = scm_to_double (t2);

  root = nan ("");
  brentroot (c_max_iters, c_tol, c_t1, c_t2, call_func, &func, &root,
             &err, &iter_no);

  result[0] = (err == 0) ? scm_from_double (root) : SCM_BOOL_F;
  result[1] = scm_from_int (err);
  result[2] = scm_from_uint (iter_no);

  return scm_c_values (result, 3);
}

//-------------------------------------------------------------------------

// FIXME: This should be extern somewhere.
_GL_ATTRIBUTE_PURE static inline SCM
scm_from_mpq (const mpq_t x)
{
  mpz_t num;
  mpz_t den;

  mpz_init (num);
  mpz_init (den);
  mpq_get_num (num, x);
  mpq_get_den (den, x);
  SCM result = scm_divide (scm_from_mpz (num), scm_from_mpz (den));
  mpz_clear (num);
  mpz_clear (den);
  return result;
}

// FIXME: This should be extern somewhere.
static inline void
scm_to_mpq (SCM x, mpq_t result)
{
  SCM xx = scm_inexact_to_exact (x);
  scm_to_mpz (scm_numerator (xx), mpq_numref (result));
  scm_to_mpz (scm_denominator (xx), mpq_denref (result));
  mpq_canonicalize (result);    /* Perhaps not necessary, but let us
                                   be safe. */
}

static void
call_qfunc (mpq_t result, const mpq_t x, void *func_p)
{
  SCM func = *(SCM *) func_p;
  SCM value = scm_call_1 (func, scm_from_mpq (x));
  scm_to_mpq (value, result);
}

VISIBLE SCM
scm_mpq_brentroot (SCM max_iters, SCM tol, SCM epsilon, SCM t1, SCM t2,
                   SCM func)
{
  mpq_t root, c_tol, c_t1, c_t2, c_epsilon;
  int err;
  unsigned int iter_no;
  SCM result[3];

  mpq_inits (root, c_tol, c_t1, c_t2, c_epsilon, NULL);

  int c_max_iters = scm_to_int (max_iters);

  scm_to_mpq (tol, c_tol);
  scm_to_mpq (t1, c_t1);
  scm_to_mpq (t2, c_t2);
  scm_to_mpq (epsilon, c_epsilon);

  qbrentroot (c_max_iters, c_tol, c_epsilon, c_t1, c_t2, call_qfunc, &func,
              root, &err, &iter_no);

  result[0] = (err == 0) ? scm_from_mpq (root) : SCM_BOOL_F;
  result[1] = scm_from_int (err);
  result[2] = scm_from_uint (iter_no);

  mpq_clears (root, c_tol, c_t1, c_t2, c_epsilon, NULL);

  return scm_c_values (result, 3);
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_brentroot (void)
{
  scm_c_define_gsubr ("f64-brentroot", 5, 0, 0, scm_f64_brentroot);
  scm_c_define_gsubr ("mpq-brentroot", 6, 0, 0, scm_mpq_brentroot);
}

//-------------------------------------------------------------------------
