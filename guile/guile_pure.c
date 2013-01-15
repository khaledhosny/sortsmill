#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <pure/runtime.h>
#include <libguile.h>
#include <stdio.h>

void init_guile_sortsmillff_pure (void);

static void
pure_expr_finalizer (void *x)
{
  pure_free (x);
}

static SCM
scm_pure_new (SCM p)
{
  return scm_from_pointer (scm_to_pointer (p), pure_expr_finalizer);
}

static SCM
scm_scm_pointer_to_pure_expr (SCM p)
{
  /* @var{pointer->pure-expr} sets up reference counting for the Pure
     garbage collector, then wraps the pointer as a @var{pure-expr}
     object. */
  return scm_call_1 (scm_c_public_ref ("sortsmillff pure",
                                       "pointer->pure-expr"), p);
}

static SCM
scm_pointer_to_pure_expr (void *p)
{
  return scm_scm_pointer_to_pure_expr (scm_from_pointer (p, NULL));
}

static SCM
scm_pure_expr_to_scm_pointer (SCM x)
{
  return scm_call_1 (scm_c_private_ref ("sortsmillff pure",
                                        "procedure:pure-expr->pointer"), x);
}

static pure_expr *
scm_pure_expr_to_pointer (SCM x)
{
  return (pure_expr *) scm_to_pointer (scm_pure_expr_to_scm_pointer (x));
}

static void
mpz_clear_void_ptr (void *z)
{
  mpz_clear (*(mpz_t *) z);
}

static SCM
scm_big_integer_to_pure_expr (SCM n)
{
  scm_dynwind_begin (0);

  mpz_t temp;
  mpz_init (temp);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &temp, SCM_F_WIND_EXPLICITLY);

  scm_to_mpz (n, temp);
  pure_expr *result = pure_mpz (temp);

  scm_dynwind_end ();

  return scm_pointer_to_pure_expr (result);
}

static SCM
scm_pure_expr_to_small_integer_or_f (SCM x)
{
  int32_t n;
  bool success = pure_is_int (scm_pure_expr_to_pointer (x), &n);
  return (success) ? scm_from_int32 (n) : SCM_BOOL_F;
}

static SCM
scm_pure_expr_to_big_integer_or_f (SCM x)
{
  scm_dynwind_begin (0);

  mpz_t n;
  mpz_init (n);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &n, SCM_F_WIND_EXPLICITLY);

  bool success = pure_is_mpz (scm_pure_expr_to_pointer (x), &n);
  SCM result = (success) ? scm_from_mpz (n) : SCM_BOOL_F;

  scm_dynwind_end ();

  return result;
}

static SCM
scm_rational_to_pure_expr (SCM r)
{
  scm_dynwind_begin (0);

  const mpz_t z[2];
  mpz_inits (*(mpz_t *) z[0], *(mpz_t *) z[1], NULL);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &z[0], SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &z[1], SCM_F_WIND_EXPLICITLY);

  scm_to_mpz (scm_numerator (r), *(mpz_t *) &z[0]);
  scm_to_mpz (scm_denominator (r), *(mpz_t *) &z[1]);
  pure_expr *result = pure_rationalz (z);

  scm_dynwind_end ();

  return scm_pointer_to_pure_expr (result);
}

static SCM
scm_pure_expr_to_rational_or_f (SCM x)
{
  scm_dynwind_begin (0);

  mpz_t z[2];
  mpz_inits (*(mpz_t *) z[0], *(mpz_t *) z[1], NULL);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &z[0], SCM_F_WIND_EXPLICITLY);
  scm_dynwind_unwind_handler (mpz_clear_void_ptr, &z[1], SCM_F_WIND_EXPLICITLY);

  bool success = pure_is_rationalz (scm_pure_expr_to_pointer (x), z);
  SCM result = SCM_BOOL_F;
  if (success)
    result = scm_divide (scm_from_mpz (z[0]), scm_from_mpz (z[1]));

  scm_dynwind_end ();

  return result;
}

static SCM
scm_pure_expr_to_inexact_or_f (SCM x)
{
  double r;
  bool success = pure_is_double (scm_pure_expr_to_pointer (x), &r);
  return (success) ? scm_from_double (r) : SCM_BOOL_F;
}

static SCM
scm_complex_to_pure_expr (SCM z)
{
  double c[2];
  c[0] = scm_to_double (scm_real_part (z));
  c[1] = scm_to_double (scm_imag_part (z));
  return scm_pointer_to_pure_expr (pure_complex (c));
}

static SCM
scm_pure_expr_to_complex_or_f (SCM x)
{
  double c[2];
  bool success = pure_is_complex (scm_pure_expr_to_pointer (x), c);
  SCM result = SCM_BOOL_F;
  if (success)
    result = scm_make_rectangular (scm_from_double (c[0]),
                                   scm_from_double (c[1]));
  return result;
}

static SCM
scm_pure_expr_to_pointer_or_f (SCM x)
{
  void *p;
  bool success = pure_is_pointer (scm_pure_expr_to_pointer (x), &p);
  return (success) ? scm_from_pointer (p, NULL) : SCM_BOOL_F;
}

VISIBLE void
init_guile_sortsmillff_pure (void)
{
  scm_c_define_gsubr ("scm-pure-new", 1, 0, 0, scm_pure_new);
  scm_c_define_gsubr ("big-integer->pure-expr", 1, 0, 0,
                      scm_big_integer_to_pure_expr);
  scm_c_define_gsubr ("pure-expr->small-integer-or-f", 1, 0, 0,
                      scm_pure_expr_to_small_integer_or_f);
  scm_c_define_gsubr ("pure-expr->big-integer-or-f", 1, 0, 0,
                      scm_pure_expr_to_big_integer_or_f);
  scm_c_define_gsubr ("rational->pure-expr", 1, 0, 0,
                      scm_rational_to_pure_expr);
  scm_c_define_gsubr ("pure-expr->rational-or-f", 1, 0, 0,
                      scm_pure_expr_to_rational_or_f);
  scm_c_define_gsubr ("pure-expr->inexact-or-f", 1, 0, 0,
                      scm_pure_expr_to_inexact_or_f);
  scm_c_define_gsubr ("complex->pure-expr", 1, 0, 0, scm_complex_to_pure_expr);
  scm_c_define_gsubr ("pure-expr->complex-or-f", 1, 0, 0,
                      scm_pure_expr_to_complex_or_f);
  scm_c_define_gsubr ("pointer-pure-expr->pointer-or-f", 1, 0, 0,
		      scm_pure_expr_to_pointer_or_f);
}
