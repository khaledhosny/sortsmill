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

static SCM value_and_exception (pure_expr * value, pure_expr * exception);

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

static SCM
scm_pure_expr_to_string_or_f (SCM x)
{
  scm_dynwind_begin (0);

  char *s;
  bool success = pure_is_string_dup (scm_pure_expr_to_pointer (x), &s);
  SCM result = SCM_BOOL_F;
  if (success)
    {
      scm_dynwind_free (s);
      result = scm_from_utf8_string (s);
    }

  scm_dynwind_end ();

  return result;
}

static SCM
scm_pure_expr_is_string (SCM x)
{
  const char *s;
  bool success = pure_is_string (scm_pure_expr_to_pointer (x), &s);
  return scm_from_bool (success);
}

static SCM
scm_symbol_pure_expr_to_small_integer_or_f (SCM x)
{
  int32_t sym;
  bool success = pure_is_symbol (scm_pure_expr_to_pointer (x), &sym);
  return (success) ? scm_from_int32 (sym) : SCM_BOOL_F;
}

static SCM
scm_eval_pure_symbol (SCM string)
{
  SCM result = SCM_BOOL_F;

  scm_dynwind_begin (0);

  char *s = scm_to_utf8_stringn (string, NULL);
  scm_dynwind_free (s);

  int32_t sym = pure_getsym (s);
  if (sym != 0)
    {
      pure_expr *exception;
      pure_expr *value = pure_symbolx (sym, &exception);
      result = value_and_exception (value, exception);
    }

  scm_dynwind_end ();

  return result;
}

static SCM
scm_pure_str (SCM x)
{
  scm_dynwind_begin (0);

  char *s = str (scm_pure_expr_to_pointer (x));
  scm_dynwind_free (s);

  SCM result = scm_from_utf8_string (s);

  scm_dynwind_end ();

  return result;
}

static SCM
scm_pure_eval (SCM x)
{
  SCM result = SCM_BOOL_F;
  pure_expr *exception = NULL;
  pure_expr *value = pure_evalx (scm_pure_expr_to_pointer (x), &exception);
  if (value != NULL || exception != NULL)
    result = value_and_exception (value, exception);
  return result;
}

static SCM
scm_pure_pointer_type (SCM tag)
{
  const char *s = pure_pointer_type (scm_to_int (tag));
  return (s == NULL) ? SCM_BOOL_F : scm_from_utf8_string (s);
}

static SCM
value_and_exception (pure_expr * value, pure_expr * exception)
{
  SCM car = (value != NULL) ? scm_pointer_to_pure_expr (value) : SCM_BOOL_F;
  SCM cdr =
    (exception != NULL) ? scm_pointer_to_pure_expr (exception) : SCM_BOOL_F;
  return scm_cons (car, cdr);
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
  scm_c_define_gsubr ("pure-expr->string-or-f", 1, 0, 0,
                      scm_pure_expr_to_string_or_f);
  scm_c_define_gsubr ("pure-expr-is-string?", 1, 0, 0, scm_pure_expr_is_string);
  scm_c_define_gsubr ("symbol-pure-expr->small-integer-or-f", 1, 0, 0,
                      scm_symbol_pure_expr_to_small_integer_or_f);
  scm_c_define_gsubr ("private:eval-pure-symbol", 1, 0, 0,
                      scm_eval_pure_symbol);
  scm_c_define_gsubr ("pure-str", 1, 0, 0, scm_pure_str);
  scm_c_define_gsubr ("private:pure-eval", 1, 0, 0, scm_pure_eval);
  scm_c_define_gsubr ("pure-pointer-type", 1, 0, 0, scm_pure_pointer_type);
}
