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

#include <sortsmillff/qbrentroot.h>
#include <stdbool.h>
#include <stdio.h>
#include <float.h>
#include <sortsmillff/gmp_constants.h>

//
// Brent's method for root-finding --
// multiple precision rational number version.
//
// See http://en.wikipedia.org/wiki/Brent%27s_method
//

static const unsigned int qbrentroot_default_max_iters = 1000000;
static const double qbrentroot_default_tol = DBL_EPSILON;       /* FIXME: Is
                                                                   this value
                                                                   appropriate? */

_GL_ATTRIBUTE_CONST static inline unsigned int
actual_max_iterations (int max_iters)
{
  return (0 <= max_iters) ? max_iters : qbrentroot_default_max_iters;
}

static inline void
actual_tolerance (mpq_t result, const mpq_t tol)
{
  if (mpq_sgn (tol) < 0)
    mpq_set_d (result, qbrentroot_default_tol);
  else
    mpq_set (result, tol);
}

static inline void
actual_epsilon (mpq_t result, const mpq_t epsilon)
{
  if (mpq_sgn (epsilon) < 0)
    mpq_set_d (result, DBL_EPSILON);
  else
    mpq_set (result, epsilon);
}

_GL_ATTRIBUTE_PURE static inline bool
bracketed (const mpq_t f1, const mpq_t f2)
{
  return (mpq_sgn (f1) <= 0 && 0 <= mpq_sgn (f2))
    || (mpq_sgn (f2) <= 0 && 0 <= mpq_sgn (f1));
}

_GL_ATTRIBUTE_PURE static inline bool
vanish (const mpq_t u)
{
  return (mpq_sgn (u) == 0);
}

static inline void
bisection (mpq_t result, const mpq_t a, const mpq_t b)
{
  mpq_sub (result, a, b);
  mpq_div (result, result, mpq_two ());
}

static inline void
linear (const mpq_t s, const mpq_t fa, const mpq_t fb, mpq_t p, mpq_t q)
{
  mpq_t fba;

  mpq_init (fba);

  mpq_div (fba, fb, fa);

  mpq_add (p, s, s);
  mpq_mul (p, p, fba);

  mpq_set_d (q, 1);
  mpq_sub (q, q, fba);

  mpq_clear (fba);
}

static inline void
inverse_quadratic (const mpq_t s, const mpq_t a, const mpq_t fa,
                   const mpq_t b, const mpq_t fb,
                   const mpq_t fc, mpq_t p, mpq_t q)
{
  mpq_t fbc, fba, fac;
  mpq_t fbc1, fba1, fac1;
  mpq_t tmp1, tmp2;

  mpq_inits (fbc, fba, fac, NULL);
  mpq_inits (fbc1, fba1, fac1, NULL);
  mpq_inits (tmp1, tmp2, NULL);

  mpq_div (fbc, fb, fc);
  mpq_div (fba, fb, fa);
  mpq_div (fac, fa, fc);

  mpq_sub (fbc1, fbc, mpq_one ());
  mpq_sub (fba1, fba, mpq_one ());
  mpq_sub (fac1, fac, mpq_one ());

  mpq_add (tmp1, s, s);
  mpq_mul (tmp1, tmp1, fac);
  mpq_sub (tmp2, fac, fbc);
  mpq_mul (tmp1, tmp1, tmp2);

  mpq_sub (tmp2, b, a);
  mpq_mul (tmp2, tmp2, fbc1);

  mpq_sub (tmp2, tmp1, tmp2);
  mpq_mul (p, fba, tmp2);

  mpq_mul (q, fac1, fba1);
  mpq_mul (q, q, fbc1);

  mpq_clears (fbc, fba, fac, NULL);
  mpq_clears (fbc1, fba1, fac1, NULL);
  mpq_clears (tmp1, tmp2, NULL);
}

static inline void
interpolate (const mpq_t a, const mpq_t fa, const mpq_t b, const mpq_t fb,
             const mpq_t fb1, const mpq_t step, const mpq_t step1,
             const mpq_t tolerance, mpq_t new_step, mpq_t new_step1)
{
  mpq_t p;
  mpq_t q;
  mpq_t s;
  mpq_t two_p;
  mpq_t tol1;
  mpq_t tol2;
  mpq_t tmp1, tmp2;

  mpq_inits (p, q, s, two_p, tol1, tol2, NULL);
  mpq_inits (tmp1, tmp2, NULL);

  bisection (s, a, b);

  if (mpq_equal (fb1, fa) || mpq_equal (fb1, fb))
    linear (s, fa, fb, p, q);
  else
    inverse_quadratic (s, a, fa, b, fb, fb1, p, q);

  if (0 < mpq_sgn (p))
    mpq_neg (q, q);
  else
    mpq_neg (p, p);

  mpq_add (two_p, p, p);

  mpq_mul (tmp2, mpq_three (), s);
  mpq_mul (tmp2, tmp2, q);
  mpq_mul (tmp1, tolerance, q);
  mpq_abs (tmp1, tmp1);
  mpq_sub (tol1, tmp2, tmp1);

  mpq_mul (tol2, step1, q);
  mpq_abs (tol2, tol2);

  if (mpq_cmp (two_p, tol1) < 0 && mpq_cmp (two_p, tol2) < 0)
    {
      mpq_div (new_step, p, q);
      mpq_set (new_step1, step);
    }
  else
    {
      mpq_set (new_step, s);
      mpq_set (new_step1, s);
    }

  mpq_clears (p, q, s, two_p, tol1, tol2, NULL);
  mpq_clears (tmp1, tmp2, NULL);
}

_GL_ATTRIBUTE_CONST static inline bool
max_iterations_exceeded (unsigned int max_iterations, unsigned int iter_no)
{
  return (max_iterations <= iter_no);
}

_GL_ATTRIBUTE_PURE static inline bool
within_tolerance (const mpq_t tolerance, const mpq_t step, const mpq_t fb)
{
  bool result;
  mpq_t abs_step;

  mpq_init (abs_step);
  mpq_abs (abs_step, step);
  result = (mpq_cmp (abs_step, tolerance) <= 0 || vanish (fb));
  mpq_clear (abs_step);

  return result;
}

_GL_ATTRIBUTE_PURE static inline bool
we_are_done (unsigned int max_iterations, unsigned int iter_no,
             const mpq_t tolerance, const mpq_t step, const mpq_t fb)
{
  return (max_iterations_exceeded (max_iterations, iter_no)
          || within_tolerance (tolerance, step, fb));
}

static inline void
step_by_at_least_tolerance (mpq_t guess, const mpq_t tolerance,
                            const mpq_t new_step, const mpq_t b)
{
  mpq_t abs_new_step;

  mpq_init (abs_new_step);

  mpq_abs (abs_new_step, new_step);

  if (mpq_cmp (tolerance, abs_new_step) < 0)
    mpq_add (guess, b, new_step);
  else if (mpq_sgn (new_step) < 0)
    mpq_sub (guess, b, tolerance);
  else
    mpq_add (guess, b, tolerance);

  mpq_clear (abs_new_step);
}

VISIBLE void
qbrentroot (int max_iters, const mpq_t tol, const mpq_t epsilon,
            const mpq_t t1, const mpq_t t2, qbrentroot_func_t func,
            void *data, mpq_t root, int *err, unsigned int *iter_no)
{
  mpq_t a, b, fa, fb;
  mpq_t b1, fb1, step, step1;
  mpq_t aa, bb, faa, fbb;
  mpq_t fguess, guess, new_step, old_step;
  mpq_t tolerance, toler, eps;
  mpq_t abs_fa, abs_fb, abs_fguess;
  mpq_t tmp1, tmp2;

  mpq_init (a);
  mpq_init (b);
  mpq_init (fa);
  mpq_init (fb);
  mpq_init (b1);
  mpq_init (fb1);
  mpq_init (step);
  mpq_init (step1);
  mpq_init (aa);
  mpq_init (bb);
  mpq_init (faa);
  mpq_init (fbb);
  mpq_init (fguess);
  mpq_init (guess);
  mpq_init (new_step);
  mpq_init (old_step);
  mpq_init (tolerance);
  mpq_init (toler);
  mpq_init (eps);
  mpq_init (abs_fa);
  mpq_init (abs_fb);
  mpq_init (abs_fguess);
  mpq_inits (tmp1, tmp2, NULL);

  const unsigned int max_iterations = actual_max_iterations (max_iters);
  actual_tolerance (toler, tol);
  actual_epsilon (eps, epsilon);

  *err = 0;                     // err == 0 means 'no error'.
  *iter_no = 0;

  mpq_set (a, t1);
  mpq_set (b, t2);
  func (fa, a, data);
  func (fb, b, data);

  if (!bracketed (fa, fb))
    *err = 1;                   // err == 1 means 'root not bracketed'.
  else
    {
      mpq_abs (abs_fa, fa);
      mpq_abs (abs_fb, fb);
      if (mpq_cmp (abs_fa, abs_fb) < 0)
        {
          // Swap a and b.
          mpq_sub (step, b, a);
          mpq_sub (step1, b, a);
          mpq_set (b1, b);
          mpq_set (fb1, fb);
          mpq_set (b, a);
          mpq_set (fb, fa);
          mpq_set (a, b1);
          mpq_set (fa, fb1);
        }
      else
        {
          mpq_sub (step, a, b);
          mpq_sub (step1, a, b);
          mpq_set (b1, a);
          mpq_set (fb1, fa);
        }

      mpq_abs (tmp2, b);
      mpq_mul (tmp2, eps, tmp2);
      mpq_add (tmp2, tmp2, tmp2);
      mpq_set_d (tmp1, 2);
      mpq_div (tmp1, toler, tmp1);
      mpq_add (tolerance, tmp2, tmp1);

      while (!we_are_done (max_iterations, *iter_no, tolerance, step, fb))
        {
          mpq_abs (tmp1, step1);
          mpq_abs (abs_fa, fa);
          mpq_abs (abs_fb, fb);
          if (mpq_cmp (tmp1, tolerance) < 0 || mpq_cmp (abs_fa, abs_fb) <= 0)
            {
              // Interpolation is stepping too slowly.
              bisection (new_step, a, b);
              mpq_set (old_step, new_step);
            }
          else
            interpolate (a, fa, b, fb, fb1, step, step1, tolerance, new_step,
                         old_step);

          step_by_at_least_tolerance (guess, tolerance, new_step, b);
          func (fguess, guess, data);
          mpq_abs (abs_fguess, fguess);

          *iter_no += 1;
          if (bracketed (fb, fguess))
            {
              if (mpq_cmp (abs_fguess, abs_fb) < 0)
                {
                  mpq_set (aa, b);
                  mpq_set (faa, fb);
                  mpq_set (bb, guess);
                  mpq_set (fbb, fguess);
                  mpq_set (step, new_step);
                  mpq_set (step1, old_step);
                }
              else
                {
                  mpq_set (aa, guess);
                  mpq_set (faa, fguess);
                  mpq_set (bb, b);
                  mpq_set (fbb, fb);
                  mpq_set (step, new_step);
                  mpq_set (step1, old_step);
                }
            }
          else
            {
              if (mpq_cmp (abs_fguess, abs_fa) < 0)
                {
                  mpq_set (aa, a);
                  mpq_set (faa, fa);
                  mpq_set (bb, guess);
                  mpq_set (fbb, fguess);
                  mpq_sub (step, guess, a);
                  mpq_sub (step1, guess, a);
                }
              else
                {
                  mpq_set (aa, guess);
                  mpq_set (faa, fguess);
                  mpq_set (bb, a);
                  mpq_set (fbb, fa);
                  mpq_sub (step, guess, a);
                  mpq_sub (step1, guess, a);
                }
            }
          mpq_set (b1, b);
          mpq_set (fb1, fb);
          mpq_set (a, aa);
          mpq_set (fa, faa);
          mpq_set (b, bb);
          mpq_set (fb, fbb);

          mpq_abs (tmp2, b);
          mpq_mul (tmp2, eps, tmp2);
          mpq_add (tmp2, tmp2, tmp2);
          mpq_set_d (tmp1, 2);
          mpq_div (tmp1, toler, tmp1);
          mpq_add (tolerance, tmp2, tmp1);
        }
      if (max_iterations_exceeded (max_iterations, *iter_no))
        *err = 2;               // err == 2 means maximum iterations exceeded.
      else
        mpq_set (root, b);
    }

  mpq_clear (a);
  mpq_clear (b);
  mpq_clear (fa);
  mpq_clear (fb);
  mpq_clear (b1);
  mpq_clear (fb1);
  mpq_clear (step);
  mpq_clear (step1);
  mpq_clear (aa);
  mpq_clear (bb);
  mpq_clear (faa);
  mpq_clear (fbb);
  mpq_clear (fguess);
  mpq_clear (guess);
  mpq_clear (new_step);
  mpq_clear (old_step);
  mpq_clear (tolerance);
  mpq_clear (toler);
  mpq_clear (eps);
  mpq_clear (abs_fa);
  mpq_clear (abs_fb);
  mpq_clear (abs_fguess);
  mpq_clears (tmp1, tmp2, NULL);
}
