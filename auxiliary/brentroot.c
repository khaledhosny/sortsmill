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

#include <brentroot.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>

//
// Brent's method for root-finding.
// See http://en.wikipedia.org/wiki/Brent%27s_method
//

static const unsigned int brentroot_default_max_iters = 1000000;
static const double brentroot_default_tol = DBL_EPSILON;

_GL_ATTRIBUTE_CONST static inline unsigned int
actual_max_iterations (int max_iters)
{
  return (0 <= max_iters) ? max_iters : brentroot_default_max_iters;
}

_GL_ATTRIBUTE_CONST static inline double
actual_tolerance (double tol)
{
  return (0.0 <= tol) ? tol : brentroot_default_tol;
}

_GL_ATTRIBUTE_CONST static inline bool
bracketed (double f1, double f2)
{
  return (f1 <= 0.0 && 0.0 <= f2) || (f2 <= 0.0 && 0.0 <= f1);
}

_GL_ATTRIBUTE_CONST static inline bool
vanish (double u)
{
  return (u == 0.0);
}

_GL_ATTRIBUTE_CONST static inline double
bisection (double a, double b)
{
  return 0.5 * (a - b);
}

static inline void
linear (double s, double fa, double fb, double *const p, double *const q)
{
  const double fba = fb / fa;
  *p = fba * 2.0 * s;
  *q = 1.0 - fba;
}

static inline void
inverse_quadratic (double s, double a, double fa, double b, double fb,
                   double fc, double *const p, double *const q)
{
  const double fbc = fb / fc;
  const double fba = fb / fa;
  const double fac = fa / fc;
  *p = fba * (2.0 * s * fac * (fac - fbc) - (b - a) * (fbc - 1.0));
  *q = (fac - 1.0) * (fba - 1.0) * (fbc - 1.0);
}

static void
interpolate (double a, double fa, double b, double fb, double fb1,
             double step, double step1, double tolerance,
             double *const new_step, double *const new_step1)
{
  double p;
  double q;

  const double s = bisection (a, b);

  if (fb1 == fa || fb1 == fb)
    linear (s, fa, fb, &p, &q);
  else
    inverse_quadratic (s, a, fa, b, fb, fb1, &p, &q);
  if (0.0 < p)
    q = -q;
  else
    p = -p;
  if (2.0 * p < fmin (3.0 * s * q - fabs (tolerance * q), fabs (step1 * q)))
    {
      *new_step = p / q;
      *new_step1 = step;
    }
  else
    {
      *new_step = s;
      *new_step1 = s;
    }
}

_GL_ATTRIBUTE_CONST static inline bool
max_iterations_exceeded (unsigned int max_iterations, unsigned int iter_no)
{
  return (max_iterations <= iter_no);
}

_GL_ATTRIBUTE_CONST static inline bool
within_tolerance (double tolerance, double step, double fb)
{
  return (fabs (step) <= tolerance || vanish (fb));
}

_GL_ATTRIBUTE_CONST static inline bool
we_are_done (unsigned int max_iterations, unsigned int iter_no,
             double tolerance, double step, double fb)
{
  return (max_iterations_exceeded (max_iterations, iter_no)
          || within_tolerance (tolerance, step, fb));
}

_GL_ATTRIBUTE_CONST static inline double
step_by_at_least_tolerance (double tolerance, double new_step, double b)
{
  double guess;
  if (tolerance < fabs (new_step))
    guess = b + new_step;
  else if (new_step < 0)
    guess = b - tolerance;
  else
    guess = b + tolerance;
  return guess;
}

void
brentroot (int max_iters, double tol, double t1, double t2,
           brentroot_func_t func, void *data, double *root, int *err,
           unsigned int *iter_no)
{
  double b1, fb1, step, step1;
  double aa, bb, faa, fbb;
  double fguess, guess, new_step, old_step;
  double tolerance;

  const unsigned int max_iterations = actual_max_iterations (max_iters);
  const double toler = actual_tolerance (tol);

  *err = 0;                     // err == 0 means 'no error'.
  *iter_no = 0;

  double a = t1;
  double b = t2;
  double fa = func (a, data);
  double fb = func (b, data);
  if (!bracketed (fa, fb))
    *err = 1;                   // err == 1 means 'root not bracketed'.
  else
    {
      if (fabs (fa) < fabs (fb))
        {
          // Swap a and b.
          step = b - a;
          step1 = HUGE_VAL;
          b1 = b;
          fb1 = fb;
          b = a;
          fb = fa;
          a = b1;
          fa = fb1;
        }
      else
        {
          step = a - b;
          step1 = HUGE_VAL;
          b1 = a;
          fb1 = fa;
        }
      tolerance = 2.0 * DBL_EPSILON * fabs (b) + 0.5 * toler;
      while (!we_are_done (max_iterations, *iter_no, tolerance, step, fb))
        {
          if (fabs (step1) < tolerance || fabs (fa) <= fabs (fb))
            {
              // Interpolation is stepping too slowly.
              new_step = bisection (a, b);
              old_step = new_step;
            }
          else
            interpolate (a, fa, b, fb, fb1, step, step1, tolerance, &new_step,
                         &old_step);

          guess = step_by_at_least_tolerance (tolerance, new_step, b);
          fguess = func (guess, data);

          *iter_no += 1;
          if (bracketed (fb, fguess))
            {
              if (fabs (fguess) < fabs (fb))
                {
                  aa = b;
                  faa = fb;
                  bb = guess;
                  fbb = fguess;
                  step = new_step;
                  step1 = old_step;
                }
              else
                {
                  aa = guess;
                  faa = fguess;
                  bb = b;
                  fbb = fb;
                  step = new_step;
                  step1 = old_step;
                }
            }
          else
            {
              if (fabs (fguess) < fabs (fa))
                {
                  aa = a;
                  faa = fa;
                  bb = guess;
                  fbb = fguess;
                  step = guess - a;
                  step1 = guess - a;
                }
              else
                {
                  aa = guess;
                  faa = fguess;
                  bb = a;
                  fbb = fa;
                  step = guess - a;
                  step1 = guess - a;
                }
            }
          b1 = b;
          fb1 = fb;
          a = aa;
          fa = faa;
          b = bb;
          fb = fbb;

          tolerance = 2.0 * DBL_EPSILON * fabs (b) + 0.5 * toler;
        }
      if (max_iterations_exceeded (max_iterations, *iter_no))
        *err = 2;               // err == 2 means maximum iterations exceeded.
      else
        *root = b;
    }
}
