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

#include <polyspline.h>
#include <pascals_triangle.h>
#include <string.h>
#include <math.h>

#ifndef MY_FAST_FMA
#ifdef FP_FAST_FMA
#define MY_FAST_FMA fma
#else
#define MY_FAST_FMA(x, y, z) ((x) * (y) + (z))
#endif
#endif

void
sbern_to_mono_double (unsigned int deg, const double *sbern, double *mono)
{
  double result[deg + 1];
  for (unsigned int j = 0; j <= deg; j++)
    result[j] = 0.0;
  for (unsigned int j = 0; j <= deg; j++)
    {
      const int *p = pascals_triangle_row_altsigns (deg - j);
      for (unsigned int i = j; i <= deg; i++)
	result[i] += sbern[j] * p[i - j];
    }
  memcpy (mono, result, (deg + 1) * sizeof (double));
}

void
mono_to_sbern_double (unsigned int deg, const double *mono, double *sbern)
{
  double result[deg + 1];
  for (unsigned int j = 0; j <= deg; j++)
    result[j] = 0.0;
  for (unsigned int j = 0; j <= deg; j++)
    {
      const int *p = pascals_triangle_row (deg - j);
      for (unsigned int i = j; i <= deg; i++)
	result[i] += mono[j] * p[i - j];
    }
  memcpy (sbern, result, (deg + 1) * sizeof (double));
}

void
bern_to_mono_double (unsigned int deg, const double *bern, double *mono)
{
  double sbern[deg + 1];
  bern_to_sbern_double (deg, bern, sbern);
  sbern_to_mono_double (deg, sbern, mono);
}

void
mono_to_bern_double (unsigned int deg, const double *mono, double *bern)
{
  double sbern[deg + 1];
  mono_to_sbern_double (deg, mono, sbern);
  sbern_to_bern_double (deg, sbern, bern);
}

double
eval_mono_double (unsigned int deg, const double *spline, double t)
{
  // Horner’s rule.
  double x = spline[deg];
  for (unsigned int i = 1; i <= deg; i++)
    x = MY_FAST_FMA (x, t, spline[deg - i]);
  return x;
}
