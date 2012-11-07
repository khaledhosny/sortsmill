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

#include <bernstein.h>
#include <pascals_triangle.h>
#include <string.h>

void
sbern_to_bern (unsigned int deg, const double *sbern, double *bern)
{
  const int *bc = pascals_triangle_row (deg);
  for (unsigned int i = 0; i <= deg; i++)
    bern[i] = sbern[i] / bc[i];
}

void
bern_to_sbern (unsigned int deg, const double *bern, double *sbern)
{
  const int *bc = pascals_triangle_row (deg);
  for (unsigned int i = 0; i <= deg; i++)
    sbern[i] = bern[i] * bc[i];
}

double
eval_sbern_double (unsigned int deg, const double *spline, double t)
{
  double v;

  const double s = 1.0 - t;

  if (t <= 0.5)
    {
      // Horner form in the variable @var{u} = @var{t} / @var{s}.
      double u = t / s;
      v = spline[deg];
      for (unsigned int i = 1; i <= deg; i++)
        v = v * u + spline[deg - i];

      // Multiply by @var{s} raised to the power @var{deg}.
      double power = s;
      unsigned int i = deg;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  else
    {
      // Horner form in the variable @var{u} = @var{s} / @var{t}.
      double u = s / t;
      v = spline[0];
      for (unsigned int i = 1; i <= deg; i++)
        v = v * u + spline[i];

      // Multiply by @var{t} raised to the power @var{deg}.
      double power = t;
      unsigned int i = deg;
      while (i != 0)
        {
          if ((i & 1) != 0)
            v *= power;
          i >>= 1;
          if (i != 0)
            power *= power;
        }
    }
  return v;
}

double
eval_bern_double (unsigned int deg, const double *spline, double t)
{
  double sbern[deg + 1];
  bern_to_sbern (deg, spline, sbern);
  return eval_sbern_double (deg, sbern, t);
}

double
evaldc_sbern_double (unsigned int deg, const double *spline, double t)
{
  double b[deg + 1];
  sbern_to_bern (deg, spline, b);
  for (unsigned int i = 0; i < deg; i++)
    for (unsigned int j = 0; j < deg; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

double
evaldc_bern_double (unsigned int deg, const double *spline, double t)
{
  double b[deg + 1];
  memcpy (b, spline, (deg + 1) * sizeof (double));
  for (unsigned int i = 0; i < deg; i++)
    for (unsigned int j = 0; j < deg; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

void
subdiv_sbern_double (unsigned int deg, const double *spline, double t,
                     double *a, double *b)
{
  double bern[deg + 1];
  sbern_to_bern (deg, spline, bern);
  subdiv_bern_double (deg, bern, t, a, b);
  bern_to_sbern (deg, a, a);
  bern_to_sbern (deg, b, b);
}

void
subdiv_bern_double (unsigned int deg, const double *spline, double t,
                    double *a, double *b)
{
  memmove (b, spline, (deg + 1) * sizeof (double));
  for (unsigned int i = 0; i < deg; i++)
    {
      a[i] = b[0];
      for (unsigned int j = 0; j < deg; j++)
        b[j] += t * (b[j + 1] - b[j]);
    }
  a[deg] = b[0];
}
