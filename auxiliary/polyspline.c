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
#include <precomputed_data.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>

//-------------------------------------------------------------------------
//
// Change of basis.
//

#define CHANGE_BASIS(NAME, GET_MATRIX)					\
  void									\
  NAME (unsigned int deg, const double *from, double *to,		\
	size_t num_splines)						\
  {									\
    if (0 < num_splines)						\
      {									\
	size_t result_size = num_splines * (deg + 1) * sizeof (double);	\
	double result[result_size];					\
									\
	gsl_matrix m1 =							\
	  gsl_matrix_const_view_array (from, num_splines,		\
				       deg + 1).matrix;			\
	gsl_matrix m2 =							\
	  gsl_matrix_const_view_array (GET_MATRIX (deg), deg + 1,	\
				       deg + 1).matrix;			\
	gsl_matrix_view v3 =						\
	  gsl_matrix_view_array (result, num_splines, deg + 1);		\
	gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &m1, &m2, 0.0,	\
			&v3.matrix);					\
	memcpy (to, result, result_size);				\
      }									\
  }

// FIXME: For these, maybe use special matrix multiplication for
// triangular matrices.
VISIBLE CHANGE_BASIS (sbern_to_mono_double, get_mono_basis_in_sbern);
VISIBLE CHANGE_BASIS (mono_to_sbern_double, get_sbern_basis_in_mono);

// Doing this with a single transformation matrix seems less stable
// than doing it this way.
VISIBLE void
bern_to_mono_double (unsigned int deg, const double *from, double *to,
		     size_t num_splines)
{
  double sbern[deg + 1];
  bern_to_sbern_double (deg, from, sbern, 1);
  sbern_to_mono_double (deg, sbern, to, 1);
}

// Doing this with a single transformation matrix seems less stable
// than doing it this way.
VISIBLE void
mono_to_bern_double (unsigned int deg, const double *from, double *to,
		     size_t num_splines)
{
  double sbern[deg + 1];
  mono_to_sbern_double (deg, from, sbern, 1);
  sbern_to_bern_double (deg, sbern, to, 1);
}

// The matrix here is diagonal, so use the diagonal directly.
VISIBLE void
sbern_to_bern_double (unsigned int deg, const double *from, double *to,
		      size_t num_splines)
{
  const double *bc = get_binomial_coefficients (deg);
  const unsigned int n = deg + 1;
  for (unsigned int i = 0; i < n * num_splines; i++)
    to[i] = from[i] / bc[i % n];
}

// The matrix here is diagonal, so use the diagonal directly.
VISIBLE void
bern_to_sbern_double (unsigned int deg, const double *from, double *to,
		      size_t num_splines)
{
  const double *bc = get_binomial_coefficients (deg);
  const unsigned int n = deg + 1;
  for (unsigned int i = 0; i < n * num_splines; i++)
    to[i] = from[i] * bc[i % n];
}

//-------------------------------------------------------------------------
//
// Evaluation at a point.
//

VISIBLE double
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
        v = MY_FAST_FMA (v, u, spline[deg - i]);

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
        v = MY_FAST_FMA (v, u, spline[i]);

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

VISIBLE double
eval_bern_double (unsigned int deg, const double *spline, double t)
{
  double sbern[deg + 1];
  bern_to_sbern_double (deg, spline, sbern, 1);
  return eval_sbern_double (deg, sbern, t);
}

VISIBLE double
evaldc_sbern_double (unsigned int deg, const double *spline, double t)
{
  double b[deg + 1];
  sbern_to_bern_double (deg, spline, b, 1);
  for (unsigned int i = 0; i < deg; i++)
    for (unsigned int j = 0; j < deg; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

VISIBLE double
evaldc_bern_double (unsigned int deg, const double *spline, double t)
{
  double b[deg + 1];
  memcpy (b, spline, (deg + 1) * sizeof (double));
  for (unsigned int i = 0; i < deg; i++)
    for (unsigned int j = 0; j < deg; j++)
      b[j] += t * (b[j + 1] - b[j]);
  return b[0];
}

VISIBLE double
eval_mono_double (unsigned int deg, const double *spline, double t)
{
  // Horner’s rule.
  double x = spline[deg];
  for (unsigned int i = 1; i <= deg; i++)
    x = MY_FAST_FMA (x, t, spline[deg - i]);
  return x;
}

//-------------------------------------------------------------------------
//
// Subdivision.
//

VISIBLE void
subdiv_sbern_double (unsigned int deg, const double *spline, double t,
                     double *a, double *b)
{
  double bern[deg + 1];
  sbern_to_bern_double (deg, spline, bern, 1);
  subdiv_bern_double (deg, bern, t, a, b);
  bern_to_sbern_double (deg, a, a, 1);
  bern_to_sbern_double (deg, b, b, 1);
}

VISIBLE void
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

//-------------------------------------------------------------------------
