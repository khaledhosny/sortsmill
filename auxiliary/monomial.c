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
#include <precomputed_data.h>
#include <string.h>
#include <math.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_blas.h>

VISIBLE void
sbern_to_mono_double (unsigned int deg, const double *sbern, double *mono)
{
  int num_splines = 1;

  if (0 < num_splines)
    {
      size_t result_size = num_splines * (deg + 1) * sizeof (double);
      double result[result_size];

      gsl_matrix m1 =
        gsl_matrix_const_view_array (sbern, num_splines, deg + 1).matrix;
      gsl_matrix m2 =
        gsl_matrix_const_view_array (get_mono_basis_in_sbern (deg), deg + 1,
                                     deg + 1).matrix;
      gsl_matrix_view v3 = gsl_matrix_view_array (result, num_splines, deg + 1);
      gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &m1, &m2, 0.0,
                      &v3.matrix);
      memcpy (mono, result, result_size);
    }
}

VISIBLE void
mono_to_sbern_double (unsigned int deg, const double *mono, double *sbern)
{
  int num_splines = 1;

  if (0 < num_splines)
    {
      size_t result_size = num_splines * (deg + 1) * sizeof (double);
      double result[result_size];

      gsl_matrix m1 =
        gsl_matrix_const_view_array (mono, num_splines, deg + 1).matrix;
      gsl_matrix m2 =
        gsl_matrix_const_view_array (get_sbern_basis_in_mono (deg), deg + 1,
                                     deg + 1).matrix;
      gsl_matrix_view v3 = gsl_matrix_view_array (result, num_splines, deg + 1);
      gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &m1, &m2, 0.0,
                      &v3.matrix);
      memcpy (sbern, result, result_size);
    }
}

// FIXME: Do directly.
VISIBLE void
bern_to_mono_double (unsigned int deg, const double *bern, double *mono)
{
  double sbern[deg + 1];
  bern_to_sbern_double (deg, bern, sbern);
  sbern_to_mono_double (deg, sbern, mono);
}

// FIXME: Do directly.
VISIBLE void
mono_to_bern_double (unsigned int deg, const double *mono, double *bern)
{
  double sbern[deg + 1];
  mono_to_sbern_double (deg, mono, sbern);
  sbern_to_bern_double (deg, sbern, bern);
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
