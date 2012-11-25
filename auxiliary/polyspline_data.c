#include <config.h>

/*
 * Copyright (C) 2012 by Barry Schwartz
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sortsmillff/polyspline.h>
#include <precomputed_polyspline_data.h>
#include <sortsmillff/xgc.h>
#include <stdio.h>
#include <string.h>

VISIBLE const double *
fl_binomial_coefficients (unsigned int degree)
{
  const double *data = fl_precomputed_binomial_coefficients (degree);
  if (data == NULL)
    {
      unsigned int degmax = polyspline_precomputed_degree_max ();
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * sizeof (double));
      memcpy (data1, fl_precomputed_binomial_coefficients (degmax),
              (degmax + 1) * sizeof (double));
      for (unsigned int i = degmax + 1; i <= degree; i++)
        {
          data1[i] = 1.0;
          for (unsigned int j = i - 1; 0 < j; j--)
            data1[j] += data1[j - 1];
        }
      data = (const double *) data1;
    }
  return data;
}

static void
_fill_sbern_basis_in_mono (unsigned int deg, double A[deg + 1][deg + 1])
{
  for (unsigned int i = 0; i <= deg; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        A[i][j] = 0.0;
      memcpy (&A[i][i], fl_binomial_coefficients (deg - i),
              (deg - i + 1) * sizeof (double));
    }
}

static void
_fill_mono_basis_in_sbern (unsigned int deg, double A[deg + 1][deg + 1])
{
  for (unsigned int i = 0; i <= deg; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        A[i][j] = 0.0;
      memcpy (&A[i][i], fl_binomial_coefficients (deg - i),
              (deg - i + 1) * sizeof (double));
      for (unsigned int j = i + 1; j <= deg; j += 2)
        A[i][j] = -A[i][j];
    }
}

VISIBLE const double *
fl_sbern_basis_in_mono (unsigned int degree)
{
  const double *data = fl_precomputed_sbern_basis_in_mono (degree);
  if (data == NULL)
    {
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * (degree + 1) *
                                       sizeof (double));
      _fill_sbern_basis_in_mono (degree, (double (*)[degree + 1]) data1);
      data = (const double *) data1;
    }
  return data;
}

VISIBLE const double *
fl_mono_basis_in_sbern (unsigned int degree)
{
  const double *data = fl_precomputed_mono_basis_in_sbern (degree);
  if (data == NULL)
    {
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * (degree + 1) *
                                       sizeof (double));
      _fill_mono_basis_in_sbern (degree, (double (*)[degree + 1]) data1);
      data = (const double *) data1;
    }
  return data;
}
