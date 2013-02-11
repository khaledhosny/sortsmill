#include <config.h>

// Copyright (C) 2012, 2013 by Barry Schwartz
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

#include <sortsmill/polyspline.h>
#include <precomputed_polyspline_data.h>
#include <sortsmill/xgc.h>
#include <sortsmill/gmp_constants.h>
#include <xalloc.h>
#include <stdio.h>
#include <string.h>

//-------------------------------------------------------------------------

VISIBLE const double *
fl_binomial_coefficients (unsigned int degree)
{
  const double *data = fl_precomputed_binomial_coefficients (degree);
  if (data == NULL)
    {
      const unsigned int degmax = polyspline_precomputed_degree_max ();
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

VISIBLE const double *
fl_binomial_coefficients_altsigns (unsigned int degree)
{
  const double *data = fl_precomputed_binomial_coefficients_altsigns (degree);
  if (data == NULL)
    {
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * sizeof (double));
      memcpy (data1, fl_binomial_coefficients (degree),
              (degree + 1) * sizeof (double));
      for (unsigned int i = 1; i <= degree; i += 2)
        data1[i] = -data1[i];
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

static void
_fill_spower_middle_row (unsigned int deg, double A[deg + 1][deg + 1])
{
  const unsigned int n = deg;
  const unsigned int q = n / 2 + n % 2;

  // A middle row for the extra term in polynomials of even degree.
  if (n % 2 == 0)
    {
      for (unsigned int j = 0; j <= q; j++)
        A[q][j] = 0.0;
      A[q][q] = 1.0;
      for (unsigned int j = q + 1; j <= n; j++)
        A[q][j] = 0.0;
    }
}

static void
_fill_sbern_basis_in_spower (unsigned int deg, double A[deg + 1][deg + 1])
{
  const unsigned int n = deg;
  const unsigned int q = n / 2 + n % 2;

  for (unsigned int i = 0; i < q; i++)
    {
      // Fill in a top row.
      for (unsigned int j = 0; j <= i; j++)
        A[i][j] = 0.0;
      const unsigned int d = n - (2 * i) - 1;
      memcpy (&A[i][i], fl_binomial_coefficients (d),
              (d + 1) * sizeof (double));
      for (unsigned int j = i + d + 1; j <= n; j++)
        A[i][j] = 0.0;

      // The corresponding bottom row is the reverse.
      for (unsigned int j = 0; j <= n; j++)
        A[n - i][n - j] = A[i][j];
    }

  _fill_spower_middle_row (n, A);
}

static void
_fill_spower_basis_in_sbern (unsigned int deg, double A[deg + 1][deg + 1])
{
  const unsigned int n = deg;
  const unsigned int q = n / 2 + n % 2;

  double sg[n + 1];             // Alternating signs.
  for (int i = 0; i <= n; i++)
    sg[i] = 1 - 2 * (i % 2);

  const double *coef[n + 1];    // Pascal’s triangle.
  for (unsigned int i = 0; i <= n; i++)
    coef[i] = fl_binomial_coefficients (i);

  for (unsigned int i = 0; i <= q; i++)
    for (unsigned int j = 0; j <= n; j++)
      A[i][j] = 0.0;

  for (unsigned int i = 0; i < q; i++)
    {
      // Fill in a top row.
      for (unsigned int j = i; j < q; j++)
        A[i][j] = sg[j - i] * coef[n - j - i][j - i];
      if (n % 2 == 0)
        A[i][q] = sg[q - i];
      for (unsigned int j = i + 1; j < q; j++)
        A[i][n - j] = sg[j - i] * coef[n - j - i - 1][j - i - 1];

      // The corresponding bottom row is the reverse.
      for (unsigned int j = 0; j <= n; j++)
        A[n - i][n - j] = A[i][j];
    }

  _fill_spower_middle_row (n, A);
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

VISIBLE const double *
fl_sbern_basis_in_spower (unsigned int degree)
{
  const double *data = fl_precomputed_sbern_basis_in_spower (degree);
  if (data == NULL)
    {
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * (degree + 1) *
                                       sizeof (double));
      _fill_sbern_basis_in_spower (degree, (double (*)[degree + 1]) data1);
      data = (const double *) data1;
    }
  return data;
}

VISIBLE const double *
fl_spower_basis_in_sbern (unsigned int degree)
{
  const double *data = fl_precomputed_spower_basis_in_sbern (degree);
  if (data == NULL)
    {
      double *data1 =
        (double *) x_gc_malloc_atomic ((degree + 1) * (degree + 1) *
                                       sizeof (double));
      _fill_spower_basis_in_sbern (degree, (double (*)[degree + 1]) data1);
      data = (const double *) data1;
    }
  return data;
}

//-------------------------------------------------------------------------

#define _FF_GMP_BINOMIAL_COEFS(TYPE)				\
  VISIBLE __##TYPE##_struct *					\
  TYPE##_binomial_coefficients (unsigned int degree)		\
  {								\
    __##TYPE##_struct *data;					\
    const __##TYPE##_struct *coef =				\
      TYPE##_precomputed_binomial_coefficients (degree);	\
    if (coef != NULL)						\
      {								\
	data = (__##TYPE##_struct *)				\
	  xmalloc ((degree + 1) * sizeof (__##TYPE##_struct));	\
	for (unsigned int i = 0; i <= degree; i++)		\
	  {							\
	    TYPE##_init (&data[i]);				\
	    TYPE##_set (&data[i], &coef[i]);			\
	  }							\
      }								\
    else							\
      {								\
	const unsigned int degmax =				\
	  polyspline_precomputed_degree_max ();			\
								\
	data = (__##TYPE##_struct *)				\
	  xmalloc ((degree + 1) * sizeof (__##TYPE##_struct));	\
	for (unsigned int i = 0; i <= degree; i++)		\
	  TYPE##_init (&data[i]);				\
								\
	const __##TYPE##_struct *coef =				\
	  TYPE##_precomputed_binomial_coefficients (degmax);	\
	for (unsigned int i = 0; i <= degmax; i++)		\
	  TYPE##_set (&data[i], &coef[i]);			\
								\
	for (unsigned int i = degmax + 1; i <= degree; i++)	\
	  {							\
	    TYPE##_set (&data[i], TYPE##_one ());		\
	    for (unsigned int j = i - 1; 0 < j; j--)		\
	      TYPE##_add (&data[j], &data[j], &data[j - 1]);	\
	  }							\
      }								\
    return data;						\
  }

_FF_GMP_BINOMIAL_COEFS (mpz);
_FF_GMP_BINOMIAL_COEFS (mpq);

#define _FF_GMP_BINOMIAL_COEFS_ALTSIGNS(TYPE)				\
  VISIBLE __##TYPE##_struct *						\
  TYPE##_binomial_coefficients_altsigns (unsigned int degree)		\
  {									\
    __##TYPE##_struct *data;						\
    const __##TYPE##_struct *coef =					\
      TYPE##_precomputed_binomial_coefficients_altsigns (degree);	\
    if (coef != NULL)							\
      {									\
	data = (__##TYPE##_struct *)					\
	  xmalloc ((degree + 1) * sizeof (__##TYPE##_struct));		\
	for (unsigned int i = 0; i <= degree; i++)			\
	  {								\
	    TYPE##_init (&data[i]);					\
	    TYPE##_set (&data[i], &coef[i]);				\
	  }								\
      }									\
    else								\
      {									\
	data = TYPE##_binomial_coefficients (degree);			\
	for (unsigned int i = 1; i <= degree; i += 2)			\
	  TYPE##_neg (&data[i], &data[i]);				\
      }									\
    return data;							\
  }

_FF_GMP_BINOMIAL_COEFS_ALTSIGNS (mpz);
_FF_GMP_BINOMIAL_COEFS_ALTSIGNS (mpq);

#define _FF_FREE_GMP_BINOMIAL_COEFS(TYPE)			\
  VISIBLE void							\
  free_##TYPE##_binomial_coefficients (unsigned int degree,	\
				       __##TYPE##_struct *coef)	\
  {								\
    for (unsigned int i = 0; i <= degree; i++)			\
      TYPE##_clear (&coef[i]);					\
    free (coef);						\
  }

_FF_FREE_GMP_BINOMIAL_COEFS (mpz);
_FF_FREE_GMP_BINOMIAL_COEFS (mpq);

//-------------------------------------------------------------------------

#define _FF_GMP_FILL_SBERN_BASIS_IN_MONO(TYPE)				\
  static void								\
  _##TYPE##_fill_sbern_basis_in_mono (unsigned int deg,			\
				      TYPE##_t A[deg + 1][deg + 1])	\
  {									\
    for (unsigned int i = 0; i <= deg; i++)				\
      {									\
	for (unsigned int j = 0; j < i; j++)				\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], TYPE##_zero ());			\
	  }								\
	__##TYPE##_struct *coef =					\
	  TYPE##_binomial_coefficients (deg - i);			\
	for (unsigned int j = i; j <= deg; j++)				\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], &coef[j - i]);				\
	  }								\
        free_##TYPE##_binomial_coefficients (deg - i, coef);            \
      }									\
  }

#define _FF_GMP_FILL_MONO_BASIS_IN_SBERN(TYPE)				\
  static void								\
  _##TYPE##_fill_mono_basis_in_sbern (unsigned int deg,			\
				      TYPE##_t A[deg + 1][deg + 1])	\
  {									\
    for (unsigned int i = 0; i <= deg; i++)				\
      {									\
	for (unsigned int j = 0; j < i; j++)				\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], TYPE##_zero ());			\
	  }								\
	__##TYPE##_struct *coef =					\
	  TYPE##_binomial_coefficients (deg - i);			\
	for (unsigned int j = i; j <= deg; j += 2)			\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], &coef[j - i]);				\
	  }								\
	for (unsigned int j = i + 1; j <= deg; j += 2)			\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_neg (A[i][j], &coef[j - i]);				\
	  }								\
        free_##TYPE##_binomial_coefficients (deg - i, coef);            \
      }									\
  }

#define _FF_GMP_FILL_SPOWER_MIDDLE_ROW(TYPE, N, Q, A)           \
  /* A middle row for the extra term in polynomials of  */      \
  /* even degree.                                       */      \
  if ((N) % 2 == 0)                                             \
    {                                                           \
      for (unsigned int j = 0; j <= (Q); j++)                   \
        {                                                       \
          TYPE##_init (A[Q][j]);                                \
          TYPE##_set (A[Q][j], TYPE##_zero ());			\
        }                                                       \
      TYPE##_init (A[Q][Q]);                                    \
      TYPE##_set (A[Q][Q], TYPE##_one ());                      \
      for (unsigned int j = (Q) + 1; j <= (N); j++)             \
        {                                                       \
          TYPE##_init (A[Q][j]);                                \
          TYPE##_set (A[Q][j], TYPE##_zero ());			\
        }                                                       \
    }                                                           \

#define _FF_GMP_FILL_SBERN_BASIS_IN_SPOWER(TYPE)			\
  static void								\
  _##TYPE##_fill_sbern_basis_in_spower (unsigned int deg,		\
					TYPE##_t A[deg + 1][deg + 1])	\
  {									\
    const unsigned int n = deg;						\
    const unsigned int q = n / 2 + n % 2;				\
									\
    for (unsigned int i = 0; i < q; i++)				\
      {									\
	/* Fill in a top row. */					\
	for (unsigned int j = 0; j <= i; j++)				\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], TYPE##_zero ());			\
	  }								\
	const unsigned int d = n - (2 * i) - 1;				\
	__##TYPE##_struct *coef = TYPE##_binomial_coefficients (d);	\
	for (unsigned int j = 0; j <= d; j++)				\
	  {								\
	    TYPE##_init (A[i][i + j]);					\
	    TYPE##_set (A[i][i + j], &coef[j]);				\
	  }								\
	free_##TYPE##_binomial_coefficients (d, coef);			\
	for (unsigned int j = i + d + 1; j <= n; j++)			\
	  {								\
	    TYPE##_init (A[i][j]);					\
	    TYPE##_set (A[i][j], TYPE##_zero ());			\
	  }								\
									\
	/* The corresponding bottom row is the reverse. */		\
	for (unsigned int j = 0; j <= n; j++)				\
          {                                                             \
            TYPE##_init (A[n - i][n - j]);                              \
            TYPE ##_set (A[n - i][n - j], A[i][j]);                     \
          }                                                             \
      }									\
									\
    _FF_GMP_FILL_SPOWER_MIDDLE_ROW(TYPE, n, q, A);                      \
  }

#define _FF_GMP_FILL_SPOWER_BASIS_IN_SBERN(TYPE)			\
  static void                                                           \
  _##TYPE##_fill_spower_basis_in_sbern (unsigned int deg,               \
                                        TYPE##_t A[deg + 1][deg + 1])   \
  {                                                                     \
    const unsigned int n = deg;                                         \
    const unsigned int q = n / 2 + n % 2;                               \
                                                                        \
    TYPE##_t sg[n + 1];              /* Alternating signs. */           \
    for (unsigned int i = 0; i <= n; i++)                               \
      {                                                                 \
        TYPE##_init (sg[i]);                                            \
        TYPE##_set (sg[i], (((i % 2) == 0) ?                            \
                            TYPE##_one () : TYPE##_neg_one ()));        \
      }                                                                 \
                                                                        \
    __##TYPE##_struct *coef[n + 1];  /* Pascal’s triangle. */           \
    for (unsigned int i = 0; i <= n; i++)                               \
      coef[i] = TYPE##_binomial_coefficients (i);                       \
                                                                        \
    for (unsigned int i = 0; i <= q; i++)                               \
      for (unsigned int j = 0; j <= n; j++)                             \
        {                                                               \
          TYPE##_init (A[i][j]);                                        \
          TYPE##_set (A[i][j], TYPE##_zero ());                         \
        }                                                               \
                                                                        \
    for (unsigned int i = 0; i < q; i++)                                \
      {                                                                 \
        /* Fill in a top row. */                                        \
        for (unsigned int j = i; j < q; j++)                            \
          TYPE##_mul (A[i][j], sg[j - i], &coef[n - j - i][j - i]);     \
        if (n % 2 == 0)                                                 \
          TYPE##_set (A[i][q], sg[q - i]);                              \
        for (unsigned int j = i + 1; j < q; j++)                        \
          TYPE##_mul (A[i][n - j], sg[j - i],                           \
                      &coef[n - j - i - 1][j - i - 1]);                 \
                                                                        \
        /* The corresponding bottom row is the reverse. */              \
        for (unsigned int j = 0; j <= n; j++)                           \
          {                                                             \
            TYPE##_init (A[n - i][n - j]);                              \
            TYPE##_set (A[n - i][n - j], A[i][j]);                      \
          }                                                             \
      }                                                                 \
                                                                        \
    _FF_GMP_FILL_SPOWER_MIDDLE_ROW(TYPE, n, q, A);                      \
                                                                        \
    for (unsigned int i = 0; i <= n; i++)                               \
      free_##TYPE##_binomial_coefficients (i, coef[i]);                 \
  }

_FF_GMP_FILL_SBERN_BASIS_IN_MONO (mpz);
_FF_GMP_FILL_MONO_BASIS_IN_SBERN (mpz);
_FF_GMP_FILL_SBERN_BASIS_IN_SPOWER (mpz);
_FF_GMP_FILL_SPOWER_BASIS_IN_SBERN (mpz);

_FF_GMP_FILL_SBERN_BASIS_IN_MONO (mpq);
_FF_GMP_FILL_MONO_BASIS_IN_SBERN (mpq);
_FF_GMP_FILL_SBERN_BASIS_IN_SPOWER (mpq);
_FF_GMP_FILL_SPOWER_BASIS_IN_SBERN (mpq);

#define _FF_GMP_SBERN_BASIS_IN_MONO(TYPE)			\
  VISIBLE __##TYPE##_struct *					\
  TYPE##_sbern_basis_in_mono (unsigned int degree)		\
  {								\
    const unsigned int size = (degree + 1) * (degree + 1);	\
    __##TYPE##_struct *data =					\
      (__##TYPE##_struct *) xmalloc (size * sizeof (TYPE##_t));	\
    const __##TYPE##_struct *precomp_data =			\
      TYPE##_precomputed_sbern_basis_in_mono (degree);		\
    if (precomp_data != NULL)					\
      for (unsigned int i = 0; i < size; i++)			\
	{							\
	  TYPE##_init (&data[i]);				\
	  TYPE##_set (&data[i], &precomp_data[i]);		\
	}							\
    else							\
      _##TYPE##_fill_sbern_basis_in_mono			\
	(degree, (__##TYPE##_struct (*)[degree + 1][1]) data);	\
    return data;						\
  }

#define _FF_GMP_MONO_BASIS_IN_SBERN(TYPE)			\
  VISIBLE __##TYPE##_struct *					\
  TYPE##_mono_basis_in_sbern (unsigned int degree)		\
  {								\
    const unsigned int size = (degree + 1) * (degree + 1);	\
    __##TYPE##_struct *data =					\
      (__##TYPE##_struct *) xmalloc (size * sizeof (TYPE##_t));	\
    const __##TYPE##_struct *precomp_data =			\
      TYPE##_precomputed_mono_basis_in_sbern (degree);		\
    if (precomp_data != NULL)					\
      for (unsigned int i = 0; i < size; i++)			\
	{							\
	  TYPE##_init (&data[i]);				\
	  TYPE##_set (&data[i], &precomp_data[i]);		\
	}							\
    else							\
      _##TYPE##_fill_mono_basis_in_sbern			\
	(degree, (__##TYPE##_struct (*)[degree + 1][1]) data);	\
    return data;						\
  }

#define _FF_GMP_SBERN_BASIS_IN_SPOWER(TYPE)			\
  VISIBLE __##TYPE##_struct *					\
  TYPE##_sbern_basis_in_spower (unsigned int degree)		\
  {								\
    const unsigned int size = (degree + 1) * (degree + 1);	\
    __##TYPE##_struct *data =					\
      (__##TYPE##_struct *) xmalloc (size * sizeof (TYPE##_t));	\
    const __##TYPE##_struct *precomp_data =			\
      TYPE##_precomputed_sbern_basis_in_spower (degree);        \
    if (precomp_data != NULL)					\
      for (unsigned int i = 0; i < size; i++)			\
	{							\
	  TYPE##_init (&data[i]);				\
	  TYPE##_set (&data[i], &precomp_data[i]);		\
	}							\
    else							\
      _##TYPE##_fill_sbern_basis_in_spower			\
	(degree, (__##TYPE##_struct (*)[degree + 1][1]) data);	\
    return data;						\
  }

#define _FF_GMP_SPOWER_BASIS_IN_SBERN(TYPE)			\
  VISIBLE __##TYPE##_struct *					\
  TYPE##_spower_basis_in_sbern (unsigned int degree)		\
  {								\
    const unsigned int size = (degree + 1) * (degree + 1);	\
    __##TYPE##_struct *data =					\
      (__##TYPE##_struct *) xmalloc (size * sizeof (TYPE##_t));	\
    const __##TYPE##_struct *precomp_data =			\
      TYPE##_precomputed_spower_basis_in_sbern (degree);        \
    if (precomp_data != NULL)					\
      for (unsigned int i = 0; i < size; i++)			\
	{							\
	  TYPE##_init (&data[i]);				\
	  TYPE##_set (&data[i], &precomp_data[i]);		\
	}							\
    else							\
      _##TYPE##_fill_spower_basis_in_sbern			\
	(degree, (__##TYPE##_struct (*)[degree + 1][1]) data);	\
    return data;						\
  }

_FF_GMP_SBERN_BASIS_IN_MONO (mpz);
_FF_GMP_MONO_BASIS_IN_SBERN (mpz);
_FF_GMP_SBERN_BASIS_IN_SPOWER (mpz);
_FF_GMP_SPOWER_BASIS_IN_SBERN (mpz);

_FF_GMP_SBERN_BASIS_IN_MONO (mpq);
_FF_GMP_MONO_BASIS_IN_SBERN (mpq);
_FF_GMP_SBERN_BASIS_IN_SPOWER (mpq);
_FF_GMP_SPOWER_BASIS_IN_SBERN (mpq);

#define _FREE_FF_GMP_TRANSFORMATION_MATRIX(TYPE)			\
  VISIBLE void								\
  free_##TYPE##_transformation_matrix (unsigned int degree,		\
				       __##TYPE##_struct *data)		\
  {									\
    for (unsigned int i = 0; i < (degree + 1) * (degree + 1); i++)	\
      TYPE##_clear (&data[i]);						\
    free (data);							\
  }

_FREE_FF_GMP_TRANSFORMATION_MATRIX (mpz);
_FREE_FF_GMP_TRANSFORMATION_MATRIX (mpq);

//-------------------------------------------------------------------------
