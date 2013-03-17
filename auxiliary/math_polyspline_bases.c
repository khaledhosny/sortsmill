#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 by Barry Schwartz
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

#include <sortsmill/math.h>
#include <sortsmill/guile.h>
#include <sortsmill/initialized_global_constants.h>
#include <sortsmill/copy_with_strides.h>
#include <xalloc.h>
#include <assert.h>

//-------------------------------------------------------------------------

VISIBLE void
unsplit_f64_spower (size_t degree,
                    ssize_t stride0, const double *a0,
                    ssize_t stride1, const double *a1,
                    ssize_t result_stride, double *result)
{
  if (degree % 2 == 1)
    {
      copy_f64_with_strides (result_stride, result, stride0, a0,
                             degree / 2 + 1);
      copy_f64_with_strides (-result_stride,
                             &result[result_stride * (ssize_t) degree],
                             stride1, a1, degree / 2 + 1);
    }
  else
    {
      copy_f64_with_strides (result_stride, result, stride0, a0, degree / 2);
      result[result_stride * (ssize_t) (degree / 2)] =
        0.5 * (a0[degree / 2] + a1[degree / 2]);
      copy_f64_with_strides (-result_stride,
                             &result[result_stride * (ssize_t) degree],
                             stride1, a1, degree / 2);
    }
}

VISIBLE void
unsplit_scm_spower (size_t degree,
                    ssize_t stride0, const SCM *a0,
                    ssize_t stride1, const SCM *a1,
                    ssize_t result_stride, SCM *result)
{
  if (degree % 2 == 1)
    {
      copy_scm_with_strides (result_stride, result, stride0, a0,
                             degree / 2 + 1);
      copy_scm_with_strides (-result_stride,
                             &result[result_stride * (ssize_t) degree],
                             stride1, a1, degree / 2 + 1);
    }
  else
    {
      copy_scm_with_strides (result_stride, result, stride0, a0, degree / 2);
      result[result_stride * (ssize_t) (degree / 2)] =
        scm_divide (scm_sum (a0[degree / 2], a1[degree / 2]),
                    scm_from_uint (2));
      copy_scm_with_strides (-result_stride,
                             &result[result_stride * (ssize_t) degree],
                             stride1, a1, degree / 2);
    }
}

//-------------------------------------------------------------------------

VISIBLE void
constant_f64_mono (double w, size_t degree, ssize_t stride, double *constant)
{
  //    (w)
  //    (w 0)
  //    (w 0 0)
  //    (w 0 0 0)
  //    (w 0 0 0 0)
  //         ⋮
  //
  constant[0] = w;
  for (size_t i = 1; i <= degree; i++)
    constant[stride * (ssize_t) i] = 0.0;
}

VISIBLE void
constant_scm_mono (SCM w, size_t degree, ssize_t stride, SCM *constant)
{
  //    (w)
  //    (w 0)
  //    (w 0 0)
  //    (w 0 0 0)
  //    (w 0 0 0 0)
  //         ⋮
  //
  constant[0] = w;
  const SCM number_zero = scm_from_uint (0);
  for (size_t i = 1; i <= degree; i++)
    constant[stride * (ssize_t) i] = number_zero;
}

VISIBLE void
constant_f64_bern (double w, size_t degree, ssize_t stride, double *constant)
{
  //    (w)
  //    (w w)
  //    (w w w)
  //    (w w w w)
  //    (w w w w w)
  //         ⋮
  //
  for (size_t i = 0; i <= degree; i++)
    constant[stride * (ssize_t) i] = w;
}

VISIBLE void
constant_scm_bern (SCM w, size_t degree, ssize_t stride, SCM *constant)
{
  //    (w)
  //    (w w)
  //    (w w w)
  //    (w w w w)
  //    (w w w w w)
  //         ⋮
  //
  for (size_t i = 0; i <= degree; i++)
    constant[stride * (ssize_t) i] = w;
}

VISIBLE void
constant_f64_sbern (double w, size_t degree, ssize_t stride, double *constant)
{
  // The coefficients of a(t) = 1 are the successive rows of Pascal’s
  // triangle:
  //
  //    (w)
  //    (w w)
  //    (w 2w w)
  //    (w 3w 3w w)
  //    (w 4w 6w 4w w)
  //    (w 5w 10w 10w 5w w)
  //    (w 6w 15w 20w 15w 6w w)
  //         ⋮
  //
  for (size_t i = 0; i <= degree; i++)
    constant[stride * (ssize_t) i] = bincoef (degree, i) * w;
}

VISIBLE void
constant_scm_sbern (SCM w, size_t degree, ssize_t stride, SCM *constant)
{
  // The coefficients of a(t) = 1 are the successive rows of Pascal’s
  // triangle:
  //
  //    (w)
  //    (w w)
  //    (w 2w w)
  //    (w 3w 3w w)
  //    (w 4w 6w 4w w)
  //    (w 5w 10w 10w 5w w)
  //    (w 6w 15w 20w 15w 6w w)
  //         ⋮
  //
  mpz_t C;
  mpz_init (C);
  for (size_t i = 0; i <= degree; i++)
    {
      mpz_bincoef_ui (C, degree, i);
      constant[stride * (ssize_t) i] = scm_product (scm_from_mpz (C), w);
    }
  mpz_clear (C);
}

VISIBLE void
constant_f64_spower (double w, size_t degree, ssize_t stride, double *constant)
{
  //    (w)
  //    (w w)
  //    (w 0 w)
  //    (w 0 0 w)
  //    (w 0 0 0 w)
  //         ⋮
  //
  constant[0] = w;
  for (size_t i = 1; i < degree; i++)
    constant[stride * (ssize_t) i] = 0.0;
  constant[stride * (ssize_t) degree] = w;
}

VISIBLE void
constant_scm_spower (SCM w, size_t degree, ssize_t stride, SCM *constant)
{
  //    (w)
  //    (w w)
  //    (w 0 w)
  //    (w 0 0 w)
  //    (w 0 0 0 w)
  //         ⋮
  //
  const SCM number_zero = scm_from_uint (0);
  constant[0] = w;
  for (size_t i = 1; i < degree; i++)
    constant[stride * (ssize_t) i] = number_zero;
  constant[stride * (ssize_t) degree] = w;
}

//-------------------------------------------------------------------------

VISIBLE void
one_f64_mono (size_t degree, ssize_t stride, double *one)
{
  //    (1)
  //    (1 0)
  //    (1 0 0)
  //    (1 0 0 0)
  //    (1 0 0 0 0)
  //         ⋮
  //
  constant_f64_mono (1.0, degree, stride, one);
}

VISIBLE void
one_scm_mono (size_t degree, ssize_t stride, SCM *one)
{
  //    (1)
  //    (1 0)
  //    (1 0 0)
  //    (1 0 0 0)
  //    (1 0 0 0 0)
  //         ⋮
  //
  constant_scm_mono (scm_from_int (1), degree, stride, one);
}

VISIBLE void
one_f64_bern (size_t degree, ssize_t stride, double *one)
{
  //    (1)
  //    (1 1)
  //    (1 1 1)
  //    (1 1 1 1)
  //    (1 1 1 1 1)
  //         ⋮
  //
  constant_f64_bern (1.0, degree, stride, one);
}

VISIBLE void
one_scm_bern (size_t degree, ssize_t stride, SCM *one)
{
  //    (1)
  //    (1 1)
  //    (1 1 1)
  //    (1 1 1 1)
  //    (1 1 1 1 1)
  //         ⋮
  //
  constant_scm_bern (scm_from_int (1), degree, stride, one);
}

VISIBLE void
one_f64_sbern (size_t degree, ssize_t stride, double *one)
{
  // The coefficients of a(t) = 1 are the successive rows of Pascal’s
  // triangle:
  //
  //    (1)
  //    (1 1)
  //    (1 2 1)
  //    (1 3 3 1)
  //    (1 4 6 4 1)
  //    (1 5 10 10 5 1)
  //    (1 6 15 20 15 6 1)
  //         ⋮
  //
  constant_f64_sbern (1.0, degree, stride, one);
}

VISIBLE void
one_scm_sbern (size_t degree, ssize_t stride, SCM *one)
{
  // The coefficients of a(t) = 1 are the successive rows of Pascal’s
  // triangle:
  //
  //    (1)
  //    (1 1)
  //    (1 2 1)
  //    (1 3 3 1)
  //    (1 4 6 4 1)
  //    (1 5 10 10 5 1)
  //    (1 6 15 20 15 6 1)
  //         ⋮
  //
  constant_scm_sbern (scm_from_int (1), degree, stride, one);
}

VISIBLE void
one_f64_spower (size_t degree, ssize_t stride, double *one)
{
  //    (1)
  //    (1 1)
  //    (1 0 1)
  //    (1 0 0 1)
  //    (1 0 0 0 1)
  //         ⋮
  //
  constant_f64_spower (1.0, degree, stride, one);
}

VISIBLE void
one_scm_spower (size_t degree, ssize_t stride, SCM *one)
{
  //    (1)
  //    (1 1)
  //    (1 0 1)
  //    (1 0 0 1)
  //    (1 0 0 0 1)
  //         ⋮
  //
  constant_scm_spower (scm_from_int (1), degree, stride, one);
}

//-------------------------------------------------------------------------

// This routine is used below in a couple of places.
static void
fill_spower_middle_row (size_t n, mpq_t T[n + 1][n + 1])
{
  const size_t q = n / 2 + n % 2;

  // A middle row for the extra term in polynomials of even degree.
  if (n % 2 == 0)
    {
      for (size_t j = 0; j <= q; j++)
        mpq_set (T[q][j], mpq_zero ());
      mpq_set (T[q][q], mpq_one ());
      for (size_t j = q + 1; j <= n; j++)
        mpq_set (T[q][j], mpq_zero ());
    }
}

//-------------------------------------------------------------------------

// Multiply a row vector by this matrix to convert coefficients from
// monomial basis to Bernstein basis.
//
// The matrix is upper triangular.
//
// The matrix’s rows are the Bernstein coefficients of the monomial
// basis polynomials. For example:
//
//    1  = 1⋅(1 − t)³ + 1⋅3t(1 − t)² + 1⋅3t²(1 - t) + 1⋅t³
//    t  =             ⅓⋅3t(1 − t)² + ⅔⋅3t²(1 - t) + 1⋅t³
//    t² =                            ⅓⋅3t²(1 - t) + 1⋅t³
//    t³ =                                           1⋅t³
//
VISIBLE void
mpq_coefficients_mono_to_bern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_t divisor;
  mpq_init (divisor);

  for (size_t i = 0; i <= degree; i++)
    {
      for (size_t j = 0; j < i; j++)
        mpq_set (T[i][j], mpq_zero ());
      for (size_t j = i; j <= degree; j++)
        mpq_bincoef_ui (T[i][j], degree - i, j - i);
    }

  for (size_t j = 1; j < degree; j++)
    {
      mpq_bincoef_ui (divisor, degree, j);
      for (size_t i = 0; i <= degree; i++)
        mpq_div (T[i][j], T[i][j], divisor);
    }

  mpq_clear (divisor);
}

// Multiply a row vector by this matrix to convert coefficients from
// monomial basis to scaled Bernstein basis.
//
// The matrix is upper triangular.
//
// The matrix’s rows are the scaled Bernstein coefficients of the
// monomial basis polynomials. For example:
//
//    1  = (1 − t)³ + 3t(1 − t)² + 3t²(1 - t) + t³
//    t  =             t(1 − t)² + 2t²(1 - t) + t³
//    t² =                          t²(1 - t) + t³
//    t³ =                                      t³
//
VISIBLE void
mpq_coefficients_mono_to_sbern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  for (size_t i = 0; i <= degree; i++)
    {
      for (size_t j = 0; j < i; j++)
        mpq_set (T[i][j], mpq_zero ());
      for (size_t j = i; j <= degree; j++)
        mpq_bincoef_ui (T[i][j], degree - i, j - i);
    }
}

// Multiply a row vector by this matrix to convert coefficients from
// Bernstein basis to monomial basis.
//
// The matrix is upper triangular.
//
// The matrix’s rows are the monomial coefficients of the Bernstein
// basis polynomials. For example:
//
//    (1 − t)³   = 1 − 3t + 3t² −  t³
//    3t(1 − t)² =     3t − 6t² + 3t³
//    3t²(1 - t) =          3t² − 3t³
//    t³         =                 t³
//
VISIBLE void
mpq_coefficients_bern_to_mono (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_t factor;
  mpq_init (factor);

  for (size_t i = 0; i <= degree; i++)
    {
      for (size_t j = 0; j < i; j++)
        mpq_set (T[i][j], mpq_zero ());
      for (size_t j = i; j <= degree; j++)
        mpq_bincoef_ui (T[i][j], degree - i, j - i);
      for (size_t j = i + 1; j <= degree; j += 2)
        mpq_neg (T[i][j], T[i][j]);
    }

  for (size_t i = 1; i < degree; i++)
    {
      mpq_bincoef_ui (factor, degree, i);
      for (size_t j = 0; j <= degree; j++)
        mpq_mul (T[i][j], T[i][j], factor);
    }

  mpq_clear (factor);
}

// Multiply a row vector by this matrix to convert coefficients from
// Bernstein basis to scaled Bernstein basis.
//
// The matrix is diagonal and positive-definite.
//
// The matrix’s rows are the scaled Bernstein coefficients of the
// Bernstein basis polynomials. For example:
//
//    (1 − t)³   = 1⋅(1 − t)³
//    3t(1 − t)² = 3⋅t(1 − t)²
//    3t²(1 - t) = 3⋅t²(1 - t)
//    t³         = 1⋅t³
//
VISIBLE void
mpq_coefficients_bern_to_sbern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  for (size_t i = 0; i <= degree; i++)
    for (size_t j = 0; j <= degree; j++)
      if (i == j)
        mpq_bincoef_ui (T[i][j], degree, i);
      else
        mpq_set (T[i][j], mpq_zero ());
}

// Multiply a row vector by this matrix to convert coefficients from
// scaled Bernstein basis to monomial basis.
//
// The matrix is upper triangular.
//
// The matrix’s rows are the monomial coefficients of the scaled
// Bernstein basis polynomials. For example:
//
//    (1 − t)³  = 1 − 3t + 3t² − t³
//    t(1 − t)² =      t − 2t² + t³
//    t²(1 - t) =           t² - t³
//    t³        =                t³
//
VISIBLE void
mpq_coefficients_sbern_to_mono (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  for (size_t i = 0; i <= degree; i++)
    {
      for (size_t j = 0; j < i; j++)
        mpq_set (T[i][j], mpq_zero ());
      for (size_t j = i; j <= degree; j++)
        mpq_bincoef_ui (T[i][j], degree - i, j - i);
      for (size_t j = i + 1; j <= degree; j += 2)
        mpq_neg (T[i][j], T[i][j]);
    }
}

// Multiply a row vector by this matrix to convert coefficients from
// scaled Bernstein basis to Bernstein basis.
//
// The matrix is diagonal and positive-definite.
//
// The matrix’s rows are the Bernstein coefficients of the scaled
// Bernstein basis polynomials. For example:
//
//    (1 − t)³  = 1⋅(1 − t)³
//    t(1 − t)² = ⅓⋅t(1 − t)²
//    t²(1 - t) = ⅓⋅t²(1 - t)
//    t³        = 1⋅t³
//
VISIBLE void
mpq_coefficients_sbern_to_bern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  for (size_t i = 0; i <= degree; i++)
    for (size_t j = 0; j <= degree; j++)
      if (i == j)
        {
          // The following produces canonical form, making a call to
          // mpq_canonicalize() unnecessary.
          mpz_set (mpq_numref (T[i][j]), mpz_one ());
          mpz_bincoef_ui (mpq_denref (T[i][j]), degree, i);
        }
      else
        mpq_set (T[i][j], mpq_zero ());
}

// Multiply a row vector by this matrix to convert coefficients from
// scaled Bernstein basis to Sánchez-Reyes s-power basis.
//
// The matrix’s rows are the Sánchez-Reyes coefficients of the scaled
// Bernstein basis polynomials.
VISIBLE void
mpq_coefficients_sbern_to_spower (size_t degree,
                                  mpq_t T[degree + 1][degree + 1])
{
  const size_t n = degree;
  const size_t q = n / 2 + n % 2;

  mpq_matrix_set_zero (q + 1, n + 1, T);

  for (size_t i = 0; i < q; i++)
    {
      // Fill in a top-half row.
      for (size_t j = i; j < q; j++)
        {
          mpq_bincoef_ui (T[i][j], n - j - i, j - i);
          if ((j - i) % 2 == 1)
            mpq_neg (T[i][j], T[i][j]);
        }
      if (n % 2 == 0)
        mpq_set (T[i][q], (((q - i) % 2 == 1) ? mpq_neg_one () : mpq_one ()));
      for (size_t j = i + 1; j < q; j++)
        {
          mpq_bincoef_ui (T[i][n - j], n - j - i - 1, j - i - 1);
          if ((j - i) % 2 == 1)
            mpq_neg (T[i][n - j], T[i][n - j]);
        }

      // The corresponding bottom-half row is the reverse.
      for (size_t j = 0; j <= n; j++)
        mpq_set (T[n - i][n - j], T[i][j]);
    }

  // For special handling of even degrees, put an extra ‘1’ in the
  // middle of the matrix.
  fill_spower_middle_row (n, T);
}

// Multiply a row vector by this matrix to convert coefficients from
// Sánchez-Reyes s-power basis to scaled Bernstein basis.
//
// The matrix’s rows are the scaled Bernstein coefficients of the
// Sánchez-Reyes basis polynomials.
VISIBLE void
mpq_coefficients_spower_to_sbern (size_t degree,
                                  mpq_t T[degree + 1][degree + 1])
{
  const size_t n = degree;
  const size_t q = n / 2 + n % 2;

  for (size_t i = 0; i < q; i++)
    {
      // Fill in a top-half row.
      for (size_t j = 0; j <= i; j++)
        mpq_set (T[i][j], mpq_zero ());
      const size_t d = n - (2 * i) - 1;
      for (size_t j = i; j <= i + d; j++)
        mpq_bincoef_ui (T[i][j], d, j - i);
      for (size_t j = i + d + 1; j <= n; j++)
        mpq_set (T[i][j], mpq_zero ());

      // The corresponding bottom-half row is the reverse.
      for (size_t j = 0; j <= n; j++)
        mpq_set (T[n - i][n - j], T[i][j]);
    }

  // For special handling of even degrees, put an extra ‘1’ in the
  // middle of the matrix.
  fill_spower_middle_row (n, T);
}

//-------------------------------------------------------------------------

// Multiply a row vector by this matrix to make no change to the
// vector.
static void
mpq_identity_matrix (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  for (size_t i = 0; i <= degree; i++)
    for (size_t j = 0; j <= degree; j++)
      mpq_set (T[i][j], ((i == j) ? mpq_one () : mpq_zero ()));
}

VISIBLE void
mpq_coefficients_mono_to_mono (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_identity_matrix (degree, T);
}

VISIBLE void
mpq_coefficients_bern_to_bern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_identity_matrix (degree, T);
}

VISIBLE void
mpq_coefficients_sbern_to_sbern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_identity_matrix (degree, T);
}

VISIBLE void
mpq_coefficients_spower_to_spower (size_t degree,
                                   mpq_t T[degree + 1][degree + 1])
{
  mpq_identity_matrix (degree, T);
}

//-------------------------------------------------------------------------
//
// Matrices constructed by composing our other transformations, rather
// than filling them in directly.

static void
compose_matrices (void (*func1) (size_t degree,
                                 mpq_t T[degree + 1][degree + 1]),
                  void (*func2) (size_t degree,
                                 mpq_t T[degree + 1][degree + 1]),
                  size_t degree, mpq_t T[degree + 1][degree + 1])
{
  mpq_t T1[degree + 1][degree + 1];
  mpq_t T2[degree + 1][degree + 1];

  mpq_matrix_init (degree + 1, degree + 1, T1);
  mpq_matrix_init (degree + 1, degree + 1, T2);

  func1 (degree, T1);
  func2 (degree, T2);

  mpq_matrix_gemm (CblasNoTrans, CblasNoTrans,
                   degree + 1, degree + 1, degree + 1,
                   mpq_one (), T1, T2, mpq_zero (), T);

  mpq_matrix_clear (degree + 1, degree + 1, T1);
  mpq_matrix_clear (degree + 1, degree + 1, T2);
}

VISIBLE void
mpq_coefficients_mono_to_spower (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  return compose_matrices (mpq_coefficients_mono_to_sbern,
                           mpq_coefficients_sbern_to_spower, degree, T);
}

VISIBLE void
mpq_coefficients_bern_to_spower (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  return compose_matrices (mpq_coefficients_bern_to_sbern,
                           mpq_coefficients_sbern_to_spower, degree, T);
}

VISIBLE void
mpq_coefficients_spower_to_mono (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  return compose_matrices (mpq_coefficients_spower_to_sbern,
                           mpq_coefficients_sbern_to_mono, degree, T);
}

VISIBLE void
mpq_coefficients_spower_to_bern (size_t degree, mpq_t T[degree + 1][degree + 1])
{
  return compose_matrices (mpq_coefficients_spower_to_sbern,
                           mpq_coefficients_sbern_to_bern, degree, T);
}

//-------------------------------------------------------------------------

static SCM
scm_coefficient_matrix (void (*func) (size_t degree,
                                      mpq_t T[degree + 1][degree + 1]),
                        size_t degree)
{
  scm_dynwind_begin (0);

  mpq_t T[degree + 1][degree + 1];
  mpq_matrix_init (degree + 1, degree + 1, T);
  scm_dynwind_mpq_matrix_clear (degree + 1, degree + 1, T);

  func (degree, T);

  SCM result = scm_from_mpq_matrix (degree + 1, degree + 1, T);

  scm_dynwind_end ();

  return result;
}

//-------------------------------------------------------------------------

#define __PRECOMPUTED_MAX_DEGREE 9

typedef struct
{
  SCM matrices[__PRECOMPUTED_MAX_DEGREE + 1];
} _precomputed_matrices_t;

static void
initialize_precomputed_matrices (_precomputed_matrices_t **precomputed,
                                 void (*func) (size_t deg,
                                               mpq_t T[deg + 1][deg + 1]))
{
  (*precomputed) = XMALLOC (_precomputed_matrices_t);
  for (size_t degree = 0; degree <= __PRECOMPUTED_MAX_DEGREE; degree++)
    (*precomputed)->matrices[degree] =
      scm_permanent_object (scm_coefficient_matrix (func, degree));
}

#define _PRECOMPUTED_MATRICES_BASIS_TO_BASIS(BASIS1, BASIS2)            \
  INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,              \
                        precomputed_##BASIS1##_to_##BASIS2,             \
                        initialize_precomputed_matrices,                \
                        mpq_coefficients_##BASIS1##_to_##BASIS2);

#define _PRECOMPUTED_MATRICES_FROM_BASIS_TO_BASIS(BASIS)        \
  _PRECOMPUTED_MATRICES_BASIS_TO_BASIS (BASIS, mono)            \
  _PRECOMPUTED_MATRICES_BASIS_TO_BASIS (BASIS, bern)            \
  _PRECOMPUTED_MATRICES_BASIS_TO_BASIS (BASIS, sbern)           \
  _PRECOMPUTED_MATRICES_BASIS_TO_BASIS (BASIS, spower)

_PRECOMPUTED_MATRICES_FROM_BASIS_TO_BASIS (mono);
_PRECOMPUTED_MATRICES_FROM_BASIS_TO_BASIS (bern);
_PRECOMPUTED_MATRICES_FROM_BASIS_TO_BASIS (sbern);
_PRECOMPUTED_MATRICES_FROM_BASIS_TO_BASIS (spower);

//-------------------------------------------------------------------------

#define _SCM_C_COEF_BASIS_TO_BASIS(BASIS1, BASIS2)                      \
  VISIBLE SCM                                                           \
  scm_c_coefficients_##BASIS1##_to_##BASIS2 (size_t degree)             \
  {                                                                     \
    return (degree <= __PRECOMPUTED_MAX_DEGREE) ?                       \
      precomputed_##BASIS1##_to_##BASIS2 ()->matrices[degree] :         \
      scm_coefficient_matrix (mpq_coefficients_##BASIS1##_to_##BASIS2,  \
                              degree);                                  \
  }

#define _SCM_C_COEF_FROM_BASIS(BASIS)           \
  _SCM_C_COEF_BASIS_TO_BASIS (BASIS, mono)      \
  _SCM_C_COEF_BASIS_TO_BASIS (BASIS, bern)      \
  _SCM_C_COEF_BASIS_TO_BASIS (BASIS, sbern)     \
  _SCM_C_COEF_BASIS_TO_BASIS (BASIS, spower)

_SCM_C_COEF_FROM_BASIS (mono);
_SCM_C_COEF_FROM_BASIS (bern);
_SCM_C_COEF_FROM_BASIS (sbern);
_SCM_C_COEF_FROM_BASIS (spower);

//-------------------------------------------------------------------------

#define _SCM_COEF_BASIS_TO_BASIS(BASIS1, BASIS2)                        \
  VISIBLE SCM                                                           \
  scm_coefficients_##BASIS1##_to_##BASIS2 (SCM degree)                  \
  {                                                                     \
    return                                                              \
      scm_c_coefficients_##BASIS1##_to_##BASIS2 (scm_to_size_t (degree)); \
  }

#define _SCM_COEF_FROM_BASIS(BASIS)             \
  _SCM_COEF_BASIS_TO_BASIS (BASIS, mono)        \
  _SCM_COEF_BASIS_TO_BASIS (BASIS, bern)        \
  _SCM_COEF_BASIS_TO_BASIS (BASIS, sbern)       \
  _SCM_COEF_BASIS_TO_BASIS (BASIS, spower)

_SCM_COEF_FROM_BASIS (mono);
_SCM_COEF_FROM_BASIS (bern);
_SCM_COEF_FROM_BASIS (sbern);
_SCM_COEF_FROM_BASIS (spower);

//-------------------------------------------------------------------------

#define _FF_CHANGE_BASIS_F64(FROM, TO)                          \
  void                                                          \
  change_basis_f64_##FROM##_to_##TO (size_t degree,             \
                                     ssize_t stride,            \
                                     const double *spline,      \
                                     ssize_t result_stride,     \
                                     double *result)            \
  {                                                             \
    mpq_t T[degree + 1][degree + 1];                            \
    mpq_t x[1][degree + 1];                                     \
    mpq_t y[1][degree + 1];                                     \
                                                                \
    mpq_matrix_init (degree + 1, degree + 1, T);                \
    mpq_matrix_init (1, degree + 1, x);                         \
    mpq_matrix_init (1, degree + 1, y);                         \
                                                                \
    mpq_coefficients_##FROM##_to_##TO (degree, T);              \
                                                                \
    for (size_t i = 0; i <= degree; i++)                        \
      mpq_set_d (x[0][i], spline[stride * (ssize_t) i]);        \
                                                                \
    mpq_matrix_gemm (CblasNoTrans, CblasNoTrans,                \
                     1, degree + 1, degree + 1,                 \
                     mpq_one (), x, T, mpq_zero (), y);         \
                                                                \
    for (size_t i = 0; i <= degree; i++)                        \
      result[result_stride * (ssize_t) i] =                     \
        mpq_get_d (y[0][i]);                                    \
                                                                \
    mpq_matrix_clear (degree + 1, degree + 1, T);               \
    mpq_matrix_clear (1, degree + 1, x);                        \
    mpq_matrix_clear (1, degree + 1, y);                        \
  }

VISIBLE _FF_CHANGE_BASIS_F64 (mono, mono);
VISIBLE _FF_CHANGE_BASIS_F64 (mono, bern);
VISIBLE _FF_CHANGE_BASIS_F64 (mono, sbern);
VISIBLE _FF_CHANGE_BASIS_F64 (mono, spower);
VISIBLE _FF_CHANGE_BASIS_F64 (bern, mono);
VISIBLE _FF_CHANGE_BASIS_F64 (bern, bern);
VISIBLE _FF_CHANGE_BASIS_F64 (bern, sbern);
VISIBLE _FF_CHANGE_BASIS_F64 (bern, spower);
VISIBLE _FF_CHANGE_BASIS_F64 (sbern, mono);
VISIBLE _FF_CHANGE_BASIS_F64 (sbern, bern);
VISIBLE _FF_CHANGE_BASIS_F64 (sbern, sbern);
VISIBLE _FF_CHANGE_BASIS_F64 (sbern, spower);
VISIBLE _FF_CHANGE_BASIS_F64 (spower, mono);
VISIBLE _FF_CHANGE_BASIS_F64 (spower, bern);
VISIBLE _FF_CHANGE_BASIS_F64 (spower, sbern);
VISIBLE _FF_CHANGE_BASIS_F64 (spower, spower);

//-------------------------------------------------------------------------

void init_math_polyspline_bases (void);

#define _SCM_C_DEFINE_GSUBR_COEF_BASIS_TO_BASIS(BASIS1, BASIS2)         \
  scm_c_define_gsubr ("coefficients_" #BASIS1 "_to_" #BASIS2, 1, 0, 0,  \
                      scm_coefficients_##BASIS1##_to_##BASIS2)

#define _SCM_C_DEFINE_GSUBR_COEF_FROM_BASIS(BASIS)              \
  _SCM_C_DEFINE_GSUBR_COEF_BASIS_TO_BASIS (BASIS, mono);        \
  _SCM_C_DEFINE_GSUBR_COEF_BASIS_TO_BASIS (BASIS, bern);        \
  _SCM_C_DEFINE_GSUBR_COEF_BASIS_TO_BASIS (BASIS, sbern);       \
  _SCM_C_DEFINE_GSUBR_COEF_BASIS_TO_BASIS (BASIS, spower)

VISIBLE void
init_math_polyspline_bases (void)
{
  _SCM_C_DEFINE_GSUBR_COEF_FROM_BASIS (mono);
  _SCM_C_DEFINE_GSUBR_COEF_FROM_BASIS (bern);
  _SCM_C_DEFINE_GSUBR_COEF_FROM_BASIS (sbern);
  _SCM_C_DEFINE_GSUBR_COEF_FROM_BASIS (spower);
}

//-------------------------------------------------------------------------
