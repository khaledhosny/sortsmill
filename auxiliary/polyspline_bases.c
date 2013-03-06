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

#include <sortsmill/polyspline/bases.h>
#include <sortsmill/bincoef.h>
#include <sortsmill/gmp_constants.h>
#include <sortsmill/initialized_global_constants.h>
#include <xalloc.h>

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
static mpqmat_t
coefficients_mono_to_bern__base (unsigned int degree)
{
  mpq_t divisor;
  mpq_init (divisor);

  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);

  for (unsigned int i = 0; i <= degree; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
      for (unsigned int j = i; j <= degree; j++)
        mpq_bincoef_ui (MPQMAT_REF (T)[i][j], degree - i, j - i);

    }

  for (unsigned int j = 1; j < degree; j++)
    {
      mpq_bincoef_ui (divisor, degree, j);
      for (unsigned int i = 0; i <= degree; i++)
        mpq_div (MPQMAT_REF (T)[i][j], MPQMAT_REF (T)[i][j], divisor);
    }

  mpq_clear (divisor);
  return T;
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
static mpqmat_t
coefficients_mono_to_sbern__base (unsigned int degree)
{
  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
      for (unsigned int j = i; j <= degree; j++)
        mpq_bincoef_ui (MPQMAT_REF (T)[i][j], degree - i, j - i);
    }
  return T;
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
static mpqmat_t
coefficients_bern_to_mono__base (unsigned int degree)
{
  mpq_t factor;
  mpq_init (factor);

  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
      for (unsigned int j = i; j <= degree; j++)
        mpq_bincoef_ui (MPQMAT_REF (T)[i][j], degree - i, j - i);
      for (unsigned int j = i + 1; j <= degree; j += 2)
        mpq_neg (MPQMAT_REF (T)[i][j], MPQMAT_REF (T)[i][j]);
    }

  for (unsigned int i = 1; i < degree; i++)
    {
      mpq_bincoef_ui (factor, degree, i);
      for (unsigned int j = 0; j <= degree; j++)
        mpq_mul (MPQMAT_REF (T)[i][j], MPQMAT_REF (T)[i][j], factor);
    }

  mpq_clear (factor);
  return T;
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
static mpqmat_t
coefficients_bern_to_sbern__base (unsigned int degree)
{
  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    for (unsigned int j = 0; j <= degree; j++)
      if (i == j)
        mpq_bincoef_ui (MPQMAT_REF (T)[i][j], degree, i);
      else
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
  return T;
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
static mpqmat_t
coefficients_sbern_to_mono__base (unsigned int degree)
{
  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    {
      for (unsigned int j = 0; j < i; j++)
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
      for (unsigned int j = i; j <= degree; j++)
        mpq_bincoef_ui (MPQMAT_REF (T)[i][j], degree - i, j - i);
      for (unsigned int j = i + 1; j <= degree; j += 2)
        mpq_neg (MPQMAT_REF (T)[i][j], MPQMAT_REF (T)[i][j]);
    }
  return T;
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
static mpqmat_t
coefficients_sbern_to_bern__base (unsigned int degree)
{
  mpqmat_t T = scm_c_make_mpqmat_t (degree + 1, degree + 1);
  for (unsigned int i = 0; i <= degree; i++)
    for (unsigned int j = 0; j <= degree; j++)
      if (i == j)
        {
          // The following produces canonical form, making a call to
          // mpq_canonicalize() unnecessary.
          mpz_set (mpq_numref (MPQMAT_REF (T)[i][j]), mpz_one ());
          mpz_bincoef_ui (mpq_denref (MPQMAT_REF (T)[i][j]), degree, i);
        }
      else
        mpq_set (MPQMAT_REF (T)[i][j], mpq_zero ());
  return T;
}

//-------------------------------------------------------------------------

#define __PRECOMPUTED_MAX_DEGREE 9

typedef struct
{
  mpqmat_t matrices[__PRECOMPUTED_MAX_DEGREE + 1];
} _precomputed_matrices_t;

static void
initialize_precomputed_matrices (_precomputed_matrices_t **precomputed,
                                 mpqmat_t (*base_function) (unsigned int
                                                            degree))
{
  (*precomputed) = XMALLOC (_precomputed_matrices_t);
  for (unsigned int degree = 0; degree <= __PRECOMPUTED_MAX_DEGREE; degree++)
    (*precomputed)->matrices[degree] =
      scm_make_mpqmat_t_permanent (base_function (degree));
}

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_mono_to_bern,
                      initialize_precomputed_matrices,
                      coefficients_mono_to_bern__base);

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_mono_to_sbern,
                      initialize_precomputed_matrices,
                      coefficients_mono_to_sbern__base);

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_bern_to_mono,
                      initialize_precomputed_matrices,
                      coefficients_bern_to_mono__base);

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_bern_to_sbern,
                      initialize_precomputed_matrices,
                      coefficients_bern_to_sbern__base);

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_sbern_to_mono,
                      initialize_precomputed_matrices,
                      coefficients_sbern_to_mono__base);

INITIALIZED_CONSTANT (static, _precomputed_matrices_t *,
                      precomputed_sbern_to_bern,
                      initialize_precomputed_matrices,
                      coefficients_sbern_to_bern__base);

//-------------------------------------------------------------------------

VISIBLE mpqmat_t
coefficients_mono_to_bern (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_mono_to_bern ()->matrices[degree] :
    coefficients_mono_to_bern__base (degree);
}

VISIBLE mpqmat_t
coefficients_mono_to_sbern (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_mono_to_sbern ()->matrices[degree] :
    coefficients_mono_to_sbern__base (degree);
}

VISIBLE mpqmat_t
coefficients_bern_to_mono (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_bern_to_mono ()->matrices[degree] :
    coefficients_bern_to_mono__base (degree);
}

VISIBLE mpqmat_t
coefficients_bern_to_sbern (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_bern_to_sbern ()->matrices[degree] :
    coefficients_bern_to_sbern__base (degree);
}

VISIBLE mpqmat_t
coefficients_sbern_to_mono (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_sbern_to_mono ()->matrices[degree] :
    coefficients_sbern_to_mono__base (degree);
}

VISIBLE mpqmat_t
coefficients_sbern_to_bern (unsigned int degree)
{
  return (degree <= __PRECOMPUTED_MAX_DEGREE) ?
    precomputed_sbern_to_bern ()->matrices[degree] :
    coefficients_sbern_to_bern__base (degree);
}

//-------------------------------------------------------------------------
