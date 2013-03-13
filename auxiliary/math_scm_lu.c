#include <config.h>

// Copyright (C) 2013 Barry Schwartz
// Copyright (C) 1996, 1997, 1998, 1999, 2000, 2007, 2009 Gerard Jungman, Brian Gough
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or (at
// your option) any later version.
// 
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

// Based on the GSL implementation, linalg/lu.c, by G. Jungman.

#include <sortsmill/math/scm_matrix.h>

/* Factorise a general N x N matrix A into,
 *
 *   P A = L U
 *
 * where P is a permutation matrix, L is unit lower triangular and U
 * is upper triangular.
 *
 * L is stored in the strict lower triangular part of the input
 * matrix. The diagonal elements of L are unity and are not stored.
 *
 * U is stored in the diagonal and upper triangular part of the
 * input matrix.  
 * 
 * P is stored in the permutation p. Column j of P is column k of the
 * identity matrix, where k = p[j].
 *
 * signum gives the sign of the permutation, (-1)^n, where n is the
 * number of interchanges in the permutation. 
 *
 * See Golub & Van Loan, Matrix Computations, Algorithm 3.4.1 (Gauss
 * Elimination with Partial Pivoting).
 */

static void
initialize_permutation (size_t n, size_t p[n])
{
  for (size_t i = 0; i < n; i++)
    p[i] = i;
}

VISIBLE void
scm_linalg_LU_decomp (size_t n, SCM A[n][n], size_t p[n], int *signum)
{
  initialize_permutation (n, p);
  *signum = 1;

  for (size_t j = 0; j < n - 1; j++)
    {
      // Find maximum in the j-th column.

      SCM max = scm_magnitude (A[j][j]);
      size_t i_pivot = j;

      for (size_t i = j + 1; i < n; i++)
        {
          SCM aij = scm_magnitude (A[i][j]);

          if (scm_is_true (scm_less_p (max, aij)))
            {
              max = aij;
              i_pivot = i;
            }
        }

      if (i_pivot != j)
        {
          scm_matrix_swap_rows (n, n, A, j, i_pivot);

          size_t temp = p[i_pivot];
          p[i_pivot] = p[j];
          p[j] = temp;

          *signum = -(*signum);
        }

      SCM ajj = A[j][j];

      if (scm_is_false (scm_zero_p (ajj)))
        {
          for (size_t i = j + 1; i < n; i++)
            {
              SCM aij = scm_divide (A[i][j], ajj);
              A[i][j] = aij;

              for (size_t k = j + 1; k < n; k++)
                {
                  SCM aik = A[i][k];
                  SCM ajk = A[j][k];
                  A[i][k] = scm_difference (aik, scm_product (aij, ajk));
                }
            }
        }
    }
}

VISIBLE void
scm_linalg_LU_solve (size_t n, SCM LU[n][n], size_t p[n], SCM b[n], SCM x[n])
{
  // Copy x <- b.
  memcpy (&x[0], &b[0], n * sizeof (SCM));

  // Solve for x.
  scm_linalg_LU_svx (n, LU, p, x);
}

static void
permute_vector (size_t n, size_t p[n], SCM x[n])
{
  // Do a simple permutation by copying. Note that there is an
  // implementation of in-place permutation in the GSL sources, which
  // could be adapted if desired.

  SCM temp[n];
  for (size_t i = 0; i < n; i++)
    temp[i] = x[p[i]];
  memcpy (x, temp, n * sizeof (SCM));
}

VISIBLE void
scm_linalg_LU_svx (size_t n, SCM LU[n][n], size_t p[n], SCM x[n])
{
  // Apply permutation to RHS.
  permute_vector (n, p, x);

  // Solve for c using forward-substitution, L c = P b.
  scm_matrix_trsv (CblasLower, CblasNoTrans, CblasUnit, n, LU, x);

  // Perform back-substitution, U x = c.
  scm_matrix_trsv (CblasUpper, CblasNoTrans, CblasNonUnit, n, LU, x);
}

VISIBLE void
scm_linalg_LU_refine (size_t n, SCM A[n][n], SCM LU[n][n],
                      size_t p[n], SCM b[n], SCM x[n], SCM residual[n])
{
  // Compute residual, residual = (A * x - b).
  memcpy (residual, b, n * sizeof (SCM));
  scm_matrix_gemm (CblasNoTrans, CblasNoTrans, n, 1, n, scm_from_int (1), A,
                   (SCM (*)[1]) x, scm_from_int (-1), (SCM (*)[1]) residual);

  // Find correction, delta = - (A^-1) * residual, and apply it.
  scm_linalg_LU_svx (n, LU, p, residual);
  for (size_t i = 0; i < n; i++)
    x[i] = scm_difference (x[i], residual[i]);
}

VISIBLE void
scm_linalg_LU_invert (size_t n, SCM LU[n][n], size_t p[n], SCM inverse[n][n])
{
  SCM temp[n];

  for (size_t j = 0; j < n; j++)
    {
      // temp = j-th column of inverse.

      for (size_t i = 0; i < n; i++)
        temp[i] = scm_from_int ((int) (i == j));

      scm_linalg_LU_svx (n, LU, p, temp);

      for (size_t i = 0; i < n; i++)
        inverse[i][j] = temp[i];
    }
}

VISIBLE SCM
scm_linalg_LU_det (size_t n, SCM LU[n][n], int signum)
{
  SCM det = scm_from_int (signum);

  for (size_t i = 0; i < n; i++)
    det = scm_product (det, LU[i][i]);

  return det;
}

VISIBLE SCM
scm_linalg_LU_lndet (size_t n, SCM LU[n][n])
{
  SCM lndet = scm_from_int (0);

  SCM scm_log = scm_c_public_ref ("rnrs base", "log");

  for (size_t i = 0; i < n; i++)
    lndet = scm_sum (lndet, scm_call_1 (scm_log, scm_magnitude (LU[i][i])));

  return lndet;
}

VISIBLE int
scm_linalg_LU_sgndet (size_t n, SCM LU[n][n], int signum)
{
  int s = signum;

  size_t i = 0;
  while (s != 0 && i < n)
    {
      SCM u = LU[i][i];
      if (scm_is_true (scm_negative_p (u)))
        s *= -1;
      else if (scm_is_true (scm_zero_p (u)))
        s = 0;
      i++;
    }

  return s;
}
