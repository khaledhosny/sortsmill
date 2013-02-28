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

#include <sortsmill/gmp_matrix.h>
#include <sortsmill/gmp_constants.h>

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
initialize_permutation (unsigned int n, size_t p[n])
{
  for (unsigned int i = 0; i < n; i++)
    p[i] = i;
}

VISIBLE void
mpq_linalg_LU_decomp (unsigned int n, mpq_t A[n][n], size_t p[n], int *signum)
{
  initialize_permutation (n, p);
  *signum = 1;

  mpq_t max, aij, aik, ajj, ajk, product;
  mpq_inits (max, aij, aik, ajj, ajk, product, NULL);

  for (unsigned int j = 0; j < n - 1; j++)
    {
      // Find maximum in the j-th column.

      mpq_abs (max, A[j][j]);
      unsigned int i_pivot = j;

      for (unsigned int i = j + 1; i < n; i++)
        {
          mpq_abs (aij, A[i][j]);

          if (mpq_cmp (max, aij) < 0)
            {
              mpq_set (max, aij);
              i_pivot = i;
            }
        }

      if (i_pivot != j)
        {
          mpq_matrix_swap_rows (n, n, A, j, i_pivot);

          unsigned int temp = p[i_pivot];
          p[i_pivot] = p[j];
          p[j] = temp;

          *signum = -(*signum);
        }

      mpq_set (ajj, A[j][j]);

      if (mpq_sgn (ajj) != 0)
        {
          for (unsigned int i = j + 1; i < n; i++)
            {
              mpq_div (aij, A[i][j], ajj);
              mpq_set (A[i][j], aij);

              for (unsigned int k = j + 1; k < n; k++)
                {
                  mpq_set (aik, A[i][k]);
                  mpq_set (ajk, A[j][k]);
                  mpq_mul (product, aij, ajk);
                  mpq_sub (A[i][k], aik, product);
                }
            }
        }
    }

  mpq_clears (max, aij, aik, ajj, ajk, product, NULL);
}

VISIBLE void
mpq_linalg_LU_solve (unsigned int n, mpq_t LU[n][n], size_t p[n],
                     mpq_t b[n], mpq_t x[n], bool *singular)
{
  // Copy x <- b.
  mpq_matrix_memcpy (n, 1, (mpq_t (*)[1]) x, (mpq_t (*)[1]) b);

  // Solve for x.
  mpq_linalg_LU_svx (n, LU, p, x, singular);
}

static void
permute_vector (unsigned int n, size_t p[n], mpq_t x[n])
{
  // Do a simple permutation by copying. Note that there is an
  // implementation of in-place permutation in the GSL sources, which
  // could be adapted if desired.

  mpq_t temp[n];
  mpq_vector_init (n, temp);

  for (unsigned int i = 0; i < n; i++)
    mpq_set (temp[i], x[p[i]]);

  mpq_matrix_memcpy (n, 1, (mpq_t (*)[1]) x, (mpq_t (*)[1]) temp);

  mpq_vector_clear (n, temp);
}

VISIBLE void
mpq_linalg_LU_svx (unsigned int n, mpq_t LU[n][n], size_t p[n], mpq_t x[n],
                   bool *singular)
{
  // Apply permutation to RHS.
  permute_vector (n, p, x);

  // Solve for c using forward-substitution, L c = P b.
  mpq_matrix_trsv (CblasLower, CblasNoTrans, CblasUnit, n, LU, x, singular);

  if (!*singular)
    // Perform back-substitution, U x = c.
    mpq_matrix_trsv (CblasUpper, CblasNoTrans, CblasNonUnit, n, LU, x,
                     singular);
}

VISIBLE void
mpq_linalg_LU_invert (unsigned int n, mpq_t LU[n][n], size_t p[n],
                      mpq_t inverse[n][n], bool *singular)
{
  mpq_t temp[n];
  mpq_vector_init (n, temp);

  *singular = false;

  unsigned int j = 0;
  while (!*singular && j < n)
    {
      // temp = j-th column of inverse.

      for (unsigned int i = 0; i < n; i++)
        mpq_set_si (temp[i], ((int) (i == j)), 1);

      mpq_linalg_LU_svx (n, LU, p, temp, singular);

      if (!*singular)
        for (unsigned int i = 0; i < n; i++)
          mpq_set (inverse[i][j], temp[i]);

      j++;
    }

  mpq_vector_clear (n, temp);
}

void
mpq_linalg_LU_det (unsigned int n, mpq_t LU[n][n], int signum, mpq_t det)
{
  mpq_set_si (det, signum, 1);

  for (unsigned int i = 0; i < n; i++)
    mpq_mul (det, det, LU[i][i]);
}

VISIBLE int
mpq_linalg_LU_sgndet (unsigned int n, mpq_t LU[n][n], int signum)
{
  int s = signum;

  unsigned int i = 0;
  while (s != 0 && i < n)
    {
      s *= mpq_sgn (LU[i][i]);
      i++;
    }

  return s;
}
