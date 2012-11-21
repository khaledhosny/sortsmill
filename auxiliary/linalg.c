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

#include <linalg.h>
#include <gmp_constants.h>
#include <stdbool.h>

VISIBLE void
mpq_matrix_init (unsigned int m, unsigned int n, mpq_t A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      mpq_init (A[i][j]);
}

VISIBLE void
mpq_matrix_clear (unsigned int m, unsigned int n, mpq_t A[m][n])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      mpq_clear (A[i][j]);
}

VISIBLE void
mpq_matrix_set_all (unsigned int m, unsigned int n, mpq_t A[m][n],
                    const mpq_t x)
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      mpq_set (A[i][j], x);
}

VISIBLE void
mpq_matrix_set_zero (unsigned int m, unsigned int n, mpq_t A[m][n])
{
  mpq_matrix_set_all (m, n, A, mpq_zero ());
}

VISIBLE void
mpq_matrix_transpose_memcpy (unsigned int m, unsigned int n,
                             mpq_t A[m][n], mpq_t result[n][m])
{
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      mpq_set (result[j][i], A[i][j]);
}

VISIBLE void
mpq_matrix_scale (unsigned int m, unsigned int n, mpq_t A[m][n],
                  const mpq_t x)
{
  if (mpq_sgn (x) == 0)
    mpq_matrix_set_zero (m, n, A);
  else if (!mpq_equal (x, mpq_one ()))
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        mpq_mul (A[i][j], A[i][j], x);
}

_GL_ATTRIBUTE_CONST static inline unsigned int
_trans_row (CBLAS_TRANSPOSE_t trans, unsigned int i, unsigned int j)
{
  return (trans == CblasNoTrans) ? i : j;
}

_GL_ATTRIBUTE_CONST static inline unsigned int
_trans_col (CBLAS_TRANSPOSE_t trans, unsigned int i, unsigned int j)
{
  return (trans == CblasNoTrans) ? j : i;
}

static inline void
_mul_by_alpha (mpq_t x, bool alpha_is_one, const mpq_t alpha)
{
  if (!alpha_is_one)
    mpq_mul (x, x, alpha);
}

static inline void
_set_C_entry (mpq_t C_entry, bool beta_is_zero, bool beta_is_one,
              const mpq_t beta, const mpq_t x)
{
  if (beta_is_zero)
    mpq_set (C_entry, x);
  else if (beta_is_one)
    mpq_add (C_entry, C_entry, x);
  else
    {
      mpq_mul (C_entry, C_entry, beta);
      mpq_add (C_entry, C_entry, x);
    }
}

VISIBLE void
mpq_matrix_gemm (CBLAS_TRANSPOSE_t TransA, CBLAS_TRANSPOSE_t TransB,
                 unsigned int m, unsigned int n, unsigned int k,
                 const mpq_t alpha,
                 mpq_t _FF_TRANSMATRIX (A, TransA, m, k),
                 mpq_t _FF_TRANSMATRIX (B, TransB, k, n),
                 const mpq_t beta, mpq_t C[m][n])
{
  if (mpq_sgn (alpha) == 0)
    mpq_matrix_scale (m, n, C, beta);
  else
    {
      assert ("NOT YET IMPLEMENTED");
    }
}
