#include <config.h>

// Copyright (C) 2012, 2013 Barry Schwartz
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

#include <sortsmill/gmp_matrix.h>
#include <stdbool.h>

_GL_ATTRIBUTE_PURE static inline bool
mpz_equal (const mpz_t a, const mpz_t b)
{
  return (mpz_cmp (a, b) == 0);
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

#undef _GMP_MATRIX
#define _GMP_MATRIX(_m_ignored, n) _GMP_TYPE (_t) (*)[(unsigned int) (n)]

#undef _GMP_TYPE_MPZ
#undef _GMP_TYPE_MPQ
#undef _GMP_TYPE_MPF
#undef _GMP_TYPE
#undef _GMP_TYPE2
#define _GMP_TYPE_MPZ 1
#define _GMP_TYPE_MPQ 0
#define _GMP_TYPE_MPF 0
#define _GMP_TYPE(y) mpz##y
#define _GMP_TYPE2(x, y) x##mpz##y
#include <gmp_matrix_init.c>
#include <gmp_matrix_set.c>
#include <gmp_matrix_copy.c>
#include <gmp_matrix_swap.c>
#include <gmp_matrix_mult.c>
#include <gmp_matrix_add.c>
#include <gmp_matrix_pred.c>

#undef _GMP_TYPE_MPZ
#undef _GMP_TYPE_MPQ
#undef _GMP_TYPE_MPF
#undef _GMP_TYPE
#undef _GMP_TYPE2
#define _GMP_TYPE_MPZ 0
#define _GMP_TYPE_MPQ 1
#define _GMP_TYPE_MPF 0
#define _GMP_TYPE(y) mpq##y
#define _GMP_TYPE2(x, y) x##mpq##y
#include <gmp_matrix_init.c>
#include <gmp_matrix_set.c>
#include <gmp_matrix_copy.c>
#include <gmp_matrix_swap.c>
#include <gmp_matrix_mult.c>
#include <gmp_matrix_add.c>
#include <gmp_matrix_pred.c>
#include <gmp_matrix_trsv.c>
