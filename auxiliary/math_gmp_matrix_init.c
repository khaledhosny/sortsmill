#include <config.h>

// Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/math/gmp_matrix.h>
#include <xalloc.h>

VISIBLE void
_GMP_TYPE (_matrix_init) (size_t m, size_t n, _GMP_TYPE (_t) A[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_init) (A[i][j]);
}

VISIBLE void
_GMP_TYPE (_matrix_clear) (size_t m, size_t n, _GMP_TYPE (_t) A[m][n])
{
  for (size_t i = 0; i < m; i++)
    for (size_t j = 0; j < n; j++)
      _GMP_TYPE (_clear) (A[i][j]);
}

typedef struct
{
  size_t m;
  size_t n;
  void *elems;
} _GMP_TYPE2 (_, _t_matrix_view);

//                                                          *INDENT-OFF*
VISIBLE void
_GMP_TYPE2 (scm_dynwind_, _matrix_unwind_handler) (void *p)
//                                                           *INDENT-ON*
{
  _GMP_TYPE2 (_, _t_matrix_view) * q = (_GMP_TYPE2 (_, _t_matrix_view) *) p;
  _GMP_TYPE (_matrix_clear)
    (q->m, q->n, (_GMP_TYPE (_t) (*)[(size_t) q->n]) q->elems);
  free (q);
}

//                                                          *INDENT-OFF*
VISIBLE void
_GMP_TYPE2 (scm_dynwind_, _matrix_clear) (size_t m, size_t n,
                                          _GMP_TYPE (_t) A[m][n])
//                                                           *INDENT-ON*

{
  _GMP_TYPE2 (_, _t_matrix_view) * q = XMALLOC (_GMP_TYPE2 (_, _t_matrix_view));
  q->m = m;
  q->n = n;
  q->elems = &A[0][0];
  scm_dynwind_unwind_handler (_GMP_TYPE2 (scm_dynwind_, _matrix_unwind_handler),
                              q, SCM_F_WIND_EXPLICITLY);
}
