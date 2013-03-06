/* -*- coding: utf-8 -*-
 *
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_GUILE_MATH_MATRICES_MPQMAT_H
#define _SORTSMILL_GUILE_MATH_MATRICES_MPQMAT_H

#include <sortsmill/c_version.h>
#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

typedef struct
{
  unsigned int size1;
  unsigned int size2;
  mpq_t *data;
} mpqmat_struct_t;

typedef mpqmat_struct_t *mpqmat_t;


#if _FF_C99_OR_GREATER

/* Refer to an mpqmat_t object as if it were a rank-2 variable-length
   array. */
#define MPQMAT_REF(mpqmat)                                      \
  ((mpq_t (*)[(unsigned int) (mpqmat)->size2]) (mpqmat)->data)

#endif /* _FF_C99_OR_GREATER */

/* Refer to an mpqmat_t object as if it were a rank-1 C array. */
#define MPQVEC_REF(mpqmat) ((mpq_t *) (mpqmat)->data)


SCM scm_mpqmat_p (SCM obj);
bool scm_is_mpqmat (SCM obj);
SCM scm_pointer_to_mpqmat (SCM pointer);
SCM scm_mpqmat_to_pointer (SCM mpqmat);
SCM scm_matrix_to_mpqmat (SCM A);
SCM scm_mpqmat_to_matrix (SCM mpqmat);

SCM scm_from_mpqmat_t (const mpqmat_t A);
mpqmat_t scm_to_mpqmat_t (SCM mpqmat);

/* Make an uninitialized mpqmat. The ‘permanent’ versions create
   objects (global constants, for instance) that are permanently
   protected from garbage collection. */
SCM scm_c_make_mpqmat (unsigned int m, unsigned int n);
SCM scm_c_make_permanent_mpqmat (unsigned int m, unsigned int n);
mpqmat_t scm_c_make_mpqmat_t (unsigned int m, unsigned int n);
mpqmat_t scm_c_make_permanent_mpqmat_t (unsigned int m, unsigned int n);

/* Take an existing mpqmat_t and convert it to a permanent one. */
mpqmat_t scm_make_mpqmat_t_permanent (const mpqmat_t A);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MATRICES_MPQMAT_H */
