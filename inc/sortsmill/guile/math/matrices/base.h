/*
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

#ifndef _SORTSMILL_GUILE_MATH_MATRICES_BASE_H
#define _SORTSMILL_GUILE_MATH_MATRICES_BASE_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

static inline ssize_t
scm_c_matrix_lbnd0 (scm_t_array_handle *handlep)
{
  return scm_array_handle_dims (handlep)[0].lbnd;
}

static inline ssize_t
scm_c_matrix_lbnd1 (scm_t_array_handle *handlep)
{
  return
    scm_array_handle_dims (handlep)[scm_array_handle_rank (handlep) - 1].lbnd;
}

static inline size_t
scm_c_matrix_dim0 (scm_t_array_handle *handlep)
{
  return (scm_array_handle_rank (handlep) == 1) ? 1 :
    (size_t) (scm_array_handle_dims (handlep)[0].ubnd -
              scm_array_handle_dims (handlep)[0].lbnd) + 1;
}

static inline size_t
scm_c_matrix_dim1 (scm_t_array_handle *handlep)
{
  const size_t i = scm_array_handle_rank (handlep) - 1;
  return (size_t) (scm_array_handle_dims (handlep)[i].ubnd -
                   scm_array_handle_dims (handlep)[i].lbnd) + 1;
}

static inline ssize_t
scm_c_matrix_inc0 (scm_t_array_handle *handlep)
{
  /* The return value is arbitrary if rank is one. */
  return (scm_array_handle_rank (handlep) == 1) ? 0 :
    scm_array_handle_dims (handlep)[0].inc;
}

static inline ssize_t
scm_c_matrix_inc1 (scm_t_array_handle *handlep)
{
  return
    scm_array_handle_dims (handlep)[scm_array_handle_rank (handlep) - 1].inc;
}

void assert_valid_scm_matrix_indices (const char *who, SCM A,
                                      ssize_t i, ssize_t j,
                                      ssize_t i_base, ssize_t j_base,
                                      size_t i_dim, size_t j_dim);

SCM scm_c_matrix_0ref (SCM A, ssize_t i, ssize_t j);
SCM scm_matrix_0ref (SCM A, SCM i, SCM j);
SCM scm_c_matrix_1ref (SCM A, ssize_t i, ssize_t j);
SCM scm_matrix_1ref (SCM A, SCM i, SCM j);
SCM scm_c_matrix_ref (SCM A, ssize_t i, ssize_t j);
SCM scm_matrix_ref (SCM A, SCM i, SCM j);

SCM scm_c_matrix_0set_x (SCM A, ssize_t i, ssize_t j, SCM x);
SCM scm_matrix_0set_x (SCM A, SCM i, SCM j, SCM x);
SCM scm_c_matrix_1set_x (SCM A, ssize_t i, ssize_t j, SCM x);
SCM scm_matrix_1set_x (SCM A, SCM i, SCM j, SCM x);
SCM scm_c_matrix_set_x (SCM A, ssize_t i, ssize_t j, SCM x);
SCM scm_matrix_set_x (SCM A, SCM i, SCM j, SCM x);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MATRICES_BASE_H */
