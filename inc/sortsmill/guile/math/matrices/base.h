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
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

static inline ssize_t
scm_c_matrix_rows_lbnd (scm_t_array_handle *handlep)
{
  return scm_array_handle_dims (handlep)[0].lbnd;
}

static inline ssize_t
scm_c_matrix_columns_lbnd (scm_t_array_handle *handlep)
{
  return
    scm_array_handle_dims (handlep)[scm_array_handle_rank (handlep) - 1].lbnd;
}

static inline size_t
scm_c_matrix_numrows (scm_t_array_handle *handlep)
{
  return (scm_array_handle_rank (handlep) == 1) ? 1 :
    (size_t) (scm_array_handle_dims (handlep)[0].ubnd -
              scm_array_handle_dims (handlep)[0].lbnd) + 1;
}

static inline size_t
scm_c_matrix_numcols (scm_t_array_handle *handlep)
{
  const size_t i = scm_array_handle_rank (handlep) - 1;
  return (size_t) (scm_array_handle_dims (handlep)[i].ubnd -
                   scm_array_handle_dims (handlep)[i].lbnd) + 1;
}

static inline ssize_t
scm_c_matrix_row_inc (scm_t_array_handle *handlep)
{
  /* The return value is arbitrary if rank is one. */
  return (scm_array_handle_rank (handlep) == 1) ? 0 :
    scm_array_handle_dims (handlep)[0].inc;
}

static inline ssize_t
scm_c_matrix_column_inc (scm_t_array_handle *handlep)
{
  return
    scm_array_handle_dims (handlep)[scm_array_handle_rank (handlep) - 1].inc;
}

void assert_valid_scm_matrix_indices (const char *who, SCM A,
                                      ssize_t i, ssize_t j,
                                      ssize_t i_base, ssize_t j_base,
                                      size_t i_dim, size_t j_dim);
void assert_is_matrix (SCM who, SCM A);
void assert_array_handle_is_matrix (const char *who, SCM A,
                                    scm_t_array_handle *handlep_A);

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

bool scm_is_matrix (SCM A);
void scm_c_matrix_shape (SCM A, ssize_t *rows_lbnd, ssize_t *rows_ubnd,
                         ssize_t *columns_lbnd, ssize_t *columns_ubnd);
void scm_c_matrix_dimensions (SCM A, size_t *row_count, size_t *column_count);
size_t scm_c_matrix_row_count (SCM A);
size_t scm_c_matrix_column_count (SCM A);
size_t scm_c_row_matrix_size (SCM A);
size_t scm_c_column_matrix_size (SCM A);
bool scm_is_square_matrix (SCM A);
bool scm_are_conformable_for_matrix_product (SCM A, SCM B);
bool scm_are_conformable_for_matrix_sum (SCM A, SCM B);
SCM scm_matrix_p (SCM A);
SCM scm_matrix_shape (SCM A);
SCM scm_matrix_dimensions (SCM A);
SCM scm_matrix_row_count (SCM A);
SCM scm_matrix_column_count (SCM A);
SCM scm_row_matrix_size (SCM A);
SCM scm_column_matrix_size (SCM A);
SCM scm_square_matrix_p (SCM A);
SCM scm_conformable_for_matrix_product_p (SCM A, SCM B);
SCM scm_conformable_for_matrix_sum_p (SCM A, SCM B);

SCM scm_c_matrix_0row (SCM A, ssize_t i);
SCM scm_c_matrix_1row (SCM A, ssize_t i);
SCM scm_c_matrix_row (SCM A, ssize_t i);
SCM scm_matrix_0row (SCM A, SCM i);
SCM scm_matrix_1row (SCM A, SCM i);
SCM scm_matrix_row (SCM A, SCM i);
SCM scm_c_matrix_0column_transpose (SCM A, ssize_t i);
SCM scm_c_matrix_1column_transpose (SCM A, ssize_t i);
SCM scm_c_matrix_column_transpose (SCM A, ssize_t i);
SCM scm_matrix_0column_transpose (SCM A, SCM i);
SCM scm_matrix_1column_transpose (SCM A, SCM i);
SCM scm_matrix_column_transpose (SCM A, SCM i);
SCM scm_c_matrix_0column (SCM A, ssize_t i);
SCM scm_c_matrix_1column (SCM A, ssize_t i);
SCM scm_c_matrix_column (SCM A, ssize_t i);
SCM scm_matrix_0column (SCM A, SCM i);
SCM scm_matrix_1column (SCM A, SCM i);
SCM scm_matrix_column (SCM A, SCM i);
SCM scm_vector_to_matrix (SCM v);
SCM scm_row_matrix_to_vector (SCM A);
SCM scm_matrix_transpose (SCM A);
SCM scm_matrix_diagonal (SCM A);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MATRICES_BASE_H */
