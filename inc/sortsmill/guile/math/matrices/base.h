/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
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

bool scm_array_handle_is_matrix (scm_t_array_handle *handlep);
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

SCM scm_matrix_0based (SCM A);
SCM scm_matrix_1based (SCM A);
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
SCM scm_c_matrix_0block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n);
SCM scm_c_matrix_1block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n);
SCM scm_c_matrix_block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n);
SCM scm_matrix_0block (SCM A, SCM i, SCM j, SCM m, SCM n);
SCM scm_matrix_1block (SCM A, SCM i, SCM j, SCM m, SCM n);
SCM scm_matrix_block (SCM A, SCM i, SCM j, SCM m, SCM n);
SCM scm_matrix_transpose (SCM A);
SCM scm_matrix_diagonal (SCM A);
SCM scm_matrix_as_rank2_array (SCM v);
SCM scm_matrix_as_min_rank_array (SCM A);
SCM scm_row_matrix_to_vector (SCM A);

SCM scm_matrix_1x1_to_scalar (SCM A);
SCM scm_scalar_to_matrix (SCM x);
SCM scm_scalar_to_u8matrix (SCM x);
SCM scm_scalar_to_s8matrix (SCM x);
SCM scm_scalar_to_u16matrix (SCM x);
SCM scm_scalar_to_s16matrix (SCM x);
SCM scm_scalar_to_u32matrix (SCM x);
SCM scm_scalar_to_s32matrix (SCM x);
SCM scm_scalar_to_u64matrix (SCM x);
SCM scm_scalar_to_s64matrix (SCM x);
SCM scm_scalar_to_f32matrix (SCM x);
SCM scm_scalar_to_f64matrix (SCM x);
SCM scm_scalar_to_c32matrix (SCM x);
SCM scm_scalar_to_c64matrix (SCM x);
SCM scm_scalar_to_typed_matrix (SCM type, SCM x);

SCM scm_c_zero_matrix (size_t m, size_t n);
SCM scm_c_zero_u8matrix (size_t m, size_t n);
SCM scm_c_zero_s8matrix (size_t m, size_t n);
SCM scm_c_zero_u16matrix (size_t m, size_t n);
SCM scm_c_zero_s16matrix (size_t m, size_t n);
SCM scm_c_zero_u32matrix (size_t m, size_t n);
SCM scm_c_zero_s32matrix (size_t m, size_t n);
SCM scm_c_zero_u64matrix (size_t m, size_t n);
SCM scm_c_zero_s64matrix (size_t m, size_t n);
SCM scm_c_zero_f32matrix (size_t m, size_t n);
SCM scm_c_zero_f64matrix (size_t m, size_t n);
SCM scm_c_zero_c32matrix (size_t m, size_t n);
SCM scm_c_zero_c64matrix (size_t m, size_t n);
SCM scm_c_typed_zero_matrix (SCM type, size_t m, size_t n);
SCM scm_zero_matrix (SCM m, SCM n);
SCM scm_zero_u8matrix (SCM m, SCM n);
SCM scm_zero_s8matrix (SCM m, SCM n);
SCM scm_zero_u16matrix (SCM m, SCM n);
SCM scm_zero_s16matrix (SCM m, SCM n);
SCM scm_zero_u32matrix (SCM m, SCM n);
SCM scm_zero_s32matrix (SCM m, SCM n);
SCM scm_zero_u64matrix (SCM m, SCM n);
SCM scm_zero_s64matrix (SCM m, SCM n);
SCM scm_zero_f32matrix (SCM m, SCM n);
SCM scm_zero_f64matrix (SCM m, SCM n);
SCM scm_zero_c32matrix (SCM m, SCM n);
SCM scm_zero_c64matrix (SCM m, SCM n);
SCM scm_typed_zero_matrix (SCM type, SCM m, SCM n);

SCM scm_c_filled_matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_u8matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_s8matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_u16matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_s16matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_u32matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_s32matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_u64matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_s64matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_f32matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_f64matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_c32matrix (SCM fill, size_t m, size_t n);
SCM scm_c_filled_c64matrix (SCM fill, size_t m, size_t n);
SCM scm_c_typed_filled_matrix (SCM type, SCM fill, size_t m, size_t n);
SCM scm_filled_matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_u8matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_s8matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_u16matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_s16matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_u32matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_s32matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_u64matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_s64matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_f32matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_f64matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_c32matrix (SCM fill, SCM m, SCM n);
SCM scm_filled_c64matrix (SCM fill, SCM m, SCM n);
SCM scm_typed_filled_matrix (SCM type, SCM fill, SCM m, SCM n);

SCM scm_c_I_matrix (size_t m, size_t n);
SCM scm_c_I_u8matrix (size_t m, size_t n);
SCM scm_c_I_s8matrix (size_t m, size_t n);
SCM scm_c_I_u16matrix (size_t m, size_t n);
SCM scm_c_I_s16matrix (size_t m, size_t n);
SCM scm_c_I_u32matrix (size_t m, size_t n);
SCM scm_c_I_s32matrix (size_t m, size_t n);
SCM scm_c_I_u64matrix (size_t m, size_t n);
SCM scm_c_I_s64matrix (size_t m, size_t n);
SCM scm_c_I_f32matrix (size_t m, size_t n);
SCM scm_c_I_f64matrix (size_t m, size_t n);
SCM scm_c_I_c32matrix (size_t m, size_t n);
SCM scm_c_I_c64matrix (size_t m, size_t n);
SCM scm_c_typed_I_matrix (SCM type, size_t m, size_t n);
SCM scm_I_matrix (SCM m, SCM n);
SCM scm_I_u8matrix (SCM m, SCM n);
SCM scm_I_s8matrix (SCM m, SCM n);
SCM scm_I_u16matrix (SCM m, SCM n);
SCM scm_I_s16matrix (SCM m, SCM n);
SCM scm_I_u32matrix (SCM m, SCM n);
SCM scm_I_s32matrix (SCM m, SCM n);
SCM scm_I_u64matrix (SCM m, SCM n);
SCM scm_I_s64matrix (SCM m, SCM n);
SCM scm_I_f32matrix (SCM m, SCM n);
SCM scm_I_f64matrix (SCM m, SCM n);
SCM scm_I_c32matrix (SCM m, SCM n);
SCM scm_I_c64matrix (SCM m, SCM n);
SCM scm_typed_I_matrix (SCM type, SCM m, SCM n);

SCM scm_c_scalar_matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_u8matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_s8matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_u16matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_s16matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_u32matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_s32matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_u64matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_s64matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_f32matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_f64matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_c32matrix (SCM x, size_t m, size_t n);
SCM scm_c_scalar_c64matrix (SCM x, size_t m, size_t n);
SCM scm_c_typed_scalar_matrix (SCM type, SCM x, size_t m, size_t n);
SCM scm_scalar_matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_u8matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_s8matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_u16matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_s16matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_u32matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_s32matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_u64matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_s64matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_f32matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_f64matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_c32matrix (SCM x, SCM m, SCM n);
SCM scm_scalar_c64matrix (SCM x, SCM m, SCM n);
SCM scm_typed_scalar_matrix (SCM type, SCM x, SCM m, SCM n);

SCM scm_row_matrix_to_diagonal_matrix (SCM diag_as_row);

SCM scm_matrix_copy (SCM A);

SCM scm_c_matrix_mapped_to_typed_matrix (SCM type, SCM A, SCM (*proc) (SCM));
SCM scm_matrix_mapped_to_typed_matrix (SCM type, SCM A, SCM proc);
SCM scm_matrix_to_u8matrix (SCM A);
SCM scm_matrix_to_s8matrix (SCM A);
SCM scm_matrix_to_u16matrix (SCM A);
SCM scm_matrix_to_s16matrix (SCM A);
SCM scm_matrix_to_u32matrix (SCM A);
SCM scm_matrix_to_s32matrix (SCM A);
SCM scm_matrix_to_u64matrix (SCM A);
SCM scm_matrix_to_s64matrix (SCM A);
SCM scm_matrix_to_f32matrix (SCM A);
SCM scm_matrix_to_f64matrix (SCM A);
SCM scm_matrix_to_c32matrix (SCM A);
SCM scm_matrix_to_c64matrix (SCM A);
SCM scm_matrix_to_matrix (SCM A);
SCM scm_matrix_to_typed_matrix (SCM type, SCM A);
SCM scm_matrix_exact_to_inexact (SCM A);
SCM scm_matrix_inexact_to_exact (SCM A);

bool scm_c_for_all_in_matrix (bool sense, SCM pred, SCM A);
bool scm_c_for_all_in_matrix_0ij (bool sense, SCM pred, SCM A);
bool scm_c_for_all_in_matrix_1ij (bool sense, SCM pred, SCM A);
bool scm_c_for_all_in_matrix_ij (bool sense, SCM pred, SCM A);
SCM scm_for_all_in_matrix (SCM pred, SCM A);
SCM scm_for_all_in_matrix_0ij (SCM pred, SCM A);
SCM scm_for_all_in_matrix_1ij (SCM pred, SCM A);
SCM scm_for_all_in_matrix_ij (SCM pred, SCM A);
SCM scm_exists_in_matrix (SCM pred, SCM A);
SCM scm_exists_in_matrix_0ij (SCM pred, SCM A);
SCM scm_exists_in_matrix_1ij (SCM pred, SCM A);
SCM scm_exists_in_matrix_ij (SCM pred, SCM A);
bool scm_is_zero_matrix (SCM A);
SCM scm_zero_matrix_p (SCM A);
bool scm_is_I_matrix (SCM A);
SCM scm_I_matrix_p (SCM A);
SCM scm_matrix_num_eq_p (SCM A, SCM B);
SCM scm_matrix_eq_p (SCM A, SCM B);
SCM scm_matrix_eqv_p (SCM A, SCM B);
SCM scm_matrix_equal_p (SCM A, SCM B);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MATRICES_BASE_H */
