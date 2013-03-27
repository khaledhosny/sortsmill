#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <sortsmill/guile.h>
#include <intl.h>

//-------------------------------------------------------------------------

static void
raise_row_index_outside_bounds (const char *who, SCM A, ssize_t i,
                                ssize_t i_base, size_t i_dim)
{
  SCM args = scm_list_3 (scm_from_ssize_t (i),
                         scm_from_ssize_t (i_base),
                         scm_from_ssize_t (i_base + (ssize_t) (i_dim - 1)));
  SCM message =
    scm_c_locale_sformat (_("matrix error: "
                            "the row index ~a is outside bounds [~a,~a]"),
                          args);
  rnrs_raise_condition (scm_list_4
                        (rnrs_make_assertion_violation (),
                         rnrs_c_make_who_condition (who),
                         rnrs_make_message_condition (message),
                         rnrs_make_irritants_condition
                         (scm_list_2 (A, scm_from_ssize_t (i)))));
}

static void
raise_column_index_outside_bounds (const char *who, SCM A, ssize_t j,
                                   ssize_t j_base, size_t j_dim)
{
  SCM args = scm_list_3 (scm_from_ssize_t (j),
                         scm_from_ssize_t (j_base),
                         scm_from_ssize_t (j_base + (ssize_t) (j_dim - 1)));
  SCM message =
    scm_c_locale_sformat (_("matrix error: "
                            "the column index ~a is outside bounds [~a,~a]"),
                          args);
  rnrs_raise_condition (scm_list_4
                        (rnrs_make_assertion_violation (),
                         rnrs_c_make_who_condition (who),
                         rnrs_make_message_condition (message),
                         rnrs_make_irritants_condition
                         (scm_list_2 (A, scm_from_ssize_t (j)))));
}

static inline void
assert_row_index_inside_bounds (const char *who, SCM A, ssize_t i,
                                ssize_t i_base, size_t i_dim)
{
  if (i < i_base || i_base + (ssize_t) i_dim <= i)
    raise_row_index_outside_bounds (who, A, i, i_base, i_dim);
}

static inline void
assert_column_index_inside_bounds (const char *who, SCM A, ssize_t j,
                                   ssize_t j_base, size_t j_dim)
{
  if (j < j_base || j_base + (ssize_t) j_dim <= j)
    raise_column_index_outside_bounds (who, A, j, j_base, j_dim);
}

VISIBLE void
assert_valid_scm_matrix_indices (const char *who, SCM A,
                                 ssize_t i, ssize_t j,
                                 ssize_t i_base, ssize_t j_base,
                                 size_t i_dim, size_t j_dim)
{
  assert_row_index_inside_bounds (who, A, i, i_base, i_dim);
  assert_column_index_inside_bounds (who, A, j, j_base, j_dim);
}

//-------------------------------------------------------------------------

static SCM
entry_ref (const char *who, SCM A, scm_t_array_handle *handlep_A,
           ssize_t i, ssize_t j, ssize_t i_base, ssize_t j_base)
{
  assert_valid_scm_matrix_indices (who, A, i, j, i_base, j_base,
                                   scm_c_matrix_dim0 (handlep_A),
                                   scm_c_matrix_dim1 (handlep_A));

  const SCM *_A = scm_array_handle_elements (handlep_A);
  return _A[(i - i_base) * scm_c_matrix_inc0 (handlep_A) +
            (j - j_base) * scm_c_matrix_inc1 (handlep_A)];
}

VISIBLE SCM
scm_c_matrix_0ref (SCM A, ssize_t i, ssize_t j)
{
  const char *who = "scm_c_matrix_0ref";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  SCM entry = entry_ref (who, A, &handle_A, i, j, 0, 0);

  scm_dynwind_end ();

  return entry;
}

VISIBLE SCM
scm_matrix_0ref (SCM A, SCM i, SCM j)
{
  return scm_c_matrix_0ref (A, scm_to_ssize_t (i), scm_to_ssize_t (j));
}

VISIBLE SCM
scm_c_matrix_1ref (SCM A, ssize_t i, ssize_t j)
{
  const char *who = "scm_c_matrix_1ref";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  SCM entry = entry_ref (who, A, &handle_A, i, j, 1, 1);

  scm_dynwind_end ();

  return entry;
}

VISIBLE SCM
scm_matrix_1ref (SCM A, SCM i, SCM j)
{
  return scm_c_matrix_1ref (A, scm_to_ssize_t (i), scm_to_ssize_t (j));
}

VISIBLE SCM
scm_c_matrix_ref (SCM A, ssize_t i, ssize_t j)
{
  const char *who = "scm_c_matrix_ref";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  SCM entry = entry_ref (who, A, &handle_A, i, j,
                         scm_c_matrix_lbnd0 (&handle_A),
                         scm_c_matrix_lbnd1 (&handle_A));

  scm_dynwind_end ();

  return entry;
}

VISIBLE SCM
scm_matrix_ref (SCM A, SCM i, SCM j)
{
  return scm_c_matrix_ref (A, scm_to_ssize_t (i), scm_to_ssize_t (j));
}

//-------------------------------------------------------------------------

void init_guile_sortsmill_math_matrices_base (void);

VISIBLE void
init_guile_sortsmill_math_matrices_base (void)
{
  scm_c_define_gsubr ("matrix-0ref", 3, 0, 0, scm_matrix_0ref);
  scm_c_define_gsubr ("matrix-1ref", 3, 0, 0, scm_matrix_1ref);
  scm_c_define_gsubr ("matrix-ref", 3, 0, 0, scm_matrix_ref);
}

//-------------------------------------------------------------------------
