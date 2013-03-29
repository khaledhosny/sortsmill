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
#include <sortsmill/initialized_global_constants.h>
#include <intl.h>
#include <stdint.h>
#include <assert.h>

//-------------------------------------------------------------------------

static void
raise_not_a_matrix (SCM who, SCM A)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_c_make_message_condition (_("matrix error: "
                                       "not a (non-empty) matrix")),
      rnrs_make_irritants_condition (scm_list_1 (A))));
}

static void
raise_not_a_row_matrix (SCM who, SCM A)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_c_make_message_condition (_("matrix error: "
                                       "not a (non-empty) row matrix")),
      rnrs_make_irritants_condition (scm_list_1 (A))));
}

static void
raise_not_a_column_matrix (SCM who, SCM A)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_c_make_message_condition (_("matrix error: "
                                       "not a (non-empty) column matrix")),
      rnrs_make_irritants_condition (scm_list_1 (A))));
}

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

// FIXME: This should be reusable or have a generally reusable
// equivalent.
static inline void
assert_row_index_inside_bounds (const char *who, SCM A, ssize_t i,
                                ssize_t i_base, size_t i_dim)
{
  if (i < i_base || i_base + (ssize_t) i_dim <= i)
    raise_row_index_outside_bounds (who, A, i, i_base, i_dim);
}

// FIXME: This should be reusable or have a generally reusable
// equivalent.
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

VISIBLE void
assert_is_matrix (SCM who, SCM A)
{
  if (!scm_is_matrix (A))
    raise_not_a_matrix (who, A);
}

VISIBLE void
assert_array_handle_is_matrix (const char *who, SCM A,
                               scm_t_array_handle *handlep_A)
{
  bool is_a_matrix;

  switch (scm_array_handle_rank (handlep_A))
    {
    case 1:
      is_a_matrix =
        (scm_array_handle_dims (handlep_A)[0].lbnd <=
         scm_array_handle_dims (handlep_A)[0].ubnd);
      break;

    case 2:
      is_a_matrix =
        ((scm_array_handle_dims (handlep_A)[0].lbnd <=
          scm_array_handle_dims (handlep_A)[0].ubnd)
         && (scm_array_handle_dims (handlep_A)[1].lbnd <=
             scm_array_handle_dims (handlep_A)[1].ubnd));
      break;

    default:
      is_a_matrix = false;
      break;
    }

  if (!is_a_matrix)
    raise_not_a_matrix (scm_from_utf8_string (who), A);
}

//-------------------------------------------------------------------------

#define _SCM_MATRIX_ENTRY_REF(NAME, TYPE, ELEMTYPE, FROM_TYPE)          \
  SCM                                                                   \
  NAME (scm_t_array_handle *handlep_A, ssize_t i, ssize_t j)            \
  {                                                                     \
    const TYPE *_A = scm_array_handle_##ELEMTYPE##elements (handlep_A); \
    return FROM_TYPE (_A[i * scm_c_matrix_row_inc (handlep_A) +         \
                         j * scm_c_matrix_column_inc (handlep_A)]);     \
  }

#define _SCM_MATRIX_COMPLEX_ENTRY_REF(NAME, TYPE, ELEMTYPE)             \
  SCM                                                                   \
  NAME (scm_t_array_handle *handlep_A, ssize_t i, ssize_t j)            \
  {                                                                     \
    const TYPE *_A = scm_array_handle_##ELEMTYPE##elements (handlep_A); \
    const TYPE *v = &_A[2 * i * scm_c_matrix_row_inc (handlep_A) +      \
                        2 * j * scm_c_matrix_column_inc (handlep_A)];   \
    return scm_c_make_rectangular (v[0], v[1]);                         \
  }

static _SCM_MATRIX_ENTRY_REF (nonuniform_entry_ref, SCM,
                              /* empty */ , /* empty */ );
static _SCM_MATRIX_ENTRY_REF (u8_entry_ref, uint8_t, u8_, scm_from_uint8);
static _SCM_MATRIX_ENTRY_REF (s8_entry_ref, int8_t, s8_, scm_from_int8);
static _SCM_MATRIX_ENTRY_REF (u16_entry_ref, uint16_t, u16_, scm_from_uint16);
static _SCM_MATRIX_ENTRY_REF (s16_entry_ref, int16_t, s16_, scm_from_int16);
static _SCM_MATRIX_ENTRY_REF (u32_entry_ref, uint32_t, u32_, scm_from_uint32);
static _SCM_MATRIX_ENTRY_REF (s32_entry_ref, int32_t, s32_, scm_from_int32);
static _SCM_MATRIX_ENTRY_REF (u64_entry_ref, uint64_t, u64_, scm_from_uint64);
static _SCM_MATRIX_ENTRY_REF (s64_entry_ref, int64_t, s64_, scm_from_int64);
static _SCM_MATRIX_ENTRY_REF (f32_entry_ref, float, f32_, scm_from_double);
static _SCM_MATRIX_ENTRY_REF (f64_entry_ref, double, f64_, scm_from_double);
static _SCM_MATRIX_COMPLEX_ENTRY_REF (c32_entry_ref, float, c32_);
static _SCM_MATRIX_COMPLEX_ENTRY_REF (c64_entry_ref, double, c64_);

typedef SCM _scm_matrix_entry_ref_t (scm_t_array_handle *handlep_A,
                                     ssize_t i, ssize_t j);

static _scm_matrix_entry_ref_t *_entry_ref[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = nonuniform_entry_ref,
  [_FF_INDEX_ARRAY_U8] = u8_entry_ref,
  [_FF_INDEX_ARRAY_S8] = s8_entry_ref,
  [_FF_INDEX_ARRAY_U16] = u16_entry_ref,
  [_FF_INDEX_ARRAY_S16] = s16_entry_ref,
  [_FF_INDEX_ARRAY_U32] = u32_entry_ref,
  [_FF_INDEX_ARRAY_S32] = s32_entry_ref,
  [_FF_INDEX_ARRAY_U64] = u64_entry_ref,
  [_FF_INDEX_ARRAY_S64] = s64_entry_ref,
  [_FF_INDEX_ARRAY_F32] = f32_entry_ref,
  [_FF_INDEX_ARRAY_F64] = f64_entry_ref,
  [_FF_INDEX_ARRAY_C32] = c32_entry_ref,
  [_FF_INDEX_ARRAY_C64] = c64_entry_ref
};

static inline SCM
entry_ref (const char *who, SCM A, scm_t_array_handle *handlep_A,
           ssize_t i, ssize_t j, ssize_t i_base, ssize_t j_base)
{
  assert_valid_scm_matrix_indices (who, A, i, j, i_base, j_base,
                                   scm_c_matrix_numrows (handlep_A),
                                   scm_c_matrix_numcols (handlep_A));
  const scm_t_array_type_index t =
    scm_array_handle_to_array_type_index (handlep_A);
  return _entry_ref[t] (handlep_A, i - i_base, j - j_base);
}

VISIBLE SCM
scm_c_matrix_0ref (SCM A, ssize_t i, ssize_t j)
{
  const char *who = "scm_c_matrix_0ref";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

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
  assert_array_handle_is_matrix (who, A, &handle_A);

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
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM entry = entry_ref (who, A, &handle_A, i, j,
                         scm_c_matrix_rows_lbnd (&handle_A),
                         scm_c_matrix_columns_lbnd (&handle_A));

  scm_dynwind_end ();

  return entry;
}

VISIBLE SCM
scm_matrix_ref (SCM A, SCM i, SCM j)
{
  return scm_c_matrix_ref (A, scm_to_ssize_t (i), scm_to_ssize_t (j));
}

//-------------------------------------------------------------------------

#define _SCM_MATRIX_ENTRY_SET_X(NAME, TYPE, ELEMTYPE, TO_TYPE)          \
  void                                                                  \
  NAME (scm_t_array_handle *handlep_A, ssize_t i, ssize_t j, SCM v)     \
  {                                                                     \
    TYPE *_A =                                                          \
      scm_array_handle_##ELEMTYPE##writable_elements (handlep_A);       \
    _A[i * scm_c_matrix_row_inc (handlep_A) +                           \
       j * scm_c_matrix_column_inc (handlep_A)] = TO_TYPE (v);          \
  }

#define _SCM_MATRIX_COMPLEX_ENTRY_SET_X(NAME, TYPE, ELEMTYPE)           \
  void                                                                  \
  NAME (scm_t_array_handle *handlep_A, ssize_t i, ssize_t j, SCM v)     \
  {                                                                     \
    TYPE *_A =                                                          \
      scm_array_handle_##ELEMTYPE##writable_elements (handlep_A);       \
    TYPE *a = &_A[2 * i * scm_c_matrix_row_inc (handlep_A) +            \
                  2 * j * scm_c_matrix_column_inc (handlep_A)];         \
    a[0] = scm_to_double (scm_real_part (v));                           \
    a[1] = scm_to_double (scm_imag_part (v));                           \
  }

static _SCM_MATRIX_ENTRY_SET_X (nonuniform_entry_set_x, SCM,
                                /* empty */ , /* empty */ );
static _SCM_MATRIX_ENTRY_SET_X (u8_entry_set_x, uint8_t, u8_, scm_to_uint8);
static _SCM_MATRIX_ENTRY_SET_X (s8_entry_set_x, int8_t, s8_, scm_to_int8);
static _SCM_MATRIX_ENTRY_SET_X (u16_entry_set_x, uint16_t, u16_, scm_to_uint16);
static _SCM_MATRIX_ENTRY_SET_X (s16_entry_set_x, int16_t, s16_, scm_to_int16);
static _SCM_MATRIX_ENTRY_SET_X (u32_entry_set_x, uint32_t, u32_, scm_to_uint32);
static _SCM_MATRIX_ENTRY_SET_X (s32_entry_set_x, int32_t, s32_, scm_to_int32);
static _SCM_MATRIX_ENTRY_SET_X (u64_entry_set_x, uint64_t, u64_, scm_to_uint64);
static _SCM_MATRIX_ENTRY_SET_X (s64_entry_set_x, int64_t, s64_, scm_to_int64);
static _SCM_MATRIX_ENTRY_SET_X (f32_entry_set_x, float, f32_, scm_to_double);
static _SCM_MATRIX_ENTRY_SET_X (f64_entry_set_x, double, f64_, scm_to_double);
static _SCM_MATRIX_COMPLEX_ENTRY_SET_X (c32_entry_set_x, float, c32_);
static _SCM_MATRIX_COMPLEX_ENTRY_SET_X (c64_entry_set_x, double, c64_);

typedef void _scm_matrix_entry_set_x_t (scm_t_array_handle *handlep_A,
                                        ssize_t i, ssize_t j, SCM v);

static _scm_matrix_entry_set_x_t *_entry_set_x[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = nonuniform_entry_set_x,
  [_FF_INDEX_ARRAY_U8] = u8_entry_set_x,
  [_FF_INDEX_ARRAY_S8] = s8_entry_set_x,
  [_FF_INDEX_ARRAY_U16] = u16_entry_set_x,
  [_FF_INDEX_ARRAY_S16] = s16_entry_set_x,
  [_FF_INDEX_ARRAY_U32] = u32_entry_set_x,
  [_FF_INDEX_ARRAY_S32] = s32_entry_set_x,
  [_FF_INDEX_ARRAY_U64] = u64_entry_set_x,
  [_FF_INDEX_ARRAY_S64] = s64_entry_set_x,
  [_FF_INDEX_ARRAY_F32] = f32_entry_set_x,
  [_FF_INDEX_ARRAY_F64] = f64_entry_set_x,
  [_FF_INDEX_ARRAY_C32] = c32_entry_set_x,
  [_FF_INDEX_ARRAY_C64] = c64_entry_set_x
};

static inline void
entry_set_x (const char *who, SCM A, scm_t_array_handle *handlep_A,
             ssize_t i, ssize_t j, ssize_t i_base, ssize_t j_base, SCM v)
{
  assert_valid_scm_matrix_indices (who, A, i, j, i_base, j_base,
                                   scm_c_matrix_numrows (handlep_A),
                                   scm_c_matrix_numcols (handlep_A));
  const scm_t_array_type_index t =
    scm_array_handle_to_array_type_index (handlep_A);
  _entry_set_x[t] (handlep_A, i - i_base, j - j_base, v);
}

VISIBLE SCM
scm_c_matrix_0set_x (SCM A, ssize_t i, ssize_t j, SCM v)
{
  const char *who = "scm_c_matrix_0set_x";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  entry_set_x (who, A, &handle_A, i, j, 0, 0, v);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_matrix_0set_x (SCM A, SCM i, SCM j, SCM v)
{
  return scm_c_matrix_0set_x (A, scm_to_ssize_t (i), scm_to_ssize_t (j), v);
}

VISIBLE SCM
scm_c_matrix_1set_x (SCM A, ssize_t i, ssize_t j, SCM v)
{
  const char *who = "scm_c_matrix_1set_x";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  entry_set_x (who, A, &handle_A, i, j, 1, 1, v);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_matrix_1set_x (SCM A, SCM i, SCM j, SCM v)
{
  return scm_c_matrix_1set_x (A, scm_to_ssize_t (i), scm_to_ssize_t (j), v);
}

VISIBLE SCM
scm_c_matrix_set_x (SCM A, ssize_t i, ssize_t j, SCM v)
{
  const char *who = "scm_c_matrix_set_x";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  entry_set_x (who, A, &handle_A, i, j,
               scm_c_matrix_rows_lbnd (&handle_A),
               scm_c_matrix_columns_lbnd (&handle_A), v);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_matrix_set_x (SCM A, SCM i, SCM j, SCM v)
{
  return scm_c_matrix_set_x (A, scm_to_ssize_t (i), scm_to_ssize_t (j), v);
}

//-------------------------------------------------------------------------

VISIBLE bool
scm_is_matrix (SCM A)
{
  bool is_a_matrix;

  if (scm_is_false (scm_array_p (A, SCM_UNDEFINED)))
    is_a_matrix = false;
  else
    {
      scm_t_array_handle handle_A;

      scm_array_get_handle (A, &handle_A);

      const size_t rank = scm_array_handle_rank (&handle_A);

      switch (rank)
        {
        case 1:
          is_a_matrix =
            (scm_array_handle_dims (&handle_A)[0].lbnd <=
             scm_array_handle_dims (&handle_A)[0].ubnd);
          break;

        case 2:
          is_a_matrix =
            ((scm_array_handle_dims (&handle_A)[0].lbnd <=
              scm_array_handle_dims (&handle_A)[0].ubnd)
             && (scm_array_handle_dims (&handle_A)[1].lbnd <=
                 scm_array_handle_dims (&handle_A)[1].ubnd));
          break;

        default:
          is_a_matrix = false;
          break;
        }

      scm_array_handle_release (&handle_A);
    }

  return is_a_matrix;
}

VISIBLE void
scm_c_matrix_shape (SCM A, ssize_t *rows_lbnd, ssize_t *rows_ubnd,
                    ssize_t *columns_lbnd, ssize_t *columns_ubnd)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      *rows_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
      *rows_ubnd = scm_array_handle_dims (&handle_A)[0].lbnd;
      *columns_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
      *columns_ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;
      break;

    case 2:
      *rows_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
      *rows_ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;
      *columns_lbnd = scm_array_handle_dims (&handle_A)[1].lbnd;
      *columns_ubnd = scm_array_handle_dims (&handle_A)[1].ubnd;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string ("scm_c_matrix_shape"), A);
}

VISIBLE void
scm_c_matrix_dimensions (SCM A, size_t *row_count, size_t *column_count)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      *row_count = 1;
      *column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      break;

    case 2:
      *row_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      *column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[1].ubnd -
                  scm_array_handle_dims (&handle_A)[1].lbnd) + 1;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string
                              ("scm_c_matrix_dimensions"), A);
}

VISIBLE size_t
scm_c_matrix_row_count (SCM A)
{
  scm_t_array_handle handle_A;
  size_t row_count;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      row_count = 1;
      break;

    case 2:
      row_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string
                              ("scm_c_matrix_row_count"), A);

  return row_count;
}

VISIBLE size_t
scm_c_matrix_column_count (SCM A)
{
  scm_t_array_handle handle_A;
  size_t column_count;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      break;

    case 2:
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[1].ubnd -
                  scm_array_handle_dims (&handle_A)[1].lbnd) + 1;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string
                              ("scm_c_matrix_column_count"), A);

  return column_count;
}

VISIBLE size_t
scm_c_row_matrix_size (SCM A)
{
  const char *who = "scm_c_row_matrix_size";

  scm_t_array_handle handle_A;
  size_t row_count = SIZE_MAX;
  size_t column_count;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      row_count = 1;
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      break;

    case 2:
      row_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[1].ubnd -
                  scm_array_handle_dims (&handle_A)[1].lbnd) + 1;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string (who), A);

  if (row_count != 1 || column_count == 0)
    raise_not_a_row_matrix (scm_from_utf8_string (who), A);

  return column_count;
}

VISIBLE size_t
scm_c_column_matrix_size (SCM A)
{
  const char *who = "scm_c_column_matrix_size";

  scm_t_array_handle handle_A;
  size_t row_count;
  size_t column_count = SIZE_MAX;

  scm_array_get_handle (A, &handle_A);

  const size_t rank = scm_array_handle_rank (&handle_A);

  switch (rank)
    {
    case 1:
      row_count = 1;
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      break;

    case 2:
      row_count =
        (size_t) (scm_array_handle_dims (&handle_A)[0].ubnd -
                  scm_array_handle_dims (&handle_A)[0].lbnd) + 1;
      column_count =
        (size_t) (scm_array_handle_dims (&handle_A)[1].ubnd -
                  scm_array_handle_dims (&handle_A)[1].lbnd) + 1;
      break;

    default:
      // Do nothing.
      break;
    }

  scm_array_handle_release (&handle_A);

  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_latin1_string (who), A);

  if (column_count != 1 || row_count == 0)
    raise_not_a_column_matrix (scm_from_utf8_string (who), A);

  return row_count;
}

VISIBLE bool
scm_is_square_matrix (SCM A)
{
  size_t row_count;
  size_t column_count;
  scm_c_matrix_dimensions (A, &row_count, &column_count);
  return (0 < row_count && 0 < column_count && row_count == column_count);
}

VISIBLE bool
scm_are_conformable_for_matrix_product (SCM A, SCM B)
{
  size_t row_count_A;
  size_t column_count_A;
  size_t row_count_B;
  size_t column_count_B;

  scm_c_matrix_dimensions (A, &row_count_A, &column_count_A);
  scm_c_matrix_dimensions (B, &row_count_B, &column_count_B);

  return (0 < row_count_A && 0 < column_count_A
          && 0 < row_count_B && 0 < column_count_B
          && column_count_A == row_count_B);
}

VISIBLE bool
scm_are_conformable_for_matrix_sum (SCM A, SCM B)
{
  size_t row_count_A;
  size_t column_count_A;
  size_t row_count_B;
  size_t column_count_B;

  scm_c_matrix_dimensions (A, &row_count_A, &column_count_A);
  scm_c_matrix_dimensions (B, &row_count_B, &column_count_B);

  return (0 < row_count_A && 0 < column_count_A
          && 0 < row_count_B && 0 < column_count_B
          && row_count_A == row_count_B && column_count_A == column_count_B);
}

VISIBLE SCM
scm_matrix_p (SCM A)
{
  return scm_from_bool (scm_is_matrix (A));
}

VISIBLE SCM
scm_matrix_shape (SCM A)
{
  ssize_t rows_lbnd;
  ssize_t rows_ubnd;
  ssize_t columns_lbnd;
  ssize_t columns_ubnd;
  scm_c_matrix_shape (A, &rows_lbnd, &rows_ubnd, &columns_lbnd, &columns_ubnd);
  return scm_list_2 (scm_list_2 (scm_from_ssize_t (rows_lbnd),
                                 scm_from_ssize_t (rows_ubnd)),
                     scm_list_2 (scm_from_ssize_t (columns_lbnd),
                                 scm_from_ssize_t (columns_ubnd)));
}

VISIBLE SCM
scm_matrix_dimensions (SCM A)
{
  size_t row_count;
  size_t column_count;
  scm_c_matrix_dimensions (A, &row_count, &column_count);
  return scm_list_2 (scm_from_size_t (row_count),
                     scm_from_size_t (column_count));
}

VISIBLE SCM
scm_matrix_row_count (SCM A)
{
  return scm_from_size_t (scm_c_matrix_row_count (A));
}

VISIBLE SCM
scm_matrix_column_count (SCM A)
{
  return scm_from_size_t (scm_c_matrix_column_count (A));
}

VISIBLE SCM
scm_row_matrix_size (SCM A)
{
  return scm_from_size_t (scm_c_row_matrix_size (A));
}

VISIBLE SCM
scm_column_matrix_size (SCM A)
{
  return scm_from_size_t (scm_c_column_matrix_size (A));
}

VISIBLE SCM
scm_square_matrix_p (SCM A)
{
  return scm_from_bool (scm_is_square_matrix (A));
}

VISIBLE SCM
scm_conformable_for_matrix_product_p (SCM A, SCM B)
{
  return scm_from_bool (scm_are_conformable_for_matrix_product (A, B));
}

VISIBLE SCM
scm_conformable_for_matrix_sum_p (SCM A, SCM B)
{
  return scm_from_bool (scm_are_conformable_for_matrix_sum (A, B));
}

//-------------------------------------------------------------------------

// FIXME: This seems quite reusable, though perhaps it could have a
// better name, and maybe the code could be compiled.
static void
scm_c_initialize_from_eval_string (SCM *proc, const char *s)
{
  *proc = scm_eval_string (scm_from_utf8_string (s));
}

INITIALIZED_CONSTANT (static, SCM, matrix_row_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (i) (lambda (j) (list i j)))");

INITIALIZED_CONSTANT (static, SCM, vector_row_mapfunc,
                      scm_c_initialize_from_eval_string, "(lambda (i) list)");

INITIALIZED_CONSTANT (static, SCM, matrix_column_transpose_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (j) (lambda (i) (list i j)))");

INITIALIZED_CONSTANT (static, SCM, vector_column_transpose_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (j) (lambda (i) (list i j)))");

INITIALIZED_CONSTANT (static, SCM, vector_to_matrix_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda () (lambda (i j) (list j)))");

static SCM
matrix_row (const char *who, SCM A, scm_t_array_handle *handlep_A,
            ssize_t i, ssize_t i_base)
{
  assert_row_index_inside_bounds (who, A, i, i_base,
                                  scm_c_matrix_numrows (handlep_A));
  const ssize_t lbnd = scm_c_matrix_columns_lbnd (handlep_A);
  const size_t numcols = scm_c_matrix_numcols (handlep_A);
  const ssize_t ubnd = lbnd + (ssize_t) (numcols - 1);
  SCM bounds = scm_list_1 (scm_list_2 (scm_from_ssize_t (lbnd),
                                       scm_from_ssize_t (ubnd)));
  SCM mapfunc_func =
    (scm_array_handle_rank (handlep_A) == 1) ?
    vector_row_mapfunc () : matrix_row_mapfunc ();
  ssize_t row = (i - i_base) + scm_c_matrix_rows_lbnd (handlep_A);
  SCM mapfunc = scm_call_1 (mapfunc_func, scm_from_ssize_t (row));
  return scm_make_shared_array (A, mapfunc, bounds);
}

static SCM
matrix_column_transpose (const char *who, SCM A, scm_t_array_handle *handlep_A,
                         ssize_t j, ssize_t j_base)
{
  assert_column_index_inside_bounds (who, A, j, j_base,
                                     scm_c_matrix_numcols (handlep_A));
  const ssize_t lbnd = scm_c_matrix_rows_lbnd (handlep_A);
  const size_t numrows = scm_c_matrix_numrows (handlep_A);
  const ssize_t ubnd = lbnd + (ssize_t) (numrows - 1);
  SCM bounds = scm_list_1 (scm_list_2 (scm_from_ssize_t (lbnd),
                                       scm_from_ssize_t (ubnd)));
  SCM mapfunc_func =
    (scm_array_handle_rank (handlep_A) == 1) ?
    vector_column_transpose_mapfunc () : matrix_column_transpose_mapfunc ();
  ssize_t column = (j - j_base) + scm_c_matrix_columns_lbnd (handlep_A);
  SCM mapfunc = scm_call_1 (mapfunc_func, scm_from_ssize_t (column));
  return scm_make_shared_array (A, mapfunc, bounds);
}

VISIBLE SCM
scm_c_matrix_0row (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_0row";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM row = matrix_row (who, A, &handle_A, i, 0);

  scm_dynwind_end ();

  return row;
}

VISIBLE SCM
scm_c_matrix_1row (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_1row";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM row = matrix_row (who, A, &handle_A, i, 1);

  scm_dynwind_end ();

  return row;
}

VISIBLE SCM
scm_c_matrix_row (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_row";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM row =
    matrix_row (who, A, &handle_A, i, scm_c_matrix_rows_lbnd (&handle_A));

  scm_dynwind_end ();

  return row;
}

VISIBLE SCM
scm_c_matrix_0column_transpose (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_0column_transpose";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM column_transpose = matrix_column_transpose (who, A, &handle_A, i, 0);

  scm_dynwind_end ();

  return column_transpose;
}

VISIBLE SCM
scm_c_matrix_1column_transpose (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_1column_transpose";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM column_transpose = matrix_column_transpose (who, A, &handle_A, i, 1);

  scm_dynwind_end ();

  return column_transpose;
}

VISIBLE SCM
scm_c_matrix_column_transpose (SCM A, ssize_t i)
{
  const char *who = "scm_c_matrix_column_transpose";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  SCM column_transpose = matrix_column_transpose (who, A, &handle_A, i,
                                                  scm_c_matrix_columns_lbnd
                                                  (&handle_A));

  scm_dynwind_end ();

  return column_transpose;
}

VISIBLE SCM
scm_matrix_0row (SCM A, SCM i)
{
  return scm_c_matrix_0row (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_1row (SCM A, SCM i)
{
  return scm_c_matrix_1row (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_row (SCM A, SCM i)
{
  return scm_c_matrix_row (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_0column_transpose (SCM A, SCM i)
{
  return scm_c_matrix_0column_transpose (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_1column_transpose (SCM A, SCM i)
{
  return scm_c_matrix_1column_transpose (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_column_transpose (SCM A, SCM i)
{
  return scm_c_matrix_column_transpose (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_vector_to_matrix (SCM v)
{
  const char *who = "scm_vector_to_matrix";

  scm_t_array_handle handle_v;

  scm_dynwind_begin (0);

  scm_array_get_handle (v, &handle_v);
  scm_dynwind_array_handle_release (&handle_v);
  assert_array_handle_is_matrix (who, v, &handle_v);

  SCM A;
  switch (scm_array_handle_rank (&handle_v))
    {
    case 1:
      {
        ssize_t lbnd = scm_array_handle_dims (&handle_v)[0].lbnd;
        SCM scm_lbnd = scm_from_ssize_t (lbnd);
        ssize_t ubnd = scm_array_handle_dims (&handle_v)[0].ubnd;
        SCM scm_ubnd = scm_from_ssize_t (ubnd);
        SCM bounds = scm_list_2 (scm_list_2 (scm_lbnd, scm_lbnd),
                                 scm_list_2 (scm_lbnd, scm_ubnd));
        SCM mapfunc = scm_call_0 (vector_to_matrix_mapfunc ());
        A = scm_make_shared_array (v, mapfunc, bounds);
      }
      break;

    case 2:
      A = v;
      break;

    default:
      assert (false);
    }

  scm_dynwind_end ();

  return A;
}

VISIBLE SCM
scm_row_matrix_to_vector (SCM A)
{
  if (scm_c_matrix_row_count (A) != 1)
    raise_not_a_row_matrix (scm_from_utf8_string ("scm_row_matrix_to_vector"),
                            A);
  return scm_c_matrix_1row (A, 1);
}

//-------------------------------------------------------------------------

void init_guile_sortsmill_math_matrices_base (void);

VISIBLE void
init_guile_sortsmill_math_matrices_base (void)
{
  scm_c_define_gsubr ("matrix-0ref", 3, 0, 0, scm_matrix_0ref);
  scm_c_define_gsubr ("matrix-1ref", 3, 0, 0, scm_matrix_1ref);
  scm_c_define_gsubr ("matrix-ref", 3, 0, 0, scm_matrix_ref);

  scm_c_define_gsubr ("matrix-0set!", 4, 0, 0, scm_matrix_0set_x);
  scm_c_define_gsubr ("matrix-1set!", 4, 0, 0, scm_matrix_1set_x);
  scm_c_define_gsubr ("matrix-set!", 4, 0, 0, scm_matrix_set_x);

  scm_c_define_gsubr ("matrix?", 1, 0, 0, scm_matrix_p);
  scm_c_define_gsubr ("matrix-shape", 1, 0, 0, scm_matrix_shape);
  scm_c_define_gsubr ("matrix-dimensions", 1, 0, 0, scm_matrix_dimensions);
  scm_c_define_gsubr ("matrix-row-count", 1, 0, 0, scm_matrix_row_count);
  scm_c_define_gsubr ("matrix-column-count", 1, 0, 0, scm_matrix_column_count);
  scm_c_define_gsubr ("row-matrix-size", 1, 0, 0, scm_row_matrix_size);
  scm_c_define_gsubr ("column-matrix-size", 1, 0, 0, scm_column_matrix_size);
  scm_c_define_gsubr ("square-matrix?", 1, 0, 0, scm_square_matrix_p);
  scm_c_define_gsubr ("conformable-for-matrix*?", 2, 0, 0,
                      scm_conformable_for_matrix_product_p);
  scm_c_define_gsubr ("conformable-for-matrix+?", 2, 0, 0,
                      scm_conformable_for_matrix_sum_p);

  scm_c_define_gsubr ("matrix-0row", 2, 0, 0, scm_matrix_0row);
  scm_c_define_gsubr ("matrix-1row", 2, 0, 0, scm_matrix_1row);
  scm_c_define_gsubr ("matrix-row", 2, 0, 0, scm_matrix_row);
  scm_c_define_gsubr ("matrix-0column-transpose", 2, 0, 0,
                      scm_matrix_0column_transpose);
  scm_c_define_gsubr ("matrix-1column-transpose", 2, 0, 0,
                      scm_matrix_1column_transpose);
  scm_c_define_gsubr ("matrix-column-transpose", 2, 0, 0,
                      scm_matrix_column_transpose);
  scm_c_define_gsubr ("vector->matrix", 1, 0, 0, scm_vector_to_matrix);
  scm_c_define_gsubr ("row-matrix->vector", 1, 0, 0, scm_row_matrix_to_vector);
}

//-------------------------------------------------------------------------
