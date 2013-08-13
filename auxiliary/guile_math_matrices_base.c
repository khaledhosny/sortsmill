#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#include <sortsmill/guile.h>
#include <sortsmill/initialized_global_constants.h>
#include <sortsmill/copy_with_strides.h>
#include <intl.h>
#include <basics.h>
#include <stdint.h>
#include <assert.h>

//-------------------------------------------------------------------------

// FIXME: This looks reusable if given a better name.
INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _scm_zero,
                      scm_c_initialize_from_eval_string, "0");

// FIXME: This looks reusable if given a better name.
INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _scm_one,
                      scm_c_initialize_from_eval_string, "1");

// FIXME: This looks reusable if given a better name.
INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _scm_zero_p,
                      scm_c_initialize_from_eval_string, "zero?");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _scm_I_element_p,
                      scm_c_initialize_from_eval_string,
                      "(lambda (x i j) (if (= i j) (= x 1) (zero? x)))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _num_eq_0ij,
                      scm_c_initialize_from_eval_string,
                      "(lambda (B)"
                      "  (lambda (x i j) (= x (matrix-0ref B i j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _eq_0ij,
                      scm_c_initialize_from_eval_string,
                      "(lambda (B)"
                      "  (lambda (x i j) (eq? x (matrix-0ref B i j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _eqv_0ij,
                      scm_c_initialize_from_eval_string,
                      "(lambda (B)"
                      "  (lambda (x i j) (eqv? x (matrix-0ref B i j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, _equal_0ij,
                      scm_c_initialize_from_eval_string,
                      "(lambda (B)"
                      "  (lambda (x i j) (equal? x (matrix-0ref B i j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM,
                      vector_as_rank2_array_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (i j) (list j))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM,
                      vector_to_column_matrix_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (i j) (list i))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, matrix_diagonal_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (rows_lbnd cols_lbnd)"
                      "  (lambda (i) (list (+ rows_lbnd i)"
                      "                    (+ cols_lbnd i))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, vector_diagonal_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (lbnd) (lambda (i) (list (+ lbnd i))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, matrix_block_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (rows_offset cols_offset)"
                      "  (lambda (i j) (list (+ rows_offset i)"
                      "                      (+ cols_offset j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM,
                      vector_in_matrix_block_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (rows_offset cols_offset)"
                      "  (lambda (j) (list rows_offset (+ cols_offset j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, vector_block_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (offset) (lambda (i) (list (+ offset i))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, column_to_vector_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (i) (list i 0))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, matrix_n_based_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (rows_offset cols_offset)"
                      "  (lambda (i j) (list (+ rows_offset i)"
                      "                      (+ cols_offset j))))");

INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, vector_n_based_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (offset) (lambda (i) (list (+ offset i))))");

//-------------------------------------------------------------------------

static void
raise_not_a_valid_matrix_type (SCM who, SCM type)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_c_make_message_condition (_("matrix error: "
                                       "not a valid matrix type")),
      rnrs_make_irritants_condition (scm_list_1 (type))));
}

static void
raise_attempt_to_create_empty_matrix (SCM who, SCM numrows, SCM numcols)
{
  SCM irritants = scm_list_2 (numrows, numcols);
  SCM message =
    scm_c_locale_sformat (_("matrix error: "
                            "attempt to create a ~ax~a matrix"),
                          irritants);
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (irritants)));
}

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
raise_not_a_1x1_matrix (SCM who, SCM A)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_make_who_condition (who),
      rnrs_c_make_message_condition (_("matrix error: "
                                       "not a 1x1 matrix")),
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

static void
raise_bad_block_specification (SCM who, SCM A, SCM i, SCM j, SCM m, SCM n,
                               SCM i_base, SCM j_base)
{
  scm_dynwind_begin (0);

  assert_is_matrix (who, A);

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);

  const size_t i_dim = scm_c_matrix_numrows (&handle_A);
  const size_t j_dim = scm_c_matrix_numcols (&handle_A);

  char *_who = (char *) scm_to_utf8_stringn (who, NULL);
  scm_dynwind_free (_who);

  const ssize_t _i = scm_to_ssize_t (i);
  const ssize_t _j = scm_to_ssize_t (j);
  const size_t _m = scm_to_size_t (m);
  const size_t _n = scm_to_size_t (n);
  const ssize_t _i_base = scm_to_ssize_t (i_base);
  const ssize_t _j_base = scm_to_ssize_t (j_base);

  assert_row_index_inside_bounds (_who, A, _i, _i_base, i_dim);
  assert_column_index_inside_bounds (_who, A, _j, _j_base, j_dim);

  if (_m <= 0 || _n <= 0)
    raise_attempt_to_create_empty_matrix (who, m, n);
  else
    {
      SCM message =
        scm_c_locale_sformat (_("matrix error: "
                                "~ax~a block starting at ~a,~a does not fit"
                                " in ~ax~a matrix indexed starting at ~a,~a"),
                              scm_list_n (m, n, i, j,
                                          scm_from_size_t (i_dim),
                                          scm_from_size_t (j_dim),
                                          i_base, j_base, SCM_UNDEFINED));
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_make_who_condition (who),
          rnrs_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_5 (A, i, j, m, n))));
    }

  scm_dynwind_end ();
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
scm_array_handle_is_matrix (scm_t_array_handle *handlep)
{
  bool is_a_matrix;

  switch (scm_array_handle_rank (handlep))
    {
    case 1:
      is_a_matrix = (scm_array_handle_dims (handlep)[0].lbnd <=
                     scm_array_handle_dims (handlep)[0].ubnd);
      break;

    case 2:
      is_a_matrix = ((scm_array_handle_dims (handlep)[0].lbnd <=
                      scm_array_handle_dims (handlep)[0].ubnd)
                     && (scm_array_handle_dims (handlep)[1].lbnd <=
                         scm_array_handle_dims (handlep)[1].ubnd));
      break;

    default:
      is_a_matrix = false;
      break;
    }

  return is_a_matrix;
}

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
      is_a_matrix = scm_array_handle_is_matrix (&handle_A);
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

VISIBLE SCM
scm_matrix_as_rank2_array (SCM v)
{
  scm_t_array_handle handle_v;
  SCM A;

  scm_array_get_handle (v, &handle_v);

  if (!scm_array_handle_is_matrix (&handle_v))
    {
      scm_array_handle_release (&handle_v);
      raise_not_a_matrix (scm_from_latin1_string ("scm_matrix_as_rank2_array"),
                          v);
    }

  switch (scm_array_handle_rank (&handle_v))
    {
    case 1:
      {
        const ssize_t lbnd = scm_array_handle_dims (&handle_v)[0].lbnd;
        const ssize_t ubnd = scm_array_handle_dims (&handle_v)[0].ubnd;

        scm_array_handle_release (&handle_v);

        const SCM scm_lbnd = scm_from_ssize_t (lbnd);
        const SCM scm_ubnd = scm_from_ssize_t (ubnd);
        const SCM bounds = scm_list_2 (scm_list_2 (scm_lbnd, scm_lbnd),
                                       scm_list_2 (scm_lbnd, scm_ubnd));
        const SCM mapfunc = vector_as_rank2_array_mapfunc ();
        A = scm_make_shared_array (v, mapfunc, bounds);
      }
      break;

    case 2:
      scm_array_handle_release (&handle_v);
      A = v;
      break;

    default:
      assert (false);
    }

  return A;
}

VISIBLE SCM
scm_matrix_as_min_rank_array (SCM A)
{
  return (scm_c_matrix_row_count (A) == 1) ? scm_c_matrix_1row (A, 1) : A;
}

VISIBLE SCM
scm_row_matrix_to_vector (SCM A)
{
  if (scm_c_matrix_row_count (A) != 1)
    raise_not_a_row_matrix (scm_from_utf8_string ("scm_row_matrix_to_vector"),
                            A);
  return scm_c_matrix_1row (A, 1);
}

VISIBLE SCM
scm_matrix_transpose (SCM A)
{
  SCM At = SCM_UNDEFINED;

  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string ("scm_matrix_transpose"), A);
    }

  switch (scm_array_handle_rank (&handle_A))
    {
    case 1:
      {
        const ssize_t lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
        const ssize_t ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;

        scm_array_handle_release (&handle_A);

        const SCM scm_lbnd = scm_from_ssize_t (lbnd);
        const SCM scm_ubnd = scm_from_ssize_t (ubnd);
        const SCM mapfunc = vector_to_column_matrix_mapfunc ();
        const SCM bounds = scm_list_2 (scm_list_2 (scm_lbnd, scm_ubnd),
                                       scm_list_2 (scm_lbnd, scm_lbnd));
        At = scm_make_shared_array (A, mapfunc, bounds);
      }
      break;

    case 2:
      scm_array_handle_release (&handle_A);
      At = scm_transpose_array (A, scm_list_2 (scm_from_int (1),
                                               scm_from_int (0)));
      break;

    default:
      assert (false);
    }

  return At;
}

VISIBLE SCM
scm_matrix_diagonal (SCM A)
{
  SCM d = SCM_UNDEFINED;

  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string ("scm_matrix_diagonal"), A);
    }

  switch (scm_array_handle_rank (&handle_A))
    {
    case 1:
      {
        const ssize_t lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;

        scm_array_handle_release (&handle_A);

        const SCM bounds = scm_list_1 (scm_from_int (1));
        const SCM mapfunc = scm_call_1 (vector_diagonal_mapfunc (),
                                        scm_from_ssize_t (lbnd));
        d = scm_make_shared_array (A, mapfunc, bounds);
      }
      break;

    case 2:
      {
        const ssize_t rows_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
        const ssize_t rows_ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;
        const ssize_t columns_lbnd = scm_array_handle_dims (&handle_A)[1].lbnd;
        const ssize_t columns_ubnd = scm_array_handle_dims (&handle_A)[1].ubnd;

        scm_array_handle_release (&handle_A);

        const size_t n = (size_t) sszmin (rows_ubnd - rows_lbnd,
                                          columns_ubnd - columns_lbnd) + 1;
        const SCM bounds = scm_list_1 (scm_from_size_t (n));
        const SCM mapfunc = scm_call_2 (matrix_diagonal_mapfunc (),
                                        scm_from_ssize_t (rows_lbnd),
                                        scm_from_ssize_t (columns_lbnd));
        d = scm_make_shared_array (A, mapfunc, bounds);
      }
      break;

    default:
      assert (false);
    }

  return d;
}

static bool
block_is_legal (scm_t_array_handle *handlep, ssize_t i, ssize_t j, size_t m,
                size_t n, ssize_t i_base, ssize_t j_base)
{
  size_t m1 = scm_c_matrix_numrows (handlep);
  size_t n1 = scm_c_matrix_numcols (handlep);
  return (i_base <= i && j_base <= j && 0 < m && 0 < n && m + (i - i_base) <= m1
          && n + (j - j_base) <= n1);
}

static SCM
matrix_block (const char *who,
              SCM A, ssize_t i, ssize_t j, size_t m, size_t n,
              ssize_t i_base, ssize_t j_base)
{
  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  if (!block_is_legal (&handle_A, i, j, m, n, i_base, j_base))
    {
      scm_array_handle_release (&handle_A);
      raise_bad_block_specification (scm_from_latin1_string (who),
                                     A, scm_from_ssize_t (i),
                                     scm_from_ssize_t (j),
                                     scm_from_size_t (m),
                                     scm_from_size_t (n),
                                     scm_from_ssize_t (i_base),
                                     scm_from_ssize_t (j_base));
    }

  SCM mapfunc;
  SCM bounds;

  switch (scm_array_handle_rank (&handle_A))
    {
    case 1:
      {
        assert (m == 1);

        const ssize_t lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;

        scm_array_handle_release (&handle_A);

        mapfunc = scm_call_1 (vector_block_mapfunc (),
                              scm_from_ssize_t (lbnd + (j - j_base)));
        bounds = scm_list_1 (scm_from_size_t (n));
      }
      break;

    case 2:
      {
        const ssize_t rows_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
        const ssize_t columns_lbnd = scm_array_handle_dims (&handle_A)[1].lbnd;

        scm_array_handle_release (&handle_A);

        if (m == 1)
          {
            mapfunc =
              scm_call_2 (vector_in_matrix_block_mapfunc (),
                          scm_from_ssize_t (rows_lbnd + (i - i_base)),
                          scm_from_ssize_t (columns_lbnd + (j - j_base)));
            bounds = scm_list_1 (scm_from_size_t (n));
          }
        else
          {
            mapfunc =
              scm_call_2 (matrix_block_mapfunc (),
                          scm_from_ssize_t (rows_lbnd + (i - i_base)),
                          scm_from_ssize_t (columns_lbnd + (j - j_base)));
            bounds = scm_list_2 (scm_from_size_t (m), scm_from_size_t (n));
          }
      }
      break;

    default:
      assert (false);
    }

  return scm_make_shared_array (A, mapfunc, bounds);
}

VISIBLE SCM
scm_c_matrix_0block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n)
{
  return matrix_block ("scm_c_matrix_0block", A, i, j, m, n, 0, 0);
}

VISIBLE SCM
scm_c_matrix_1block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n)
{
  return matrix_block ("scm_c_matrix_1block", A, i, j, m, n, 1, 1);
}

VISIBLE SCM
scm_c_matrix_block (SCM A, ssize_t i, ssize_t j, size_t m, size_t n)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  ssize_t i_base = scm_c_matrix_rows_lbnd (&handle_A);
  ssize_t j_base = scm_c_matrix_columns_lbnd (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_block", A, i, j, m, n, i_base, j_base);
}

VISIBLE SCM
scm_matrix_0block (SCM A, SCM i, SCM j, SCM m, SCM n)
{
  return scm_c_matrix_0block (A, scm_to_ssize_t (i), scm_to_ssize_t (j),
                              scm_to_size_t (m), scm_to_size_t (n));
}


VISIBLE SCM
scm_matrix_1block (SCM A, SCM i, SCM j, SCM m, SCM n)
{
  return scm_c_matrix_1block (A, scm_to_ssize_t (i), scm_to_ssize_t (j),
                              scm_to_size_t (m), scm_to_size_t (n));
}

VISIBLE SCM
scm_matrix_block (SCM A, SCM i, SCM j, SCM m, SCM n)
{
  return scm_c_matrix_block (A, scm_to_ssize_t (i), scm_to_ssize_t (j),
                             scm_to_size_t (m), scm_to_size_t (n));
}

VISIBLE SCM
scm_c_matrix_0row (SCM A, ssize_t i)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numcols = scm_c_matrix_numcols (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_0row", A, i, 0, 1, numcols, 0, 0);
}

VISIBLE SCM
scm_c_matrix_1row (SCM A, ssize_t i)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numcols = scm_c_matrix_numcols (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_1row", A, i, 1, 1, numcols, 1, 1);
}

VISIBLE SCM
scm_c_matrix_row (SCM A, ssize_t i)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numcols = scm_c_matrix_numcols (&handle_A);
  ssize_t i_base = scm_c_matrix_rows_lbnd (&handle_A);
  ssize_t j_base = scm_c_matrix_columns_lbnd (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_row", A, i, j_base, 1, numcols,
                       i_base, j_base);
}

static SCM
column_transpose (SCM (*column_func) (SCM, ssize_t), SCM A, ssize_t j)
{
  SCM vector;

  SCM column = column_func (A, j);

  scm_t_array_handle handle_col;
  scm_array_get_handle (column, &handle_col);

  // This routine assumes the result of calling @var{column_func}
  // satisfies the following assertions.
  assert (scm_c_matrix_rows_lbnd (&handle_col) == 0);
  assert (scm_c_matrix_columns_lbnd (&handle_col) == 0);

  size_t rank = scm_array_handle_rank (&handle_col);

  if (rank == 1)
    {
      scm_array_handle_release (&handle_col);
      vector = column;
    }
  else
    {
      size_t numrows = scm_c_matrix_numrows (&handle_col);

      scm_array_handle_release (&handle_col);

      SCM mapfunc = column_to_vector_mapfunc ();
      SCM bounds = scm_list_1 (scm_from_size_t (numrows));
      vector = scm_make_shared_array (column, mapfunc, bounds);
    }

  return vector;
}

VISIBLE SCM
scm_c_matrix_0column_transpose (SCM A, ssize_t j)
{
  return column_transpose (scm_c_matrix_0column, A, j);
}

VISIBLE SCM
scm_c_matrix_1column_transpose (SCM A, ssize_t j)
{
  return column_transpose (scm_c_matrix_1column, A, j);
}

VISIBLE SCM
scm_c_matrix_column_transpose (SCM A, ssize_t j)
{
  return column_transpose (scm_c_matrix_column, A, j);
}

VISIBLE SCM
scm_c_matrix_0column (SCM A, ssize_t j)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numrows = scm_c_matrix_numrows (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_0column", A, 0, j, numrows, 1, 0, 0);
}

VISIBLE SCM
scm_c_matrix_1column (SCM A, ssize_t j)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numrows = scm_c_matrix_numrows (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_1column", A, 1, j, numrows, 1, 1, 1);
}

VISIBLE SCM
scm_c_matrix_column (SCM A, ssize_t j)
{
  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);
  size_t numrows = scm_c_matrix_numrows (&handle_A);
  ssize_t i_base = scm_c_matrix_rows_lbnd (&handle_A);
  ssize_t j_base = scm_c_matrix_columns_lbnd (&handle_A);
  scm_array_handle_release (&handle_A);

  return matrix_block ("scm_c_matrix_column", A, i_base, j, numrows, 1,
                       i_base, j_base);
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
scm_matrix_0column (SCM A, SCM i)
{
  return scm_c_matrix_0column (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_1column (SCM A, SCM i)
{
  return scm_c_matrix_1column (A, scm_to_ssize_t (i));
}

VISIBLE SCM
scm_matrix_column (SCM A, SCM i)
{
  return scm_c_matrix_column (A, scm_to_ssize_t (i));
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_matrix_1x1_to_scalar (SCM A)
{
  const char *who = "scm_matrix_1x1_to_scalar";

  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  if (scm_c_matrix_numrows (&handle_A) != 1
      || scm_c_matrix_numcols (&handle_A) != 1)
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_1x1_matrix (scm_from_latin1_string (who), A);
    }

  scm_array_handle_release (&handle_A);

  return scm_c_matrix_0ref (A, 0, 0);
}

VISIBLE SCM
scm_scalar_to_matrix (SCM x)
{
  return scm_c_make_vector (1, x);
}

#define _SCM_SCALAR_TO_TYPED_MATRIX(TYPE)                       \
  SCM                                                           \
  scm_scalar_to_##TYPE##matrix (SCM x)                          \
  {                                                             \
    return scm_make_##TYPE##vector (scm_from_int (1), x);       \
  }

VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (u8);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (s8);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (u16);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (s16);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (u32);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (s32);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (u64);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (s64);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (f32);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (f64);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (c32);
VISIBLE _SCM_SCALAR_TO_TYPED_MATRIX (c64);

typedef SCM _scm_scalar_to_matrix_t (SCM x);

static _scm_scalar_to_matrix_t *_scalar_to_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_scalar_to_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_scalar_to_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_scalar_to_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_scalar_to_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_scalar_to_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_scalar_to_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_scalar_to_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_scalar_to_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_scalar_to_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_scalar_to_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_scalar_to_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_scalar_to_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_scalar_to_c64matrix
};

VISIBLE SCM
scm_scalar_to_typed_matrix (SCM type, SCM x)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_scalar_to_typed_matrix"), type);
  return _scalar_to_matrix[i] (x);
}

//-------------------------------------------------------------------------

static SCM
scm_matrix_n_based (const char *who, SCM A, ssize_t n)
{
  SCM A_rebased = SCM_UNDEFINED;

  scm_t_array_handle handle_A;

  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  SCM scm_n = scm_from_ssize_t (n);

  switch (scm_array_handle_rank (&handle_A))
    {
    case 1:
      {
        const ssize_t lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;

        if (lbnd == n)
          A_rebased = A;
        else
          {
            const ssize_t ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;
            const SCM scm_ubnd = scm_from_ssize_t (n + (ubnd - lbnd));
            const SCM bounds = scm_list_1 (scm_list_2 (scm_n, scm_ubnd));
            const SCM mapfunc = scm_call_1 (vector_n_based_mapfunc (),
                                            scm_from_ssize_t (lbnd - n));
            A_rebased = scm_make_shared_array (A, mapfunc, bounds);
          }
      }
      break;

    case 2:
      {
        const ssize_t rows_lbnd = scm_array_handle_dims (&handle_A)[0].lbnd;
        const ssize_t columns_lbnd = scm_array_handle_dims (&handle_A)[1].lbnd;

        if (rows_lbnd == n && columns_lbnd == n)
          A_rebased = A;
        else
          {
            const ssize_t rows_ubnd = scm_array_handle_dims (&handle_A)[0].ubnd;
            const ssize_t columns_ubnd =
              scm_array_handle_dims (&handle_A)[1].ubnd;
            const ssize_t rows_diff = rows_ubnd - rows_lbnd;
            const ssize_t columns_diff = columns_ubnd - columns_lbnd;
            const SCM scm_rows_ubnd = scm_from_ssize_t (n + rows_diff);
            const SCM scm_columns_ubnd = scm_from_ssize_t (n + columns_diff);
            const SCM bounds = scm_list_2 (scm_list_2 (scm_n, scm_rows_ubnd),
                                           scm_list_2 (scm_n,
                                                       scm_columns_ubnd));
            const SCM scm_rows_offset = scm_from_ssize_t (rows_lbnd - n);
            const SCM scm_columns_offset = scm_from_ssize_t (columns_lbnd - n);
            const SCM mapfunc = scm_call_2 (matrix_n_based_mapfunc (),
                                            scm_rows_offset,
                                            scm_columns_offset);
            A_rebased = scm_make_shared_array (A, mapfunc, bounds);
          }
      }
      break;

    default:
      assert (false);
    }

  scm_array_handle_release (&handle_A);

  return A_rebased;
}

VISIBLE SCM
scm_matrix_0based (SCM A)
{
  return scm_matrix_n_based ("scm_matrix_0based", A, 0);
}

VISIBLE SCM
scm_matrix_1based (SCM A)
{
  return scm_matrix_n_based ("scm_matrix_1based", A, 1);
}

//-------------------------------------------------------------------------

#define _VOID_READONLY_ELEMENTS_FUNC(ELEMTYPE)                          \
  void *                                                                \
  _void_##ELEMTYPE##readonly_elements (scm_t_array_handle *handlep)     \
  {                                                                     \
    return (void *)                                                     \
      scm_array_handle_##ELEMTYPE##elements (handlep);                  \
  }

static _VOID_READONLY_ELEMENTS_FUNC ( /* empty */ );
static _VOID_READONLY_ELEMENTS_FUNC (u8_);
static _VOID_READONLY_ELEMENTS_FUNC (s8_);
static _VOID_READONLY_ELEMENTS_FUNC (u16_);
static _VOID_READONLY_ELEMENTS_FUNC (s16_);
static _VOID_READONLY_ELEMENTS_FUNC (u32_);
static _VOID_READONLY_ELEMENTS_FUNC (s32_);
static _VOID_READONLY_ELEMENTS_FUNC (u64_);
static _VOID_READONLY_ELEMENTS_FUNC (s64_);
static _VOID_READONLY_ELEMENTS_FUNC (f32_);
static _VOID_READONLY_ELEMENTS_FUNC (f64_);
static _VOID_READONLY_ELEMENTS_FUNC (c32_);
static _VOID_READONLY_ELEMENTS_FUNC (c64_);

typedef void *_void_readonly_elements_func_t (scm_t_array_handle *);

static _void_readonly_elements_func_t *_void_readonly_elements_func[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = _void_readonly_elements,
  [_FF_INDEX_ARRAY_U8] = _void_u8_readonly_elements,
  [_FF_INDEX_ARRAY_S8] = _void_s8_readonly_elements,
  [_FF_INDEX_ARRAY_U16] = _void_u16_readonly_elements,
  [_FF_INDEX_ARRAY_S16] = _void_s16_readonly_elements,
  [_FF_INDEX_ARRAY_U32] = _void_u32_readonly_elements,
  [_FF_INDEX_ARRAY_S32] = _void_s32_readonly_elements,
  [_FF_INDEX_ARRAY_U64] = _void_u64_readonly_elements,
  [_FF_INDEX_ARRAY_S64] = _void_s64_readonly_elements,
  [_FF_INDEX_ARRAY_F32] = _void_f32_readonly_elements,
  [_FF_INDEX_ARRAY_F64] = _void_f64_readonly_elements,
  [_FF_INDEX_ARRAY_C32] = _void_c32_readonly_elements,
  [_FF_INDEX_ARRAY_C64] = _void_c64_readonly_elements
};

#define _VOID_WRITABLE_ELEMENTS_FUNC(ELEMTYPE)                          \
  void *                                                                \
  _void_##ELEMTYPE##writable_elements (scm_t_array_handle *handlep)     \
  {                                                                     \
    return (void *)                                                     \
      scm_array_handle_##ELEMTYPE##writable_elements (handlep);         \
  }

static _VOID_WRITABLE_ELEMENTS_FUNC ( /* empty */ );
static _VOID_WRITABLE_ELEMENTS_FUNC (u8_);
static _VOID_WRITABLE_ELEMENTS_FUNC (s8_);
static _VOID_WRITABLE_ELEMENTS_FUNC (u16_);
static _VOID_WRITABLE_ELEMENTS_FUNC (s16_);
static _VOID_WRITABLE_ELEMENTS_FUNC (u32_);
static _VOID_WRITABLE_ELEMENTS_FUNC (s32_);
static _VOID_WRITABLE_ELEMENTS_FUNC (u64_);
static _VOID_WRITABLE_ELEMENTS_FUNC (s64_);
static _VOID_WRITABLE_ELEMENTS_FUNC (f32_);
static _VOID_WRITABLE_ELEMENTS_FUNC (f64_);
static _VOID_WRITABLE_ELEMENTS_FUNC (c32_);
static _VOID_WRITABLE_ELEMENTS_FUNC (c64_);

typedef void *_void_writable_elements_func_t (scm_t_array_handle *);

static _void_writable_elements_func_t *_void_writable_elements_func[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = _void_writable_elements,
  [_FF_INDEX_ARRAY_U8] = _void_u8_writable_elements,
  [_FF_INDEX_ARRAY_S8] = _void_s8_writable_elements,
  [_FF_INDEX_ARRAY_U16] = _void_u16_writable_elements,
  [_FF_INDEX_ARRAY_S16] = _void_s16_writable_elements,
  [_FF_INDEX_ARRAY_U32] = _void_u32_writable_elements,
  [_FF_INDEX_ARRAY_S32] = _void_s32_writable_elements,
  [_FF_INDEX_ARRAY_U64] = _void_u64_writable_elements,
  [_FF_INDEX_ARRAY_S64] = _void_s64_writable_elements,
  [_FF_INDEX_ARRAY_F32] = _void_f32_writable_elements,
  [_FF_INDEX_ARRAY_F64] = _void_f64_writable_elements,
  [_FF_INDEX_ARRAY_C32] = _void_c32_writable_elements,
  [_FF_INDEX_ARRAY_C64] = _void_c64_writable_elements
};

static size_t _matrix_element_size[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = SIZE_MAX,  // This should never be used.
  [_FF_INDEX_ARRAY_NONUNIFORM] = sizeof (SCM),
  [_FF_INDEX_ARRAY_U8] = sizeof (uint8_t),
  [_FF_INDEX_ARRAY_S8] = sizeof (int8_t),
  [_FF_INDEX_ARRAY_U16] = sizeof (uint16_t),
  [_FF_INDEX_ARRAY_S16] = sizeof (int16_t),
  [_FF_INDEX_ARRAY_U32] = sizeof (uint32_t),
  [_FF_INDEX_ARRAY_S32] = sizeof (int32_t),
  [_FF_INDEX_ARRAY_U64] = sizeof (uint64_t),
  [_FF_INDEX_ARRAY_S64] = sizeof (int64_t),
  [_FF_INDEX_ARRAY_F32] = sizeof (float),
  [_FF_INDEX_ARRAY_F64] = sizeof (double),
  [_FF_INDEX_ARRAY_C32] = 2 * sizeof (float),
  [_FF_INDEX_ARRAY_C64] = 2 * sizeof (double)
};

static SCM
_scm_from_type_indexed_element (scm_t_array_type_index type_index,
                                const void *elem)
{
  SCM result = SCM_UNSPECIFIED;

  switch (type_index)
    {
    case _FF_INDEX_ARRAY_NONUNIFORM:
      result = *(const SCM *) elem;
      break;
    case _FF_INDEX_ARRAY_U8:
      result = scm_from_uint8 (*(const uint8_t *) elem);
      break;
    case _FF_INDEX_ARRAY_S8:
      result = scm_from_int8 (*(const int8_t *) elem);
      break;
    case _FF_INDEX_ARRAY_U16:
      result = scm_from_uint16 (*(const uint16_t *) elem);
      break;
    case _FF_INDEX_ARRAY_S16:
      result = scm_from_int16 (*(const int16_t *) elem);
      break;
    case _FF_INDEX_ARRAY_U32:
      result = scm_from_uint32 (*(const uint32_t *) elem);
      break;
    case _FF_INDEX_ARRAY_S32:
      result = scm_from_int32 (*(const int32_t *) elem);
      break;
    case _FF_INDEX_ARRAY_U64:
      result = scm_from_uint64 (*(const uint64_t *) elem);
      break;
    case _FF_INDEX_ARRAY_S64:
      result = scm_from_int64 (*(const int64_t *) elem);
      break;
    case _FF_INDEX_ARRAY_F32:
      result = scm_from_double (*(float *) elem);
      break;
    case _FF_INDEX_ARRAY_F64:
      result = scm_from_double (*(double *) elem);
      break;
    case _FF_INDEX_ARRAY_C32:
      result =
        scm_c_make_rectangular (((float *) elem)[0], ((float *) elem)[1]);
      break;
    case _FF_INDEX_ARRAY_C64:
      result =
        scm_c_make_rectangular (((double *) elem)[0], ((double *) elem)[1]);
      break;
    default:
      assert (false);
      break;
    }

  return result;
}

static void
_scm_to_type_indexed_element (scm_t_array_type_index type_index, void *elem,
                              SCM value)
{
  switch (type_index)
    {
    case _FF_INDEX_ARRAY_NONUNIFORM:
      *(SCM *) elem = value;
      break;
    case _FF_INDEX_ARRAY_U8:
      *(uint8_t *) elem = scm_to_uint8 (value);
      break;
    case _FF_INDEX_ARRAY_S8:
      *(int8_t *) elem = scm_to_int8 (value);
      break;
    case _FF_INDEX_ARRAY_U16:
      *(uint16_t *) elem = scm_to_uint16 (value);
      break;
    case _FF_INDEX_ARRAY_S16:
      *(int16_t *) elem = scm_to_int16 (value);
      break;
    case _FF_INDEX_ARRAY_U32:
      *(uint32_t *) elem = scm_to_uint32 (value);
      break;
    case _FF_INDEX_ARRAY_S32:
      *(int32_t *) elem = scm_to_int32 (value);
      break;
    case _FF_INDEX_ARRAY_U64:
      *(uint64_t *) elem = scm_to_uint64 (value);
      break;
    case _FF_INDEX_ARRAY_S64:
      *(int64_t *) elem = scm_to_int64 (value);
      break;
    case _FF_INDEX_ARRAY_F32:
      *(float *) elem = scm_to_double (value);
      break;
    case _FF_INDEX_ARRAY_F64:
      *(double *) elem = scm_to_double (value);
      break;
    case _FF_INDEX_ARRAY_C32:
      ((float *) elem)[0] = scm_to_double (scm_real_part (value));
      ((float *) elem)[1] = scm_to_double (scm_imag_part (value));
      break;
    case _FF_INDEX_ARRAY_C64:
      ((double *) elem)[0] = scm_to_double (scm_real_part (value));
      ((double *) elem)[1] = scm_to_double (scm_imag_part (value));
      break;
    default:
      assert (false);
      break;
    }
}

//-------------------------------------------------------------------------

static SCM
make_filled_matrix (const char *who, SCM type, SCM fill, size_t numrows,
                    size_t numcols)
{
  const SCM scm_numrows = scm_from_size_t (numrows);
  const SCM scm_numcols = scm_from_size_t (numcols);

  if (numrows == 0 || numcols == 0)
    raise_attempt_to_create_empty_matrix (scm_from_latin1_string (who),
                                          scm_numrows, scm_numcols);

  const SCM bounds = (numrows == 1) ?
    scm_list_1 (scm_numcols) : scm_list_2 (scm_numrows, scm_numcols);

  return scm_make_typed_array (type, fill, bounds);
}

typedef void _scm_matrix_alterer_t (void *elements, const void *data,
                                    size_t numrows, ssize_t row_inc,
                                    size_t numcols, ssize_t column_inc);

static SCM
make_altered_filled_matrix (const char *who, SCM type, SCM fill,
                            _scm_matrix_alterer_t * alterer, const void *data,
                            size_t numrows, size_t numcols)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type (scm_from_latin1_string (who), type);

  SCM A = make_filled_matrix (who, type, fill, numrows, numcols);
  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);

  // WARNING! The alterer must not raise exceptions or otherwise
  // return non-locally.
  alterer (_void_writable_elements_func[i] (&handle_A), data,
           numrows, scm_c_matrix_row_inc (&handle_A),
           numcols, scm_c_matrix_column_inc (&handle_A));

  scm_array_handle_release (&handle_A);

  return A;
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

VISIBLE SCM
scm_c_filled_matrix (SCM fill, size_t m, size_t n)
{
  return make_filled_matrix ("scm_c_filled_matrix", SCM_BOOL_T, fill, m, n);
}

#define _SCM_C_TYPED_FILLED_MATRIX(ELEMTYPE)                            \
  SCM                                                                   \
  scm_c_filled_##ELEMTYPE##matrix (SCM fill, size_t m, size_t n)        \
  {                                                                     \
    return make_filled_matrix ("scm_c_filled_" #ELEMTYPE "matrix",      \
                               scm_symbol_##ELEMTYPE (), fill, m, n);   \
  }

VISIBLE _SCM_C_TYPED_FILLED_MATRIX (u8);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (s8);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (u16);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (s16);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (u32);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (s32);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (u64);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (s64);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (f32);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (f64);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (c32);
VISIBLE _SCM_C_TYPED_FILLED_MATRIX (c64);

#define _SCM_FILLED_MATRIX(ELEMTYPE)                            \
  SCM                                                           \
  scm_filled_##ELEMTYPE##matrix (SCM fill, SCM m, SCM n)        \
  {                                                             \
    if (SCM_UNBNDP (n))                                         \
      n = m;                                                    \
    return scm_c_filled_##ELEMTYPE##matrix (fill,               \
                                            scm_to_size_t (m),  \
                                            scm_to_size_t (n)); \
  }

VISIBLE _SCM_FILLED_MATRIX ( /* empty */ );
VISIBLE _SCM_FILLED_MATRIX (u8);
VISIBLE _SCM_FILLED_MATRIX (s8);
VISIBLE _SCM_FILLED_MATRIX (u16);
VISIBLE _SCM_FILLED_MATRIX (s16);
VISIBLE _SCM_FILLED_MATRIX (u32);
VISIBLE _SCM_FILLED_MATRIX (s32);
VISIBLE _SCM_FILLED_MATRIX (u64);
VISIBLE _SCM_FILLED_MATRIX (s64);
VISIBLE _SCM_FILLED_MATRIX (f32);
VISIBLE _SCM_FILLED_MATRIX (f64);
VISIBLE _SCM_FILLED_MATRIX (c32);
VISIBLE _SCM_FILLED_MATRIX (c64);

typedef SCM _scm_c_typed_filled_matrix_t (SCM fill, size_t m, size_t n);

static _scm_c_typed_filled_matrix_t *_c_typed_filled_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_c_filled_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_c_filled_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_c_filled_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_c_filled_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_c_filled_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_c_filled_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_c_filled_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_c_filled_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_c_filled_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_c_filled_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_c_filled_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_c_filled_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_c_filled_c64matrix
};

VISIBLE SCM
scm_c_typed_filled_matrix (SCM type, SCM fill, size_t m, size_t n)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_c_typed_filled_matrix"), type);
  return _c_typed_filled_matrix[i] (fill, m, n);
}

VISIBLE SCM
scm_typed_filled_matrix (SCM type, SCM fill, SCM m, SCM n)
{
  if (SCM_UNBNDP (n))
    n = m;
  return scm_c_typed_filled_matrix (type, fill, scm_to_size_t (m),
                                    scm_to_size_t (n));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

VISIBLE SCM
scm_c_zero_matrix (size_t m, size_t n)
{
  return make_filled_matrix ("scm_c_zero_matrix", SCM_BOOL_T, _scm_zero (), m,
                             n);
}

#define _SCM_C_TYPED_ZERO_MATRIX(ELEMTYPE)                              \
  SCM                                                                   \
  scm_c_zero_##ELEMTYPE##matrix (size_t m, size_t n)                    \
  {                                                                     \
    return make_filled_matrix ("scm_c_zero_" #ELEMTYPE "matrix",        \
                               scm_symbol_##ELEMTYPE (),                \
                               _scm_zero (), m, n);                     \
  }

VISIBLE _SCM_C_TYPED_ZERO_MATRIX (u8);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (s8);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (u16);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (s16);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (u32);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (s32);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (u64);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (s64);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (f32);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (f64);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (c32);
VISIBLE _SCM_C_TYPED_ZERO_MATRIX (c64);

#define _SCM_ZERO_MATRIX(ELEMTYPE)                              \
  SCM                                                           \
  scm_zero_##ELEMTYPE##matrix (SCM m, SCM n)                    \
  {                                                             \
    if (SCM_UNBNDP (n))                                         \
      n = m;                                                    \
    return scm_c_zero_##ELEMTYPE##matrix (scm_to_size_t (m),    \
                                          scm_to_size_t (n));   \
  }

VISIBLE _SCM_ZERO_MATRIX ( /* empty */ );
VISIBLE _SCM_ZERO_MATRIX (u8);
VISIBLE _SCM_ZERO_MATRIX (s8);
VISIBLE _SCM_ZERO_MATRIX (u16);
VISIBLE _SCM_ZERO_MATRIX (s16);
VISIBLE _SCM_ZERO_MATRIX (u32);
VISIBLE _SCM_ZERO_MATRIX (s32);
VISIBLE _SCM_ZERO_MATRIX (u64);
VISIBLE _SCM_ZERO_MATRIX (s64);
VISIBLE _SCM_ZERO_MATRIX (f32);
VISIBLE _SCM_ZERO_MATRIX (f64);
VISIBLE _SCM_ZERO_MATRIX (c32);
VISIBLE _SCM_ZERO_MATRIX (c64);

typedef SCM _scm_c_typed_zero_matrix_t (size_t m, size_t n);

static _scm_c_typed_zero_matrix_t *_c_typed_zero_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_c_zero_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_c_zero_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_c_zero_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_c_zero_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_c_zero_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_c_zero_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_c_zero_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_c_zero_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_c_zero_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_c_zero_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_c_zero_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_c_zero_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_c_zero_c64matrix
};

VISIBLE SCM
scm_c_typed_zero_matrix (SCM type, size_t m, size_t n)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_c_typed_zero_matrix"), type);
  return _c_typed_zero_matrix[i] (m, n);
}

VISIBLE SCM
scm_typed_zero_matrix (SCM type, SCM m, SCM n)
{
  if (SCM_UNBNDP (n))
    n = m;
  return scm_c_typed_zero_matrix (type, scm_to_size_t (m), scm_to_size_t (n));
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --

static void
fill_diagonal (void *elements, const void *data,
               size_t numrows, ssize_t row_inc,
               size_t numcols, ssize_t column_inc)
{
  SCM *elems = (SCM *) elements;
  SCM x = *(SCM *) data;
  const size_t n = szmin (numrows, numcols);
  const ssize_t diag_inc = row_inc + column_inc;
  for (ssize_t i = 0; i < n; i++)
    {
      *elems = x;
      elems += diag_inc;
    }
}

#define _FILL_TYPED_DIAGONAL(ELEMTYPE, TYPE)                    \
  void                                                          \
  fill_diagonal_##ELEMTYPE (void *elements, const void *data,   \
                            size_t numrows, ssize_t row_inc,    \
                            size_t numcols, ssize_t column_inc) \
  {                                                             \
    TYPE *elems = (TYPE*) elements;                             \
    TYPE x = *(TYPE *) data;                                    \
    const size_t n = szmin (numrows, numcols);                  \
    const ssize_t diag_inc = row_inc + column_inc;              \
    for (ssize_t i = 0; i < n; i++)                             \
      {                                                         \
        *elems = x;                                             \
        elems += diag_inc;                                      \
      }                                                         \
  }

#define _FILL_COMPLEX_DIAGONAL(ELEMTYPE, TYPE)                  \
  void                                                          \
  fill_diagonal_##ELEMTYPE (void *elements, const void *data,   \
                            size_t numrows, ssize_t row_inc,    \
                            size_t numcols, ssize_t column_inc) \
  {                                                             \
    TYPE *elems = (TYPE*) elements;                             \
    const double real_part = ((const double *) data)[0];        \
    const double imag_part = ((const double *) data)[1];        \
    const size_t n = szmin (numrows, numcols);                  \
    const ssize_t diag_inc = 2 * (row_inc + column_inc);        \
    for (ssize_t i = 0; i < n; i++)                             \
      {                                                         \
        elems[0] = real_part;                                   \
        elems[1] = imag_part;                                   \
        elems += diag_inc;                                      \
      }                                                         \
  }

static _FILL_TYPED_DIAGONAL (u8, uint8_t);
static _FILL_TYPED_DIAGONAL (s8, int8_t);
static _FILL_TYPED_DIAGONAL (u16, uint16_t);
static _FILL_TYPED_DIAGONAL (s16, int16_t);
static _FILL_TYPED_DIAGONAL (u32, uint32_t);
static _FILL_TYPED_DIAGONAL (s32, int32_t);
static _FILL_TYPED_DIAGONAL (u64, uint64_t);
static _FILL_TYPED_DIAGONAL (s64, int64_t);
static _FILL_TYPED_DIAGONAL (f32, float);
static _FILL_TYPED_DIAGONAL (f64, double);
static _FILL_COMPLEX_DIAGONAL (c32, float);
static _FILL_COMPLEX_DIAGONAL (c64, double);

VISIBLE SCM
scm_c_scalar_matrix (SCM x, size_t m, size_t n)
{
  return make_altered_filled_matrix ("scm_c_scalar_matrix", SCM_BOOL_T,
                                     _scm_zero (), fill_diagonal, &x, m, n);
}

#define _SCM_C_TYPED_SCALAR_MATRIX(ELEMTYPE, TYPE, TO_TYPE)             \
  SCM                                                                   \
  scm_c_scalar_##ELEMTYPE##matrix (SCM x, size_t m, size_t n)           \
  {                                                                     \
    TYPE _x = TO_TYPE (x);                                              \
    return                                                              \
      make_altered_filled_matrix ("scm_c_scalar_" #ELEMTYPE "matrix",   \
                                  scm_symbol_##ELEMTYPE (),             \
                                  _scm_zero (),                         \
                                  fill_diagonal_##ELEMTYPE, &_x, m, n); \
  }

#define _SCM_C_COMPLEX_SCALAR_MATRIX(ELEMTYPE)                          \
  SCM                                                                   \
  scm_c_scalar_##ELEMTYPE##matrix (SCM x, size_t m, size_t n)           \
  {                                                                     \
    const double data[2] = {                                            \
      scm_to_double (scm_real_part (x)),                                \
      scm_to_double (scm_imag_part (x))                                 \
    };                                                                  \
    return                                                              \
      make_altered_filled_matrix ("scm_c_scalar_" #ELEMTYPE "matrix",   \
                                  scm_symbol_##ELEMTYPE (),             \
                                  _scm_zero (),                         \
                                  fill_diagonal_##ELEMTYPE, data,       \
                                  m, n);                                \
  }

VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (u8, uint8_t, scm_to_uint8);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (s8, int8_t, scm_to_int8);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (u16, uint16_t, scm_to_uint16);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (s16, int16_t, scm_to_int16);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (u32, uint32_t, scm_to_uint32);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (s32, int32_t, scm_to_int32);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (u64, uint64_t, scm_to_uint64);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (s64, int64_t, scm_to_int64);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (f32, float, scm_to_double);
VISIBLE _SCM_C_TYPED_SCALAR_MATRIX (f64, double, scm_to_double);
VISIBLE _SCM_C_COMPLEX_SCALAR_MATRIX (c32);
VISIBLE _SCM_C_COMPLEX_SCALAR_MATRIX (c64);

#define _SCM_SCALAR_MATRIX(ELEMTYPE)                                    \
  SCM                                                                   \
  scm_scalar_##ELEMTYPE##matrix (SCM x, SCM m, SCM n)                   \
  {                                                                     \
    if (SCM_UNBNDP (n))                                                 \
      n = m;                                                            \
    return scm_c_scalar_##ELEMTYPE##matrix (x, scm_to_size_t (m),       \
                                            scm_to_size_t (n));         \
  }

VISIBLE _SCM_SCALAR_MATRIX ( /* empty */ );
VISIBLE _SCM_SCALAR_MATRIX (u8);
VISIBLE _SCM_SCALAR_MATRIX (s8);
VISIBLE _SCM_SCALAR_MATRIX (u16);
VISIBLE _SCM_SCALAR_MATRIX (s16);
VISIBLE _SCM_SCALAR_MATRIX (u32);
VISIBLE _SCM_SCALAR_MATRIX (s32);
VISIBLE _SCM_SCALAR_MATRIX (u64);
VISIBLE _SCM_SCALAR_MATRIX (s64);
VISIBLE _SCM_SCALAR_MATRIX (f32);
VISIBLE _SCM_SCALAR_MATRIX (f64);
VISIBLE _SCM_SCALAR_MATRIX (c32);
VISIBLE _SCM_SCALAR_MATRIX (c64);

typedef SCM _scm_c_typed_scalar_matrix_t (SCM x, size_t m, size_t n);

static _scm_c_typed_scalar_matrix_t *_c_typed_scalar_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_c_scalar_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_c_scalar_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_c_scalar_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_c_scalar_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_c_scalar_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_c_scalar_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_c_scalar_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_c_scalar_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_c_scalar_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_c_scalar_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_c_scalar_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_c_scalar_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_c_scalar_c64matrix
};

VISIBLE SCM
scm_c_typed_scalar_matrix (SCM type, SCM x, size_t m, size_t n)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_c_typed_scalar_matrix"), type);
  return _c_typed_scalar_matrix[i] (x, m, n);
}

VISIBLE SCM
scm_typed_scalar_matrix (SCM type, SCM x, SCM m, SCM n)
{
  if (SCM_UNBNDP (n))
    n = m;
  return scm_c_typed_scalar_matrix (type, x, scm_to_size_t (m),
                                    scm_to_size_t (n));
}

VISIBLE SCM
scm_c_I_matrix (size_t m, size_t n)
{
  SCM x = _scm_one ();
  return make_altered_filled_matrix ("scm_c_I_matrix", SCM_BOOL_T,
                                     _scm_zero (), fill_diagonal, &x, m, n);
}

#define _SCM_C_TYPED_I_MATRIX(ELEMTYPE, TYPE)                           \
  SCM                                                                   \
  scm_c_I_##ELEMTYPE##matrix (size_t m, size_t n)                       \
  {                                                                     \
    TYPE _x = 1;                                                        \
    return                                                              \
      make_altered_filled_matrix ("scm_c_I_" #ELEMTYPE "matrix",        \
                                  scm_symbol_##ELEMTYPE (),             \
                                  _scm_zero (),                         \
                                  fill_diagonal_##ELEMTYPE, &_x, m, n); \
  }

#define _SCM_C_COMPLEX_I_MATRIX(ELEMTYPE)                               \
  SCM                                                                   \
  scm_c_I_##ELEMTYPE##matrix (size_t m, size_t n)                       \
  {                                                                     \
    const double data[2] = { 1.0, 0.0 };                                \
    return                                                              \
      make_altered_filled_matrix ("scm_c_I_" #ELEMTYPE "matrix",        \
                                  scm_symbol_##ELEMTYPE (),             \
                                  _scm_zero (),                         \
                                  fill_diagonal_##ELEMTYPE, data,       \
                                  m, n);                                \
  }

VISIBLE _SCM_C_TYPED_I_MATRIX (u8, uint8_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (s8, int8_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (u16, uint16_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (s16, int16_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (u32, uint32_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (s32, int32_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (u64, uint64_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (s64, int64_t);
VISIBLE _SCM_C_TYPED_I_MATRIX (f32, float);
VISIBLE _SCM_C_TYPED_I_MATRIX (f64, double);
VISIBLE _SCM_C_COMPLEX_I_MATRIX (c32);
VISIBLE _SCM_C_COMPLEX_I_MATRIX (c64);

#define _SCM_I_MATRIX(ELEMTYPE)                                 \
  SCM                                                           \
  scm_I_##ELEMTYPE##matrix (SCM m, SCM n)                       \
  {                                                             \
    if (SCM_UNBNDP (n))                                         \
      n = m;                                                    \
    return scm_c_I_##ELEMTYPE##matrix (scm_to_size_t (m),       \
                                       scm_to_size_t (n));      \
  }

VISIBLE _SCM_I_MATRIX ( /* empty */ );
VISIBLE _SCM_I_MATRIX (u8);
VISIBLE _SCM_I_MATRIX (s8);
VISIBLE _SCM_I_MATRIX (u16);
VISIBLE _SCM_I_MATRIX (s16);
VISIBLE _SCM_I_MATRIX (u32);
VISIBLE _SCM_I_MATRIX (s32);
VISIBLE _SCM_I_MATRIX (u64);
VISIBLE _SCM_I_MATRIX (s64);
VISIBLE _SCM_I_MATRIX (f32);
VISIBLE _SCM_I_MATRIX (f64);
VISIBLE _SCM_I_MATRIX (c32);
VISIBLE _SCM_I_MATRIX (c64);

typedef SCM _scm_c_typed_I_matrix_t (size_t m, size_t n);

static _scm_c_typed_I_matrix_t *_c_typed_I_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_c_I_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_c_I_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_c_I_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_c_I_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_c_I_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_c_I_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_c_I_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_c_I_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_c_I_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_c_I_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_c_I_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_c_I_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_c_I_c64matrix
};

VISIBLE SCM
scm_c_typed_I_matrix (SCM type, size_t m, size_t n)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_c_typed_I_matrix"), type);
  return _c_typed_I_matrix[i] (m, n);
}

VISIBLE SCM
scm_typed_I_matrix (SCM type, SCM m, SCM n)
{
  if (SCM_UNBNDP (n))
    n = m;
  return scm_c_typed_I_matrix (type, scm_to_size_t (m), scm_to_size_t (n));
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_row_matrix_to_diagonal_matrix (SCM diag_as_row)
{
  scm_dynwind_begin (0);

  const size_t n = scm_c_row_matrix_size (diag_as_row);
  SCM type = scm_array_type (diag_as_row);
  SCM A = scm_c_typed_zero_matrix (type, n, n);

  scm_t_array_type_index type_index = scm_array_type_to_array_type_index (type);

  scm_t_array_handle handle_diag;
  scm_array_get_handle (diag_as_row, &handle_diag);
  scm_dynwind_array_handle_release (&handle_diag);
  const void *_diag = _void_readonly_elements_func[type_index] (&handle_diag);

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  void *_A = _void_writable_elements_func[type_index] (&handle_A);

  const ssize_t diag_inc = scm_c_matrix_column_inc (&handle_diag);
  const ssize_t row_inc = scm_c_matrix_row_inc (&handle_A);
  const ssize_t col_inc = scm_c_matrix_column_inc (&handle_A);

  copy_type_indexed_with_strides (type_index, row_inc + col_inc, _A,
                                  diag_inc, _diag, n);

  scm_dynwind_end ();

  return A;
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_matrix_copy (SCM A)
{
  const char *who = "scm_matrix_copy";

  scm_dynwind_begin (0);

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  const SCM type = scm_array_handle_element_type (&handle_A);
  const scm_t_array_type_index type_index =
    scm_array_type_to_array_type_index (type);
  const size_t rank = scm_array_handle_rank (&handle_A);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle_A);

  SCM bounds = (rank == 1) ?
    scm_list_1 (scm_list_2 (scm_from_ssize_t (dims[0].lbnd),
                            scm_from_ssize_t (dims[0].ubnd))) :
    scm_list_2 (scm_list_2 (scm_from_ssize_t (dims[0].lbnd),
                            scm_from_ssize_t (dims[0].ubnd)),
                scm_list_2 (scm_from_ssize_t (dims[1].lbnd),
                            scm_from_ssize_t (dims[1].ubnd)));
  SCM B = scm_make_typed_array (type, SCM_UNSPECIFIED, bounds);

  scm_t_array_handle handle_B;
  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  const scm_t_array_dim *dims_B = scm_array_handle_dims (&handle_B);

  const void *_A = _void_readonly_elements_func[type_index] (&handle_A);
  void *_B = _void_writable_elements_func[type_index] (&handle_B);

  switch (rank)
    {
    case 1:
      copy_type_indexed_with_strides (type_index, dims_B[0].inc, _B,
                                      dims[0].inc, _A,
                                      (dims[0].ubnd - dims[0].lbnd) + 1);
      break;

    case 2:
      {
        const size_t elem_size = _matrix_element_size[type_index];
        const ssize_t src_row_stride = dims[0].inc * (ssize_t) elem_size;
        const ssize_t dest_row_stride = dims_B[0].inc * (ssize_t) elem_size;
        for (ssize_t i = 0; i <= dims[0].ubnd - dims[0].lbnd; i++)
          {
            copy_type_indexed_with_strides (type_index, dims_B[1].inc, _B,
                                            dims[1].inc, _A,
                                            (dims[1].ubnd - dims[1].lbnd) + 1);
            _A = (void *) ((char *) _A + src_row_stride);
            _B = (void *) ((char *) _B + dest_row_stride);
          }
      }
      break;

    default:
      assert (false);
    }

  scm_dynwind_end ();

  return B;
}

//-------------------------------------------------------------------------

#define _SCM_MATRIX_TO_TYPED_MATRIX(NAME, PROC_DECL, PROC_IS_NULL, PROC_CALL) \
  SCM                                                                   \
  NAME (SCM type, SCM A, PROC_DECL (proc))                              \
  {                                                                     \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_array_handle_is_matrix (#NAME, A, &handle_A);                \
                                                                        \
    const size_t m = scm_c_matrix_numrows (&handle_A);                  \
    const size_t n = scm_c_matrix_numcols (&handle_A);                  \
                                                                        \
    SCM B = scm_c_typed_filled_matrix (type, SCM_UNSPECIFIED, m, n);    \
                                                                        \
    scm_t_array_handle handle_B;                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
                                                                        \
    const scm_t_array_type_index src_type_index =                       \
      scm_array_handle_to_array_type_index (&handle_A);                 \
    const void *_A =                                                    \
      _void_readonly_elements_func[src_type_index] (&handle_A);         \
    const size_t src_elem_size = _matrix_element_size[src_type_index];  \
    const ssize_t src_row_stride =                                      \
      scm_c_matrix_row_inc (&handle_A) * (ssize_t) src_elem_size;       \
    const ssize_t src_col_stride =                                      \
      scm_c_matrix_column_inc (&handle_A) * (ssize_t) src_elem_size;    \
                                                                        \
    const scm_t_array_type_index dst_type_index =                       \
      scm_array_type_to_array_type_index (type);                        \
    void *_B =                                                          \
      _void_writable_elements_func[dst_type_index] (&handle_B);         \
    const size_t dst_elem_size = _matrix_element_size[dst_type_index];  \
    const ssize_t dst_row_stride =                                      \
      scm_c_matrix_row_inc (&handle_B) * (ssize_t) dst_elem_size;       \
    const ssize_t dst_col_stride =                                      \
      scm_c_matrix_column_inc (&handle_B) * (ssize_t) dst_elem_size;    \
                                                                        \
    if (PROC_IS_NULL (proc) && src_type_index == dst_type_index)        \
      for (size_t i = 0; i < m; i++)                                    \
        {                                                               \
          copy_type_indexed_with_strides (src_type_index,               \
                                          scm_c_matrix_column_inc (&handle_B), _B, \
                                          scm_c_matrix_column_inc (&handle_A), _A, \
                                          n);                           \
          _A = (void *) ((char *) _A + src_row_stride);                 \
          _B = (void *) ((char *) _B + dst_row_stride);                 \
        }                                                               \
    else                                                                \
      for (size_t i = 0; i < m; i++)                                    \
        {                                                               \
          const void *pA = _A;                                          \
          void *pB = _B;                                                \
          for (size_t j = 0; j < n; j++)                                \
            {                                                           \
              const SCM x =                                             \
                _scm_from_type_indexed_element (src_type_index, pA);    \
              if (PROC_IS_NULL (proc))                                  \
                _scm_to_type_indexed_element (dst_type_index, pB, x);   \
              else                                                      \
                _scm_to_type_indexed_element (dst_type_index, pB,       \
                                              PROC_CALL (proc, x));     \
              pA = (void *) ((char *) pA + src_col_stride);             \
              pB = (void *) ((char *) pB + dst_col_stride);             \
            }                                                           \
          _A = (void *) ((char *) _A + src_row_stride);                 \
          _B = (void *) ((char *) _B + dst_row_stride);                 \
        }                                                               \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return B;                                                           \
  }

#define _C_MAT_TO_TYPED_MAT_PROC_DECL(proc)    SCM (*proc) (SCM)
#define _C_MAT_TO_TYPED_MAT_PROC_IS_NULL(proc) ((proc) == NULL)
#define _C_MAT_TO_TYPED_MAT_PROC_CALL(proc, x) ((*(proc)) ((x)))

#define _MAT_TO_TYPED_MAT_PROC_DECL(proc)    SCM proc
#define _MAT_TO_TYPED_MAT_PROC_IS_NULL(proc) (SCM_UNBNDP (proc))
#define _MAT_TO_TYPED_MAT_PROC_CALL(proc, x) (scm_call_1 ((proc), (x)))

VISIBLE _SCM_MATRIX_TO_TYPED_MATRIX (scm_c_matrix_mapped_to_typed_matrix,
                                     _C_MAT_TO_TYPED_MAT_PROC_DECL,
                                     _C_MAT_TO_TYPED_MAT_PROC_IS_NULL,
                                     _C_MAT_TO_TYPED_MAT_PROC_CALL);

VISIBLE _SCM_MATRIX_TO_TYPED_MATRIX (scm_matrix_mapped_to_typed_matrix,
                                     _MAT_TO_TYPED_MAT_PROC_DECL,
                                     _MAT_TO_TYPED_MAT_PROC_IS_NULL,
                                     _MAT_TO_TYPED_MAT_PROC_CALL);

#define _SCM_MATRIX_TO_EXACT_MATRIX(ELEMTYPE)                           \
  SCM                                                                   \
  scm_matrix_to_##ELEMTYPE##matrix (SCM A)                              \
  {                                                                     \
    SCM B = SCM_UNSPECIFIED;                                            \
                                                                        \
    switch (scm_to_array_type_index (A))                                \
      {                                                                 \
      case _FF_INDEX_ARRAY_U8:                                          \
      case _FF_INDEX_ARRAY_S8:                                          \
      case _FF_INDEX_ARRAY_U16:                                         \
      case _FF_INDEX_ARRAY_S16:                                         \
      case _FF_INDEX_ARRAY_U32:                                         \
      case _FF_INDEX_ARRAY_S32:                                         \
      case _FF_INDEX_ARRAY_U64:                                         \
      case _FF_INDEX_ARRAY_S64:                                         \
        B = scm_c_matrix_mapped_to_typed_matrix (scm_symbol_##ELEMTYPE (), \
                                                 A,                     \
                                                 NULL);                 \
        break;                                                          \
                                                                        \
      case _FF_INDEX_ARRAY_NONUNIFORM:                                  \
      case _FF_INDEX_ARRAY_F32:                                         \
      case _FF_INDEX_ARRAY_F64:                                         \
      case _FF_INDEX_ARRAY_C32:                                         \
      case _FF_INDEX_ARRAY_C64:                                         \
        B = scm_c_matrix_mapped_to_typed_matrix (scm_symbol_##ELEMTYPE (), \
                                                 A,                     \
                                                 scm_inexact_to_exact); \
        break;                                                          \
                                                                        \
      default:                                                          \
        assert_is_matrix (scm_from_latin1_string ("scm_matrix_to_"      \
                                                  #ELEMTYPE "matrix"),  \
                          A);                                           \
        /* If there is a matrix type we missed, the following  */       \
        /* exception may be raised.                            */       \
        rnrs_raise_condition                                            \
          (scm_list_4                                                   \
           (rnrs_make_assertion_violation (),                           \
            rnrs_c_make_who_condition ("scm_matrix_to_"                 \
                                       #ELEMTYPE "matrix"),             \
            rnrs_c_make_message_condition ("internal error"),           \
            rnrs_make_irritants_condition (scm_list_1 (A))));           \
        break;                                                          \
      }                                                                 \
                                                                        \
    return B;                                                           \
  }

#define _SCM_MATRIX_TO_INEXACT_MATRIX(ELEMTYPE)                         \
  SCM                                                                   \
  scm_matrix_to_##ELEMTYPE##matrix (SCM A)                              \
  {                                                                     \
    SCM B = SCM_UNSPECIFIED;                                            \
                                                                        \
    switch (scm_to_array_type_index (A))                                \
      {                                                                 \
      case _FF_INDEX_NOT_AN_ARRAY:                                      \
        assert_is_matrix (scm_from_latin1_string ("scm_matrix_to_"      \
                                                  #ELEMTYPE "matrix"),  \
                          A);                                           \
        break;                                                          \
                                                                        \
      default:                                                          \
        B = scm_c_matrix_mapped_to_typed_matrix (scm_symbol_##ELEMTYPE (), \
                                                 A,NULL);               \
        break;                                                          \
      }                                                                 \
                                                                        \
    return B;                                                           \
  }

VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (u8);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (s8);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (u16);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (s16);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (u32);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (s32);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (u64);
VISIBLE _SCM_MATRIX_TO_EXACT_MATRIX (s64);
VISIBLE _SCM_MATRIX_TO_INEXACT_MATRIX (f32);
VISIBLE _SCM_MATRIX_TO_INEXACT_MATRIX (f64);
VISIBLE _SCM_MATRIX_TO_INEXACT_MATRIX (c32);
VISIBLE _SCM_MATRIX_TO_INEXACT_MATRIX (c64);

VISIBLE SCM
scm_matrix_to_matrix (SCM A)
{
  SCM B = SCM_UNSPECIFIED;

  switch (scm_to_array_type_index (A))
    {
    case _FF_INDEX_NOT_AN_ARRAY:
      assert_is_matrix (scm_from_latin1_string ("scm_matrix_to_matrix"), A);
      break;

    default:
      B = scm_c_matrix_mapped_to_typed_matrix (SCM_BOOL_T, A, NULL);
      break;
    }

  return B;
}

typedef SCM _scm_matrix_to_typed_matrix_t (SCM);

static _scm_matrix_to_typed_matrix_t *_matrix_to_typed_matrix[] = {
  [_FF_INDEX_NOT_AN_ARRAY] = NULL,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_matrix_to_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_matrix_to_u8matrix,
  [_FF_INDEX_ARRAY_S8] = scm_matrix_to_s8matrix,
  [_FF_INDEX_ARRAY_U16] = scm_matrix_to_u16matrix,
  [_FF_INDEX_ARRAY_S16] = scm_matrix_to_s16matrix,
  [_FF_INDEX_ARRAY_U32] = scm_matrix_to_u32matrix,
  [_FF_INDEX_ARRAY_S32] = scm_matrix_to_s32matrix,
  [_FF_INDEX_ARRAY_U64] = scm_matrix_to_u64matrix,
  [_FF_INDEX_ARRAY_S64] = scm_matrix_to_s64matrix,
  [_FF_INDEX_ARRAY_F32] = scm_matrix_to_f32matrix,
  [_FF_INDEX_ARRAY_F64] = scm_matrix_to_f64matrix,
  [_FF_INDEX_ARRAY_C32] = scm_matrix_to_c32matrix,
  [_FF_INDEX_ARRAY_C64] = scm_matrix_to_c64matrix
};

VISIBLE SCM
scm_matrix_to_typed_matrix (SCM type, SCM A)
{
  const scm_t_array_type_index i = scm_array_type_to_array_type_index (type);
  if (i == _FF_INDEX_NOT_AN_ARRAY)
    raise_not_a_valid_matrix_type
      (scm_from_latin1_string ("scm_matrix_to_typed_matrix"), type);
  return _matrix_to_typed_matrix[i] (A);
}

VISIBLE SCM
scm_matrix_exact_to_inexact (SCM A)
{
  SCM B = SCM_UNSPECIFIED;

  switch (scm_to_array_type_index (A))
    {
    case _FF_INDEX_NOT_AN_ARRAY:
      assert_is_matrix (scm_from_latin1_string ("scm_matrix_exact_to_inexact"),
                        A);
      break;

    default:
      B =
        scm_c_matrix_mapped_to_typed_matrix (SCM_BOOL_T, A,
                                             scm_exact_to_inexact);
      break;
    }

  return B;
}

VISIBLE SCM
scm_matrix_inexact_to_exact (SCM A)
{
  SCM B = SCM_UNSPECIFIED;

  switch (scm_to_array_type_index (A))
    {
    case _FF_INDEX_NOT_AN_ARRAY:
      assert_is_matrix (scm_from_latin1_string ("scm_matrix_inexact_to_exact"),
                        A);
      break;

    default:
      B =
        scm_c_matrix_mapped_to_typed_matrix (SCM_BOOL_T, A,
                                             scm_inexact_to_exact);
      break;
    }

  return B;
}

//-------------------------------------------------------------------------

VISIBLE bool
scm_c_for_all_in_matrix (bool sense, SCM pred, SCM A)
{
  const char *who = "scm_c_for_all_in_matrix";

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  const size_t m = scm_c_matrix_numrows (&handle_A);
  const size_t n = scm_c_matrix_numcols (&handle_A);

  scm_array_handle_release (&handle_A);

  bool is_sense = true;
  size_t i = 0;
  while (is_sense && i < m)
    {
      size_t j = 0;
      while (is_sense && j < n)
        {
          SCM x = scm_c_matrix_0ref (A, i, j);
          const bool p = (bool) scm_is_true (scm_call_1 (pred, x));
          is_sense = (p == sense);
          j++;
        }
      i++;
    }

  return is_sense;
}

VISIBLE SCM
scm_for_all_in_matrix (SCM pred, SCM A)
{
  return scm_from_bool (scm_c_for_all_in_matrix (true, pred, A));
}

VISIBLE SCM
scm_exists_in_matrix (SCM pred, SCM A)
{
  return scm_from_bool (!scm_c_for_all_in_matrix (false, pred, A));
}

VISIBLE bool
scm_is_zero_matrix (SCM A)
{
  return scm_c_for_all_in_matrix (true, _scm_zero_p (), A);
}

VISIBLE SCM
scm_zero_matrix_p (SCM A)
{
  return scm_from_bool (scm_is_zero_matrix (A));
}

static bool
scm_c_for_all_in_matrix_ij_based (const char *who,
                                  bool sense, SCM pred, SCM A,
                                  ssize_t i_base, ssize_t j_base)
{
  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  const size_t m = scm_c_matrix_numrows (&handle_A);
  const size_t n = scm_c_matrix_numcols (&handle_A);

  scm_array_handle_release (&handle_A);

  bool is_sense = true;
  size_t i = 0;
  while (is_sense && i < m)
    {
      size_t j = 0;
      while (is_sense && j < n)
        {
          SCM x = scm_c_matrix_0ref (A, i, j);
          const bool p =
            (bool) scm_is_true (scm_call_3
                                (pred, x, scm_from_ssize_t (i_base + i),
                                 scm_from_ssize_t (j_base + j)));
          is_sense = (p == sense);
          j++;
        }
      i++;
    }

  return is_sense;
}

VISIBLE bool
scm_c_for_all_in_matrix_0ij (bool sense, SCM pred, SCM A)
{
  return scm_c_for_all_in_matrix_ij_based ("scm_c_for_all_in_matrix_0ij",
                                           sense, pred, A, 0, 0);
}

VISIBLE bool
scm_c_for_all_in_matrix_1ij (bool sense, SCM pred, SCM A)
{
  return scm_c_for_all_in_matrix_ij_based ("scm_c_for_all_in_matrix_1ij",
                                           sense, pred, A, 1, 1);
}

VISIBLE bool
scm_c_for_all_in_matrix_ij (bool sense, SCM pred, SCM A)
{
  const char *who = "scm_c_for_all_in_matrix_ij";

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);

  if (!scm_array_handle_is_matrix (&handle_A))
    {
      scm_array_handle_release (&handle_A);
      raise_not_a_matrix (scm_from_latin1_string (who), A);
    }

  const ssize_t i_base = scm_c_matrix_rows_lbnd (&handle_A);
  const ssize_t j_base = scm_c_matrix_columns_lbnd (&handle_A);

  scm_array_handle_release (&handle_A);

  return scm_c_for_all_in_matrix_ij_based (who, sense, pred, A, i_base, j_base);
}

VISIBLE SCM
scm_for_all_in_matrix_0ij (SCM pred, SCM A)
{
  return scm_from_bool (scm_c_for_all_in_matrix_0ij (true, pred, A));
}

VISIBLE SCM
scm_exists_in_matrix_0ij (SCM pred, SCM A)
{
  return scm_from_bool (!scm_c_for_all_in_matrix_0ij (false, pred, A));
}

VISIBLE SCM
scm_for_all_in_matrix_1ij (SCM pred, SCM A)
{
  return scm_from_bool (scm_c_for_all_in_matrix_1ij (true, pred, A));
}

VISIBLE SCM
scm_exists_in_matrix_1ij (SCM pred, SCM A)
{
  return scm_from_bool (!scm_c_for_all_in_matrix_1ij (false, pred, A));
}

VISIBLE SCM
scm_for_all_in_matrix_ij (SCM pred, SCM A)
{
  return scm_from_bool (scm_c_for_all_in_matrix_ij (true, pred, A));
}

VISIBLE SCM
scm_exists_in_matrix_ij (SCM pred, SCM A)
{
  return scm_from_bool (!scm_c_for_all_in_matrix_ij (false, pred, A));
}

VISIBLE bool
scm_is_I_matrix (SCM A)
{
  // This does not take account of whether the matrix is square. That
  // behavior is consistent with our definition of I-matrix.
  return scm_c_for_all_in_matrix_0ij (true, _scm_I_element_p (), A);
}

VISIBLE SCM
scm_I_matrix_p (SCM A)
{
  return scm_from_bool (scm_is_I_matrix (A));
}

static SCM
_scm_matrix_comparison_p (const char *who, SCM comparison_0ij_of_B, SCM A,
                          SCM B)
{
  scm_dynwind_begin (0);

  scm_t_array_handle handle_A;
  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_array_handle_is_matrix (who, A, &handle_A);

  scm_t_array_handle handle_B;
  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  assert_array_handle_is_matrix (who, B, &handle_B);

  const size_t m_A = scm_c_matrix_numrows (&handle_A);
  const size_t n_A = scm_c_matrix_numcols (&handle_A);
  const size_t m_B = scm_c_matrix_numrows (&handle_B);
  const size_t n_B = scm_c_matrix_numcols (&handle_B);

  scm_dynwind_end ();

  SCM equal = SCM_BOOL_F;
  if (m_A == m_B && n_A == n_B)
    {
      SCM pred = scm_call_1 (comparison_0ij_of_B, B);
      equal = scm_for_all_in_matrix_0ij (pred, A);
    }
  return equal;
}

VISIBLE SCM
scm_matrix_num_eq_p (SCM A, SCM B)
{
  return _scm_matrix_comparison_p ("scm_matrix_num_eq_p", _num_eq_0ij (), A, B);
}

VISIBLE SCM
scm_matrix_eq_p (SCM A, SCM B)
{
  return _scm_matrix_comparison_p ("scm_matrix_eq_p", _eq_0ij (), A, B);
}

VISIBLE SCM
scm_matrix_eqv_p (SCM A, SCM B)
{
  return _scm_matrix_comparison_p ("scm_matrix_eqv_p", _eqv_0ij (), A, B);
}

VISIBLE SCM
scm_matrix_equal_p (SCM A, SCM B)
{
  return _scm_matrix_comparison_p ("scm_matrix_equal_p", _equal_0ij (), A, B);
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
  scm_c_define_gsubr ("matrix-0column", 2, 0, 0, scm_matrix_0column);
  scm_c_define_gsubr ("matrix-1column", 2, 0, 0, scm_matrix_1column);
  scm_c_define_gsubr ("matrix-column", 2, 0, 0, scm_matrix_column);
  scm_c_define_gsubr ("matrix-as-rank2-array", 1, 0, 0,
                      scm_matrix_as_rank2_array);
  scm_c_define_gsubr ("matrix-as-min-rank-array", 1, 0, 0,
                      scm_matrix_as_min_rank_array);
  scm_c_define_gsubr ("row-matrix->vector", 1, 0, 0, scm_row_matrix_to_vector);
  scm_c_define_gsubr ("matrix-transpose", 1, 0, 0, scm_matrix_transpose);
  scm_c_define_gsubr ("matrix-diagonal", 1, 0, 0, scm_matrix_diagonal);
  scm_c_define_gsubr ("matrix-0block", 5, 0, 0, scm_matrix_0block);
  scm_c_define_gsubr ("matrix-1block", 5, 0, 0, scm_matrix_1block);
  scm_c_define_gsubr ("matrix-block", 5, 0, 0, scm_matrix_block);

  scm_c_define_gsubr ("matrix-0based", 1, 0, 0, scm_matrix_0based);
  scm_c_define_gsubr ("matrix-1based", 1, 0, 0, scm_matrix_1based);
  scm_c_define_gsubr ("matrix-1x1->scalar", 1, 0, 0, scm_matrix_1x1_to_scalar);
  scm_c_define_gsubr ("scalar->matrix", 1, 0, 0, scm_scalar_to_matrix);
  scm_c_define_gsubr ("scalar->u8matrix", 1, 0, 0, scm_scalar_to_u8matrix);
  scm_c_define_gsubr ("scalar->s8matrix", 1, 0, 0, scm_scalar_to_s8matrix);
  scm_c_define_gsubr ("scalar->u16matrix", 1, 0, 0, scm_scalar_to_u16matrix);
  scm_c_define_gsubr ("scalar->s16matrix", 1, 0, 0, scm_scalar_to_s16matrix);
  scm_c_define_gsubr ("scalar->u32matrix", 1, 0, 0, scm_scalar_to_u32matrix);
  scm_c_define_gsubr ("scalar->s32matrix", 1, 0, 0, scm_scalar_to_s32matrix);
  scm_c_define_gsubr ("scalar->u64matrix", 1, 0, 0, scm_scalar_to_u64matrix);
  scm_c_define_gsubr ("scalar->s64matrix", 1, 0, 0, scm_scalar_to_s64matrix);
  scm_c_define_gsubr ("scalar->f32matrix", 1, 0, 0, scm_scalar_to_f32matrix);
  scm_c_define_gsubr ("scalar->f64matrix", 1, 0, 0, scm_scalar_to_f64matrix);
  scm_c_define_gsubr ("scalar->c32matrix", 1, 0, 0, scm_scalar_to_c32matrix);
  scm_c_define_gsubr ("scalar->c64matrix", 1, 0, 0, scm_scalar_to_c64matrix);
  scm_c_define_gsubr ("scalar->typed-matrix", 2, 0, 0,
                      scm_scalar_to_typed_matrix);

  scm_c_define_gsubr ("zero-matrix", 1, 1, 0, scm_zero_matrix);
  scm_c_define_gsubr ("zero-u8matrix", 1, 1, 0, scm_zero_u8matrix);
  scm_c_define_gsubr ("zero-s8matrix", 1, 1, 0, scm_zero_s8matrix);
  scm_c_define_gsubr ("zero-u16matrix", 1, 1, 0, scm_zero_u16matrix);
  scm_c_define_gsubr ("zero-s16matrix", 1, 1, 0, scm_zero_s16matrix);
  scm_c_define_gsubr ("zero-u32matrix", 1, 1, 0, scm_zero_u32matrix);
  scm_c_define_gsubr ("zero-s32matrix", 1, 1, 0, scm_zero_s32matrix);
  scm_c_define_gsubr ("zero-u64matrix", 1, 1, 0, scm_zero_u64matrix);
  scm_c_define_gsubr ("zero-s64matrix", 1, 1, 0, scm_zero_s64matrix);
  scm_c_define_gsubr ("zero-f32matrix", 1, 1, 0, scm_zero_f32matrix);
  scm_c_define_gsubr ("zero-f64matrix", 1, 1, 0, scm_zero_f64matrix);
  scm_c_define_gsubr ("zero-c32matrix", 1, 1, 0, scm_zero_c32matrix);
  scm_c_define_gsubr ("zero-c64matrix", 1, 1, 0, scm_zero_c64matrix);
  scm_c_define_gsubr ("typed-zero-matrix", 2, 1, 0, scm_typed_zero_matrix);

  scm_c_define_gsubr ("filled-matrix", 2, 1, 0, scm_filled_matrix);
  scm_c_define_gsubr ("filled-u8matrix", 2, 1, 0, scm_filled_u8matrix);
  scm_c_define_gsubr ("filled-s8matrix", 2, 1, 0, scm_filled_s8matrix);
  scm_c_define_gsubr ("filled-u16matrix", 2, 1, 0, scm_filled_u16matrix);
  scm_c_define_gsubr ("filled-s16matrix", 2, 1, 0, scm_filled_s16matrix);
  scm_c_define_gsubr ("filled-u32matrix", 2, 1, 0, scm_filled_u32matrix);
  scm_c_define_gsubr ("filled-s32matrix", 2, 1, 0, scm_filled_s32matrix);
  scm_c_define_gsubr ("filled-u64matrix", 2, 1, 0, scm_filled_u64matrix);
  scm_c_define_gsubr ("filled-s64matrix", 2, 1, 0, scm_filled_s64matrix);
  scm_c_define_gsubr ("filled-f32matrix", 2, 1, 0, scm_filled_f32matrix);
  scm_c_define_gsubr ("filled-f64matrix", 2, 1, 0, scm_filled_f64matrix);
  scm_c_define_gsubr ("filled-c32matrix", 2, 1, 0, scm_filled_c32matrix);
  scm_c_define_gsubr ("filled-c64matrix", 2, 1, 0, scm_filled_c64matrix);
  scm_c_define_gsubr ("typed-filled-matrix", 3, 1, 0, scm_typed_filled_matrix);

  scm_c_define_gsubr ("I-matrix", 1, 1, 0, scm_I_matrix);
  scm_c_define_gsubr ("I-u8matrix", 1, 1, 0, scm_I_u8matrix);
  scm_c_define_gsubr ("I-s8matrix", 1, 1, 0, scm_I_s8matrix);
  scm_c_define_gsubr ("I-u16matrix", 1, 1, 0, scm_I_u16matrix);
  scm_c_define_gsubr ("I-s16matrix", 1, 1, 0, scm_I_s16matrix);
  scm_c_define_gsubr ("I-u32matrix", 1, 1, 0, scm_I_u32matrix);
  scm_c_define_gsubr ("I-s32matrix", 1, 1, 0, scm_I_s32matrix);
  scm_c_define_gsubr ("I-u64matrix", 1, 1, 0, scm_I_u64matrix);
  scm_c_define_gsubr ("I-s64matrix", 1, 1, 0, scm_I_s64matrix);
  scm_c_define_gsubr ("I-f32matrix", 1, 1, 0, scm_I_f32matrix);
  scm_c_define_gsubr ("I-f64matrix", 1, 1, 0, scm_I_f64matrix);
  scm_c_define_gsubr ("I-c32matrix", 1, 1, 0, scm_I_c32matrix);
  scm_c_define_gsubr ("I-c64matrix", 1, 1, 0, scm_I_c64matrix);
  scm_c_define_gsubr ("typed-I-matrix", 2, 1, 0, scm_typed_I_matrix);

  scm_c_define_gsubr ("scalar-matrix", 2, 1, 0, scm_scalar_matrix);
  scm_c_define_gsubr ("scalar-u8matrix", 2, 1, 0, scm_scalar_u8matrix);
  scm_c_define_gsubr ("scalar-s8matrix", 2, 1, 0, scm_scalar_s8matrix);
  scm_c_define_gsubr ("scalar-u16matrix", 2, 1, 0, scm_scalar_u16matrix);
  scm_c_define_gsubr ("scalar-s16matrix", 2, 1, 0, scm_scalar_s16matrix);
  scm_c_define_gsubr ("scalar-u32matrix", 2, 1, 0, scm_scalar_u32matrix);
  scm_c_define_gsubr ("scalar-s32matrix", 2, 1, 0, scm_scalar_s32matrix);
  scm_c_define_gsubr ("scalar-u64matrix", 2, 1, 0, scm_scalar_u64matrix);
  scm_c_define_gsubr ("scalar-s64matrix", 2, 1, 0, scm_scalar_s64matrix);
  scm_c_define_gsubr ("scalar-f32matrix", 2, 1, 0, scm_scalar_f32matrix);
  scm_c_define_gsubr ("scalar-f64matrix", 2, 1, 0, scm_scalar_f64matrix);
  scm_c_define_gsubr ("scalar-c32matrix", 2, 1, 0, scm_scalar_c32matrix);
  scm_c_define_gsubr ("scalar-c64matrix", 2, 1, 0, scm_scalar_c64matrix);
  scm_c_define_gsubr ("typed-scalar-matrix", 3, 1, 0, scm_typed_scalar_matrix);

  scm_c_define_gsubr ("row-matrix->diagonal-matrix", 1, 0, 0,
                      scm_row_matrix_to_diagonal_matrix);

  scm_c_define_gsubr ("matrix-copy", 1, 0, 0, scm_matrix_copy);

  scm_c_define_gsubr ("private:matrix-mapped-to-typed-matrix", 2, 1, 0,
                      scm_matrix_mapped_to_typed_matrix);

  scm_c_define_gsubr ("matrix->matrix", 1, 0, 0, scm_matrix_to_matrix);
  scm_c_define_gsubr ("matrix->u8matrix", 1, 0, 0, scm_matrix_to_u8matrix);
  scm_c_define_gsubr ("matrix->s8matrix", 1, 0, 0, scm_matrix_to_s8matrix);
  scm_c_define_gsubr ("matrix->u16matrix", 1, 0, 0, scm_matrix_to_u16matrix);
  scm_c_define_gsubr ("matrix->s16matrix", 1, 0, 0, scm_matrix_to_s16matrix);
  scm_c_define_gsubr ("matrix->u32matrix", 1, 0, 0, scm_matrix_to_u32matrix);
  scm_c_define_gsubr ("matrix->s32matrix", 1, 0, 0, scm_matrix_to_s32matrix);
  scm_c_define_gsubr ("matrix->u64matrix", 1, 0, 0, scm_matrix_to_u64matrix);
  scm_c_define_gsubr ("matrix->s64matrix", 1, 0, 0, scm_matrix_to_s64matrix);
  scm_c_define_gsubr ("matrix->f32matrix", 1, 0, 0, scm_matrix_to_f32matrix);
  scm_c_define_gsubr ("matrix->f64matrix", 1, 0, 0, scm_matrix_to_f64matrix);
  scm_c_define_gsubr ("matrix->c32matrix", 1, 0, 0, scm_matrix_to_c32matrix);
  scm_c_define_gsubr ("matrix->c64matrix", 1, 0, 0, scm_matrix_to_c64matrix);
  scm_c_define_gsubr ("matrix->typed-matrix", 2, 0, 0,
                      scm_matrix_to_typed_matrix);
  scm_c_define_gsubr ("matrix-exact->inexact", 1, 0, 0,
                      scm_matrix_exact_to_inexact);
  scm_c_define_gsubr ("matrix-inexact->exact", 1, 0, 0,
                      scm_matrix_inexact_to_exact);

  scm_c_define_gsubr ("for-all-in-matrix", 2, 0, 0, scm_for_all_in_matrix);
  scm_c_define_gsubr ("exists-in-matrix", 2, 0, 0, scm_exists_in_matrix);
  scm_c_define_gsubr ("for-all-in-matrix-0ij", 2, 0, 0,
                      scm_for_all_in_matrix_0ij);
  scm_c_define_gsubr ("exists-in-matrix-0ij", 2, 0, 0,
                      scm_exists_in_matrix_0ij);
  scm_c_define_gsubr ("for-all-in-matrix-1ij", 2, 0, 0,
                      scm_for_all_in_matrix_1ij);
  scm_c_define_gsubr ("exists-in-matrix-1ij", 2, 0, 0,
                      scm_exists_in_matrix_1ij);
  scm_c_define_gsubr ("for-all-in-matrix-ij", 2, 0, 0,
                      scm_for_all_in_matrix_ij);
  scm_c_define_gsubr ("exists-in-matrix-ij", 2, 0, 0, scm_exists_in_matrix_ij);
  scm_c_define_gsubr ("zero-matrix?", 1, 0, 0, scm_zero_matrix_p);
  scm_c_define_gsubr ("I-matrix?", 1, 0, 0, scm_I_matrix_p);
  scm_c_define_gsubr ("matrix=?", 2, 0, 0, scm_matrix_num_eq_p);
  scm_c_define_gsubr ("matrix-eq?", 2, 0, 0, scm_matrix_eq_p);
  scm_c_define_gsubr ("matrix-eqv?", 2, 0, 0, scm_matrix_eqv_p);
  scm_c_define_gsubr ("matrix-equal?", 2, 0, 0, scm_matrix_equal_p);
}

//-------------------------------------------------------------------------
