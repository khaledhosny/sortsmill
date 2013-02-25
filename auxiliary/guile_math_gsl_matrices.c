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

#include <assert.h>
#include <sortsmill/guile.h>
#include <sortsmill/gmp_constants.h>
#include <sortsmill/gmp_matrix.h>
#include <sortsmill/scm_matrix.h>
#include <sortsmill/xgc.h>
#include <intl.h>

void init_guile_sortsmill_math_gsl_matrices (void);

VISIBLE void
exception__array_has_no_elements (const char *who, SCM irritants)
{
  const char *message = _("array has no elements");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

VISIBLE void
exception__expected_array_of_rank_1 (const char *who, SCM irritants)
{
  const char *message = _("expected array of rank 1");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

VISIBLE void
exception__expected_array_of_rank_2 (const char *who, SCM irritants)
{
  const char *message = _("expected array of rank 2");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

VISIBLE void
exception__expected_array_of_rank_1_or_2 (const char *who, SCM irritants)
{
  const char *message = _("expected array of rank 1 or 2");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

VISIBLE void
exception__layout_incompatible_with_gsl (const char *who, SCM irritants)
{
  const char *message =
    _("an f64matrix was encountered whose layout is incompatible with GSL");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

VISIBLE void
exception__unexpected_array_type (const char *who, SCM a)
{
  const char *message = _("unexpected array type");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (scm_list_1 (a))));
}

VISIBLE gsl_vector_const_view
scm_gsl_vector_const_view_array_handle (SCM array, scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_vector_const_view_array_handle";

  const double *my_elems;
  size_t stride;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    exception__expected_array_of_rank_1 (who, scm_list_1 (array));

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n = dims[0].ubnd - dims[0].lbnd + 1;
  if (n < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (0 < dims[0].inc)
    {
      my_elems = elems;
      stride = dims[0].inc;
    }
  else
    {
      double *buffer = scm_gc_malloc_pointerless (n * sizeof (double), who);
      for (size_t i = 0; i < n; i++)
        buffer[i] = elems[i * dims[0].inc];
      my_elems = buffer;
      stride = 1;
    }
  return gsl_vector_const_view_array_with_stride (my_elems, stride, n);
}

VISIBLE gsl_vector_view
scm_gsl_vector_view_array_handle (SCM array, scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_vector_view_array_handle";

  double *my_elems;
  size_t stride;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    exception__expected_array_of_rank_1 (who, scm_list_1 (array));

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n = dims[0].ubnd - dims[0].lbnd + 1;
  if (n < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (0 < dims[0].inc)
    {
      my_elems = elems;
      stride = dims[0].inc;
    }
  else
    exception__layout_incompatible_with_gsl (who, scm_list_1 (array));
  return gsl_vector_view_array_with_stride (my_elems, stride, n);
}

static gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle_rank1 (const char *who,
                                              SCM array,
                                              scm_t_array_handle *handlep)
{
  const double *my_elems;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    exception__expected_array_of_rank_1 (who, scm_list_1 (array));
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  if (n0 < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (dims[0].inc == 1)
    my_elems = elems;
  else
    {
      double *buffer = scm_gc_malloc_pointerless (n0 * sizeof (double), who);
      for (size_t i = 0; i < n0; i++)
        buffer[i] = elems[i * dims[0].inc];
      my_elems = buffer;
    }
  return gsl_matrix_const_view_array_with_tda (my_elems, 1, n0, n0);
}

static gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle_rank2 (const char *who,
                                              SCM array,
                                              scm_t_array_handle *handlep)
{
  const double *my_elems;
  size_t tda;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    exception__expected_array_of_rank_2 (who, scm_list_1 (array));
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (dims[1].inc == 1 && 0 < dims[0].inc)
    {
      my_elems = elems;
      tda = dims[0].inc;
    }
  else
    {
      double *buffer =
        scm_gc_malloc_pointerless (n0 * n1 * sizeof (double), who);
      for (size_t i = 0; i < n0; i++)
        for (size_t j = 0; j < n1; j++)
          buffer[i * n1 + j] = elems[i * dims[0].inc + j * dims[1].inc];
      my_elems = buffer;
      tda = n1;
    }
  return gsl_matrix_const_view_array_with_tda (my_elems, n0, n1, tda);
}

VISIBLE gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle (SCM array, scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_matrix_const_view_array_handle";

  typedef gsl_matrix_const_view (*func_t) (const char *, SCM array,
                                           scm_t_array_handle *);
  static const func_t func[] = {
    NULL,
    scm_gsl_matrix_const_view_array_handle_rank1,
    scm_gsl_matrix_const_view_array_handle_rank2
  };

  const size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1 && rank != 2)
    exception__expected_array_of_rank_1_or_2 (who, scm_list_1 (array));
  return func[rank] (who, array, handlep);
}

static gsl_matrix_view
scm_gsl_matrix_view_array_handle_rank1 (const char *who,
                                        SCM array, scm_t_array_handle *handlep)
{
  double *my_elems;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    exception__expected_array_of_rank_1 (who, scm_list_1 (array));
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  if (n0 < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (dims[0].inc == 1)
    my_elems = elems;
  else
    exception__layout_incompatible_with_gsl (who, scm_list_1 (array));
  return gsl_matrix_view_array_with_tda (my_elems, 1, n0, n0);
}

static gsl_matrix_view
scm_gsl_matrix_view_array_handle_rank2 (const char *who,
                                        SCM array, scm_t_array_handle *handlep)
{
  double *my_elems;
  size_t tda;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    exception__expected_array_of_rank_2 (who, scm_list_1 (array));
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    exception__array_has_no_elements (who, scm_list_1 (array));
  if (dims[1].inc == 1 && 0 < dims[0].inc)
    {
      my_elems = elems;
      tda = dims[0].inc;
    }
  else
    exception__layout_incompatible_with_gsl (who, scm_list_1 (array));
  return gsl_matrix_view_array_with_tda (my_elems, n0, n1, tda);
}

// FIXME: This has not been tested.
VISIBLE gsl_matrix_view
scm_gsl_matrix_view_array_handle (SCM array, scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_matrix_view_array_handle";

  typedef gsl_matrix_view (*func_t) (const char *, SCM array,
                                     scm_t_array_handle *);
  static const func_t func[] = {
    NULL,
    scm_gsl_matrix_view_array_handle_rank1,
    scm_gsl_matrix_view_array_handle_rank2
  };

  const size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1 && rank != 2)
    exception__expected_array_of_rank_1_or_2 (who, scm_list_1 (array));
  return func[rank] (who, array, handlep);
}

VISIBLE SCM
scm_gsl_vector_to_f64vector (const gsl_vector *v, int low_index)
{
  scm_t_array_handle handle;

  const char *who = "scm_gsl_vector_to_f64vector";

  if (v->size < 1)
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("gsl_vector size is zero"))));

  SCM bounds = scm_list_2 (scm_from_int (low_index),
                           scm_from_int (low_index + v->size - 1));
  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_1 (bounds));

  scm_dynwind_begin (0);

  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);

  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  double *elems = scm_array_handle_f64_writable_elements (&handle);
  for (size_t i = 0; i < v->size; i++)
    elems[i * dims[0].inc] = v->data[i * v->stride];

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_matrix_to_f64matrix (const gsl_matrix *m, int low_index)
{
  scm_t_array_handle handle;

  const char *who = "scm_gsl_matrix_to_f64matrix";

  if (m->size1 < 1)
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("gsl_matrix size1 is zero"))));

  if (m->size2 < 1)
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("gsl_matrix size2 is zero"))));

  SCM row_bounds = scm_list_2 (scm_from_int (low_index),
                               scm_from_int (low_index + m->size1 - 1));
  SCM column_bounds = scm_list_2 (scm_from_int (low_index),
                                  scm_from_int (low_index + m->size2 - 1));
  SCM result = scm_make_typed_array (scm_symbol_f64 (), SCM_UNSPECIFIED,
                                     scm_list_2 (row_bounds, column_bounds));
  scm_dynwind_begin (0);

  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);

  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  double *elems = scm_array_handle_f64_writable_elements (&handle);
  for (size_t i = 0; i < m->size1; i++)
    for (size_t j = 0; j < m->size2; j++)
      elems[i * dims[0].inc + j * dims[1].inc] = m->data[i * m->tda + j];

  scm_dynwind_end ();

  return result;
}

static void
scm_array_handle_illegal_to_mpz_matrix (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        mpz_t A[m][n])
{
  assert (false);
}

static void
scm_array_handle_illegal_to_mpq_matrix (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        mpq_t A[m][n])
{
  assert (false);
}

static void
assert_rank_1_or_2_array (SCM who, SCM array)
{
  scm_call_2 (scm_c_private_ref ("sortsmill math gsl matrices",
                                 "assert-rank-1-or-2-array"), who, array);
}

static void
assert_c_rank_1_or_2_array (const char *who, SCM array,
                            scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1 && rank != 2)
    assert_rank_1_or_2_array (scm_from_utf8_string (who), array);
}

static void
scm_array_handle_nonuniform_to_mpz_matrix (scm_t_array_handle *handlep,
                                           unsigned int m, unsigned int n,
                                           mpz_t A[m][n])
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const SCM *elems = scm_array_handle_elements (handlep);
  if (rank == 1)
    {
      assert (m == 1);
      for (unsigned int j = 0; j < n; j++)
        {
          SCM x = elems[j * dims[0].inc];
          scm_to_mpz (x, A[0][j]);
        }
    }
  else
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        {
          SCM x = elems[i * dims[0].inc + j * dims[1].inc];
          scm_to_mpz (x, A[i][j]);
        }
}

static void
scm_array_handle_nonuniform_to_mpq_matrix (scm_t_array_handle *handlep,
                                           unsigned int m, unsigned int n,
                                           mpq_t A[m][n])
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const SCM *elems = scm_array_handle_elements (handlep);
  if (rank == 1)
    {
      assert (m == 1);
      for (unsigned int j = 0; j < n; j++)
        {
          SCM x = elems[j * dims[0].inc];
          scm_to_mpq (x, A[0][j]);
        }
    }
  else
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        {
          SCM x = elems[i * dims[0].inc + j * dims[1].inc];
          scm_to_mpq (x, A[i][j]);
        }
}

#define _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX(LONG_TYPE, MEDIUM_TYPE,     \
                                            SHORT_TYPE)                 \
  void                                                                  \
  scm_array_handle_##SHORT_TYPE##_to_mpz_matrix                         \
  (scm_t_array_handle *handlep, unsigned int m, unsigned int n,         \
   mpz_t A[m][n])                                                       \
  {                                                                     \
    const size_t rank = scm_array_handle_rank (handlep);                \
    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
    const LONG_TYPE *elems =                                            \
      scm_array_handle_##SHORT_TYPE##_elements (handlep);               \
    if (rank == 1)                                                      \
      {                                                                 \
        assert (m == 1);                                                \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            LONG_TYPE x = elems[j * dims[0].inc];                       \
            scm_to_mpz (scm_from_##MEDIUM_TYPE (x), A[0][j]);           \
          }                                                             \
      }                                                                 \
    else                                                                \
      for (unsigned int i = 0; i < m; i++)                              \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            LONG_TYPE x = elems[i * dims[0].inc + j * dims[1].inc];     \
            scm_to_mpz (scm_from_##MEDIUM_TYPE (x), A[i][j]);           \
          }                                                             \
  }

static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (uint8_t, uint8, u8);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (int8_t, int8, s8);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (uint16_t, uint16, u16);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (int16_t, int16, s16);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (uint32_t, uint32, u32);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (int32_t, int32, s32);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (uint64_t, uint64, u64);
static _SCM_ARRAY_HANDLE_INT_TO_MPZ_MATRIX (int64_t, int64, s64);

typedef void _scm_to_mpz_matrix_func_t (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        mpz_t A[m][n]);

static _scm_to_mpz_matrix_func_t *scm_to_mpz_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_mpz_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_array_handle_nonuniform_to_mpz_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_mpz_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_mpz_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_mpz_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_mpz_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_mpz_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_mpz_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_mpz_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_mpz_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_illegal_to_mpz_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_illegal_to_mpz_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_illegal_to_mpz_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_illegal_to_mpz_matrix
};

#define _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX(LONG_TYPE, MEDIUM_TYPE,     \
                                            SHORT_TYPE)                 \
  void                                                                  \
  scm_array_handle_##SHORT_TYPE##_to_mpq_matrix                         \
  (scm_t_array_handle *handlep, unsigned int m, unsigned int n,         \
   mpq_t A[m][n])                                                       \
  {                                                                     \
    const size_t rank = scm_array_handle_rank (handlep);                \
    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
    const LONG_TYPE *elems =                                            \
      scm_array_handle_##SHORT_TYPE##_elements (handlep);               \
    if (rank == 1)                                                      \
      {                                                                 \
        assert (m == 1);                                                \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            LONG_TYPE x = elems[j * dims[0].inc];                       \
            scm_to_mpz (scm_from_##MEDIUM_TYPE (x),                     \
                        mpq_numref (A[0][j]));                          \
            mpz_set (mpq_denref (A[0][j]), mpz_one ());                 \
          }                                                             \
      }                                                                 \
    else                                                                \
      for (unsigned int i = 0; i < m; i++)                              \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            LONG_TYPE x = elems[i * dims[0].inc + j * dims[1].inc];     \
            scm_to_mpz (scm_from_##MEDIUM_TYPE (x),                     \
                        mpq_numref (A[i][j]));                          \
            mpz_set (mpq_denref (A[i][j]), mpz_one ());                 \
          }                                                             \
  }

static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (uint8_t, uint8, u8);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (int8_t, int8, s8);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (uint16_t, uint16, u16);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (int16_t, int16, s16);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (uint32_t, uint32, u32);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (int32_t, int32, s32);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (uint64_t, uint64, u64);
static _SCM_ARRAY_HANDLE_INT_TO_MPQ_MATRIX (int64_t, int64, s64);

typedef void _scm_to_mpq_matrix_func_t (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        mpq_t A[m][n]);

static _scm_to_mpq_matrix_func_t *scm_to_mpq_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_mpq_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_array_handle_nonuniform_to_mpq_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_mpq_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_mpq_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_mpq_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_mpq_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_mpq_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_mpq_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_mpq_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_mpq_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_illegal_to_mpq_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_illegal_to_mpq_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_illegal_to_mpq_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_illegal_to_mpq_matrix
};

VISIBLE void
scm_array_handle_to_mpz_matrix (SCM array, scm_t_array_handle *handlep,
                                unsigned int m, unsigned int n, mpz_t A[m][n])
{
  const char *who = "scm_array_handle_to_mpz_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  if (scm_is_integer_array (array))
    scm_to_mpz_matrix_func[scm_array_handle_to_array_type_index (handlep)]
      (handlep, m, n, A);
  else
    exception__unexpected_array_type (who, array);
}

VISIBLE void
scm_array_handle_to_mpq_matrix (SCM array, scm_t_array_handle *handlep,
                                unsigned int m, unsigned int n, mpq_t A[m][n])
{
  const char *who = "scm_array_handle_to_mpq_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  if (scm_is_exact_array (array))
    scm_to_mpq_matrix_func[scm_array_handle_to_array_type_index (handlep)]
      (handlep, m, n, A);
  else
    exception__unexpected_array_type (who, array);
}

VISIBLE void
scm_array_handle_to_scm_matrix (SCM array, scm_t_array_handle *handlep,
                                unsigned int m, unsigned int n, SCM A[m][n])
{
  const char *who = "scm_array_handle_to_scm_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const SCM *elems = scm_array_handle_elements (handlep);
  if (rank == 1)
    {
      assert (m == 1);
      for (unsigned int j = 0; j < n; j++)
        A[0][j] = elems[j * dims[0].inc];
    }
  else
    for (unsigned int i = 0; i < m; i++)
      for (unsigned int j = 0; j < n; j++)
        A[i][j] = elems[i * dims[0].inc + j * dims[1].inc];
}

VISIBLE SCM
scm_from_mpz_matrix (unsigned int m, unsigned int n, mpz_t A[m][n])
{
  scm_t_array_handle handle;

  SCM bounds = scm_list_2 (scm_list_2 (scm_from_uint (1), scm_from_uint (m)),
                           scm_list_2 (scm_from_uint (1), scm_from_uint (n)));
  SCM array = scm_make_array (SCM_UNSPECIFIED, bounds);

  scm_dynwind_begin (0);

  scm_array_get_handle (array, &handle);
  scm_dynwind_array_handle_release (&handle);

  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  SCM *elems = scm_array_handle_writable_elements (&handle);
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      elems[i * dims[0].inc + j * dims[1].inc] = scm_from_mpz (A[i][j]);

  scm_dynwind_end ();

  return array;
}

VISIBLE SCM
scm_from_mpq_matrix (unsigned int m, unsigned int n, mpq_t A[m][n])
{
  scm_t_array_handle handle;

  SCM bounds = scm_list_2 (scm_list_2 (scm_from_uint (1), scm_from_uint (m)),
                           scm_list_2 (scm_from_uint (1), scm_from_uint (n)));
  SCM array = scm_make_array (SCM_UNSPECIFIED, bounds);

  scm_dynwind_begin (0);

  scm_array_get_handle (array, &handle);
  scm_dynwind_array_handle_release (&handle);

  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  SCM *elems = scm_array_handle_writable_elements (&handle);
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      elems[i * dims[0].inc + j * dims[1].inc] = scm_from_mpq (A[i][j]);

  scm_dynwind_end ();

  return array;
}

VISIBLE SCM
scm_from_scm_matrix (unsigned int m, unsigned int n, SCM A[m][n])
{
  scm_t_array_handle handle;

  SCM bounds = scm_list_2 (scm_list_2 (scm_from_uint (1), scm_from_uint (m)),
                           scm_list_2 (scm_from_uint (1), scm_from_uint (n)));
  SCM array = scm_make_array (SCM_UNSPECIFIED, bounds);

  scm_dynwind_begin (0);

  scm_array_get_handle (array, &handle);
  scm_dynwind_array_handle_release (&handle);

  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  SCM *elems = scm_array_handle_writable_elements (&handle);
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      elems[i * dims[0].inc + j * dims[1].inc] = A[i][j];

  scm_dynwind_end ();

  return array;
}

static void
assert_f64_rank_1_or_2_array (SCM who, SCM array)
{
  scm_call_2 (scm_c_private_ref ("sortsmill math gsl matrices",
                                 "assert-f64-rank-1-or-2-array"), who, array);
}

static void
assert_c_f64_rank_1_or_2_array (const char *who, SCM array,
                                scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  if (!scm_is_typed_array (array, scm_symbol_f64 ())
      || (rank != 1 && rank != 2))
    assert_f64_rank_1_or_2_array (scm_from_utf8_string (who), array);
}

static void
assert_exact_rank_1_or_2_array (SCM who, SCM array)
{
  scm_call_2 (scm_c_private_ref ("sortsmill math gsl matrices",
                                 "assert-exact-rank-1-or-2-array"), who, array);
}

static void
assert_c_exact_rank_1_or_2_array (const char *who, SCM array,
                                  scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  if (!scm_is_exact_array (array) || (rank != 1 && rank != 2))
    assert_exact_rank_1_or_2_array (scm_from_utf8_string (who), array);
}

static void
assert_CblasTrans_flag (SCM who, SCM trans)
{
  scm_call_2
    (scm_c_private_ref
     ("sortsmill math gsl matrices", "assert-CblasTrans-flag"), who, trans);
}

static void
assert_c_CblasTrans_flag (const char *who, SCM trans, int c_trans)
{
  if (c_trans != CblasNoTrans && c_trans != CblasTrans
      && c_trans != CblasConjTrans)
    assert_CblasTrans_flag (scm_from_utf8_string (who), trans);
}

static CBLAS_TRANSPOSE_t
scm_to_CBLAS_TRANSPOSE_t (const char *who, SCM trans)
{
  CBLAS_TRANSPOSE_t c_trans = scm_to_int (trans);
  assert_c_CblasTrans_flag (who, trans, c_trans);
  return c_trans;
}

static inline size_t
transposed_size (CBLAS_TRANSPOSE_t trans, size_t size1, size_t size2)
{
  return (trans == CblasNoTrans) ? size1 : size2;
}

static size_t
matrix_dim1 (scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  return (rank == 1) ? 1 : dims[0].ubnd - dims[0].lbnd + 1;
}

static size_t
matrix_dim2 (scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  return dims[rank - 1].ubnd - dims[rank - 1].lbnd + 1;
}

static void
matrix_has_dimension_of_size_zero (const char *who, SCM A)
{
  const char *localized_message = _("matrix has dimension of size zero");
  SCM message = scm_from_locale_string (localized_message);
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_1 (A))));
}

static void
non_conformable_for_multiplication (const char *who, SCM A, SCM B,
                                    size_t m_A, size_t k_A,
                                    size_t k_B, size_t n_B)
{
  const char *localized_message =
    _("non-conformable matrices: ~ax~a multiplied by ~ax~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_4 (scm_from_int (m_A),
                                         scm_from_int (k_A),
                                         scm_from_int (k_B),
                                         scm_from_int (n_B)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (A, B))));
}

static void
non_conformable_for_gemm_C_lead (const char *who, SCM A, SCM C,
                                 size_t m_A, size_t k_A, size_t m_C, size_t n_C)
{
  const char *localized_message =
    _("lead dimensions mismatched: ~ax~a and ~ax~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_4 (scm_from_int (m_A),
                                         scm_from_int (k_A),
                                         scm_from_int (m_C),
                                         scm_from_int (n_C)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (A, C))));
}

static void
non_conformable_for_gemm_C_trailing (const char *who, SCM B, SCM C,
                                     size_t k_B, size_t n_B,
                                     size_t m_C, size_t n_C)
{
  const char *localized_message =
    _("trailing dimensions mismatched: ~ax~a and ~ax~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_4 (scm_from_int (k_B),
                                         scm_from_int (n_B),
                                         scm_from_int (m_C),
                                         scm_from_int (n_C)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (B, C))));
}

static void
assert_conformable_for_gemm (const char *who,
                             CBLAS_TRANSPOSE_t _TransA,
                             CBLAS_TRANSPOSE_t _TransB,
                             SCM A, SCM B, SCM C,
                             size_t m_A, size_t k_A,
                             size_t k_B, size_t n_B, size_t m_C, size_t n_C)
{
  if ((m_A | k_A) == 0)
    matrix_has_dimension_of_size_zero (who, A);
  if ((k_B | n_B) == 0)
    matrix_has_dimension_of_size_zero (who, B);
  if ((m_C | n_C) == 0)
    matrix_has_dimension_of_size_zero (who, C);
  if (k_A != k_B)
    non_conformable_for_multiplication (who, A, B, m_A, k_A, k_B, n_B);
  if (m_A != m_C)
    non_conformable_for_gemm_C_lead (who, A, C, m_A, k_A, m_C, n_C);
  if (n_B != n_C)
    non_conformable_for_gemm_C_trailing (who, B, C, k_B, n_B, m_C, n_C);
}

gsl_matrix_const_view null_gsl_matrix_const_view = { 0 };

VISIBLE SCM
scm_gsl_blas_dgemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B,
                    SCM beta, SCM C)
{
  // Note: if beta = 0 exactly, then C is ignored. You can set it to
  // anything.

  const char *who = "scm_gsl_blas_dgemm";

  scm_t_array_handle handle_A;
  scm_t_array_handle handle_B;
  scm_t_array_handle handle_C;

  CBLAS_TRANSPOSE_t _TransA = scm_to_CBLAS_TRANSPOSE_t (who, TransA);
  CBLAS_TRANSPOSE_t _TransB = scm_to_CBLAS_TRANSPOSE_t (who, TransB);

  const double _alpha = scm_to_double (alpha);
  const double _beta = scm_to_double (beta);

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_f64_rank_1_or_2_array (who, A, &handle_A);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  assert_c_f64_rank_1_or_2_array (who, B, &handle_B);

  gsl_matrix_const_view _A =
    scm_gsl_matrix_const_view_array_handle (A, &handle_A);
  gsl_matrix_const_view _B =
    scm_gsl_matrix_const_view_array_handle (B, &handle_B);

  const size_t m_A =
    transposed_size (_TransA, _A.matrix.size1, _A.matrix.size2);
  const size_t k_A =
    transposed_size (_TransA, _A.matrix.size2, _A.matrix.size1);

  const size_t k_B =
    transposed_size (_TransB, _B.matrix.size1, _B.matrix.size2);
  const size_t n_B =
    transposed_size (_TransB, _B.matrix.size2, _B.matrix.size1);

  if (_beta != 0)
    {
      scm_array_get_handle (C, &handle_C);
      scm_dynwind_array_handle_release (&handle_C);
      assert_c_f64_rank_1_or_2_array (who, C, &handle_C);
    }

  gsl_matrix_const_view _C =
    (_beta != 0) ?
    scm_gsl_matrix_const_view_array_handle (C, &handle_C) :
    null_gsl_matrix_const_view;

  size_t m_C;
  size_t n_C;

  if (_beta == 0)
    {
      m_C = m_A;
      n_C = n_B;
    }
  else
    {
      m_C = _C.matrix.size1;
      n_C = _C.matrix.size2;
    }

  assert_conformable_for_gemm (who, _TransA, _TransB, A, B, C,
                               m_A, k_A, k_B, n_B, m_C, n_C);

  double result_buffer[m_C][n_C];
  gsl_matrix_view _result = gsl_matrix_view_array (&result_buffer[0][0],
                                                   m_C, n_C);
  if (_beta != 0)
    gsl_matrix_memcpy (&_result.matrix, &_C.matrix);
  const int errval =
    gsl_blas_dgemm (_TransA, _TransB, _alpha, &_A.matrix, &_B.matrix,
                    _beta, &_result.matrix);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_n (TransA, TransB, alpha, A, B, beta, C,
                               SCM_UNDEFINED), SCM_UNDEFINED));
  SCM result = scm_gsl_matrix_to_f64matrix (&_result.matrix, 1);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_mpz_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B, SCM beta,
                  SCM C)
{
  // Note: if beta = 0 exactly, then C is ignored. You can set it to
  // anything.

  const char *who = "scm_gsl_mpz_dgemm";

  scm_t_array_handle handle_A;
  scm_t_array_handle handle_B;
  scm_t_array_handle handle_C;

  CBLAS_TRANSPOSE_t _TransA = scm_to_CBLAS_TRANSPOSE_t (who, TransA);
  CBLAS_TRANSPOSE_t _TransB = scm_to_CBLAS_TRANSPOSE_t (who, TransB);

  scm_dynwind_begin (0);

  mpz_t _alpha;
  mpz_init (_alpha);
  scm_dynwind_mpz_clear (_alpha);
  scm_to_mpz (alpha, _alpha);

  mpz_t _beta;
  mpz_init (_beta);
  scm_dynwind_mpz_clear (_beta);
  scm_to_mpz (beta, _beta);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_exact_rank_1_or_2_array (who, A, &handle_A);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  assert_c_exact_rank_1_or_2_array (who, B, &handle_B);

  const size_t dim1_A = matrix_dim1 (&handle_A);
  const size_t dim2_A = matrix_dim2 (&handle_A);
  const size_t m_A = transposed_size (_TransA, dim1_A, dim2_A);
  const size_t k_A = transposed_size (_TransA, dim2_A, dim1_A);

  const size_t dim1_B = matrix_dim1 (&handle_B);
  const size_t dim2_B = matrix_dim2 (&handle_B);
  const size_t k_B = transposed_size (_TransB, dim1_B, dim2_B);
  const size_t n_B = transposed_size (_TransB, dim2_B, dim1_B);

  mpz_t _A[dim1_A][dim2_A];
  mpz_matrix_init (dim1_A, dim2_A, _A);
  scm_dynwind_mpz_matrix_clear (dim1_A, dim2_A, _A);
  scm_array_handle_to_mpz_matrix (A, &handle_A, dim1_A, dim2_A, _A);

  mpz_t _B[dim1_B][dim2_B];
  mpz_matrix_init (dim1_B, dim2_B, _B);
  scm_dynwind_mpz_matrix_clear (dim1_B, dim2_B, _B);
  scm_array_handle_to_mpz_matrix (B, &handle_B, dim1_B, dim2_B, _B);

  size_t m_C;
  size_t n_C;
  if (mpz_sgn (_beta) == 0)
    {
      m_C = m_A;
      n_C = n_B;
    }
  else
    {
      scm_array_get_handle (C, &handle_C);
      scm_dynwind_array_handle_release (&handle_C);
      assert_c_exact_rank_1_or_2_array (who, C, &handle_C);

      m_C = matrix_dim1 (&handle_C);
      n_C = matrix_dim2 (&handle_C);
    }

  mpz_t _C[m_C][n_C];
  mpz_matrix_init (m_C, n_C, _C);
  scm_dynwind_mpz_matrix_clear (m_C, n_C, _C);
  if (mpz_sgn (_beta) != 0)
    scm_array_handle_to_mpz_matrix (C, &handle_C, m_C, n_C, _C);

  assert_conformable_for_gemm (who, _TransA, _TransB, A, B, C,
                               m_A, k_A, k_B, n_B, m_C, n_C);
  mpz_matrix_gemm (_TransA, _TransB, m_A, n_B, k_A, _alpha, _A, _B, _beta, _C);

  SCM result = scm_from_mpz_matrix (m_C, n_C, _C);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_mpq_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B, SCM beta,
                  SCM C)
{
  // Note: if beta = 0 exactly, then C is ignored. You can set it to
  // anything.

  const char *who = "scm_gsl_mpq_dgemm";

  scm_t_array_handle handle_A;
  scm_t_array_handle handle_B;
  scm_t_array_handle handle_C;

  CBLAS_TRANSPOSE_t _TransA = scm_to_CBLAS_TRANSPOSE_t (who, TransA);
  CBLAS_TRANSPOSE_t _TransB = scm_to_CBLAS_TRANSPOSE_t (who, TransB);

  scm_dynwind_begin (0);

  mpq_t _alpha;
  mpq_init (_alpha);
  scm_dynwind_mpq_clear (_alpha);
  scm_to_mpq (alpha, _alpha);

  mpq_t _beta;
  mpq_init (_beta);
  scm_dynwind_mpq_clear (_beta);
  scm_to_mpq (beta, _beta);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_exact_rank_1_or_2_array (who, A, &handle_A);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  assert_c_exact_rank_1_or_2_array (who, B, &handle_B);

  const size_t dim1_A = matrix_dim1 (&handle_A);
  const size_t dim2_A = matrix_dim2 (&handle_A);
  const size_t m_A = transposed_size (_TransA, dim1_A, dim2_A);
  const size_t k_A = transposed_size (_TransA, dim2_A, dim1_A);

  const size_t dim1_B = matrix_dim1 (&handle_B);
  const size_t dim2_B = matrix_dim2 (&handle_B);
  const size_t k_B = transposed_size (_TransB, dim1_B, dim2_B);
  const size_t n_B = transposed_size (_TransB, dim2_B, dim1_B);

  mpq_t _A[dim1_A][dim2_A];
  mpq_matrix_init (dim1_A, dim2_A, _A);
  scm_dynwind_mpq_matrix_clear (dim1_A, dim2_A, _A);
  scm_array_handle_to_mpq_matrix (A, &handle_A, dim1_A, dim2_A, _A);

  mpq_t _B[dim1_B][dim2_B];
  mpq_matrix_init (dim1_B, dim2_B, _B);
  scm_dynwind_mpq_matrix_clear (dim1_B, dim2_B, _B);
  scm_array_handle_to_mpq_matrix (B, &handle_B, dim1_B, dim2_B, _B);

  size_t m_C;
  size_t n_C;
  if (mpq_sgn (_beta) == 0)
    {
      m_C = m_A;
      n_C = n_B;
    }
  else
    {
      scm_array_get_handle (C, &handle_C);
      scm_dynwind_array_handle_release (&handle_C);
      assert_c_exact_rank_1_or_2_array (who, C, &handle_C);

      m_C = matrix_dim1 (&handle_C);
      n_C = matrix_dim2 (&handle_C);
    }

  mpq_t _C[m_C][n_C];
  mpq_matrix_init (m_C, n_C, _C);
  scm_dynwind_mpq_matrix_clear (m_C, n_C, _C);
  if (mpq_sgn (_beta) != 0)
    scm_array_handle_to_mpq_matrix (C, &handle_C, m_C, n_C, _C);

  assert_conformable_for_gemm (who, _TransA, _TransB, A, B, C,
                               m_A, k_A, k_B, n_B, m_C, n_C);
  mpq_matrix_gemm (_TransA, _TransB, m_A, n_B, k_A, _alpha, _A, _B, _beta, _C);

  SCM result = scm_from_mpq_matrix (m_C, n_C, _C);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_scm_gemm (SCM TransA, SCM TransB, SCM alpha, SCM A, SCM B, SCM beta,
                  SCM C)
{
  // Note: if beta = 0 exactly, then C is ignored. You can set it to
  // anything.

  const char *who = "scm_gsl_scm_dgemm";

  scm_t_array_handle handle_A;
  scm_t_array_handle handle_B;
  scm_t_array_handle handle_C;

  CBLAS_TRANSPOSE_t _TransA = scm_to_CBLAS_TRANSPOSE_t (who, TransA);
  CBLAS_TRANSPOSE_t _TransB = scm_to_CBLAS_TRANSPOSE_t (who, TransB);

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  assert_c_rank_1_or_2_array (who, B, &handle_B);

  const size_t dim1_A = matrix_dim1 (&handle_A);
  const size_t dim2_A = matrix_dim2 (&handle_A);
  const size_t m_A = transposed_size (_TransA, dim1_A, dim2_A);
  const size_t k_A = transposed_size (_TransA, dim2_A, dim1_A);

  const size_t dim1_B = matrix_dim1 (&handle_B);
  const size_t dim2_B = matrix_dim2 (&handle_B);
  const size_t k_B = transposed_size (_TransB, dim1_B, dim2_B);
  const size_t n_B = transposed_size (_TransB, dim2_B, dim1_B);

  SCM _A[dim1_A][dim2_A];
  scm_array_handle_to_scm_matrix (A, &handle_A, dim1_A, dim2_A, _A);

  SCM _B[dim1_B][dim2_B];
  scm_array_handle_to_scm_matrix (B, &handle_B, dim1_B, dim2_B, _B);

  size_t m_C;
  size_t n_C;
  if (scm_is_true (scm_zero_p (beta)))
    {
      m_C = m_A;
      n_C = n_B;
    }
  else
    {
      scm_array_get_handle (C, &handle_C);
      scm_dynwind_array_handle_release (&handle_C);
      assert_c_rank_1_or_2_array (who, C, &handle_C);

      m_C = matrix_dim1 (&handle_C);
      n_C = matrix_dim2 (&handle_C);
    }

  SCM _C[m_C][n_C];
  if (scm_is_false (scm_zero_p (beta)))
    scm_array_handle_to_scm_matrix (C, &handle_C, m_C, n_C, _C);

  assert_conformable_for_gemm (who, _TransA, _TransB, A, B, C,
                               m_A, k_A, k_B, n_B, m_C, n_C);
  scm_matrix_gemm (_TransA, _TransB, m_A, n_B, k_A, alpha, _A, _B, beta, _C);

  SCM result = scm_from_scm_matrix (m_C, n_C, _C);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_svd_golub_reinsch (SCM a)
{
  scm_t_array_handle handle_a;

  const char *who = "scm_gsl_svd_golub_reinsch";

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle_a);
  scm_dynwind_array_handle_release (&handle_a);

  gsl_matrix_const_view ma =
    scm_gsl_matrix_const_view_array_handle (a, &handle_a);

  double u_buf[ma.matrix.size1 * ma.matrix.size2];
  double v_buf[ma.matrix.size2 * ma.matrix.size2];
  double s_buf[ma.matrix.size2];
  double work_buf[ma.matrix.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.matrix.size1,
                                             ma.matrix.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.matrix.size2,
                                             ma.matrix.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.matrix.size2);
  gsl_vector_view work = gsl_vector_view_array (work_buf, ma.matrix.size2);
  gsl_matrix_memcpy (&u.matrix, &ma.matrix);

  scm_dynwind_end ();

  int errval = gsl_linalg_SV_decomp (&u.matrix, &v.matrix, &s.vector,
                                     &work.vector);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_1 (a), SCM_UNDEFINED));
  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_gsl_svd_modified_golub_reinsch (SCM a)
{
  scm_t_array_handle handle_a;

  const char *who = "scm_gsl_svd_modified_golub_reinsch";

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle_a);
  scm_dynwind_array_handle_release (&handle_a);

  gsl_matrix_const_view ma =
    scm_gsl_matrix_const_view_array_handle (a, &handle_a);

  double u_buf[ma.matrix.size1 * ma.matrix.size2];
  double x_buf[ma.matrix.size2 * ma.matrix.size2];
  double v_buf[ma.matrix.size2 * ma.matrix.size2];
  double s_buf[ma.matrix.size2];
  double work_buf[ma.matrix.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.matrix.size1,
                                             ma.matrix.size2);
  gsl_matrix_view x = gsl_matrix_view_array (x_buf, ma.matrix.size2,
                                             ma.matrix.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.matrix.size2,
                                             ma.matrix.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.matrix.size2);
  gsl_vector_view work = gsl_vector_view_array (work_buf, ma.matrix.size2);
  gsl_matrix_memcpy (&u.matrix, &ma.matrix);

  scm_dynwind_end ();

  int errval = gsl_linalg_SV_decomp_mod (&u.matrix, &x.matrix, &v.matrix,
                                         &s.vector, &work.vector);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_1 (a), SCM_UNDEFINED));
  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_gsl_svd_jacobi (SCM a)
{
  scm_t_array_handle handle_a;
  const char *who = "scm_gsl_svd_jacobi";

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle_a);
  scm_dynwind_array_handle_release (&handle_a);

  gsl_matrix_const_view ma =
    scm_gsl_matrix_const_view_array_handle (a, &handle_a);

  double u_buf[ma.matrix.size1 * ma.matrix.size2];
  double v_buf[ma.matrix.size2 * ma.matrix.size2];
  double s_buf[ma.matrix.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.matrix.size1,
                                             ma.matrix.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.matrix.size2,
                                             ma.matrix.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.matrix.size2);
  gsl_matrix_memcpy (&u.matrix, &ma.matrix);

  scm_dynwind_end ();

  int errval = gsl_linalg_SV_decomp_jacobi (&u.matrix, &v.matrix, &s.vector);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_1 (a), SCM_UNDEFINED));
  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE void
init_guile_sortsmill_math_gsl_matrices (void)
{
  scm_c_define ("gsl:CblasRowMajor", scm_from_int (CblasRowMajor));
  scm_c_define ("gsl:CblasColMajor", scm_from_int (CblasColMajor));
  scm_c_define ("gsl:CblasNoTrans", scm_from_int (CblasNoTrans));
  scm_c_define ("gsl:CblasTrans", scm_from_int (CblasTrans));
  scm_c_define ("gsl:CblasConjTrans", scm_from_int (CblasConjTrans));
  scm_c_define ("gsl:CblasUpper", scm_from_int (CblasUpper));
  scm_c_define ("gsl:CblasLower", scm_from_int (CblasLower));
  scm_c_define ("gsl:CblasNonUnit", scm_from_int (CblasNonUnit));
  scm_c_define ("gsl:CblasUnit", scm_from_int (CblasUnit));
  scm_c_define ("gsl:CblasLeft", scm_from_int (CblasLeft));
  scm_c_define ("gsl:CblasRight", scm_from_int (CblasRight));

  scm_c_define_gsubr ("gsl:gemm-f64", 7, 0, 0, scm_gsl_blas_dgemm);
  scm_c_define_gsubr ("gsl:gemm-mpz", 7, 0, 0, scm_gsl_mpz_gemm);
  scm_c_define_gsubr ("gsl:gemm-mpq", 7, 0, 0, scm_gsl_mpq_gemm);
  scm_c_define_gsubr ("gsl:gemm-scm", 7, 0, 0, scm_gsl_scm_gemm);

  scm_c_define_gsubr ("gsl:svd-f64-golub-reinsch", 1, 0, 0,
                      scm_gsl_svd_golub_reinsch);
  scm_c_define_gsubr ("gsl:svd-f64-modified-golub-reinsch", 1, 0, 0,
                      scm_gsl_svd_modified_golub_reinsch);
  scm_c_define_gsubr ("gsl:svd-f64-jacobi", 1, 0, 0, scm_gsl_svd_jacobi);
}
