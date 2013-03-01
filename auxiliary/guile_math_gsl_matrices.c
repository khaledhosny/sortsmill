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
#include <sortsmill/xdie_on_null.h>
#include <intl.h>

void init_guile_sortsmill_math_gsl_matrices (void);

gsl_matrix_const_view null_gsl_matrix_const_view = { 0 };

static void
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

static void
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

static void
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

static void
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

static void
exception__expected_a_vector (const char *who, SCM irritants)
{
  const char *message = _("expected a vector, row matrix, or column matrix");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (), rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (irritants)));
}

static void
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

static void
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


static void
exception__LU_matrix_is_singular (const char *who, SCM LU)
{
  const char *message = _("LU matrix is singular");
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (scm_from_locale_string (message)),
      rnrs_make_irritants_condition (scm_list_1 (LU))));
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

  SCM bounds = scm_list_2 (scm_from_intmax (low_index),
                           scm_from_intmax (low_index + v->size - 1));
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

  SCM row_bounds = scm_list_2 (scm_from_intmax (low_index),
                               scm_from_intmax (low_index + m->size1 - 1));
  SCM column_bounds = scm_list_2 (scm_from_intmax (low_index),
                                  scm_from_intmax (low_index + m->size2 - 1));
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

//-------------------------------------------------------------------------
//
// What follows is a bunch of code to avoid using the general array
// API, for no particular reason other than for the fun of
// it. Probably this code is faster at doing something that we will
// never need to be fast.

static void
scm_array_handle_illegal_to_mpz_matrix (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        mpz_t A[m][n])
{
  assert (false);
}

static void
scm_array_handle_illegal_to_transposed_mpz_matrix (scm_t_array_handle *handlep,
                                                   unsigned int m,
                                                   unsigned int n,
                                                   mpz_t A[n][m])
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
scm_array_handle_illegal_to_transposed_mpq_matrix (scm_t_array_handle *handlep,
                                                   unsigned int m,
                                                   unsigned int n,
                                                   mpq_t A[n][m])
{
  assert (false);
}

static void
scm_array_handle_illegal_to_scm_matrix (scm_t_array_handle *handlep,
                                        unsigned int m, unsigned int n,
                                        SCM A[m][n])
{
  assert (false);
}

static void
scm_array_handle_illegal_to_transposed_scm_matrix (scm_t_array_handle *handlep,
                                                   unsigned int m,
                                                   unsigned int n, SCM A[n][m])
{
  assert (false);
}

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#define _PICK1(i, j) (i)
#define _PICK2(i, j) (j)

#define _MULT1(i) (i)
#define _MULT2(i) (2 * (i))

#define _ASSIGN(x, dest) (dest = (x))

#define _SCM_FROM_COMPLEX(x)                            \
  (scm_make_rectangular (scm_from_double ((&(x))[0]),   \
                         scm_from_double ((&(x))[1])))

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#define _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX(NAME, TYPE, SCM_TO_TYPE, \
                                              PICK)                     \
  void                                                                  \
  NAME (scm_t_array_handle *handlep,                                    \
        unsigned int m, unsigned int n,                                 \
        TYPE A[PICK (m, n)][PICK (n, m)])                               \
  {                                                                     \
    const size_t rank = scm_array_handle_rank (handlep);                \
    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
    const SCM *elems = scm_array_handle_elements (handlep);             \
    if (rank == 1)                                                      \
      {                                                                 \
        assert (m == 1);                                                \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            SCM x = elems[j * dims[0].inc];                             \
            SCM_TO_TYPE (x, A[PICK (0, j)][PICK (j, 0)]);               \
          }                                                             \
      }                                                                 \
    else                                                                \
      for (unsigned int i = 0; i < m; i++)                              \
        for (unsigned int j = 0; j < n; j++)                            \
          {                                                             \
            SCM x = elems[i * dims[0].inc + j * dims[1].inc];           \
            SCM_TO_TYPE (x, A[PICK (i, j)][PICK (j, i)]);               \
          }                                                             \
  }

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_mpz_matrix, mpz_t, scm_to_mpz, _PICK1);

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_mpq_matrix, mpq_t, scm_to_mpq, _PICK1);

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_scm_matrix, SCM, _ASSIGN, _PICK1);

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_transposed_mpz_matrix, mpz_t, scm_to_mpz,
   _PICK2);

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_transposed_mpq_matrix, mpq_t, scm_to_mpq,
   _PICK2);

static _SCM_ARRAY_HANDLE_NONUNIFORM_TO_MATRIX
  (scm_array_handle_nonuniform_to_transposed_scm_matrix, SCM, _ASSIGN, _PICK2);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#define _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX(NAME, TYPE, SCM_TO_TYPE,    \
                                            ELEMENTS, ELEM_TYPE,        \
                                            SCM_FROM_ELEM, PICK, MULT)  \
  void                                                                  \
  NAME (scm_t_array_handle *handlep, unsigned int m, unsigned int n,    \
        TYPE A[PICK (m, n)][PICK (n, m)])                               \
  {                                                                     \
    const size_t rank = scm_array_handle_rank (handlep);                \
                                                                        \
    /* Rank 1 arrays are treated as row matrices. */                    \
    assert (rank == 2 || m == 1);                                       \
                                                                        \
    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
    const ELEM_TYPE *elems = ELEMENTS (handlep);                        \
    if (rank == 1)                                                      \
      for (unsigned int j = 0; j < n; j++)                              \
        SCM_TO_TYPE (SCM_FROM_ELEM (elems[MULT (j) * dims[0].inc]),     \
                     A[PICK (0, j)][PICK (j, 0)]);                      \
    else                                                                \
      for (unsigned int i = 0; i < m; i++)                              \
        for (unsigned int j = 0; j < n; j++)                            \
          SCM_TO_TYPE                                                   \
            (SCM_FROM_ELEM (elems[MULT (i) * dims[0].inc                \
                                  + MULT (j) * dims[1].inc]),           \
             A[PICK (i, j)][PICK (j, i)]);                              \
  }

#define _SAHUTM_MPX_INT(X, SIGN1, SIGN2, SIZE)          \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX            \
  (scm_array_handle_##SIGN1##SIZE##_to_mp##X##_matrix,  \
   mp##X##_t, scm_to_mp##X,                             \
   scm_array_handle_##SIGN1##SIZE##_elements,           \
   SIGN2##int##SIZE##_t, scm_from_##SIGN2##int##SIZE,   \
   _PICK1, _MULT1);

#define _SAHUTM_SCM_INT(SIGN1, SIGN2, SIZE)             \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX            \
  (scm_array_handle_##SIGN1##SIZE##_to_scm_matrix,      \
   SCM, _ASSIGN,                                        \
   scm_array_handle_##SIGN1##SIZE##_elements,           \
   SIGN2##int##SIZE##_t, scm_from_##SIGN2##int##SIZE,   \
   _PICK1, _MULT1);

#define _SAHUTM_INT(SIGN1, SIGN2, SIZE)         \
  _SAHUTM_MPX_INT (z, SIGN1, SIGN2, SIZE);      \
  _SAHUTM_MPX_INT (q, SIGN1, SIGN2, SIZE);      \
  _SAHUTM_SCM_INT (SIGN1, SIGN2, SIZE)


#define _SAHUTM_TRANSPOSED_MPX_INT(X, SIGN1, SIGN2, SIZE)               \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX                            \
  (scm_array_handle_##SIGN1##SIZE##_to_transposed_mp##X##_matrix,       \
   mp##X##_t, scm_to_mp##X,                                             \
   scm_array_handle_##SIGN1##SIZE##_elements,                           \
   SIGN2##int##SIZE##_t, scm_from_##SIGN2##int##SIZE,                   \
   _PICK2, _MULT1);

#define _SAHUTM_TRANSPOSED_SCM_INT(SIGN1, SIGN2, SIZE)          \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX                    \
  (scm_array_handle_##SIGN1##SIZE##_to_transposed_scm_matrix,   \
   SCM, _ASSIGN,                                                \
   scm_array_handle_##SIGN1##SIZE##_elements,                   \
   SIGN2##int##SIZE##_t, scm_from_##SIGN2##int##SIZE,           \
   _PICK2, _MULT1);

#define _SAHUTM_TRANSPOSED_INT(SIGN1, SIGN2, SIZE)      \
  _SAHUTM_TRANSPOSED_MPX_INT (z, SIGN1, SIGN2, SIZE);   \
  _SAHUTM_TRANSPOSED_MPX_INT (q, SIGN1, SIGN2, SIZE);   \
  _SAHUTM_TRANSPOSED_SCM_INT (SIGN1, SIGN2, SIZE)


#define _SAHUTM_SCM_FLOAT(TYPE, SIZE)           \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX    \
  (scm_array_handle_f##SIZE##_to_scm_matrix,    \
   SCM, _ASSIGN,                                \
   scm_array_handle_f##SIZE##_elements,         \
   TYPE, scm_from_double, _PICK1,               \
   _MULT1);

#define _SAHUTM_TRANSPOSED_SCM_FLOAT(TYPE, SIZE)        \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX            \
  (scm_array_handle_f##SIZE##_to_transposed_scm_matrix, \
   SCM, _ASSIGN,                                        \
   scm_array_handle_f##SIZE##_elements,                 \
   TYPE, scm_from_double, _PICK2,                       \
   _MULT1);

#define _SAHUTM_SCM_COMPLEX(TYPE, SIZE)         \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX    \
  (scm_array_handle_c##SIZE##_to_scm_matrix,    \
   SCM, _ASSIGN,                                \
   scm_array_handle_c##SIZE##_elements,         \
   TYPE, _SCM_FROM_COMPLEX, _PICK1,             \
   _MULT2);

#define _SAHUTM_TRANSPOSED_SCM_COMPLEX(TYPE, SIZE)      \
  static _SCM_ARRAY_HANDLE_UNIFORM_TO_MATRIX            \
  (scm_array_handle_c##SIZE##_to_transposed_scm_matrix, \
   SCM, _ASSIGN,                                        \
   scm_array_handle_c##SIZE##_elements,                 \
   TYPE, _SCM_FROM_COMPLEX, _PICK2,                     \
   _MULT2);


// Unsigned ints.

_SAHUTM_INT (u, u, 8);
_SAHUTM_INT (u, u, 16);
_SAHUTM_INT (u, u, 32);
_SAHUTM_INT (u, u, 64);

_SAHUTM_TRANSPOSED_INT (u, u, 8);
_SAHUTM_TRANSPOSED_INT (u, u, 16);
_SAHUTM_TRANSPOSED_INT (u, u, 32);
_SAHUTM_TRANSPOSED_INT (u, u, 64);

// Signed ints.

_SAHUTM_INT (s,, 8);
_SAHUTM_INT (s,, 16);
_SAHUTM_INT (s,, 32);
_SAHUTM_INT (s,, 64);

_SAHUTM_TRANSPOSED_INT (s,, 8);
_SAHUTM_TRANSPOSED_INT (s,, 16);
_SAHUTM_TRANSPOSED_INT (s,, 32);
_SAHUTM_TRANSPOSED_INT (s,, 64);

// Floating point reals.

_SAHUTM_SCM_FLOAT (float, 32);
_SAHUTM_SCM_FLOAT (double, 64);

_SAHUTM_TRANSPOSED_SCM_FLOAT (float, 32);
_SAHUTM_TRANSPOSED_SCM_FLOAT (double, 64);

// Floating point complex.

_SAHUTM_SCM_COMPLEX (float, 32);
_SAHUTM_SCM_COMPLEX (double, 64);

_SAHUTM_TRANSPOSED_SCM_COMPLEX (float, 32);
_SAHUTM_TRANSPOSED_SCM_COMPLEX (double, 64);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#define _SCM_TO_MATRIX_FUNC_T(NAME, TYPE, PICK)         \
  typedef void NAME (scm_t_array_handle *handlep,       \
                     unsigned int m, unsigned int n,    \
                     TYPE A[PICK (m, n)][PICK (n, m)]);

_SCM_TO_MATRIX_FUNC_T (_scm_to_mpz_matrix_func_t, mpz_t, _PICK1);
_SCM_TO_MATRIX_FUNC_T (_scm_to_transposed_mpz_matrix_func_t, mpz_t, _PICK2);

_SCM_TO_MATRIX_FUNC_T (_scm_to_mpq_matrix_func_t, mpq_t, _PICK1);
_SCM_TO_MATRIX_FUNC_T (_scm_to_transposed_mpq_matrix_func_t, mpq_t, _PICK2);

_SCM_TO_MATRIX_FUNC_T (_scm_to_scm_matrix_func_t, SCM, _PICK1);
_SCM_TO_MATRIX_FUNC_T (_scm_to_transposed_scm_matrix_func_t, SCM, _PICK2);

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

static _scm_to_transposed_mpz_matrix_func_t
  * scm_to_transposed_mpz_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] =
    scm_array_handle_nonuniform_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_illegal_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_illegal_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_illegal_to_transposed_mpz_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_illegal_to_transposed_mpz_matrix
};

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

static _scm_to_transposed_mpq_matrix_func_t
  * scm_to_transposed_mpq_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] =
    scm_array_handle_nonuniform_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_illegal_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_illegal_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_illegal_to_transposed_mpq_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_illegal_to_transposed_mpq_matrix
};

static _scm_to_scm_matrix_func_t *scm_to_scm_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_scm_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] = scm_array_handle_nonuniform_to_scm_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_scm_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_scm_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_scm_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_scm_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_scm_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_scm_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_scm_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_scm_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_f32_to_scm_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_f64_to_scm_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_c32_to_scm_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_c64_to_scm_matrix
};

static _scm_to_transposed_scm_matrix_func_t
  * scm_to_transposed_scm_matrix_func[14] = {
  [_FF_INDEX_NOT_AN_ARRAY] = scm_array_handle_illegal_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_NONUNIFORM] =
    scm_array_handle_nonuniform_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_U8] = scm_array_handle_u8_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_S8] = scm_array_handle_s8_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_U16] = scm_array_handle_u16_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_S16] = scm_array_handle_s16_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_U32] = scm_array_handle_u32_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_S32] = scm_array_handle_s32_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_U64] = scm_array_handle_u64_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_S64] = scm_array_handle_s64_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_F32] = scm_array_handle_f32_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_F64] = scm_array_handle_f64_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_C32] = scm_array_handle_c32_to_transposed_scm_matrix,
  [_FF_INDEX_ARRAY_C64] = scm_array_handle_c64_to_transposed_scm_matrix
};

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

VISIBLE void
scm_array_handle_to_mpz_matrix (SCM array, scm_t_array_handle *handlep,
                                unsigned int m, unsigned int n, mpz_t A[m][n])
{
  const char *who = "scm_array_handle_to_mpz_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  if (scm_is_integer_array (array))
    scm_to_mpz_matrix_func
      [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
  else
    exception__unexpected_array_type (who, array);
}

VISIBLE void
scm_array_handle_to_transposed_mpz_matrix (SCM array,
                                           scm_t_array_handle *handlep,
                                           unsigned int m, unsigned int n,
                                           mpz_t A[n][m])
{
  const char *who = "scm_array_handle_to_transposed_mpz_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  if (scm_is_integer_array (array))
    scm_to_transposed_mpz_matrix_func
      [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
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
    scm_to_mpq_matrix_func
      [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
  else
    exception__unexpected_array_type (who, array);
}

VISIBLE void
scm_array_handle_to_transposed_mpq_matrix (SCM array,
                                           scm_t_array_handle *handlep,
                                           unsigned int m, unsigned int n,
                                           mpq_t A[n][m])
{
  const char *who = "scm_array_handle_to_transposed_mpq_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  if (scm_is_exact_array (array))
    scm_to_transposed_mpq_matrix_func
      [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
  else
    exception__unexpected_array_type (who, array);
}

VISIBLE void
scm_array_handle_to_scm_matrix (SCM array, scm_t_array_handle *handlep,
                                unsigned int m, unsigned int n, SCM A[m][n])
{
  const char *who = "scm_array_handle_to_scm_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  scm_to_scm_matrix_func
    [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
}

VISIBLE void
scm_array_handle_to_transposed_scm_matrix (SCM array,
                                           scm_t_array_handle *handlep,
                                           unsigned int m, unsigned int n,
                                           SCM A[n][m])
{
  const char *who = "scm_array_handle_to_transposed_scm_matrix";

  assert_c_rank_1_or_2_array (who, array, handlep);

  scm_to_transposed_scm_matrix_func
    [scm_array_handle_to_array_type_index (handlep)] (handlep, m, n, A);
}

//-------------------------------------------------------------------------

#define _SCM_ARRAY_HANDLE_TO_TYPED_VECTOR(NAME, TYPE, TO_MATRIX)        \
  void                                                                  \
  NAME (SCM array, scm_t_array_handle *handlep,                         \
        unsigned int n, TYPE v[n])                                      \
  {                                                                     \
    assert_c_rank_1_or_2_array (#NAME, array, handlep);                 \
                                                                        \
    const size_t rank = scm_array_handle_rank (handlep);                \
    if (rank == 1)                                                      \
      TO_MATRIX (array, handlep, 1, n, (TYPE (*)[n]) v);                \
    else                                                                \
      {                                                                 \
        const scm_t_array_dim *dims = scm_array_handle_dims (handlep);  \
        size_t dim1 = (dims[0].ubnd - dims[0].lbnd) + 1;                \
        size_t dim2 = (dims[1].ubnd - dims[1].lbnd) + 1;                \
        if (dim1 == 1)                                                  \
          TO_MATRIX (array, handlep, 1, n, (TYPE (*)[n]) v);            \
        else if (dim2 == 1)                                             \
          TO_MATRIX (array, handlep, n, 1, (TYPE (*)[1]) v);            \
        else                                                            \
          exception__expected_a_vector (#NAME, scm_list_1 (array));     \
      }                                                                 \
  }

VISIBLE _SCM_ARRAY_HANDLE_TO_TYPED_VECTOR (scm_array_handle_to_mpz_vector,
                                           mpz_t,
                                           scm_array_handle_to_mpz_matrix);
VISIBLE _SCM_ARRAY_HANDLE_TO_TYPED_VECTOR (scm_array_handle_to_mpq_vector,
                                           mpq_t,
                                           scm_array_handle_to_mpq_matrix);
VISIBLE _SCM_ARRAY_HANDLE_TO_TYPED_VECTOR (scm_array_handle_to_scm_vector, SCM,
                                           scm_array_handle_to_scm_matrix);

#define _SCM_FROM_TYPED_MATRIX(NAME, TYPE, CONVERT_TO_SCM)              \
  SCM                                                                   \
  NAME (unsigned int m, unsigned int n, TYPE A[m][n])                   \
  {                                                                     \
    scm_t_array_handle handle;                                          \
                                                                        \
    SCM bounds = scm_list_2 (scm_list_2 (scm_from_uint (1),             \
                                         scm_from_uint (m)),            \
                             scm_list_2 (scm_from_uint (1),             \
                                         scm_from_uint (n)));           \
    SCM array = scm_make_array (SCM_UNSPECIFIED, bounds);               \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (array, &handle);                              \
    scm_dynwind_array_handle_release (&handle);                         \
                                                                        \
    const scm_t_array_dim *dims = scm_array_handle_dims (&handle);      \
    SCM *elems = scm_array_handle_writable_elements (&handle);          \
    for (unsigned int i = 0; i < m; i++)                                \
      for (unsigned int j = 0; j < n; j++)                              \
        elems[i * dims[0].inc + j * dims[1].inc] =                      \
          CONVERT_TO_SCM (A[i][j]);                                     \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return array;                                                       \
  }

VISIBLE _SCM_FROM_TYPED_MATRIX (scm_from_mpz_matrix, mpz_t, scm_from_mpz);
VISIBLE _SCM_FROM_TYPED_MATRIX (scm_from_mpq_matrix, mpq_t, scm_from_mpq);
VISIBLE _SCM_FROM_TYPED_MATRIX (scm_from_scm_matrix, SCM, /* empty */ );

#define _SCM_FROM_TYPED_VECTOR(NAME, TYPE, CONVERT_TO_SCM)              \
  SCM                                                                   \
  NAME (unsigned int n, TYPE v[n])                                      \
  {                                                                     \
    scm_t_array_handle handle;                                          \
                                                                        \
    SCM bounds = scm_list_1 (scm_list_2 (scm_from_uint (1),             \
                                         scm_from_uint (n)));           \
    SCM array = scm_make_array (SCM_UNSPECIFIED, bounds);               \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (array, &handle);                              \
    scm_dynwind_array_handle_release (&handle);                         \
                                                                        \
    const scm_t_array_dim *dims = scm_array_handle_dims (&handle);      \
    SCM *elems = scm_array_handle_writable_elements (&handle);          \
    for (unsigned int i = 0; i < n; i++)                                \
      elems[i * dims[0].inc] = CONVERT_TO_SCM (v[i]);                   \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return array;                                                       \
  }

VISIBLE _SCM_FROM_TYPED_VECTOR (scm_from_mpz_vector, mpz_t, scm_from_mpz);
VISIBLE _SCM_FROM_TYPED_VECTOR (scm_from_mpq_vector, mpq_t, scm_from_mpq);
VISIBLE _SCM_FROM_TYPED_VECTOR (scm_from_scm_vector, SCM, /* empty */ );

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

static size_t
vector_dim (SCM array, scm_t_array_handle *handlep)
{
  size_t dim1 = matrix_dim1 (handlep);
  size_t dim2 = matrix_dim2 (handlep);

  size_t dim;
  if (dim1 == 1)
    dim = dim2;
  else if (dim2 == 1)
    dim = dim1;
  else
    {
      dim = 0;
      exception__expected_a_vector ("vector_dim", scm_list_1 (array));
    }

  return dim;
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
                             scm_list_4 (scm_from_size_t (m_A),
                                         scm_from_size_t (k_A),
                                         scm_from_size_t (k_B),
                                         scm_from_size_t (n_B)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (A, B))));
}

static void
non_conformable_for_Ax_equals_B (const char *who, SCM A, SCM B,
                                 size_t m_A, size_t n_A, size_t m_B, size_t n_B)
{
  const char *localized_message =
    _("non-conformable matrices for solution of Ax = b: ~ax~a, ~ax~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_4 (scm_from_size_t (m_A),
                                         scm_from_size_t (n_A),
                                         scm_from_size_t (m_B),
                                         scm_from_size_t (n_B)));
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
                             scm_list_4 (scm_from_size_t (m_A),
                                         scm_from_size_t (k_A),
                                         scm_from_size_t (m_C),
                                         scm_from_size_t (n_C)));
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
                             scm_list_4 (scm_from_size_t (k_B),
                                         scm_from_size_t (n_B),
                                         scm_from_size_t (m_C),
                                         scm_from_size_t (n_C)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (B, C))));
}

static void
non_conformable_for_addition (const char *localized_message, const char *who,
                              SCM A, SCM B,
                              size_t m_A, size_t n_A, size_t m_B, size_t n_B)
{
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_4 (scm_from_size_t (m_A),
                                         scm_from_size_t (n_A),
                                         scm_from_size_t (m_B),
                                         scm_from_size_t (n_B)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (A, B))));
}

static void
matrix_is_not_square (const char *who, SCM A, size_t m, size_t n)
{
  const char *localized_message = _("not a square matrix: ~ax~a");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_2 (scm_from_size_t (m),
                                         scm_from_size_t (n)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_1 (A))));
}

static void
permutation_does_not_conform_with_matrix (const char *who,
                                          size_t permutation_size,
                                          SCM permutation, SCM A)
{
  SCM dims = scm_call_1 (scm_c_private_ref ("sortsmill math gsl matrices",
                                            "array-dimensions-simplified"),
                         A);

  const char *localized_message =
    _("permutation of length ~a does not conform with ~ax~a matrix");
  SCM message = scm_sformat (scm_from_locale_string (localized_message),
                             scm_list_3 (scm_from_size_t (permutation_size),
                                         scm_car (dims), scm_cadr (dims)));
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_make_message_condition (message),
      rnrs_make_irritants_condition (scm_list_2 (permutation, A))));
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

static void
assert_conformable_for_addition (const char *localized_message,
                                 const char *who, SCM A, SCM B,
                                 size_t m_A, size_t n_A, size_t m_B, size_t n_B)
{
  if ((m_A | n_A) == 0)
    matrix_has_dimension_of_size_zero (who, A);
  if ((m_B | n_B) == 0)
    matrix_has_dimension_of_size_zero (who, B);
  if (m_A != m_B || n_A != n_B)
    non_conformable_for_addition (localized_message, who, A, B, m_A, n_A, m_B,
                                  n_B);
}

static void
assert_matrix_is_square (const char *who, SCM A, size_t m, size_t n)
{
  if ((m | n) == 0)
    matrix_has_dimension_of_size_zero (who, A);
  if (m != n)
    matrix_is_not_square (who, A, m, n);
}

static void
assert_permutation_conforms_with_matrix (const char *who, size_t n,
                                         const gsl_permutation *p,
                                         SCM permutation, SCM A)
{
  if (n != gsl_permutation_size (p))
    permutation_does_not_conform_with_matrix (who, gsl_permutation_size (p),
                                              permutation, A);
}

static void
assert_conformable_for_Ax_equals_B (const char *who, SCM A, SCM B,
                                    size_t m_A, size_t n_A,
                                    size_t m_B, size_t n_B)
{
  if (m_A != m_B)
    non_conformable_for_Ax_equals_B (who, A, B, m_A, n_A, m_B, n_B);
}

VISIBLE SCM
scm_gsl_matrix_scale (SCM A, SCM x)
{
  const char *who = "scm_gsl_matrix_scale";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_f64_rank_1_or_2_array (who, A, &handle_A);

  gsl_matrix_const_view _A =
    scm_gsl_matrix_const_view_array_handle (A, &handle_A);

  const size_t m = _A.matrix.size1;
  const size_t n = _A.matrix.size2;

  double result_buffer[m][n];
  gsl_matrix_view _result = gsl_matrix_view_array (&result_buffer[0][0],
                                                   m, n);

  gsl_matrix_memcpy (&_result.matrix, &_A.matrix);
  const int errval = gsl_matrix_scale (&_result.matrix, scm_to_double (x));
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_2 (A, x), SCM_UNDEFINED));

  SCM result = scm_gsl_matrix_to_f64matrix (&_result.matrix, 1);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_mpz_matrix_scale (SCM A, SCM x)
{
  const char *who = "scm_gsl_mpz_matrix_scale";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_exact_rank_1_or_2_array (who, A, &handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  mpz_t _A[m][n];
  mpz_matrix_init (m, n, _A);
  scm_dynwind_mpz_matrix_clear (m, n, _A);
  scm_array_handle_to_mpz_matrix (A, &handle_A, m, n, _A);

  mpz_t _x;
  mpz_init (_x);
  scm_dynwind_mpz_clear (_x);
  scm_to_mpz (x, _x);

  mpz_matrix_scale (m, n, _A, _x);

  SCM result = scm_from_mpz_matrix (m, n, _A);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_mpq_matrix_scale (SCM A, SCM x)
{
  const char *who = "scm_gsl_mpq_matrix_scale";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_exact_rank_1_or_2_array (who, A, &handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  mpq_t _A[m][n];
  mpq_matrix_init (m, n, _A);
  scm_dynwind_mpq_matrix_clear (m, n, _A);
  scm_array_handle_to_mpq_matrix (A, &handle_A, m, n, _A);

  mpq_t _x;
  mpq_init (_x);
  scm_dynwind_mpq_clear (_x);
  scm_to_mpq (x, _x);

  mpq_matrix_scale (m, n, _A, _x);

  SCM result = scm_from_mpq_matrix (m, n, _A);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_gsl_scm_matrix_scale (SCM A, SCM x)
{
  const char *who = "scm_gsl_scm_matrix_scale";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  SCM _A[m][n];
  scm_array_handle_to_scm_matrix (A, &handle_A, m, n, _A);

  scm_matrix_scale (m, n, _A, x);

  SCM result = scm_from_scm_matrix (m, n, _A);

  scm_dynwind_end ();

  return result;
}

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

static const char *
addition_non_conformable_message (void)
{
  return _("non-conformable matrices: ~ax~a plus ~ax~a");
}

static const char *
subtraction_non_conformable_message (void)
{
  return _("non-conformable matrices: ~ax~a minus ~ax~a");
}

static const char *
mul_elements_non_conformable_message (void)
{
  return _("non-conformable matrices: elementwise product of ~ax~a and ~ax~a");
}

static const char *
div_elements_non_conformable_message (void)
{
  return _("non-conformable matrices: elementwise quotient of ~ax~a and ~ax~a");
}

static const char *
equal_non_conformable_message (void)
{
  return _("non-conformable matrices: ~ax~a `equal to' ~ax~a");
}

#define _FF_SCM_GSL_MATRIX_ELEMENTWISE_BINARY_OP(OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE SCM                                                           \
  scm_gsl_matrix_##OP (SCM A, SCM B)                                    \
  {                                                                     \
    const char *who = "scm_gsl_matrix_" #OP;                            \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_f64_rank_1_or_2_array (who, A, &handle_A);                 \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_f64_rank_1_or_2_array (who, B, &handle_B);                 \
                                                                        \
    gsl_matrix_const_view _A =                                          \
      scm_gsl_matrix_const_view_array_handle (A, &handle_A);            \
    gsl_matrix_const_view _B =                                          \
      scm_gsl_matrix_const_view_array_handle (B, &handle_B);            \
                                                                        \
    const size_t m_A = _A.matrix.size1;                                 \
    const size_t n_A = _A.matrix.size2;                                 \
                                                                        \
    const size_t m_B = _B.matrix.size1;                                 \
    const size_t n_B = _B.matrix.size2;                                 \
                                                                        \
    assert_conformable_for_addition (NON_CONFORMABILITY_MESSAGE, who,   \
                                     A, B, m_A, n_A, m_B, n_B);         \
                                                                        \
    double result_buffer[m_A][n_A];                                     \
    gsl_matrix_view _result =                                           \
      gsl_matrix_view_array (&result_buffer[0][0], m_A, n_A);           \
                                                                        \
    gsl_matrix_memcpy (&_result.matrix, &_A.matrix);                    \
    const int errval = FUNC (&_result.matrix, &_B.matrix);              \
    if (errval != GSL_SUCCESS)                                          \
      scm_raise_gsl_error                                               \
        (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),             \
                     scm_from_int (errval),                             \
                     scm_from_latin1_keyword ("who"),                   \
                     scm_from_latin1_string (who),                      \
                     scm_from_latin1_keyword ("irritants"),             \
                     scm_list_2 (A, B), SCM_UNDEFINED));                \
                                                                        \
    SCM result = scm_gsl_matrix_to_f64matrix (&_result.matrix, 1);      \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }

_FF_SCM_GSL_MATRIX_ELEMENTWISE_BINARY_OP (add, gsl_matrix_add,
                                          addition_non_conformable_message ());
_FF_SCM_GSL_MATRIX_ELEMENTWISE_BINARY_OP (sub, gsl_matrix_sub,
                                          subtraction_non_conformable_message
                                          ());
_FF_SCM_GSL_MATRIX_ELEMENTWISE_BINARY_OP (mul_elements, gsl_matrix_mul_elements,
                                          mul_elements_non_conformable_message
                                          ());
_FF_SCM_GSL_MATRIX_ELEMENTWISE_BINARY_OP (div_elements, gsl_matrix_div_elements,
                                          div_elements_non_conformable_message
                                          ());

#define _FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP(X, OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE SCM                                                           \
  scm_gsl_mp##X##_matrix_##OP (SCM A, SCM B)                            \
  {                                                                     \
    const char *who = "scm_gsl_mp" #X "_matrix_" #OP;                   \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_exact_rank_1_or_2_array (who, A, &handle_A);               \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_exact_rank_1_or_2_array (who, B, &handle_B);               \
                                                                        \
    const size_t m_A = matrix_dim1 (&handle_A);                         \
    const size_t n_A = matrix_dim2 (&handle_A);                         \
                                                                        \
    const size_t m_B = matrix_dim1 (&handle_B);                         \
    const size_t n_B = matrix_dim2 (&handle_B);                         \
                                                                        \
    assert_conformable_for_addition (NON_CONFORMABILITY_MESSAGE, who,   \
                                     A, B, m_A, n_A, m_B, n_B);         \
                                                                        \
    mp##X##_t _A[m_A][n_A];                                             \
    mp##X##_matrix_init (m_A, n_A, _A);                                 \
    scm_dynwind_mp##X##_matrix_clear (m_A, n_A, _A);                    \
    scm_array_handle_to_mp##X##_matrix (A, &handle_A, m_A, n_A, _A);    \
                                                                        \
    mp##X##_t _B[m_B][n_B];                                             \
    mp##X##_matrix_init (m_B, n_B, _B);                                 \
    scm_dynwind_mp##X##_matrix_clear (m_B, n_B, _B);                    \
    scm_array_handle_to_mp##X##_matrix (B, &handle_B, m_B, n_B, _B);    \
                                                                        \
    FUNC (m_A, n_A, _A, _B);                                            \
                                                                        \
    SCM result = scm_from_mp##X##_matrix (m_A, n_A, _A);                \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }

_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (z, add, mpz_matrix_add,
                                          addition_non_conformable_message ());
_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (z, sub, mpz_matrix_sub,
                                          subtraction_non_conformable_message
                                          ());
_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (z, mul_elements,
                                          mpz_matrix_mul_elements,
                                          mul_elements_non_conformable_message
                                          ());

_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (q, add, mpq_matrix_add,
                                          addition_non_conformable_message ());
_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (q, sub, mpq_matrix_sub,
                                          subtraction_non_conformable_message
                                          ());
_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (q, mul_elements,
                                          mpq_matrix_mul_elements,
                                          mul_elements_non_conformable_message
                                          ());
_FF_SCM_GMP_MATRIX_ELEMENTWISE_BINARY_OP (q, div_elements,
                                          mpq_matrix_div_elements,
                                          div_elements_non_conformable_message
                                          ());

#define _FF_SCM_SCM_MATRIX_ELEMENTWISE_BINARY_OP(OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE SCM                                                           \
  scm_gsl_scm_matrix_##OP (SCM A, SCM B)                                \
  {                                                                     \
    const char *who = "scm_gsl_scm_matrix_" #OP;                        \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_rank_1_or_2_array (who, A, &handle_A);                     \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_rank_1_or_2_array (who, B, &handle_B);                     \
                                                                        \
    const size_t m_A = matrix_dim1 (&handle_A);                         \
    const size_t n_A = matrix_dim2 (&handle_A);                         \
                                                                        \
    const size_t m_B = matrix_dim1 (&handle_B);                         \
    const size_t n_B = matrix_dim2 (&handle_B);                         \
                                                                        \
    assert_conformable_for_addition (NON_CONFORMABILITY_MESSAGE, who,   \
                                     A, B, m_A, n_A, m_B, n_B);         \
                                                                        \
    SCM _A[m_A][n_A];                                                   \
    scm_array_handle_to_scm_matrix (A, &handle_A, m_A, n_A, _A);        \
                                                                        \
    SCM _B[m_B][n_B];                                                   \
    scm_array_handle_to_scm_matrix (B, &handle_B, m_B, n_B, _B);        \
                                                                        \
    FUNC (m_A, n_A, _A, _B);                                            \
                                                                        \
    SCM result = scm_from_scm_matrix (m_A, n_A, _A);                    \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
}

_FF_SCM_SCM_MATRIX_ELEMENTWISE_BINARY_OP (add, scm_matrix_add,
                                          addition_non_conformable_message ());
_FF_SCM_SCM_MATRIX_ELEMENTWISE_BINARY_OP (sub, scm_matrix_sub,
                                          subtraction_non_conformable_message
                                          ());
_FF_SCM_SCM_MATRIX_ELEMENTWISE_BINARY_OP (mul_elements, scm_matrix_mul_elements,
                                          mul_elements_non_conformable_message
                                          ());
_FF_SCM_SCM_MATRIX_ELEMENTWISE_BINARY_OP (div_elements, scm_matrix_div_elements,
                                          div_elements_non_conformable_message
                                          ());

#define _FF_SCM_GSL_SINGLE_MATRIX_PREDICATE(OP, FUNC)           \
  VISIBLE bool                                                  \
  scm_c_gsl_matrix_##OP (SCM A)                                 \
  {                                                             \
    const char *who = "scm_gsl_matrix_" #OP;                    \
                                                                \
    scm_t_array_handle handle_A;                                \
                                                                \
    scm_dynwind_begin (0);                                      \
                                                                \
    scm_array_get_handle (A, &handle_A);                        \
    scm_dynwind_array_handle_release (&handle_A);               \
    assert_c_f64_rank_1_or_2_array (who, A, &handle_A);         \
                                                                \
    gsl_matrix_const_view _A =                                  \
      scm_gsl_matrix_const_view_array_handle (A, &handle_A);    \
                                                                \
    const bool result = FUNC (&_A.matrix);                      \
                                                                \
    scm_dynwind_end ();                                         \
                                                                \
    return result;                                              \
  }                                                             \
                                                                \
  VISIBLE SCM                                                   \
  scm_gsl_matrix_##OP##_p (SCM A)                               \
  {                                                             \
    return scm_from_bool (scm_c_gsl_matrix_##OP (A));           \
  }

_FF_SCM_GSL_SINGLE_MATRIX_PREDICATE (isnull, gsl_matrix_isnull);
_FF_SCM_GSL_SINGLE_MATRIX_PREDICATE (ispos, gsl_matrix_ispos);
_FF_SCM_GSL_SINGLE_MATRIX_PREDICATE (isneg, gsl_matrix_isneg);
_FF_SCM_GSL_SINGLE_MATRIX_PREDICATE (isnonneg, gsl_matrix_isnonneg);

#define _FF_SCM_GMP_SINGLE_MATRIX_PREDICATE(X, OP, FUNC)                \
  VISIBLE bool                                                          \
  scm_c_gsl_mp##X##_matrix_##OP (SCM A)                                 \
  {                                                                     \
    const char *who = "scm_gsl_mp" #X "_matrix_" #OP;                   \
                                                                        \
    scm_t_array_handle handle_A;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_exact_rank_1_or_2_array (who, A, &handle_A);               \
                                                                        \
    const size_t m = matrix_dim1 (&handle_A);                           \
    const size_t n = matrix_dim2 (&handle_A);                           \
                                                                        \
    mp##X##_t _A[m][n];                                                 \
    mp##X##_matrix_init (m, n, _A);                                     \
    scm_dynwind_mp##X##_matrix_clear (m, n, _A);                        \
    scm_array_handle_to_mp##X##_matrix (A, &handle_A, m, n, _A);        \
                                                                        \
    const bool result = FUNC (m, n, _A);                                \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  VISIBLE SCM                                                           \
  scm_gsl_mp##X##_matrix_##OP##_p (SCM A)                               \
  {                                                                     \
    return scm_from_bool (scm_c_gsl_mp##X##_matrix_##OP (A));           \
  }

_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (z, isnull, mpz_matrix_isnull);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (z, ispos, mpz_matrix_ispos);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (z, isneg, mpz_matrix_isneg);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (z, isnonneg, mpz_matrix_isnonneg);

_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (q, isnull, mpq_matrix_isnull);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (q, ispos, mpq_matrix_ispos);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (q, isneg, mpq_matrix_isneg);
_FF_SCM_GMP_SINGLE_MATRIX_PREDICATE (q, isnonneg, mpq_matrix_isnonneg);

#define _FF_SCM_SCM_SINGLE_MATRIX_PREDICATE(OP, FUNC)           \
  VISIBLE bool                                                  \
  scm_c_gsl_scm_matrix_##OP (SCM A)                             \
  {                                                             \
    const char *who = "scm_gsl_scm_matrix_" #OP;                \
                                                                \
    scm_t_array_handle handle_A;                                \
                                                                \
    scm_dynwind_begin (0);                                      \
                                                                \
    scm_array_get_handle (A, &handle_A);                        \
    scm_dynwind_array_handle_release (&handle_A);               \
    assert_c_exact_rank_1_or_2_array (who, A, &handle_A);       \
                                                                \
    const size_t m = matrix_dim1 (&handle_A);                   \
    const size_t n = matrix_dim2 (&handle_A);                   \
                                                                \
    SCM _A[m][n];                                               \
    scm_array_handle_to_scm_matrix (A, &handle_A, m, n, _A);    \
                                                                \
    const bool result = FUNC (m, n, _A);                        \
                                                                \
    scm_dynwind_end ();                                         \
                                                                \
    return result;                                              \
  }                                                             \
                                                                \
  VISIBLE SCM                                                   \
  scm_gsl_scm_matrix_##OP##_p (SCM A)                           \
  {                                                             \
    return scm_from_bool (scm_c_gsl_scm_matrix_##OP (A));       \
  }

_FF_SCM_SCM_SINGLE_MATRIX_PREDICATE (isnull, scm_matrix_isnull);
_FF_SCM_SCM_SINGLE_MATRIX_PREDICATE (ispos, scm_matrix_ispos);
_FF_SCM_SCM_SINGLE_MATRIX_PREDICATE (isneg, scm_matrix_isneg);
_FF_SCM_SCM_SINGLE_MATRIX_PREDICATE (isnonneg, scm_matrix_isnonneg);

#define _FF_SCM_GSL_SAMESHAPE_MATRICES_PREDICATE(OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE bool                                                          \
  scm_c_gsl_matrix_##OP (SCM A, SCM B)                                  \
  {                                                                     \
    const char *who = "scm_gsl_matrix_" #OP;                            \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_f64_rank_1_or_2_array (who, A, &handle_A);                 \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_f64_rank_1_or_2_array (who, B, &handle_B);                 \
                                                                        \
    gsl_matrix_const_view _A =                                          \
      scm_gsl_matrix_const_view_array_handle (A, &handle_A);            \
                                                                        \
    gsl_matrix_const_view _B =                                          \
      scm_gsl_matrix_const_view_array_handle (B, &handle_B);            \
                                                                        \
    const size_t m_A = _A.matrix.size1;                                 \
    const size_t n_A = _A.matrix.size2;                                 \
                                                                        \
    const size_t m_B = _B.matrix.size1;                                 \
    const size_t n_B = _B.matrix.size2;                                 \
                                                                        \
    assert_conformable_for_addition (NON_CONFORMABILITY_MESSAGE, who,   \
                                     A, B, m_A, n_A, m_B, n_B);         \
                                                                        \
    const bool result = FUNC (&_A.matrix, &_B.matrix);                  \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  VISIBLE SCM                                                           \
  scm_gsl_matrix_##OP##_p (SCM A, SCM B)                                \
  {                                                                     \
    return scm_from_bool (scm_c_gsl_matrix_##OP (A, B));                \
  }

_FF_SCM_GSL_SAMESHAPE_MATRICES_PREDICATE (equal, gsl_matrix_equal,
                                          equal_non_conformable_message ());

#define _FF_SCM_GMP_SAMESHAPE_MATRICES_PREDICATE(X, OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE bool                                                          \
  scm_c_gsl_mp##X##_matrix_##OP (SCM A, SCM B)                          \
  {                                                                     \
    const char *who = "scm_gsl_mp" #X "_matrix_" #OP;                   \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_exact_rank_1_or_2_array (who, A, &handle_A);               \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_exact_rank_1_or_2_array (who, B, &handle_B);               \
                                                                        \
    const size_t m_A = matrix_dim1 (&handle_A);                         \
    const size_t n_A = matrix_dim2 (&handle_A);                         \
                                                                        \
    const size_t m_B = matrix_dim1 (&handle_B);                         \
    const size_t n_B = matrix_dim2 (&handle_B);                         \
                                                                        \
    mp##X##_t _A[m_A][n_A];                                             \
    mp##X##_matrix_init (m_A, n_A, _A);                                 \
    scm_dynwind_mp##X##_matrix_clear (m_A, n_A, _A);                    \
    scm_array_handle_to_mp##X##_matrix (A, &handle_A, m_A, n_A, _A);    \
                                                                        \
    mp##X##_t _B[m_B][n_B];                                             \
    mp##X##_matrix_init (m_B, n_B, _B);                                 \
    scm_dynwind_mp##X##_matrix_clear (m_B, n_B, _B);                    \
    scm_array_handle_to_mp##X##_matrix (B, &handle_B, m_B, n_B, _B);    \
                                                                        \
    const bool result = FUNC (m_A, n_A, _A, _B);                        \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  VISIBLE SCM                                                           \
  scm_gsl_mp##X##_matrix_##OP##_p (SCM A, SCM B)                        \
  {                                                                     \
    return scm_from_bool (scm_c_gsl_mp##X##_matrix_##OP (A, B));        \
  }

_FF_SCM_GMP_SAMESHAPE_MATRICES_PREDICATE (z, equal, mpz_matrix_equal,
                                          equal_non_conformable_message ());
_FF_SCM_GMP_SAMESHAPE_MATRICES_PREDICATE (q, equal, mpq_matrix_equal,
                                          equal_non_conformable_message ());

#define _FF_SCM_SCM_SAMESHAPE_MATRICES_PREDICATE(OP, FUNC, NON_CONFORMABILITY_MESSAGE) \
  VISIBLE bool                                                          \
  scm_c_gsl_scm_matrix_##OP (SCM A, SCM B)                              \
  {                                                                     \
    const char *who = "scm_gsl_scm_matrix_" #OP;                        \
                                                                        \
    scm_t_array_handle handle_A;                                        \
    scm_t_array_handle handle_B;                                        \
                                                                        \
    scm_dynwind_begin (0);                                              \
                                                                        \
    scm_array_get_handle (A, &handle_A);                                \
    scm_dynwind_array_handle_release (&handle_A);                       \
    assert_c_exact_rank_1_or_2_array (who, A, &handle_A);               \
                                                                        \
    scm_array_get_handle (B, &handle_B);                                \
    scm_dynwind_array_handle_release (&handle_B);                       \
    assert_c_exact_rank_1_or_2_array (who, B, &handle_B);               \
                                                                        \
    const size_t m_A = matrix_dim1 (&handle_A);                         \
    const size_t n_A = matrix_dim2 (&handle_A);                         \
                                                                        \
    const size_t m_B = matrix_dim1 (&handle_B);                         \
    const size_t n_B = matrix_dim2 (&handle_B);                         \
                                                                        \
    assert_conformable_for_addition (NON_CONFORMABILITY_MESSAGE, who,   \
                                     A, B, m_A, n_A, m_B, n_B);         \
                                                                        \
    SCM _A[m_A][n_A];                                                   \
    scm_array_handle_to_scm_matrix (A, &handle_A, m_A, n_A, _A);        \
                                                                        \
    SCM _B[m_B][n_B];                                                   \
    scm_array_handle_to_scm_matrix (B, &handle_B, m_B, n_B, _B);        \
                                                                        \
    const bool result = FUNC (m_A, n_A, _A, _B);                        \
                                                                        \
    scm_dynwind_end ();                                                 \
                                                                        \
    return result;                                                      \
  }                                                                     \
                                                                        \
  VISIBLE SCM                                                           \
  scm_gsl_scm_matrix_##OP##_p (SCM A, SCM B)                            \
  {                                                                     \
    return scm_from_bool (scm_c_gsl_scm_matrix_##OP (A, B));            \
  }

_FF_SCM_SCM_SAMESHAPE_MATRICES_PREDICATE (equal, scm_matrix_equal,
                                          equal_non_conformable_message ());

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

VISIBLE SCM
scm_gsl_svd_solve_vector (SCM U, SCM S, SCM V, SCM x_transpose, SCM b_transpose)
{
  scm_t_array_handle handle_U;
  scm_t_array_handle handle_V;
  scm_t_array_handle handle_S;
  scm_t_array_handle handle_x;
  scm_t_array_handle handle_b;

  const char *who = "scm_gsl_svd_solve_vector";

  scm_dynwind_begin (0);

  scm_array_get_handle (U, &handle_U);
  scm_dynwind_array_handle_release (&handle_U);
  gsl_matrix_const_view mU =
    scm_gsl_matrix_const_view_array_handle (U, &handle_U);

  scm_array_get_handle (V, &handle_V);
  scm_dynwind_array_handle_release (&handle_V);
  gsl_matrix_const_view mV =
    scm_gsl_matrix_const_view_array_handle (V, &handle_V);

  scm_array_get_handle (S, &handle_S);
  scm_dynwind_array_handle_release (&handle_S);
  gsl_vector_const_view vS =
    scm_gsl_vector_const_view_array_handle (S, &handle_S);

  scm_array_get_handle (x_transpose, &handle_x);
  scm_dynwind_array_handle_release (&handle_x);
  gsl_vector_view vx =
    scm_gsl_vector_view_array_handle (x_transpose, &handle_x);

  scm_array_get_handle (b_transpose, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  gsl_vector_const_view vb =
    scm_gsl_vector_const_view_array_handle (b_transpose, &handle_b);

  int errval =
    gsl_linalg_SV_solve (&mU.matrix, &mV.matrix, &vS.vector, &vb.vector,
                         &vx.vector);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_4 (U, S, V, b_transpose), SCM_UNDEFINED));

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_gsl_linalg_LU_decomp (SCM A)
{
  scm_t_array_handle handle_A;

  const char *who = "scm_gsl_linalg_LU_decomp";

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  gsl_matrix_view mA = scm_gsl_matrix_view_array_handle (A, &handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  assert_matrix_is_square (who, A, m, n);

  gsl_permutation *p = xdie_on_null (gsl_permutation_alloc (n));
  scm_dynwind_gsl_permutation_free (p);

  int signum;

  int errval = gsl_linalg_LU_decomp (&mA.matrix, p, &signum);
  if (errval != GSL_SUCCESS)
    scm_raise_gsl_error
      (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                   scm_from_int (errval),
                   scm_from_latin1_keyword ("who"),
                   scm_from_latin1_string (who),
                   scm_from_latin1_keyword ("irritants"),
                   scm_list_1 (A), SCM_UNDEFINED));

  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&mA.matrix, 1),
    scm_from_gsl_permutation (p),
    scm_from_int (signum)
  };

  scm_dynwind_end ();

  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_gsl_mpq_linalg_LU_decomp (SCM A)
{
  scm_t_array_handle handle_A;

  const char *who = "scm_gsl_mpq_linalg_LU_decomp";

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  assert_matrix_is_square (who, A, m, n);

  mpq_t _A[n][n];
  mpq_matrix_init (n, n, _A);
  scm_dynwind_mpq_matrix_clear (n, n, _A);

  scm_array_handle_to_mpq_matrix (A, &handle_A, n, n, _A);

  gsl_permutation *p = xdie_on_null (gsl_permutation_alloc (n));
  scm_dynwind_gsl_permutation_free (p);

  int signum;

  mpq_linalg_LU_decomp (n, _A, gsl_permutation_data (p), &signum);

  SCM values[3] = {
    scm_from_mpq_matrix (n, n, _A),
    scm_from_gsl_permutation (p),
    scm_from_int (signum)
  };

  scm_dynwind_end ();

  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_gsl_mpq_linalg_LU_decomp_fast_pivot (SCM A)
{
  scm_t_array_handle handle_A;

  const char *who = "scm_gsl_mpq_linalg_LU_decomp_fast_pivot";

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  assert_matrix_is_square (who, A, m, n);

  mpq_t _A[n][n];
  mpq_matrix_init (n, n, _A);
  scm_dynwind_mpq_matrix_clear (n, n, _A);

  scm_array_handle_to_mpq_matrix (A, &handle_A, n, n, _A);

  gsl_permutation *p = xdie_on_null (gsl_permutation_alloc (n));
  scm_dynwind_gsl_permutation_free (p);

  int signum;

  mpq_linalg_LU_decomp_fast_pivot (n, _A, gsl_permutation_data (p), &signum);

  SCM values[3] = {
    scm_from_mpq_matrix (n, n, _A),
    scm_from_gsl_permutation (p),
    scm_from_int (signum)
  };

  scm_dynwind_end ();

  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_gsl_scm_linalg_LU_decomp (SCM A)
{
  scm_t_array_handle handle_A;

  const char *who = "scm_gsl_scm_linalg_LU_decomp";

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);

  const size_t m = matrix_dim1 (&handle_A);
  const size_t n = matrix_dim2 (&handle_A);

  assert_matrix_is_square (who, A, m, n);

  SCM _A[n][n];
  scm_array_handle_to_scm_matrix (A, &handle_A, n, n, _A);

  gsl_permutation *p = xdie_on_null (gsl_permutation_alloc (n));
  scm_dynwind_gsl_permutation_free (p);

  int signum;

  scm_linalg_LU_decomp (n, _A, gsl_permutation_data (p), &signum);

  SCM values[3] = {
    scm_from_scm_matrix (n, n, _A),
    scm_from_gsl_permutation (p),
    scm_from_int (signum)
  };

  scm_dynwind_end ();

  return scm_c_values (values, 3);
}

/*static*/ VISIBLE SCM
/*xxxxxxxxxxxxxxxxxxxx*/
scm_gsl_mpq_linalg_LU_solve (SCM LU, SCM permutation, SCM b)
{
  scm_t_array_handle handle_LU;
  scm_t_array_handle handle_b;

  const char *who = "scm_gsl_mpq_linalg_LU_solve";

  scm_dynwind_begin (0);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);

  const size_t m = matrix_dim1 (&handle_LU);
  const size_t n = matrix_dim2 (&handle_LU);

  assert_matrix_is_square (who, LU, m, n);

  mpq_t _LU[n][n];
  mpq_matrix_init (n, n, _LU);
  scm_dynwind_mpq_matrix_clear (n, n, _LU);

  scm_array_handle_to_mpq_matrix (LU, &handle_LU, n, n, _LU);

  gsl_permutation *p = scm_to_gsl_permutation (permutation);
  scm_dynwind_gsl_permutation_free (p);

  assert_permutation_conforms_with_matrix (who, n, p, permutation, LU);

  scm_array_get_handle (b, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);

  const size_t n_b = vector_dim (b, &handle_b);

  assert_conformable_for_Ax_equals_B (who, LU, b, m, n, n_b, 1);

  mpq_t _b[n];
  mpq_vector_init (n, _b);
  scm_dynwind_mpq_vector_clear (n, _b);

  scm_array_handle_to_mpq_vector (b, &handle_b, n, _b);

  bool singular;
  mpq_linalg_LU_svx (n, _LU, gsl_permutation_data (p), _b, &singular);
  if (singular)
    exception__LU_matrix_is_singular (who, LU);

  SCM solution = scm_from_mpq_vector (n, _b);

  scm_dynwind_end ();

  return solution;
}

VISIBLE SCM
scm_gsl_scm_linalg_LU_solve (SCM LU, SCM permutation, SCM b)
{
  scm_t_array_handle handle_LU;
  scm_t_array_handle handle_b;

  const char *who = "scm_gsl_scm_linalg_LU_solve";

  scm_dynwind_begin (0);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);

  const size_t m = matrix_dim1 (&handle_LU);
  const size_t n = matrix_dim2 (&handle_LU);

  assert_matrix_is_square (who, LU, m, n);

  SCM _LU[n][n];

  scm_array_handle_to_scm_matrix (LU, &handle_LU, n, n, _LU);

  gsl_permutation *p = scm_to_gsl_permutation (permutation);
  scm_dynwind_gsl_permutation_free (p);

  assert_permutation_conforms_with_matrix (who, n, p, permutation, LU);

  scm_array_get_handle (b, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);

  const size_t n_b = vector_dim (b, &handle_b);

  assert_conformable_for_Ax_equals_B (who, LU, b, m, n, n_b, 1);

  SCM _b[n];

  scm_array_handle_to_scm_vector (b, &handle_b, n, _b);

  scm_linalg_LU_svx (n, _LU, gsl_permutation_data (p), _b);

  SCM solution = scm_from_scm_vector (n, _b);

  scm_dynwind_end ();

  return solution;
}

VISIBLE SCM
scm_gsl_linalg_LU_solve (SCM LU, SCM permutation, SCM B)
{
  scm_t_array_handle handle_LU;
  scm_t_array_handle handle_B;

  const char *who = "scm_gsl_linalg_LU_solve";

  scm_dynwind_begin (0);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);

  const size_t m = matrix_dim1 (&handle_LU);
  const size_t n = matrix_dim2 (&handle_LU);

  assert_matrix_is_square (who, LU, m, n);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);
  gsl_matrix_const_view mLU =
    scm_gsl_matrix_const_view_array_handle (LU, &handle_LU);

  gsl_permutation *p = scm_to_gsl_permutation (permutation);
  scm_dynwind_gsl_permutation_free (p);

  assert_permutation_conforms_with_matrix (who, n, p, permutation, LU);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);
  gsl_matrix_const_view mB =
    scm_gsl_matrix_const_view_array_handle (B, &handle_B);

  const size_t m_B = matrix_dim1 (&handle_B);
  const size_t n_B = matrix_dim2 (&handle_B);

  assert_conformable_for_Ax_equals_B (who, LU, B, m, n, m_B, n_B);

  double X[m_B][n_B];
  gsl_matrix_view mX = gsl_matrix_view_array (&X[0][0], m_B, n_B);

  for (size_t j = 0; j < n_B; j++)
    {
      gsl_vector_const_view vb = gsl_matrix_const_column (&mB.matrix, j);
      gsl_vector_view vx = gsl_matrix_column (&mX.matrix, j);

      int errval = gsl_linalg_LU_solve (&mLU.matrix, p, &vb.vector, &vx.vector);
      if (errval != GSL_SUCCESS)
        scm_raise_gsl_error
          (scm_list_n (scm_from_latin1_keyword ("gsl-errno"),
                       scm_from_int (errval),
                       scm_from_latin1_keyword ("who"),
                       scm_from_latin1_string (who),
                       scm_from_latin1_keyword ("irritants"),
                       scm_list_2 (LU, B), SCM_UNDEFINED));
    }

  SCM solution = scm_gsl_matrix_to_f64matrix (&mX.matrix, 1);

  scm_dynwind_end ();

  return solution;
}

/*
VISIBLE SCM
scm_gsl_mpq_linalg_LU_solve (SCM LU, SCM permutation, SCM B)
{
  scm_t_array_handle handle_LU;
  scm_t_array_handle handle_B;

  const char *who = "scm_gsl_mpq_linalg_LU_solve";

  scm_dynwind_begin (0);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);

  const size_t m = matrix_dim1 (&handle_LU);
  const size_t n = matrix_dim2 (&handle_LU);

  assert_matrix_is_square (who, LU, m, n);

  mpq_t _LU[n][n];
  mpq_matrix_init (n, n, _LU);
  scm_dynwind_mpq_matrix_clear (n, n, _LU);

  scm_array_handle_to_mpq_matrix (LU, &handle_LU, n, n, _LU);

  gsl_permutation *p = scm_to_gsl_permutation (permutation);
  scm_dynwind_gsl_permutation_free (p);

  assert_permutation_conforms_with_matrix (who, n, p, permutation, LU);

  scm_array_get_handle (B, &handle_B);
  scm_dynwind_array_handle_release (&handle_B);

  const size_t m_B = matrix_dim1 (&handle_B);
  const size_t n_B = matrix_dim2 (&handle_B);

  assert_conformable_for_Ax_equals_B (who, LU, B, m, n, m_B, n_B);

  mpq_t _Bt[n_B][m_B];
  mpq_matrix_init (n_B, m_B, _Bt);
  scm_dynwind_mpq_matrix_clear (n_B, m_B, _Bt);

  scm_array_handle_to_transposed_mpq_matrix (B, &handle_B, m_B, n_B, _Bt);

  bool singular;
  //?????????  mpq_linalg_LU_svx (n, _LU, gsl_permutation_data (p), _B, &singular);
  if (singular)
    exception__LU_matrix_is_singular (who, LU);

  SCM solution = scm_from_transposed_mpq_matrix (m_B, n_B, _Bt);

  scm_dynwind_end ();

  return solution;
}
*/

/*
VISIBLE SCM
scm_gsl_scm_linalg_LU_solve (SCM LU, SCM permutation, SCM b)
{
  scm_t_array_handle handle_LU;
  scm_t_array_handle handle_b;

  const char *who = "scm_gsl_scm_linalg_LU_solve";

  scm_dynwind_begin (0);

  scm_array_get_handle (LU, &handle_LU);
  scm_dynwind_array_handle_release (&handle_LU);

  const size_t m = matrix_dim1 (&handle_LU);
  const size_t n = matrix_dim2 (&handle_LU);

  assert_matrix_is_square (who, LU, m, n);

  SCM _LU[n][n];

  scm_array_handle_to_scm_matrix (LU, &handle_LU, n, n, _LU);

  gsl_permutation *p = scm_to_gsl_permutation (permutation);
  scm_dynwind_gsl_permutation_free (p);

  assert_permutation_conforms_with_matrix (who, n, p, permutation, LU);

  scm_array_get_handle (b, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);

  const size_t n_b = vector_dim (b, &handle_b);

  assert_conformable_for_Ax_equals_B (who, LU, b, m, n, n_b, 1);

  SCM _b[n];

  scm_array_handle_to_scm_vector (b, &handle_b, n, _b);

  scm_linalg_LU_svx (n, _LU, gsl_permutation_data (p), _b);

  SCM solution = scm_from_scm_vector (n, _b);

  scm_dynwind_end ();

  return solution;
}
*/

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

  scm_c_define_gsubr ("gsl:matrix-scale-f64", 2, 0, 0, scm_gsl_matrix_scale);
  scm_c_define_gsubr ("gsl:matrix-scale-mpz", 2, 0, 0,
                      scm_gsl_mpz_matrix_scale);
  scm_c_define_gsubr ("gsl:matrix-scale-mpq", 2, 0, 0,
                      scm_gsl_mpq_matrix_scale);
  scm_c_define_gsubr ("gsl:matrix-scale-scm", 2, 0, 0,
                      scm_gsl_scm_matrix_scale);

  scm_c_define_gsubr ("gsl:gemm-f64", 7, 0, 0, scm_gsl_blas_dgemm);
  scm_c_define_gsubr ("gsl:gemm-mpz", 7, 0, 0, scm_gsl_mpz_gemm);
  scm_c_define_gsubr ("gsl:gemm-mpq", 7, 0, 0, scm_gsl_mpq_gemm);
  scm_c_define_gsubr ("gsl:gemm-scm", 7, 0, 0, scm_gsl_scm_gemm);

  scm_c_define_gsubr ("gsl:matrix-add-f64", 2, 0, 0, scm_gsl_matrix_add);
  scm_c_define_gsubr ("gsl:matrix-add-mpz", 2, 0, 0, scm_gsl_mpz_matrix_add);
  scm_c_define_gsubr ("gsl:matrix-add-mpq", 2, 0, 0, scm_gsl_mpq_matrix_add);
  scm_c_define_gsubr ("gsl:matrix-add-scm", 2, 0, 0, scm_gsl_scm_matrix_add);

  scm_c_define_gsubr ("gsl:matrix-sub-f64", 2, 0, 0, scm_gsl_matrix_sub);
  scm_c_define_gsubr ("gsl:matrix-sub-mpz", 2, 0, 0, scm_gsl_mpz_matrix_sub);
  scm_c_define_gsubr ("gsl:matrix-sub-mpq", 2, 0, 0, scm_gsl_mpq_matrix_sub);
  scm_c_define_gsubr ("gsl:matrix-sub-scm", 2, 0, 0, scm_gsl_scm_matrix_sub);

  scm_c_define_gsubr ("gsl:matrix-mul-elements-f64", 2, 0, 0,
                      scm_gsl_matrix_mul_elements);
  scm_c_define_gsubr ("gsl:matrix-mul-elements-mpz", 2, 0, 0,
                      scm_gsl_mpz_matrix_mul_elements);
  scm_c_define_gsubr ("gsl:matrix-mul-elements-mpq", 2, 0, 0,
                      scm_gsl_mpq_matrix_mul_elements);
  scm_c_define_gsubr ("gsl:matrix-mul-elements-scm", 2, 0, 0,
                      scm_gsl_scm_matrix_mul_elements);

  scm_c_define_gsubr ("gsl:matrix-div-elements-f64", 2, 0, 0,
                      scm_gsl_matrix_div_elements);
  scm_c_define_gsubr ("gsl:matrix-div-elements-mpq", 2, 0, 0,
                      scm_gsl_mpq_matrix_div_elements);
  scm_c_define_gsubr ("gsl:matrix-div-elements-scm", 2, 0, 0,
                      scm_gsl_scm_matrix_div_elements);

  scm_c_define_gsubr ("gsl:matrix-isnull-f64?", 1, 0, 0,
                      scm_gsl_matrix_isnull_p);
  scm_c_define_gsubr ("gsl:matrix-isneg-f64?", 1, 0, 0, scm_gsl_matrix_isneg_p);
  scm_c_define_gsubr ("gsl:matrix-ispos-f64?", 1, 0, 0, scm_gsl_matrix_ispos_p);
  scm_c_define_gsubr ("gsl:matrix-isnonneg-f64?", 1, 0, 0,
                      scm_gsl_matrix_isnonneg_p);

  scm_c_define_gsubr ("gsl:matrix-isnull-mpz?", 1, 0, 0,
                      scm_gsl_mpz_matrix_isnull_p);
  scm_c_define_gsubr ("gsl:matrix-isneg-mpz?", 1, 0, 0,
                      scm_gsl_mpz_matrix_isneg_p);
  scm_c_define_gsubr ("gsl:matrix-ispos-mpz?", 1, 0, 0,
                      scm_gsl_mpz_matrix_ispos_p);
  scm_c_define_gsubr ("gsl:matrix-isnonneg-mpz?", 1, 0, 0,
                      scm_gsl_mpz_matrix_isnonneg_p);

  scm_c_define_gsubr ("gsl:matrix-isnull-mpq?", 1, 0, 0,
                      scm_gsl_mpq_matrix_isnull_p);
  scm_c_define_gsubr ("gsl:matrix-isneg-mpq?", 1, 0, 0,
                      scm_gsl_mpq_matrix_isneg_p);
  scm_c_define_gsubr ("gsl:matrix-ispos-mpq?", 1, 0, 0,
                      scm_gsl_mpq_matrix_ispos_p);
  scm_c_define_gsubr ("gsl:matrix-isnonneg-mpq?", 1, 0, 0,
                      scm_gsl_mpq_matrix_isnonneg_p);

  scm_c_define_gsubr ("gsl:matrix-isnull-scm?", 1, 0, 0,
                      scm_gsl_scm_matrix_isnull_p);
  scm_c_define_gsubr ("gsl:matrix-isneg-scm?", 1, 0, 0,
                      scm_gsl_scm_matrix_isneg_p);
  scm_c_define_gsubr ("gsl:matrix-ispos-scm?", 1, 0, 0,
                      scm_gsl_scm_matrix_ispos_p);
  scm_c_define_gsubr ("gsl:matrix-isnonneg-scm?", 1, 0, 0,
                      scm_gsl_scm_matrix_isnonneg_p);

  scm_c_define_gsubr ("gsl:matrix-equal-f64?", 2, 0, 0, scm_gsl_matrix_equal_p);
  scm_c_define_gsubr ("gsl:matrix-equal-mpz?", 2, 0, 0,
                      scm_gsl_mpz_matrix_equal_p);
  scm_c_define_gsubr ("gsl:matrix-equal-mpq?", 2, 0, 0,
                      scm_gsl_mpq_matrix_equal_p);
  scm_c_define_gsubr ("gsl:matrix-equal-scm?", 2, 0, 0,
                      scm_gsl_scm_matrix_equal_p);

  scm_c_define_gsubr ("gsl:svd-golub-reinsch-f64", 1, 0, 0,
                      scm_gsl_svd_golub_reinsch);
  scm_c_define_gsubr ("gsl:svd-modified-golub-reinsch-f64", 1, 0, 0,
                      scm_gsl_svd_modified_golub_reinsch);
  scm_c_define_gsubr ("gsl:svd-jacobi-f64", 1, 0, 0, scm_gsl_svd_jacobi);
  scm_c_define_gsubr ("gsl:svd-solve-vector-f64", 5, 0, 0,
                      scm_gsl_svd_solve_vector);

  scm_c_define_gsubr ("gsl:lu-decomposition-f64", 1, 0, 0,
                      scm_gsl_linalg_LU_decomp);
  scm_c_define_gsubr ("gsl:lu-decomposition-mpq", 1, 0, 0,
                      scm_gsl_mpq_linalg_LU_decomp);
  scm_c_define_gsubr ("gsl:lu-decomposition-scm", 1, 0, 0,
                      scm_gsl_scm_linalg_LU_decomp);
  scm_c_define_gsubr ("gsl:lu-decomposition-mpq-fast-pivot", 1, 0, 0,
                      scm_gsl_mpq_linalg_LU_decomp_fast_pivot);

  scm_c_define_gsubr ("gsl:lu-solve-f64", 3, 0, 0, scm_gsl_linalg_LU_solve);
  scm_c_define_gsubr ("gsl:lu-solve-vector-mpq", 3, 0, 0,
                      scm_gsl_mpq_linalg_LU_solve);
  scm_c_define_gsubr ("gsl:lu-solve-vector-scm", 3, 0, 0,
                      scm_gsl_scm_linalg_LU_solve);
}
