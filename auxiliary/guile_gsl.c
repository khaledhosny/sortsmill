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
#include <sortsmill/guile/gsl.h>
#include <sortsmill/guile/rnrs_conditions.h>
#include <sortsmill/guile/format.h>
#include <intl.h>

void init_guile_sortsmill_gsl (void);

VISIBLE SCM
scm_gsl_errno_to_symbol (SCM errval)
{
  return scm_call_1 (scm_c_public_ref ("sortsmill gsl", "gsl-errno->symbol"),
                     errval);
}

VISIBLE SCM
scm_c_gsl_errno_to_symbol (int errval)
{
  return scm_gsl_errno_to_symbol (scm_from_int (errval));
}

VISIBLE SCM
scm_raise_gsl_error (SCM arguments)
{
  return scm_apply_0 (scm_c_public_ref ("sortsmill gsl", "raise-gsl-error"),
                      arguments);
}

VISIBLE void
scm_gsl_error_handler_for_raising_a_gsl_error (const char *reason,
                                               const char *file,
                                               int line, int gsl_errno)
{
  scm_raise_gsl_error (scm_list_4 (scm_from_locale_string (reason),
                                   scm_from_locale_string (file),
                                   scm_from_int (line),
                                   scm_from_int (gsl_errno)));
}

VISIBLE void
exception__array_has_no_elements (const char *who, SCM irritants)
{
  const char *message = _("array has no elements");
  if (SCM_UNBNDP (irritants))
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_make_message_condition (scm_from_locale_string (message))));
  else
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
  if (SCM_UNBNDP (irritants))
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_make_message_condition (scm_from_locale_string (message))));
  else
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
  if (SCM_UNBNDP (irritants))
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_make_message_condition (scm_from_locale_string (message))));
  else
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
  if (SCM_UNBNDP (irritants))
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_make_message_condition (scm_from_locale_string (message))));
  else
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
  if (SCM_UNBNDP (irritants))
    rnrs_raise_condition
      (scm_list_3
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_make_message_condition (scm_from_locale_string (message))));
  else
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
scm_gsl_vector_const_view_array_handle (scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_vector_const_view_array_handle";

  const double *my_elems;
  size_t stride;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    // FIXME: Find irritants to list.
    exception__expected_array_of_rank_1 (who, SCM_UNDEFINED);

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n = dims[0].ubnd - dims[0].lbnd + 1;
  if (n < 1)
    // FIXME: Find irritants to list.
    exception__array_has_no_elements (who, SCM_UNDEFINED);
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
scm_gsl_vector_view_array_handle (scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_vector_view_array_handle";

  double *my_elems;
  size_t stride;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    // FIXME: Find irritants to list.
    exception__expected_array_of_rank_1 (who, SCM_UNDEFINED);

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n = dims[0].ubnd - dims[0].lbnd + 1;
  if (n < 1)
    // FIXME: Find irritants to list.
    exception__array_has_no_elements (who, SCM_UNDEFINED);
  if (0 < dims[0].inc)
    {
      my_elems = elems;
      stride = dims[0].inc;
    }
  else
    // FIXME: Find irritants to list.
    exception__layout_incompatible_with_gsl (who, SCM_UNDEFINED);
  return gsl_vector_view_array_with_stride (my_elems, stride, n);
}

static gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle_rank1 (const char *who,
                                              scm_t_array_handle *handlep)
{
  const double *my_elems;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1)
    // FIXME: Find irritants to list.
    exception__expected_array_of_rank_1 (who, SCM_UNDEFINED);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  if (n0 < 1)
    // FIXME: Find irritants to list.
    exception__array_has_no_elements (who, SCM_UNDEFINED);
  if (dims[0].inc == 1)
    {
      my_elems = elems;
    }
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
                                              scm_t_array_handle *handlep)
{
  const double *my_elems;
  size_t tda;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    exception__expected_array_of_rank_2 (who, SCM_UNDEFINED);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    exception__array_has_no_elements (who, SCM_UNDEFINED);
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
scm_gsl_matrix_const_view_array_handle (scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_matrix_const_view_array_handle";

  typedef gsl_matrix_const_view (*func_t) (const char *, scm_t_array_handle *);
  static const func_t func[] = {
    NULL,
    scm_gsl_matrix_const_view_array_handle_rank1,
    scm_gsl_matrix_const_view_array_handle_rank2
  };

  const size_t rank = scm_array_handle_rank (handlep);
  if (rank != 1 && rank != 2)
    // FIXME: Get irritants to return.
    exception__expected_array_of_rank_1_or_2 (who, SCM_UNDEFINED);
  return func[rank] (who, handlep);
}

VISIBLE gsl_matrix_view
scm_gsl_matrix_view_array_handle (scm_t_array_handle *handlep)
{
  const char *who = "scm_gsl_matrix_view_array_handle";

  double *my_elems;
  size_t tda;

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    // FIXME: Find irritants to list.
    exception__expected_array_of_rank_2 (who, SCM_UNDEFINED);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    // FIXME: Find irritants to list.
    exception__array_has_no_elements (who, SCM_UNDEFINED);
  if (dims[1].inc == 1 && 0 < dims[0].inc)
    {
      my_elems = elems;
      tda = dims[0].inc;
    }
  else
    // FIXME: Find irritants to list.
    exception__layout_incompatible_with_gsl (who, SCM_UNDEFINED);
  return gsl_matrix_view_array_with_tda (my_elems, n0, n1, tda);
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
  SCM result = scm_make_typed_array (scm_from_latin1_symbol ("f64"),
                                     SCM_UNSPECIFIED, scm_list_1 (bounds));

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
  SCM result = scm_make_typed_array (scm_from_latin1_symbol ("f64"),
                                     SCM_UNSPECIFIED, scm_list_2 (row_bounds,
                                                                  column_bounds));
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
assert_f64_rank_1_or_2_array (SCM who, SCM array)
{
  scm_call_2 (scm_c_private_ref ("sortsmill gsl matrices",
                                 "assert-f64-rank-1-or-2-array"), who, array);
}

static void
assert_c_f64_rank_1_or_2_array (const char *who, SCM array,
                                scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  if (!scm_is_typed_array (array, scm_from_latin1_symbol ("f64"))
      || (rank != 1 && rank != 2))
    assert_f64_rank_1_or_2_array (scm_from_utf8_string (who), array);
}

static void
assert_CblasTrans_flag (SCM who, SCM trans)
{
  scm_call_2
    (scm_c_private_ref ("sortsmill gsl matrices", "assert-CblasTrans-flag"),
     who, trans);
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

  gsl_matrix_const_view _A = scm_gsl_matrix_const_view_array_handle (&handle_A);
  gsl_matrix_const_view _B = scm_gsl_matrix_const_view_array_handle (&handle_B);

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
    scm_gsl_matrix_const_view_array_handle (&handle_C) :
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

VISIBLE void
init_guile_sortsmill_gsl (void)
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
}
