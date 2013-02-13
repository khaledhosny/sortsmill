#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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
#include <sortsmill/guile/matrices.h>
#include <sortsmill/guile/rnrs_conditions.h>
#include <libguile.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_linalg.h>
#include <intl.h>

//////////////////////////////////////////////////
// FIXME: Switch to R6RS exceptions throughout. //
//////////////////////////////////////////////////

void init_guile_sortsmill_matrices (void);

// FIXME: Put scm_c_gsl_error in a more general GSL module.
VISIBLE SCM
scm_c_gsl_error (int errval, const char *who, SCM irritants)
{
  SCM errstr =
    scm_string_append (scm_list_2 (scm_from_utf8_string (_("GSL error: ")),
                                   scm_from_utf8_string (gsl_strerror
                                                         (errval))));
  return
    rnrs_raise_condition (scm_list_4
                          (rnrs_make_error (), rnrs_c_make_who_condition (who),
                           rnrs_make_message_condition (errstr),
                           rnrs_make_irritants_condition (irritants)));
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
    scm_misc_error (who, "array has rank ~A, but should have rank 1",
                    scm_list_1 (scm_number_to_string (scm_from_int (rank),
                                                      scm_from_int (10))));

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n = dims[0].ubnd - dims[0].lbnd + 1;
  if (n < 1)
    scm_misc_error (who, "array has no elements", SCM_BOOL_F);
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

VISIBLE gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle (scm_t_array_handle *handlep)
{
  const double *my_elems;
  ssize_t tda;                  // FIXME: Should not this be size_t?

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    scm_misc_error ("scm_gsl_matrix_const_view_array_handle",
                    "array has rank ~A, but should have rank 2",
                    scm_list_1 (scm_number_to_string (scm_from_int (rank),
                                                      scm_from_int (10))));

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  const double *elems = scm_array_handle_f64_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    scm_misc_error ("scm_gsl_matrix_const_view_array_handle",
                    "array has no elements", SCM_BOOL_F);
  if (dims[1].inc == 1 && 0 < dims[0].inc)
    {
      my_elems = elems;
      tda = dims[0].inc;
    }
  else
    {
      double *buffer = scm_gc_malloc_pointerless (n0 * n1 * sizeof (double),
                                                  "scm_gsl_matrix_const_view_array_handle");
      for (size_t i = 0; i < n0; i++)
        for (size_t j = 0; j < n1; j++)
          buffer[i * n1 + j] = elems[i * dims[0].inc + j * dims[1].inc];
      my_elems = buffer;
      tda = 1;
    }
  return gsl_matrix_const_view_array_with_tda (my_elems, n0, n1, tda);
}

// FIXME: Is this needed?
VISIBLE gsl_matrix_view
scm_gsl_matrix_view_array_handle (scm_t_array_handle *handlep)
{
  double *my_elems;
  ssize_t tda;                  // FIXME: Should not this be size_t?

  assert (handlep != NULL);

  size_t rank = scm_array_handle_rank (handlep);
  if (rank != 2)
    scm_misc_error ("scm_gsl_matrix_view_array_handle",
                    "array has rank ~A, but should have rank 2",
                    scm_list_1 (scm_number_to_string (scm_from_int (rank),
                                                      scm_from_int (10))));

  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
  double *elems = scm_array_handle_f64_writable_elements (handlep);
  ssize_t n0 = dims[0].ubnd - dims[0].lbnd + 1;
  ssize_t n1 = dims[1].ubnd - dims[1].lbnd + 1;
  if (n0 < 1 || n1 < 1)
    scm_misc_error ("scm_gsl_matrix_view_array_handle",
                    "array has no elements", SCM_BOOL_F);
  if (dims[1].inc == 1 && 0 < dims[0].inc)
    {
      my_elems = elems;
      tda = dims[0].inc;
    }
  else
    {
      double *buffer = scm_gc_malloc_pointerless (n0 * n1 * sizeof (double),
                                                  "scm_gsl_matrix_view_array_handle");
      for (size_t i = 0; i < n0; i++)
        for (size_t j = 0; j < n1; j++)
          buffer[i * n1 + j] = elems[i * dims[0].inc + j * dims[1].inc];
      my_elems = buffer;
      tda = 1;
    }
  return gsl_matrix_view_array_with_tda (my_elems, n0, n1, tda);
}

VISIBLE SCM
scm_gsl_vector_to_f64vector (const gsl_vector *v, int low_index)
{
  assert (0 < v->size);

  const char *who = "scm_gsl_vector_to_f64vector";

  if (v->size < 1)
    rnrs_raise_condition (scm_list_3
                          (rnrs_make_error (), rnrs_c_make_who_condition (who),
                           rnrs_c_make_message_condition (_
                                                          ("gsl_vector size is zero"))));

  scm_t_array_handle handle;

  SCM bounds = scm_list_2 (scm_from_int (low_index),
                           scm_from_int (low_index + v->size - 1));
  SCM result =
    scm_make_typed_array (scm_from_latin1_symbol ("f64"), SCM_UNSPECIFIED,
                          scm_list_1 (bounds));
  scm_array_get_handle (result, &handle);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  double *elems = scm_array_handle_f64_writable_elements (&handle);
  for (size_t i = 0; i < v->size; i++)
    elems[i * dims[0].inc] = v->data[i * v->stride];
  scm_array_handle_release (&handle);
  return result;
}

VISIBLE SCM
scm_gsl_matrix_to_f64matrix (const gsl_matrix *m, int low_index)
{
  assert (0 < m->size1);
  assert (0 < m->size2);

  if (m->size1 < 1)
    scm_misc_error ("scm_gsl_matrix_to_f64matrix", "gsl_matrix size1 is zero",
                    SCM_BOOL_F);
  if (m->size2 < 1)
    scm_misc_error ("scm_gsl_matrix_to_f64matrix", "gsl_matrix size2 is zero",
                    SCM_BOOL_F);

  scm_t_array_handle handle;

  SCM row_bounds = scm_list_2 (scm_from_int (low_index),
                               scm_from_int (low_index + m->size1 - 1));
  SCM column_bounds = scm_list_2 (scm_from_int (low_index),
                                  scm_from_int (low_index + m->size2 - 1));
  SCM result =
    scm_make_typed_array (scm_from_latin1_symbol ("f64"), SCM_UNSPECIFIED,
                          scm_list_2 (row_bounds, column_bounds));
  scm_array_get_handle (result, &handle);
  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
  double *elems = scm_array_handle_f64_writable_elements (&handle);
  for (size_t i = 0; i < m->size1; i++)
    for (size_t j = 0; j < m->size2; j++)
      elems[i * dims[0].inc + j * dims[1].inc] = m->data[i * m->tda + j];
  scm_array_handle_release (&handle);
  return result;
}

VISIBLE SCM
scm_f64matrix_f64matrix_mult (SCM a, SCM b)
{
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  scm_array_get_handle (b, &handle_b);
  gsl_matrix mb = scm_gsl_matrix_const_view_array_handle (&handle_b).matrix;

  if (ma.size2 != mb.size1)
    {
      SCM ten = scm_from_int (10);
      scm_misc_error ("f64matrix*",
                      "non-conformable matrices: ~Ax~A multiplied by ~Ax~A",
                      scm_list_4 (scm_number_to_string
                                  (scm_from_int (ma.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (ma.size2), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size2), ten)));
    }

  double buffer[ma.size1 * mb.size2];
  gsl_matrix_view vc = gsl_matrix_view_array (buffer, ma.size1, mb.size2);

  // int gsl_blas_dgemm (CBLAS_TRANSPOSE_t TransA,
  //                     CBLAS_TRANSPOSE_t TransB,
  //                     double alpha,
  //                     const gsl_matrix * A,
  //                     const gsl_matrix * B,
  //                     double beta,
  //                     gsl_matrix * C);
  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &ma, &mb, 0.0, &vc.matrix);
  SCM result = scm_gsl_matrix_to_f64matrix (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE SCM
scm_f64matrix_f64matrix_add (SCM a, SCM b)
{
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  scm_array_get_handle (b, &handle_b);
  gsl_matrix mb = scm_gsl_matrix_const_view_array_handle (&handle_b).matrix;

  if (ma.size1 != mb.size1 || ma.size2 != mb.size2)
    {
      SCM ten = scm_from_int (10);
      scm_misc_error ("f64matrix+",
                      "non-conformable matrices: ~Ax~A plus ~Ax~A",
                      scm_list_4 (scm_number_to_string
                                  (scm_from_int (ma.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (ma.size2), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size2), ten)));
    }

  double buffer[ma.size1 * ma.size2];
  gsl_matrix_view vc = gsl_matrix_view_array (buffer, ma.size1, mb.size2);
  gsl_matrix_memcpy (&vc.matrix, &ma);

  gsl_matrix_add (&vc.matrix, &mb);
  SCM result = scm_gsl_matrix_to_f64matrix (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE SCM
scm_f64matrix_f64matrix_sub (SCM a, SCM b)
{
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  scm_array_get_handle (b, &handle_b);
  gsl_matrix mb = scm_gsl_matrix_const_view_array_handle (&handle_b).matrix;

  if (ma.size1 != mb.size1 || ma.size2 != mb.size2)
    {
      SCM ten = scm_from_int (10);
      scm_misc_error ("f64matrix-",
                      "non-conformable matrices: ~Ax~A minus ~Ax~A",
                      scm_list_4 (scm_number_to_string
                                  (scm_from_int (ma.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (ma.size2), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size1), ten),
                                  scm_number_to_string (scm_from_int
                                                        (mb.size2), ten)));
    }

  double buffer[ma.size1 * ma.size2];
  gsl_matrix_view vc = gsl_matrix_view_array (buffer, ma.size1, mb.size2);
  gsl_matrix_memcpy (&vc.matrix, &ma);

  gsl_matrix_sub (&vc.matrix, &mb);
  SCM result = scm_gsl_matrix_to_f64matrix (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE SCM
scm_f64matrix_svd_golub_reinsch (SCM a)
{
  const char *who = "scm_f64matrix_svd_golub_reinsch";

  scm_t_array_handle handle_a;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  double u_buf[ma.size1 * ma.size2];
  double v_buf[ma.size2 * ma.size2];
  double s_buf[ma.size2];
  double work_buf[ma.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.size1, ma.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.size2, ma.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.size2);
  gsl_vector_view work = gsl_vector_view_array (work_buf, ma.size2);

  gsl_matrix_memcpy (&u.matrix, &ma);
  scm_array_handle_release (&handle_a);

  int errval =
    gsl_linalg_SV_decomp (&u.matrix, &v.matrix, &s.vector, &work.vector);
  if (errval != GSL_SUCCESS)
    scm_c_gsl_error (errval, who, scm_list_1 (a));

  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_f64matrix_svd_modified_golub_reinsch (SCM a)
{
  const char *who = "scm_f64matrix_svd_modified_golub_reinsch";

  scm_t_array_handle handle_a;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  double u_buf[ma.size1 * ma.size2];
  double x_buf[ma.size2 * ma.size2];
  double v_buf[ma.size2 * ma.size2];
  double s_buf[ma.size2];
  double work_buf[ma.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.size1, ma.size2);
  gsl_matrix_view x = gsl_matrix_view_array (x_buf, ma.size2, ma.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.size2, ma.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.size2);
  gsl_vector_view work = gsl_vector_view_array (work_buf, ma.size2);

  gsl_matrix_memcpy (&u.matrix, &ma);
  scm_array_handle_release (&handle_a);

  int errval =
    gsl_linalg_SV_decomp_mod (&u.matrix, &x.matrix, &v.matrix, &s.vector,
                              &work.vector);
  if (errval != GSL_SUCCESS)
    scm_c_gsl_error (errval, who, scm_list_1 (a));

  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_f64matrix_svd_jacobi (SCM a)
{
  const char *who = "scm_f64matrix_svd_jacobi";

  scm_t_array_handle handle_a;

  scm_array_get_handle (a, &handle_a);
  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (&handle_a).matrix;

  double u_buf[ma.size1 * ma.size2];
  double v_buf[ma.size2 * ma.size2];
  double s_buf[ma.size2];
  gsl_matrix_view u = gsl_matrix_view_array (u_buf, ma.size1, ma.size2);
  gsl_matrix_view v = gsl_matrix_view_array (v_buf, ma.size2, ma.size2);
  gsl_vector_view s = gsl_vector_view_array (s_buf, ma.size2);

  gsl_matrix_memcpy (&u.matrix, &ma);
  scm_array_handle_release (&handle_a);

  int errval = gsl_linalg_SV_decomp_jacobi (&u.matrix, &v.matrix, &s.vector);
  if (errval != GSL_SUCCESS)
    scm_c_gsl_error (errval, who, scm_list_1 (a));

  SCM values[3] = {
    scm_gsl_matrix_to_f64matrix (&u.matrix, 1),
    scm_gsl_vector_to_f64vector (&s.vector, 1),
    scm_gsl_matrix_to_f64matrix (&v.matrix, 1)
  };
  return scm_c_values (values, 3);
}

VISIBLE SCM
scm_f64matrix_svd_solve_transposed (SCM U, SCM S, SCM V, SCM b_transpose)
{
  const char *who = "scm_f64matrix_svd_solve_transposed";

  SCM b_transpose__ =
    scm_call_1 (scm_c_public_ref ("sortsmill matrices", "row-matrix->vector"),
                b_transpose);

  scm_t_array_handle handle_U;
  scm_t_array_handle handle_V;
  scm_t_array_handle handle_S;
  scm_t_array_handle handle_b;

  scm_array_get_handle (U, &handle_U);
  gsl_matrix mU = scm_gsl_matrix_const_view_array_handle (&handle_U).matrix;

  scm_array_get_handle (V, &handle_V);
  gsl_matrix mV = scm_gsl_matrix_const_view_array_handle (&handle_V).matrix;

  scm_array_get_handle (S, &handle_S);
  gsl_vector vS = scm_gsl_vector_const_view_array_handle (&handle_S).vector;

  scm_array_get_handle (b_transpose__, &handle_b);
  gsl_vector vb = scm_gsl_vector_const_view_array_handle (&handle_b).vector;

  double x_transpose_buf[vb.size];
  gsl_vector_view x_transpose = gsl_vector_view_array (x_transpose_buf, vb.size);

  int errval =
    gsl_linalg_SV_solve (&mU, &mV, &vS, &vb, &x_transpose.vector);
  if (errval != GSL_SUCCESS)
    scm_c_gsl_error (errval, who, scm_list_4 (U, S, V, b_transpose));

  scm_array_handle_release (&handle_U);
  scm_array_handle_release (&handle_V);
  scm_array_handle_release (&handle_S);
  scm_array_handle_release (&handle_b);

  return scm_gsl_vector_to_f64vector (&x_transpose.vector, 1);
}

VISIBLE void
init_guile_sortsmill_matrices (void)
{
  scm_c_define_gsubr ("f64matrix-f64matrix*", 2, 0, 0,
                      scm_f64matrix_f64matrix_mult);
  scm_c_define_gsubr ("f64matrix-f64matrix+", 2, 0, 0,
                      scm_f64matrix_f64matrix_add);
  scm_c_define_gsubr ("f64matrix-f64matrix-", 2, 0, 0,
                      scm_f64matrix_f64matrix_sub);
  scm_c_define_gsubr ("f64matrix-svd-golub-reinsch", 1, 0, 0,
                      scm_f64matrix_svd_golub_reinsch);
  scm_c_define_gsubr ("f64matrix-svd-modified-golub-reinsch", 1, 0, 0,
                      scm_f64matrix_svd_modified_golub_reinsch);
  scm_c_define_gsubr ("f64matrix-svd-jacobi", 1, 0, 0,
                      scm_f64matrix_svd_jacobi);
  scm_c_define_gsubr ("f64matrix-svd-solve-transposed", 4, 0, 0,
                      scm_f64matrix_svd_solve_transposed);
}
