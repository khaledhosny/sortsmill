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
#include <sortsmillff/guile/gsl.h>
#include <libguile.h>
#include <gsl/gsl_blas.h>

void init_guile_sortsmillff_gsl (void);

/*
  int  gsl_blas_dgemm (CBLAS_TRANSPOSE_t TransA,
                     CBLAS_TRANSPOSE_t TransB,
                     double alpha,
                     const gsl_matrix * A,
                     const gsl_matrix * B,
                     double beta,
                     gsl_matrix * C);
*/

VISIBLE gsl_matrix_const_view
scm_gsl_matrix_const_view_array_handle (scm_t_array_handle * handlep)
{
  const double *my_elems;
  ssize_t tda;

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
scm_gsl_matrix_view_array_handle (scm_t_array_handle * handlep)
{
  double *my_elems;
  ssize_t tda;

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
scm_gsl_matrix_to_array_f64 (const gsl_matrix * m, int low_index)
{
  assert (0 < m->size1);
  assert (0 < m->size2);

  if (m->size1 < 1)
    scm_misc_error ("scm_gsl_matrix_to_array_f64", "gsl_matrix size1 is zero",
                    SCM_BOOL_F);
  if (m->size2 < 1)
    scm_misc_error ("scm_gsl_matrix_to_array_f64", "gsl_matrix size2 is zero",
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
scm_array_f64_matrix_mult (SCM a, SCM b)
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
      scm_misc_error ("matrix-f64*",
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

  gsl_blas_dgemm (CblasNoTrans, CblasNoTrans, 1.0, &ma, &mb, 0.0, &vc.matrix);
  SCM result = scm_gsl_matrix_to_array_f64 (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE SCM
scm_array_f64_matrix_add (SCM a, SCM b)
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
      scm_misc_error ("matrix-f64+",
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
  SCM result = scm_gsl_matrix_to_array_f64 (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE SCM
scm_array_f64_matrix_sub (SCM a, SCM b)
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
      scm_misc_error ("matrix-f64-",
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
  SCM result = scm_gsl_matrix_to_array_f64 (&vc.matrix, 1);

  scm_array_handle_release (&handle_b);
  scm_array_handle_release (&handle_a);

  return result;
}

VISIBLE void
init_guile_sortsmillff_gsl (void)
{
  scm_c_define_gsubr ("matrix-f64*", 2, 0, 0, scm_array_f64_matrix_mult);
  scm_c_define_gsubr ("matrix-f64+", 2, 0, 0, scm_array_f64_matrix_add);
  scm_c_define_gsubr ("matrix-f64-", 2, 0, 0, scm_array_f64_matrix_sub);
}
