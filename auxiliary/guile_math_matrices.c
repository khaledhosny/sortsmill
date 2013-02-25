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
#include <xalloc.h>
#include <sortsmill/guile/math/gsl.h>
#include <sortsmill/guile/math/matrices.h>
#include <sortsmill/guile/rnrs_conditions.h>
#include <sortsmill/guile/format.h>
#include <sortsmill/gmp_matrix.h>
#include <sortsmill/gmp_constants.h>
#include <intl.h>

//
// FIXME FIXME FIXME: All this complexity needs testing now and
// regression testing later. Testing, testing, testing.
//
// That is a price I pay for doing the `wrong' thing and writing all
// this C code. However I get obsessive about matrix operation speeds;
// moreover the process is educational.
//
// Be thankful I have not (yet) implemented special matrix operations
// for big integers, or for uniform types other than double.
//

void init_guile_sortsmill_math_matrices (void);

//static void
//scm_array_handle_nonuniform_to_scm_matrix (scm_t_array_handle *handlep,
//                                           size_t m, size_t n, SCM A[m][n])
//{
//  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);
//  const SCM *elems = scm_array_handle_elements (handlep);
//  for (size_t i = 0; i < m; i++)
//    for (size_t j = 0; j < n; j++)
//      A[i][j] = elems[i * dims[0].inc + j * dims[1].inc];
//}
//
//#define _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX(LONG_TYPE, MEDIUM_TYPE,    \
//                                             SHORT_TYPE)                \
//  void                                                                  \
//  scm_array_handle_##SHORT_TYPE##_to_scm_matrix                         \
//  (scm_t_array_handle *handlep, unsigned int m, unsigned int n,         \
//   SCM A[m][n])                                                         \
//  {                                                                     \
//    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
//    const LONG_TYPE *elems =                                            \
//      scm_array_handle_##SHORT_TYPE##_elements (handlep);               \
//    for (unsigned int i = 0; i < m; i++)                                \
//      for (unsigned int j = 0; j < n; j++)                              \
//        {                                                               \
//          LONG_TYPE x = elems[i * dims[0].inc + j * dims[1].inc];       \
//          A[i][j] = scm_from_##MEDIUM_TYPE (x);                         \
//        }                                                               \
//  }
//
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (uint8_t, uint8, u8);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (int8_t, int8, s8);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (uint16_t, uint16, u16);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (int16_t, int16, s16);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (uint32_t, uint32, u32);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (int32_t, int32, s32);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (uint64_t, uint64, u64);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (int64_t, int64, s64);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (float, double, f32);
//static _SCM_ARRAY_HANDLE_REAL_TO_SCM_MATRIX (double, double, f64);
//
//#define _SCM_ARRAY_HANDLE_CPX_TO_SCM_MATRIX(LONG_TYPE, SHORT_TYPE)      \
//  void                                                                  \
//  scm_array_handle_##SHORT_TYPE##_to_scm_matrix                         \
//  (scm_t_array_handle *handlep, unsigned int m, unsigned int n,         \
//   SCM A[m][n])                                                         \
//  {                                                                     \
//    const scm_t_array_dim *dims = scm_array_handle_dims (handlep);      \
//    const LONG_TYPE *elems =                                            \
//      scm_array_handle_##SHORT_TYPE##_elements (handlep);               \
//    for (unsigned int i = 0; i < m; i++)                                \
//      for (unsigned int j = 0; j < n; j++)                              \
//        {                                                               \
//          const LONG_TYPE *p =                                          \
//            &elems[2 * (i * dims[0].inc + j * dims[1].inc)];            \
//          A[i][j] = scm_c_make_rectangular (p[0], p[1]);                \
//        }                                                               \
//  }
//
//static _SCM_ARRAY_HANDLE_CPX_TO_SCM_MATRIX (float, c32);
//static _SCM_ARRAY_HANDLE_CPX_TO_SCM_MATRIX (double, c64);
//
//VISIBLE void
//scm_array_handle_to_scm_matrix (SCM A_scm, scm_t_array_handle *handlep,
//                                size_t m, size_t n, SCM A[m][n])
//{
//  const char *who = "scm_array_handle_to_scm_matrix";
//
//  if (scm_is_typed_array (A_scm, SCM_BOOL_T))
//    scm_array_handle_nonuniform_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("u8")))
//    scm_array_handle_u8_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("s8")))
//    scm_array_handle_s8_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("u16")))
//    scm_array_handle_u16_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("s16")))
//    scm_array_handle_s16_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("u32")))
//    scm_array_handle_u32_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("s32")))
//    scm_array_handle_s32_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("u64")))
//    scm_array_handle_u64_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("s64")))
//    scm_array_handle_s64_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("f32")))
//    scm_array_handle_f32_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("f64")))
//    scm_array_handle_f64_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("c32")))
//    scm_array_handle_c32_to_scm_matrix (handlep, m, n, A);
//  else if (scm_is_typed_array (A_scm, scm_from_latin1_symbol ("c64")))
//    scm_array_handle_c64_to_scm_matrix (handlep, m, n, A);
//  else
//    exception__unexpected_array_type (who, A_scm);
//}

//VISIBLE SCM
//scm_from_scm_matrix (size_t m, size_t n, SCM A[m][n])
//{
//  scm_t_array_handle handle;
//
//  SCM bounds = scm_list_2 (scm_list_2 (scm_from_uint (1), scm_from_uint (m)),
//                           scm_list_2 (scm_from_uint (1), scm_from_uint (n)));
//  SCM result = scm_make_array (SCM_UNSPECIFIED, bounds);
//
//  scm_dynwind_begin (0);
//
//  scm_array_get_handle (result, &handle);
//  scm_dynwind_array_handle_release (&handle);
//
//  const scm_t_array_dim *dims = scm_array_handle_dims (&handle);
//  SCM *elems = scm_array_handle_writable_elements (&handle);
//  for (size_t i = 0; i < m; i++)
//    for (size_t j = 0; j < n; j++)
//      elems[i * dims[0].inc + j * dims[1].inc] = A[i][j];
//
//  scm_dynwind_end ();
//
//  return result;
//}

//VISIBLE SCM
//scm_f64matrix_f64matrix_add (SCM a, SCM b)
//{
//  scm_t_array_handle handle_a;
//  scm_t_array_handle handle_b;
//
//  const char *who = "f64matrix-f64matrix+";
//
//  scm_dynwind_begin (0);
//
//  scm_array_get_handle (a, &handle_a);
//  scm_dynwind_array_handle_release (&handle_a);
//
//  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (a, &handle_a).matrix;
//
//  scm_array_get_handle (b, &handle_b);
//  scm_dynwind_array_handle_release (&handle_b);
//
//  gsl_matrix mb = scm_gsl_matrix_const_view_array_handle (b, &handle_b).matrix;
//
//  if (ma.size1 != mb.size1 || ma.size2 != mb.size2)
//    {
//      const char *localized_message =
//        _("non-conformable matrices: ~ax~a plus ~ax~a");
//      SCM message = scm_sformat (scm_from_locale_string (localized_message),
//                                 scm_list_4 (scm_from_int (ma.size1),
//                                             scm_from_int (ma.size2),
//                                             scm_from_int (mb.size1),
//                                             scm_from_int (mb.size2)));
//      rnrs_raise_condition
//        (scm_list_4
//         (rnrs_make_assertion_violation (),
//          rnrs_c_make_who_condition (who),
//          rnrs_make_message_condition (message),
//          rnrs_make_irritants_condition (scm_list_2 (a, b))));
//    }
//
//  double buffer[ma.size1 * ma.size2];
//  gsl_matrix_view vc = gsl_matrix_view_array (buffer, ma.size1, mb.size2);
//  gsl_matrix_memcpy (&vc.matrix, &ma);
//
//  gsl_matrix_add (&vc.matrix, &mb);
//
//  SCM result = scm_gsl_matrix_to_f64matrix (&vc.matrix, 1);
//
//  scm_dynwind_end ();
//
//  return result;
//}

VISIBLE SCM
scm_f64matrix_f64matrix_sub (SCM a, SCM b)
{
  scm_t_array_handle handle_a;
  scm_t_array_handle handle_b;

  const char *who = "f64matrix-f64matrix-";

  scm_dynwind_begin (0);

  scm_array_get_handle (a, &handle_a);
  scm_dynwind_array_handle_release (&handle_a);

  gsl_matrix ma = scm_gsl_matrix_const_view_array_handle (a, &handle_a).matrix;

  scm_array_get_handle (b, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);

  gsl_matrix mb = scm_gsl_matrix_const_view_array_handle (b, &handle_b).matrix;

  if (ma.size1 != mb.size1 || ma.size2 != mb.size2)
    {
      const char *localized_message =
        _("non-conformable matrices: ~ax~a minus ~ax~a");
      SCM message = scm_sformat (scm_from_locale_string (localized_message),
                                 scm_list_4 (scm_from_int (ma.size1),
                                             scm_from_int (ma.size2),
                                             scm_from_int (mb.size1),
                                             scm_from_int (mb.size2)));
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_2 (a, b))));
    }

  double buffer[ma.size1 * ma.size2];
  gsl_matrix_view vc = gsl_matrix_view_array (buffer, ma.size1, mb.size2);
  gsl_matrix_memcpy (&vc.matrix, &ma);

  gsl_matrix_sub (&vc.matrix, &mb);

  SCM result = scm_gsl_matrix_to_f64matrix (&vc.matrix, 1);

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_f64matrix_svd_solve_vector (SCM U, SCM S, SCM V,
                                SCM x_transpose, SCM b_transpose)
{
  scm_t_array_handle handle_U;
  scm_t_array_handle handle_V;
  scm_t_array_handle handle_S;
  scm_t_array_handle handle_x;
  scm_t_array_handle handle_b;

  const char *who = "scm_f64matrix_svd_solve_transposed_internal";

  scm_dynwind_begin (0);

  scm_array_get_handle (U, &handle_U);
  scm_dynwind_array_handle_release (&handle_U);
  gsl_matrix mU = scm_gsl_matrix_const_view_array_handle (U, &handle_U).matrix;

  scm_array_get_handle (V, &handle_V);
  scm_dynwind_array_handle_release (&handle_V);
  gsl_matrix mV = scm_gsl_matrix_const_view_array_handle (V, &handle_V).matrix;

  scm_array_get_handle (S, &handle_S);
  scm_dynwind_array_handle_release (&handle_S);
  gsl_vector vS = scm_gsl_vector_const_view_array_handle (S, &handle_S).vector;

  scm_array_get_handle (x_transpose, &handle_x);
  scm_dynwind_array_handle_release (&handle_x);
  gsl_vector_view vx =
    scm_gsl_vector_view_array_handle (x_transpose, &handle_x);

  scm_array_get_handle (b_transpose, &handle_b);
  scm_dynwind_array_handle_release (&handle_b);
  gsl_vector vb =
    scm_gsl_vector_const_view_array_handle (b_transpose, &handle_b).vector;

  int errval = gsl_linalg_SV_solve (&mU, &mV, &vS, &vb, &vx.vector);
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

VISIBLE void
init_guile_sortsmill_math_matrices (void)
{
  //  scm_c_define_gsubr ("f64matrix-f64matrix+", 2, 0, 0,
  //                      scm_f64matrix_f64matrix_add);
  scm_c_define_gsubr ("f64matrix-f64matrix-", 2, 0, 0,
                      scm_f64matrix_f64matrix_sub);
  scm_c_define_gsubr ("private:f64matrix-svd-solve-vector", 5, 0, 0,
                      scm_f64matrix_svd_solve_vector);
}
