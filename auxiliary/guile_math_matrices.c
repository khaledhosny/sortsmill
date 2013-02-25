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
  scm_c_define_gsubr ("private:f64matrix-svd-solve-vector", 5, 0, 0,
                      scm_f64matrix_svd_solve_vector);
}
