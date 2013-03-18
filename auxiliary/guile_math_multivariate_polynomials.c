#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 by Barry Schwartz
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

#include <sortsmill/math.h>
#include <sortsmill/guile.h>
#include <intl.h>

//
// Multivariate polynomials represented inefficiently as
// multidimensional arrays with nearly half the entries unused. Each
// array dimension corresponds to the powers of a variable.
//

//-------------------------------------------------------------------------

static void
assert_array_is_hypercubic (const char *who, SCM array,
                            scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);

  for (size_t i = 1; i < rank; i++)
    if (dims[i - 1].ubnd - dims[i - 1].lbnd != dims[i].ubnd - dims[i].lbnd)
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (_("array is not hypercubic")),
          rnrs_make_irritants_condition (scm_list_1 (array))));
}

static void
assert_array_is_zero_based (const char *who, SCM array,
                            scm_t_array_handle *handlep)
{
  const size_t rank = scm_array_handle_rank (handlep);
  const scm_t_array_dim *dims = scm_array_handle_dims (handlep);

  for (size_t i = 0; i < rank; i++)
    if (dims[i].lbnd != 0)
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (_("expected all lower bounds = 0")),
          rnrs_make_irritants_condition (scm_list_1 (array))));
}

static void
assert_array_ranks_match (const char *who,
                          SCM array1, scm_t_array_handle *handlep1,
                          SCM array2, scm_t_array_handle *handlep2)
{
  const size_t rank1 = scm_array_handle_rank (handlep1);
  const size_t rank2 = scm_array_handle_rank (handlep2);

  if (rank1 != rank2)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("mismatched array ranks")),
        rnrs_make_irritants_condition (scm_list_2 (array1, array2))));
}

static void
add_bivariate_polynomials (scm_t_array_handle *handlep1,
                           scm_t_array_handle *handlep2,
                           scm_t_array_handle *handlep3)
{
  const scm_t_array_dim *dims1 = scm_array_handle_dims (handlep1);
  const SCM *elems1 = scm_array_handle_elements (handlep1);

  const scm_t_array_dim *dims2 = scm_array_handle_dims (handlep2);
  const SCM *elems2 = scm_array_handle_elements (handlep2);

  const scm_t_array_dim *dims3 = scm_array_handle_dims (handlep3);
  SCM *elems3 = scm_array_handle_writable_elements (handlep3);

  for (ssize_t i = 0; i <= dims1[0].ubnd; i++)
    for (ssize_t j = 0; j <= dims1[0].ubnd - i; j++)
      elems3[i * dims3[0].inc + j * dims3[1].inc] =
        scm_sum (elems1[i * dims1[0].inc + j * dims1[1].inc],
                 elems2[i * dims2[0].inc + j * dims2[1].inc]);
}

static void
multiply_bivariate_polynomials (scm_t_array_handle *handlep1,
                                scm_t_array_handle *handlep2,
                                scm_t_array_handle *handlep3)
{
  const scm_t_array_dim *dims1 = scm_array_handle_dims (handlep1);
  const SCM *elems1 = scm_array_handle_elements (handlep1);

  const scm_t_array_dim *dims2 = scm_array_handle_dims (handlep2);
  const SCM *elems2 = scm_array_handle_elements (handlep2);

  const scm_t_array_dim *dims3 = scm_array_handle_dims (handlep3);
  SCM *elems3 = scm_array_handle_writable_elements (handlep3);

  for (ssize_t i = 0; i <= dims1[0].ubnd; i++)
    for (ssize_t j = 0; j <= dims1[0].ubnd - i; j++)
      for (ssize_t s = 0; s <= dims2[0].ubnd; s++)
        for (ssize_t t = 0; t <= dims2[0].ubnd - s; t++)
          elems3[(i + s) * dims3[0].inc + (j + t) * dims3[1].inc] =
            scm_sum (elems3[(i + s) * dims3[0].inc + (j + t) * dims3[1].inc],
                     scm_product (elems1[i * dims1[0].inc + j * dims1[1].inc],
                                  elems2[s * dims2[0].inc + t * dims2[1].inc]));
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_sum_of_multivariate_polynomials (SCM p, SCM q)
{
  const char *who = "scm_sum_of_multivariate_polynomials";

  scm_t_array_handle handle_p;
  scm_t_array_handle handle_q;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (p, &handle_p);
  scm_dynwind_array_handle_release (&handle_p);

  scm_array_get_handle (q, &handle_q);
  scm_dynwind_array_handle_release (&handle_q);

  assert_array_is_zero_based (who, p, &handle_p);
  assert_array_is_hypercubic (who, p, &handle_p);

  assert_array_is_zero_based (who, q, &handle_q);
  assert_array_is_hypercubic (who, q, &handle_q);

  assert_array_ranks_match (who, p, &handle_p, q, &handle_q);

  const size_t rank = scm_array_handle_rank (&handle_p);

  SCM result = scm_make_array (scm_from_int (0), scm_array_dimensions (p));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);

  switch (rank)
    {
    case 2:
      add_bivariate_polynomials (&handle_p, &handle_q, &handle);
      break;

    default:
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (_("not implemented for this rank")),
          rnrs_make_irritants_condition (scm_list_2 (p, q))));
    }

  scm_dynwind_end ();

  return result;
}

VISIBLE SCM
scm_product_of_multivariate_polynomials (SCM p, SCM q)
{
  const char *who = "scm_product_of_multivariate_polynomials";

  scm_t_array_handle handle_p;
  scm_t_array_handle handle_q;
  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (p, &handle_p);
  scm_dynwind_array_handle_release (&handle_p);
  const scm_t_array_dim *dims_p = scm_array_handle_dims (&handle_p);

  scm_array_get_handle (q, &handle_q);
  scm_dynwind_array_handle_release (&handle_q);
  const scm_t_array_dim *dims_q = scm_array_handle_dims (&handle_q);

  assert_array_is_zero_based (who, p, &handle_p);
  assert_array_is_hypercubic (who, p, &handle_p);

  assert_array_is_zero_based (who, q, &handle_q);
  assert_array_is_hypercubic (who, q, &handle_q);

  assert_array_ranks_match (who, p, &handle_p, q, &handle_q);

  const size_t degree_p = dims_p[0].ubnd - dims_p[0].lbnd;
  const size_t degree_q = dims_q[0].ubnd - dims_q[0].lbnd;
  const size_t degree = degree_p + degree_q;

  const size_t rank = scm_array_handle_rank (&handle_p);

  SCM result = scm_make_array (scm_from_int (0),
                               scm_make_list (scm_from_size_t (rank),
                                              scm_from_size_t (degree + 1)));
  scm_array_get_handle (result, &handle);
  scm_dynwind_array_handle_release (&handle);

  switch (rank)
    {
    case 2:
      multiply_bivariate_polynomials (&handle_p, &handle_q, &handle);
      break;

    default:
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (_("not implemented for this rank")),
          rnrs_make_irritants_condition (scm_list_2 (p, q))));
    }

  scm_dynwind_end ();

  return result;
}

//-------------------------------------------------------------------------

void init_guile_math_multivariate_polynomials (void);

VISIBLE void
init_guile_math_multivariate_polynomials (void)
{
  scm_c_define_gsubr ("multipoly+", 2, 0, 0,
                      scm_sum_of_multivariate_polynomials);
  scm_c_define_gsubr ("multipoly*", 2, 0, 0,
                      scm_product_of_multivariate_polynomials);
}

//-------------------------------------------------------------------------
