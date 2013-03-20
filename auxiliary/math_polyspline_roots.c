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

//-------------------------------------------------------------------------

static inline int
sign_f64 (double x)
{
  int sign;
  if (x < 0)
    sign = -1;
  else if (x == 0)
    sign = 0;
  else
    sign = 1;
  return sign;
}

static inline int
sign_scm (SCM x)
{
  int sign;
  if (scm_is_true (scm_negative_p (x)))
    sign = -1;
  else if (scm_is_true (scm_zero_p (x)))
    sign = 0;
  else
    sign = 1;
  return sign;
}

VISIBLE size_t
sign_variations_f64 (size_t degree, ssize_t stride, const double *spline)
{
  size_t count = 0;
  int old_sign = sign_f64 (spline[0]);
  for (size_t i = 1; i <= degree; i++)
    {
      const int new_sign = sign_f64 (spline[stride * (ssize_t) i]);
      if (old_sign * new_sign < 0)
        {
          old_sign = new_sign;
          count++;
        }
      else if (old_sign == 0)
        old_sign = new_sign;
    }
  return count;
}

VISIBLE size_t
sign_variations_scm (size_t degree, ssize_t stride, const SCM *spline)
{
  size_t count = 0;
  int old_sign = sign_scm (spline[0]);
  for (size_t i = 1; i <= degree; i++)
    {
      const int new_sign = sign_scm (spline[stride * (ssize_t) i]);
      if (old_sign * new_sign < 0)
        {
          old_sign = new_sign;
          count++;
        }
      else if (old_sign == 0)
        old_sign = new_sign;
    }
  return count;
}

VISIBLE SCM
scm_sign_variations_f64 (SCM spline)
{
  const char *who = "scm_sign_variations_f64";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const double *_spline = scm_array_handle_f64_elements (&handle);

  size_t count = sign_variations_f64 (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

VISIBLE SCM
scm_sign_variations_scm (SCM spline)
{
  const char *who = "scm_sign_variations_scm";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  size_t count = sign_variations_scm (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

//-------------------------------------------------------------------------

VISIBLE size_t
budan_0_1_scm_mono (size_t degree, ssize_t stride, const SCM *spline)
{
  // Budan’s 0_1 roots test. See
  // http://en.wikipedia.org/wiki/Budan%27s_theorem#Early_applications_of_Budan.27s_theorem
  //
  // Here we use the binomial expansion
  //
  //    (1 + x)ⁿ = ∑ᵢC(n,i)xⁱ for i = 0,1,...,n
  //
  // to simplify and speed things up.

  size_t count = 0;

  if (degree != 0)
    {
      scm_dynwind_begin (0);

      mpz_t C;
      mpz_init (C);
      scm_dynwind_mpz_clear (C);

      SCM p[degree + 1];

      p[0] = *spline;
      for (size_t i = 1; i < degree; i++)
        {
          mpz_bincoef_ui (C, degree, i);
          p[i] = scm_product (scm_from_mpz (C), *spline);
        }
      p[degree] = *spline;

      for (size_t k = 1; k < degree; k++)
        {
          spline += stride;
          p[0] = scm_sum (p[0], *spline);
          for (size_t i = 1; i < degree - k; i++)
            {
              mpz_bincoef_ui (C, degree - k, i);
              p[i] = scm_sum (p[i], scm_product (scm_from_mpz (C), *spline));
            }
          p[degree - k] = scm_sum (p[degree - k], *spline);
        }

      scm_dynwind_end ();

      spline += stride;
      p[0] = scm_sum (p[0], *spline);

      count = sign_variations_scm (degree, 1, p);
    }

  return count;
}

VISIBLE SCM
scm_budan_0_1_scm_mono (SCM spline)
{
  const char *who = "scm_budan_0_1_scm_mono";

  scm_t_array_handle handle;

  scm_dynwind_begin (0);

  scm_array_get_handle (spline, &handle);
  scm_dynwind_array_handle_release (&handle);
  assert_c_rank_1_or_2_array (who, spline, &handle);

  size_t dim;
  ssize_t stride;
  scm_array_handle_get_vector_dim_and_stride (who, spline, &handle,
                                              &dim, &stride);
  const SCM *_spline = scm_array_handle_elements (&handle);

  size_t count = budan_0_1_scm_mono (dim - 1, stride, _spline);

  scm_dynwind_end ();

  return scm_from_size_t (count);
}

//-------------------------------------------------------------------------

void init_math_polyspline_roots (void);

VISIBLE void
init_math_polyspline_roots (void)
{
  scm_c_define_gsubr ("poly:sign-variations-f64", 1, 0, 0,
                      scm_sign_variations_f64);
  scm_c_define_gsubr ("poly:sign-variations-scm", 1, 0, 0,
                      scm_sign_variations_scm);

  scm_c_define_gsubr ("poly:budan-0_1-scm", 1, 0, 0, scm_budan_0_1_scm_mono);
}

//-------------------------------------------------------------------------
