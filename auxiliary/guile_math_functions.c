#include <config.h>             // -*- coding: utf-8 -*-

// Copyright (C) 2013 Barry Schwartz
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

#include <sortsmill/guile.h>
#include <sortsmill/math/bincoef.h>
#include <stdint.h>
#include <intl.h>

void init_guile_sortsmill_math_functions (void);

// Binary coefficients C(n,k).
VISIBLE SCM
scm_c_bincoef (uintmax_t n, uintmax_t k)
{
  SCM coef;

  if (n <= 34)
    // C(34,17) = 2333606220, which can be represented as a 32-bit
    // unsigned integer. Therefore we are safe using machine integers.
    coef = scm_from_uintmax (bincoef (n, k));
  else
    {
      mpz_t C;
      mpz_init (C);
      mpz_bincoef_ui (C, n, k);
      coef = scm_from_mpz (C);
      mpz_clear (C);
    }
  return coef;
}

// Binary coefficients C(n,k).
VISIBLE SCM
scm_bincoef (SCM n, SCM k)
{
  SCM coef;

  if (scm_is_unsigned_integer (n, 0, UINTMAX_MAX) &&
      scm_is_unsigned_integer (k, 0, UINTMAX_MAX))
    {
      coef = scm_c_bincoef (scm_to_uintmax (n), scm_to_uintmax (k));
    }
  else
    {
      coef = SCM_UNSPECIFIED;
      SCM message =
        scm_c_locale_sformat
        (_("scm_bincoef(n,k) is not implemented "
           "for n or k negative or greater than ~a"),
         scm_list_1 (scm_from_uintmax (UINTMAX_MAX)));
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_error (),
          rnrs_c_make_who_condition ("scm_bincoef"),
          rnrs_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_2 (n, k))));
    }
  return coef;
}

VISIBLE void
init_guile_sortsmill_math_functions (void)
{
  scm_c_define_gsubr ("bincoef", 2, 0, 0, scm_bincoef);
}
