#include <config.h>

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
#include <sortsmill/xdie_on_null.h>
#include <intl.h>

void init_guile_sortsmill_math_gsl_permutations (void);

VISIBLE void
scm_gsl_permutation_unwind_handler (void *p)
{
  gsl_permutation_free ((gsl_permutation *) p);
}

VISIBLE void
scm_dynwind_gsl_permutation_free (gsl_permutation *p)
{
  scm_dynwind_unwind_handler (scm_gsl_permutation_unwind_handler, p,
                              SCM_F_WIND_EXPLICITLY);
}

VISIBLE SCM
scm_from_gsl_permutation (const gsl_permutation *p)
{
  const char *who = "scm_from_gsl_permutation";

  size_t n = gsl_permutation_size (p);

  // If any Sorts Mill Tools user needs a permutation bigger than
  //
  //         2³¹ − 1
  //
  // please let us know.
  //
  // This size was chosen because signed 32-bit integers are widely
  // supported in different programming languages on common
  // architectures.
  if (2147483647 < n)
    {
      const char *message =
        _("maximum permutation size of 2147483647 exceeded");
      rnrs_raise_condition
        (scm_list_3
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_make_message_condition (scm_from_locale_string (message))));
    }

  size_t *data = gsl_permutation_data (p);

  int32_t *v = scm_malloc (n * sizeof (int32_t));
  for (size_t i = 0; i < n; i++)
    v[i] = data[i];
  return scm_take_s32vector (v, n);
}

VISIBLE gsl_permutation *
scm_to_gsl_permutation (SCM vec)
{
  scm_t_array_handle handle;

  size_t n = scm_to_size_t (scm_s32vector_length (vec));

  gsl_permutation *p = xdie_on_null (gsl_permutation_alloc (n));
  size_t *data = gsl_permutation_data (p);

  scm_array_get_handle (vec, &handle);
  const int32_t *v = scm_array_handle_s32_elements (&handle);
  for (size_t i = 0; i < n; i++)
    data[i] = v[i];
  scm_array_handle_release (&handle);

  return p;
}

VISIBLE void
init_guile_sortsmill_math_gsl_permutations (void)
{
  // Nothing here yet.
}
