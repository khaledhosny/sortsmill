#include <config.h>

/*
 * Copyright (C) 2012 by Barry Schwartz
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <precomputed_data.h>
#include <libguile.h>
#include <xgc.h>
#include <stdlib.h>
#include <assert.h>

// Generate a non-inline version of these functions.
VISIBLE const double *get_precomputed_data_by_degree (const char *module_name,
                                                      const char
                                                      *function_name,
                                                      int degree,
                                                      precomputed_data_by_degree_t
                                                      *pdata);
VISIBLE const double *get_binomial_coefficients (int degree);
VISIBLE const double *get_altsigns (int degree);
VISIBLE const double *get_binomial_coefficients_altsigns (int degree);
VISIBLE const double *get_sbern_basis_in_mono (int degree);
VISIBLE const double *get_mono_basis_in_sbern (int degree);

VISIBLE precomputed_data_by_degree_t precomputed_binomial_coefficients =
  PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER;
VISIBLE precomputed_data_by_degree_t precomputed_altsigns =
  PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER;
VISIBLE precomputed_data_by_degree_t
  precomputed_binomial_coefficients_altsigns =
  PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER;
VISIBLE precomputed_data_by_degree_t precomputed_sbern_basis_in_mono =
  PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER;
VISIBLE precomputed_data_by_degree_t precomputed_mono_basis_in_sbern =
  PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER;

static const double *
precompute_data_for_degree (const char *module_name,
                            const char *function_name, int degree)
{
  scm_t_array_handle handle;
  size_t len;
  ssize_t inc;

  SCM func = scm_c_public_ref (module_name, function_name);
  SCM deg = scm_from_int (degree);
  SCM vector = scm_call_1 (func, deg);
  assert (scm_is_true (scm_f64vector_p (vector)));
  const double *elem = scm_f64vector_elements (vector, &handle, &len, &inc);
  double *data = (double *) x_gc_malloc (len * sizeof (double));
  for (size_t i = 0; i < len; i++)
    data[i] = elem[inc * i];
  scm_array_handle_release (&handle);
  return data;
}

// WARNING: this code is not thread-safe.
VISIBLE void
precompute_data_up_to_degree (const char *module_name,
                              const char *function_name, int degree,
                              precomputed_data_by_degree_t *pdata)
{
  if (pdata->max_degree < degree)
    {
      pdata->data =
        (const double **)
        x_gc_realloc (pdata->data, (degree + 1) * sizeof (const double *));
      for (size_t deg = pdata->max_degree + 1; deg <= degree; deg++)
        pdata->data[deg] =
          precompute_data_for_degree (module_name, function_name, deg);
      pdata->max_degree = degree;
    }
}
