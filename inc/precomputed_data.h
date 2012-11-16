/*
 * Copyright (C) 2012 by Barry Schwartz
  
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

#ifndef _PRECOMPUTED_DATA_H
#define _PRECOMPUTED_DATA_H

#include <config.h>

#include <stdlib.h>

struct precomputed_data_by_degree
{
  int max_degree;
  const double **data;
};

typedef struct precomputed_data_by_degree precomputed_data_by_degree_t;

#define PRECOMPUTED_DATA_BY_DEGREE_INITIALIZER { -1, NULL }

extern precomputed_data_by_degree_t precomputed_binomial_coefficients;
extern precomputed_data_by_degree_t precomputed_altsigns;
extern precomputed_data_by_degree_t
  precomputed_binomial_coefficients_altsigns;
extern precomputed_data_by_degree_t precomputed_sbern_basis_in_mono;
extern precomputed_data_by_degree_t precomputed_mono_basis_in_sbern;

// These functions are not thread-safe.
void precompute_data_up_to_degree (const char *module_name,
                                   const char *function_name,
                                   int degree,
                                   precomputed_data_by_degree_t *pdata);
inline const double *get_precomputed_data_by_degree (const char *module_name,
                                                     const char
                                                     *function_name,
                                                     int degree,
                                                     precomputed_data_by_degree_t
                                                     *pdata);
inline const double *get_binomial_coefficients (int degree);
inline const double *get_altsigns (int degree);
inline const double *get_binomial_coefficients_altsigns (int degree);
inline const double *get_sbern_basis_in_mono (int degree);
inline const double *get_mono_basis_in_sbern (int degree);

inline const double *
get_precomputed_data_by_degree (const char *module_name,
                                const char *function_name,
                                int degree,
                                precomputed_data_by_degree_t *pdata)
{
  if (pdata->max_degree < (int) degree)
    precompute_data_up_to_degree (module_name, function_name, degree, pdata);
  return pdata->data[degree];
}

inline const double *
get_binomial_coefficients (int degree)
{
  return get_precomputed_data_by_degree ("sortsmillff precompute",
                                         "binomial-coefficients-f64vector",
                                         degree,
                                         &precomputed_binomial_coefficients);
}

inline const double *
get_altsigns (int degree)
{
  return get_precomputed_data_by_degree ("sortsmillff precompute",
                                         "altsigns-f64vector",
                                         degree, &precomputed_altsigns);
}

inline const double *
get_binomial_coefficients_altsigns (int degree)
{
  return get_precomputed_data_by_degree ("sortsmillff precompute",
                                         "binomial-coefficients-altsigns-f64vector",
                                         degree,
                                         &precomputed_binomial_coefficients_altsigns);
}

inline const double *
get_sbern_basis_in_mono (int degree)
{
  return get_precomputed_data_by_degree ("sortsmillff precompute",
                                         "sbern-basis-in-mono-f64vector",
                                         degree,
                                         &precomputed_sbern_basis_in_mono);
}

inline const double *
get_mono_basis_in_sbern (int degree)
{
  return get_precomputed_data_by_degree ("sortsmillff precompute",
                                         "mono-basis-in-sbern-f64vector",
                                         degree,
                                         &precomputed_mono_basis_in_sbern);
}

#endif // _PRECOMPUTED_DATA_H
