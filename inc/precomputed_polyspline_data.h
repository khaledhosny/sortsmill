/*
 * Copyright (C) 2012 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _INTERNAL_PRECOMPUTED_POLYSPLINE_H
#define _INTERNAL_PRECOMPUTED_POLYSPLINE_H

#include <config.h>

#include <gmp.h>

unsigned int polyspline_precomputed_degree_max (void);

const double *fl_precomputed_binomial_coefficients (unsigned int degree);
const double *fl_precomputed_binomial_coefficients_altsigns (unsigned int degree);
const double *fl_precomputed_sbern_basis_in_mono (unsigned int degree);
const double *fl_precomputed_mono_basis_in_sbern (unsigned int degree);
const double *fl_precomputed_sbern_basis_in_spower (unsigned int degree);
const double *fl_precomputed_spower_basis_in_sbern (unsigned int degree);

const __mpz_struct *mpz_precomputed_binomial_coefficients (unsigned int degree);
const __mpz_struct *mpz_precomputed_binomial_coefficients_altsigns (unsigned int degree);
const __mpz_struct *mpz_precomputed_sbern_basis_in_mono (unsigned int degree);
const __mpz_struct *mpz_precomputed_mono_basis_in_sbern (unsigned int degree);
const __mpz_struct *mpz_precomputed_sbern_basis_in_spower (unsigned int degree);
const __mpz_struct *mpz_precomputed_spower_basis_in_sbern (unsigned int degree);

const __mpq_struct *mpq_precomputed_binomial_coefficients (unsigned int degree);
const __mpq_struct *mpq_precomputed_binomial_coefficients_altsigns (unsigned int degree);
const __mpq_struct *mpq_precomputed_sbern_basis_in_mono (unsigned int degree);
const __mpq_struct *mpq_precomputed_mono_basis_in_sbern (unsigned int degree);
const __mpq_struct *mpq_precomputed_sbern_basis_in_spower (unsigned int degree);
const __mpq_struct *mpq_precomputed_spower_basis_in_sbern (unsigned int degree);

#endif // _INTERNAL_PRECOMPUTED_POLYSPLINE_H
