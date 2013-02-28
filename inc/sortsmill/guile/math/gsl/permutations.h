/*
 * Copyright (C) 2013 Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_MATH_GSL_PERMUTATIONS_H
#define _SORTSMILL_GUILE_MATH_GSL_PERMUTATIONS_H

#include <libguile.h>
#include <gsl/gsl_permutation.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_gsl_permutation_unwind_handler (void *p);
void scm_dynwind_gsl_permutation_free (gsl_permutation *p);

SCM scm_from_gsl_permutation (const gsl_permutation *p);
gsl_permutation *scm_to_gsl_permutation (SCM vec);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_GSL_PERMUTATIONS_H */
