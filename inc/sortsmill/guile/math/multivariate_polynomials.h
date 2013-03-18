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

#ifndef _SORTSMILL_GUILE_MATH_MULTIVARIATE_POLYNOMIALS_H
#define _SORTSMILL_GUILE_MATH_MULTIVARIATE_POLYNOMIALS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*
 * Multivariate polynomials represented inefficiently as
 * multidimensional arrays with nearly half the entries unused. Each
 * array dimension corresponds to the powers of a variable.
 */

SCM scm_sum_of_multivariate_polynomials (SCM p, SCM q);
SCM scm_difference_of_multivariate_polynomials (SCM p, SCM q);
SCM scm_product_of_multivariate_polynomials (SCM p, SCM q);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_MULTIVARIATE_POLYNOMIALS_H */
