/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_MATH_F64_MATRIX_H
#define _SORTSMILL_MATH_F64_MATRIX_H

#include <libguile.h>
#include <sortsmill/c_version.h>
#include <sortsmill/math/transmatrix.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#if _FF_C99_OR_GREATER

/* Multiply by a diagonal matrix that is represented by a vector. */
void f64_matrix_mul_diagonal (CBLAS_SIDE_t Side,
                              size_t m, size_t n,
                              double A[m][n],
                              double x[(Side == CblasLeft) ? m : n]);

#endif /* _FF_C99_OR_GREATER */

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_F64_MATRIX_H */
