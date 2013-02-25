/*
 * Copyright (C) 2012, 2013 Barry Schwartz
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

#ifndef _SORTSMILL_TRANSMATRIX_H
#define _SORTSMILL_TRANSMATRIX_H

#include <gsl/gsl_blas.h>

#define _FF_TRANSMATRIX(A,T,I,J)                                        \
  A[((T) == CblasNoTrans ? (I) : (J))][((T) == CblasNoTrans ? (J) : (I))]

#define _FF_TRANSMATRIX_CAST(TYPE,T,I,J)                        \
  (TYPE (*)[(unsigned int) ((T) == CblasNoTrans ? (J) : (I))])

#endif /* _SORTSMILL_TRANSMATRIX_H */
