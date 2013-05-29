/* -*- coding: utf-8 -*- */
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

#ifndef _SORTSMILL_MATH_POLYSPLINE_INTERSECTION_H
#define _SORTSMILL_MATH_POLYSPLINE_INTERSECTION_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void plug_xy_into_line_scm_mono (size_t degree,
                                 ssize_t xstride, const SCM *xspline,
                                 ssize_t ystride, const SCM *yspline,
                                 SCM C, SCM A, SCM B,
                                 ssize_t result_stride, SCM *result);
SCM scm_plug_xy_into_line_scm_mono (SCM xspline, SCM yspline, SCM implicit_eq);
SCM scm_intersect_with_line_mono (SCM xsplines, SCM ysplines, SCM implicit_eq,
                                  SCM t0, SCM t1);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_INTERSECTION_H */
