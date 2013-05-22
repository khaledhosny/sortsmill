/* -*- coding: utf-8 -*- */

/*
 * Approximation of elliptic arcs by polynomial splines. Based on Java
 * code by Luc Maisonobe. See
 * http://www.spaceroots.org/documents/ellipse/elliptical-arc.html
 */

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

/*
 * Copyright (c) 2003-2004, Luc Maisonobe
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with
 * or without modification, are permitted provided that
 * the following conditions are met:
 * 
 *    Redistributions of source code must retain the
 *    above copyright notice, this list of conditions and
 *    the following disclaimer. 
 *    Redistributions in binary form must reproduce the
 *    above copyright notice, this list of conditions and
 *    the following disclaimer in the documentation
 *    and/or other materials provided with the
 *    distribution. 
 *    Neither the names of spaceroots.org, spaceroots.com
 *    nor the names of their contributors may be used to
 *    endorse or promote products derived from this
 *    software without specific prior written permission. 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
 * THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _SORTSMILL_MATH_POLYSPLINE_ELLIPSES_H
#define _SORTSMILL_MATH_POLYSPLINE_ELLIPSES_H

#include <stdbool.h>
#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/* FIXME: Consider providing a `spline count' function for piecewise
   linear that requires fewer arguments. */
size_t elliptic_arc_spline_count (int degree, size_t max_count,
                                  double threshold, double semimajor,
                                  double semiminor, double xcenter,
                                  double ycenter, double cos_theta,
                                  double sin_theta, double eta1, double eta2);

void elliptic_arc_piecewise_bezier (int degree, size_t spline_count,
                                    double semimajor, double semiminor,
                                    double xcenter, double ycenter,
                                    double cos_theta, double sin_theta,
                                    double eta1, double eta2,
                                    double xsplines[spline_count][degree + 1],
                                    double ysplines[spline_count][degree + 1]);

SCM scm_c_elliptic_arc_piecewise_bezier (int degree, size_t max_count,
                                         double threshold,
                                         double semimajor, double semiminor,
                                         double xcenter, double ycenter,
                                         double cos_theta, double sin_theta,
                                         double eta1, double eta2);

SCM scm_elliptic_arc_piecewise_bezier (SCM degree, SCM max_count, SCM threshold,
                                       SCM semimajor, SCM semiminor,
                                       SCM xcenter, SCM ycenter, SCM theta,
                                       SCM eta1, SCM eta2);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_MATH_POLYSPLINE_ELLIPSES_H */
