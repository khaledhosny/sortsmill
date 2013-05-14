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

#ifndef _SORTSMILL_GUILE_MATH_POLYSPLINE_ELLIPSES_H
#define _SORTSMILL_GUILE_MATH_POLYSPLINE_ELLIPSES_H

#include <sortsmill/math/polyspline/ellipses.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_from_elliptic_arc_t (elliptic_arc_t _arc);
elliptic_arc_t scm_to_elliptic_arc_t (SCM arc);

SCM scm_make_unit_circle_at_origin (void);
SCM scm_make_elliptic_arc (SCM cx, SCM cy, SCM a, SCM b,
                           SCM theta, SCM lambda1, SCM lambda2, SCM isPieSlice);
SCM scm_make_ellipse (SCM cx, SCM cy, SCM a, SCM b, SCM theta);

SCM scm_elliptic_arc_bezier_path (SCM arc, SCM degree, SCM threshold);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_MATH_POLYSPLINE_ELLIPSES_H */
