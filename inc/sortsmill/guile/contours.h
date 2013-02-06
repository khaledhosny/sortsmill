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

#ifndef _SORTSMILL_GUILE_CONTOURS_H
#define _SORTSMILL_GUILE_CONTOURS_H

#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_make_contour_point (SCM x, SCM y, SCM on_curve_p, SCM selected_p,
                            SCM name);
SCM scm_make_on_curve_point (SCM x, SCM y);
SCM scm_make_off_curve_point (SCM x, SCM y);
SCM scm_c_make_on_curve_point (double x, double y);
SCM scm_c_make_off_curve_point (double x, double y);

SCM scm_contour_point_p (SCM obj);
inline bool scm_is_contour_point (SCM obj);

inline bool
scm_is_contour_point (SCM obj)
{
  return scm_is_true (scm_contour_point_p (obj));
}

SCM scm_make_contour (SCM points, SCM closed_p, SCM degree, SCM name);
SCM scm_c_make_contour (SCM points, bool closed_p, int degree,
                        const char *name);

SCM scm_contour_p (SCM obj);
inline bool scm_is_contour (SCM obj);

inline bool
scm_is_contour (SCM obj)
{
  return scm_is_true (scm_contour_p (obj));
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_CONTOURS_H */
