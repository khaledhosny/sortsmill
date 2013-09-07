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

#ifndef _SORTSMILL_GUILE_FONTS_CONTOURS_H
#define _SORTSMILL_GUILE_FONTS_CONTOURS_H

#include <libguile.h>
#include <stdbool.h>
#include <sortsmill/core.h>

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

SCM scm_contour_point_x (SCM point);
SCM scm_contour_point_y (SCM point);
SCM scm_contour_point_on_curve_p (SCM point);
SCM scm_contour_point_selected_p (SCM point);
SCM scm_contour_point_name (SCM point);

inline double scm_c_contour_point_x (SCM point);
inline double scm_c_contour_point_y (SCM point);
inline bool scm_c_contour_point_on_curve_p (SCM point);
inline bool scm_c_contour_point_selected_p (SCM point);
inline char *scm_c_contour_point_name (SCM point);

inline double
scm_c_contour_point_x (SCM point)
{
  return scm_to_double (scm_contour_point_x (point));
}

inline double
scm_c_contour_point_y (SCM point)
{
  return scm_to_double (scm_contour_point_y (point));
}

inline bool
scm_c_contour_point_on_curve_p (SCM point)
{
  return scm_to_bool (scm_contour_point_on_curve_p (point));
}

inline bool
scm_c_contour_point_selected_p (SCM point)
{
  return scm_to_bool (scm_contour_point_selected_p (point));
}

inline char *
scm_c_contour_point_name (SCM point)
{
  return
    x_gc_grabstr (scm_to_utf8_stringn (scm_contour_point_name (point), NULL));
}

SCM scm_contour_point_x_set_x (SCM point, SCM value);
SCM scm_contour_point_y_set_x (SCM point, SCM value);
SCM scm_contour_point_on_curve_p_set_x (SCM point, SCM value);
SCM scm_contour_point_selected_p_set_x (SCM point, SCM value);
SCM scm_contour_point_name_set_x (SCM point, SCM value);

inline void scm_c_contour_point_x_set_x (SCM point, double value);
inline void scm_c_contour_point_y_set_x (SCM point, double value);
inline void scm_c_contour_point_on_curve_p_set_x (SCM point, bool value);
inline void scm_c_contour_point_selected_p_set_x (SCM point, bool value);
inline void scm_c_contour_point_name_set_x (SCM point, const char *value);

inline void
scm_c_contour_point_x_set_x (SCM point, double value)
{
  scm_contour_point_x_set_x (point, scm_from_double (value));
}

inline void
scm_c_contour_point_y_set_x (SCM point, double value)
{
  scm_contour_point_y_set_x (point, scm_from_double (value));
}

inline void
scm_c_contour_point_on_curve_p_set_x (SCM point, bool value)
{
  scm_contour_point_on_curve_p_set_x (point, scm_from_bool (value));
}

inline void
scm_c_contour_point_selected_p_set_x (SCM point, bool value)
{
  scm_contour_point_selected_p_set_x (point, scm_from_bool (value));
}

inline void
scm_c_contour_point_name_set_x (SCM point, const char *value)
{
  scm_contour_point_name_set_x (point, scm_from_utf8_string (value));
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

SCM scm_contour_points (SCM contour);
SCM scm_contour_closed_p (SCM contour);
SCM scm_contour_degree (SCM contour);
SCM scm_contour_name (SCM contour);

inline bool scm_c_contour_closed_p (SCM contour);
inline int scm_c_contour_degree (SCM contour);
inline char *scm_c_contour_name (SCM contour);

inline bool
scm_c_contour_closed_p (SCM contour)
{
  return scm_to_bool (scm_contour_closed_p (contour));
}

inline int
scm_c_contour_degree (SCM contour)
{
  return scm_to_int (scm_contour_degree (contour));
}

inline char *
scm_c_contour_name (SCM contour)
{
  return x_gc_grabstr (scm_to_utf8_stringn (scm_contour_name (contour), NULL));
}

SCM scm_contour_points_set_x (SCM contour, SCM value);
SCM scm_contour_closed_p_set_x (SCM contour, SCM value);
SCM scm_contour_degree_set_x (SCM contour, SCM value);
SCM scm_contour_name_set_x (SCM contour, SCM value);

inline void scm_c_contour_closed_p_set_x (SCM contour, bool value);
inline void scm_c_contour_degree_set_x (SCM contour, int value);
inline void scm_c_contour_name_set_x (SCM contour, const char *value);

inline void
scm_c_contour_closed_p_set_x (SCM contour, bool value)
{
  scm_contour_closed_p_set_x (contour, scm_from_bool (value));
}

inline void
scm_c_contour_degree_set_x (SCM contour, int value)
{
  scm_contour_degree_set_x (contour, scm_from_int (value));
}

inline void
scm_c_contour_name_set_x (SCM contour, const char *value)
{
  scm_contour_name_set_x (contour, scm_from_utf8_string (value));
}

SCM scm_contour_to_malloced_SplinePointList (SCM contour);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_CONTOURS_H */
