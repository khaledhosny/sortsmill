#include <config.h>

// Copyright (C) 2013 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.


#include <libguile.h>
#include <sortsmill/guile.h>

static const char *contours_module = "sortsmill fonts contours";

VISIBLE SCM
scm_make_contour_point (SCM x, SCM y, SCM on_curve_p, SCM selected_p, SCM name)
{
  return
    scm_call_8 (scm_c_public_ref (contours_module, "make-contour-point"),
                x, y, scm_from_latin1_keyword ("on-curve?"), on_curve_p,
                scm_from_latin1_keyword ("selected?"), selected_p,
                scm_from_latin1_keyword ("name"), name);
}

VISIBLE SCM
scm_make_on_curve_point (SCM x, SCM y)
{
  return scm_make_contour_point (x, y, SCM_BOOL_T, SCM_BOOL_F,
                                 scm_from_latin1_string (""));
}

VISIBLE SCM
scm_make_off_curve_point (SCM x, SCM y)
{
  return scm_make_contour_point (x, y, SCM_BOOL_F, SCM_BOOL_F,
                                 scm_from_latin1_string (""));
}

VISIBLE SCM
scm_c_make_on_curve_point (double x, double y)
{
  return scm_make_on_curve_point (scm_from_double (x), scm_from_double (y));
}

VISIBLE SCM
scm_c_make_off_curve_point (double x, double y)
{
  return scm_make_off_curve_point (scm_from_double (x), scm_from_double (y));
}

VISIBLE SCM
scm_contour_point_p (SCM obj)
{
  return
    scm_call_1 (scm_c_private_ref (contours_module, "procedure:contour-point?"),
                obj);
}

// Generate library code from the inline definition.
VISIBLE bool scm_is_contour_point (SCM obj);

#define _FF_CONTOUR_POINT_GET(FIELD_C, FIELD_SCM)			\
  VISIBLE SCM								\
  scm_contour_point_##FIELD_C (SCM point)				\
  {									\
    return scm_call_1 (scm_c_public_ref (contours_module,		\
					 "contour-point-" FIELD_SCM),	\
		       point);						\
  }

_FF_CONTOUR_POINT_GET (x, "x");
_FF_CONTOUR_POINT_GET (y, "y");
_FF_CONTOUR_POINT_GET (on_curve_p, "on-curve?");
_FF_CONTOUR_POINT_GET (selected_p, "selected?");
_FF_CONTOUR_POINT_GET (name, "name");

// Generate code from inline definitions.
VISIBLE double scm_c_contour_point_x (SCM point);
VISIBLE double scm_c_contour_point_y (SCM point);
VISIBLE bool scm_c_contour_point_on_curve_p (SCM point);
VISIBLE bool scm_c_contour_point_selected_p (SCM point);
VISIBLE char *scm_c_contour_point_name (SCM point);

#define _FF_CONTOUR_POINT_SET(FIELD_C, FIELD_SCM)			\
  VISIBLE SCM								\
  scm_contour_point_##FIELD_C##_set_x (SCM point, SCM value)		\
  {									\
    return scm_call_2 (scm_c_public_ref (contours_module,		\
					 "contour-point-" FIELD_SCM	\
					 "-set!"),			\
		       point, value);					\
  }

_FF_CONTOUR_POINT_SET (x, "x");
_FF_CONTOUR_POINT_SET (y, "y");
_FF_CONTOUR_POINT_SET (on_curve_p, "on-curve?");
_FF_CONTOUR_POINT_SET (selected_p, "selected?");
_FF_CONTOUR_POINT_SET (name, "name");

// Generate code from inline definitions.
VISIBLE void scm_c_contour_point_x_set_x (SCM point, double value);
VISIBLE void scm_c_contour_point_y_set_x (SCM point, double value);
VISIBLE void scm_c_contour_point_on_curve_p_set_x (SCM point, bool value);
VISIBLE void scm_c_contour_point_selected_p_set_x (SCM point, bool value);
VISIBLE void scm_c_contour_point_name_set_x (SCM point, const char *value);

VISIBLE SCM
scm_make_contour (SCM points, SCM closed_p, SCM degree, SCM name)
{
  return
    scm_call_7 (scm_c_public_ref (contours_module, "make-contour"), points,
                scm_from_latin1_keyword ("closed?"), closed_p,
                scm_from_latin1_keyword ("degree"), degree,
                scm_from_latin1_keyword ("name"), name);
}

VISIBLE SCM
scm_c_make_contour (SCM points, bool closed_p, int degree, const char *name)
{
  return scm_make_contour (points, scm_from_bool (closed_p),
                           scm_from_int (degree), scm_from_utf8_string (name));
}

VISIBLE SCM
scm_contour_p (SCM obj)
{
  return
    scm_call_1 (scm_c_private_ref (contours_module, "procedure:contour?"), obj);
}

// Generate library code from the inline definition.
VISIBLE bool scm_is_contour (SCM obj);

#define _FF_CONTOUR_GET(FIELD_C, FIELD_SCM)			\
  VISIBLE SCM							\
  scm_contour_##FIELD_C (SCM contour)				\
  {								\
    return scm_call_1 (scm_c_public_ref (contours_module,	\
					 "contour-" FIELD_SCM),	\
		       contour);				\
  }

_FF_CONTOUR_GET (points, "points");
_FF_CONTOUR_GET (closed_p, "closed?");
_FF_CONTOUR_GET (degree, "degree");
_FF_CONTOUR_GET (name, "name");

// Generate code from inline definitions.
VISIBLE bool scm_c_contour_closed_p (SCM contour);
VISIBLE int scm_c_contour_degree (SCM contour);
VISIBLE char *scm_c_contour_name (SCM contour);

#define _FF_CONTOUR_SET(FIELD_C, FIELD_SCM)				\
  VISIBLE SCM								\
  scm_contour_##FIELD_C##_set_x (SCM contour, SCM value)		\
  {									\
    return scm_call_2 (scm_c_public_ref (contours_module,		\
					 "contour-" FIELD_SCM "-set!"),	\
		       contour, value);					\
  }

_FF_CONTOUR_SET (points, "points");
_FF_CONTOUR_SET (closed_p, "closed?");
_FF_CONTOUR_SET (degree, "degree");
_FF_CONTOUR_SET (name, "name");

// Generate code from inline definitions.
VISIBLE void scm_c_contour_closed_p_set_x (SCM contour, bool value);
VISIBLE void scm_c_contour_degree_set_x (SCM contour, int value);
VISIBLE void scm_c_contour_name_set_x (SCM contour, const char *value);
