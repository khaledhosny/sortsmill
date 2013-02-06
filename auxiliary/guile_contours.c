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
#include <sortsmill/guile/contours.h>

VISIBLE SCM
scm_make_contour_point (SCM x, SCM y, SCM on_curve_p, SCM selected_p, SCM name)
{
  return
    scm_call_8 (scm_c_public_ref ("sortsmill contours", "make-contour-point"),
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
    scm_call_1 (scm_c_private_ref ("sortsmill contours",
                                   "procedure:contour-point?"), obj);
}

// Generate library code from the inline definition.
VISIBLE bool scm_is_contour_point (SCM obj);

VISIBLE SCM
scm_make_contour (SCM points, SCM closed_p, SCM degree, SCM name)
{
  return
    scm_call_7 (scm_c_public_ref ("sortsmill contours", "make-contour"), points,
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
    scm_call_1 (scm_c_private_ref ("sortsmill contours", "procedure:contour?"),
                obj);
}

// Generate library code from the inline definition.
VISIBLE bool scm_is_contour (SCM obj);
