#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/guile.h>
#include <sortsmill/xgc.h>

static const char my_module[] = "sortsmill fonts anchors";

//-------------------------------------------------------------------------

VISIBLE C_WRAP_SCM_CALL_1 (scm_view_anchor_classes, my_module,
                           "view:anchor-classes");
VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_anchor_points, my_module,
                           "glyph-view:anchor-points");
VISIBLE C_WRAP_SCM_CALL_2 (scm_glyph_view_anchor_points_set_x, my_module,
                           "glyph-view:anchor-points-set!");
VISIBLE C_WRAP_SCM_CALL_2 (scm_glyph_view_anchor_points_add_x, my_module,
                           "glyph-view:anchor-points-add!");
VISIBLE C_WRAP_SCM_CALL_1 (scm_anchor_point_name, my_module,
                           "anchor-point-name");
VISIBLE C_WRAP_SCM_CALL_1 (scm_anchor_point_type, my_module,
                           "anchor-point-type");
VISIBLE C_WRAP_SCM_CALL_1 (scm_anchor_point_coords, my_module,
                           "anchor-point-coords");
VISIBLE C_WRAP_SCM_CALL_1 (scm_anchor_point_selected_p, my_module,
                           "anchor-point-selected?");
VISIBLE C_WRAP_SCM_CALL_1 (scm_anchor_point_ligature_index, my_module,
                           "anchor-point-ligature-index");
VISIBLE C_WRAP_SCM_CALL_2 (scm_anchor_point_with_name, my_module,
                           "anchor-point-with-name");
VISIBLE C_WRAP_SCM_CALL_2 (scm_anchor_point_with_type, my_module,
                           "anchor-point-with-type");
VISIBLE C_WRAP_SCM_CALL_2 (scm_anchor_point_with_coords, my_module,
                           "anchor-point-with-coords");
VISIBLE C_WRAP_SCM_CALL_2 (scm_anchor_point_with_selected_p, my_module,
                           "anchor-point-with-selected?");
VISIBLE C_WRAP_SCM_CALL_2 (scm_anchor_point_with_ligature_index, my_module,
                           "anchor-point-with-ligature-index");

//-------------------------------------------------------------------------
//
// Guile procedures for internal use.

VISIBLE C_WRAP_SCM_CALL_1 (scm_from_AnchorClasses, my_module,
                           "AnchorClasses->scm");
VISIBLE C_WRAP_SCM_CALL_1 (scm_from_AnchorPoints, my_module,
                           "AnchorPoints->scm");
VISIBLE C_WRAP_SCM_CALL_2 (scm_to_AnchorPoint, my_module, "scm->AnchorPoint");
VISIBLE C_WRAP_SCM_CALL_2 (scm_to_AnchorPoints, my_module, "scm->AnchorPoints");
VISIBLE C_WRAP_SCM_CALL_2 (scm_sort_anchor_points, my_module,
                           "sort-anchor-points");
VISIBLE C_WRAP_SCM_CALL_2 (scm_sort_AnchorPoints, my_module,
                           "sort-AnchorPoints");

//-------------------------------------------------------------------------
//
// Some C functions convenient for work with anchor points.

VISIBLE SCM
scm_anchor_point_name_2 (SCM anchor_point)
{
  const char *who = "scm_c_anchor_point_name";

  SCM p = anchor_point;
  scm_c_assert_list_does_not_end_here (who, anchor_point, p);
  scm_c_assert_can_be_alist_link (who, anchor_point, p);
  while (!scm_is_eq (SCM_CAAR (p), scm_symbol__name ()))
    {
      p = SCM_CDR (p);
      scm_c_assert_list_does_not_end_here (who, anchor_point, p);
      scm_c_assert_can_be_alist_link (who, anchor_point, p);
    }
  return SCM_CDAR (p);
}

VISIBLE SCM
scm_anchor_point_coords_2 (SCM anchor_point)
{
  const char *who = "scm_anchor_point_coords_2";

  SCM p = anchor_point;
  scm_c_assert_list_does_not_end_here (who, anchor_point, p);
  scm_c_assert_can_be_alist_link (who, anchor_point, p);
  while (!scm_is_eq (SCM_CAAR (p), scm_symbol__coords ()))
    {
      p = SCM_CDR (p);
      scm_c_assert_list_does_not_end_here (who, anchor_point, p);
      scm_c_assert_can_be_alist_link (who, anchor_point, p);
    }
  return SCM_CDAR (p);
}

VISIBLE void
scm_c_anchor_point_coords (SCM anchor_point, SCM *x, SCM *y)
{
  SCM coords = scm_anchor_point_coords_2 (anchor_point);
  *x = scm_car (coords);
  *y = scm_cadr (coords);
}

//-------------------------------------------------------------------------
