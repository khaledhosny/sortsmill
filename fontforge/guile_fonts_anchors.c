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

#include <sortsmill/guile.h>

static const char my_module[] = "sortsmill fonts anchors";

VISIBLE C_WRAP_SCM_CALL_1 (scm_view_anchor_classes, my_module,
                           "view-anchor-classes");
VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_anchor_points, my_module,
                           "glyph-view-anchor-points");
VISIBLE C_WRAP_SCM_CALL_2 (scm_glyph_view_anchor_points_set_x, my_module,
                           "glyph-view-anchor-points-set!");
VISIBLE C_WRAP_SCM_CALL_2 (scm_glyph_view_anchor_points_add_x, my_module,
                           "glyph-view-anchor-points-add!");
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
