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

#ifndef _SORTSMILL_GUILE_FONTS_ANCHORS_H
#define _SORTSMILL_GUILE_FONTS_ANCHORS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*-----------------------------------------------------------------------*/

/* Some C-wrapped Guile procedures. */

SCM scm_view_anchor_classes (SCM view);
SCM scm_glyph_view_anchor_points (SCM gv);
SCM scm_glyph_view_anchor_points_set_x (SCM gv, SCM anchor_points);
SCM scm_glyph_view_anchor_points_add_x (SCM gv, SCM anchor_point);
SCM scm_anchor_point_name (SCM anchor_point);
SCM scm_anchor_point_type (SCM anchor_point);
SCM scm_anchor_point_coords (SCM anchor_point);
SCM scm_anchor_point_selected_p (SCM anchor_point);
SCM scm_anchor_point_ligature_index (SCM anchor_point);
SCM scm_anchor_point_with_name (SCM anchor_point, SCM value);
SCM scm_anchor_point_with_type (SCM anchor_point, SCM value);
SCM scm_anchor_point_with_coords (SCM anchor_point, SCM value);
SCM scm_anchor_point_with_selected_p (SCM anchor_point, SCM value);
SCM scm_anchor_point_with_ligature_index (SCM anchor_point, SCM value);

/*-----------------------------------------------------------------------*/

/* This group of C-wrapped Guile procedures is meant mainly for
   internal use. They are liable to change or go away without
   warning. */

SCM scm_from_AnchorClasses (SCM AnchorClass_linked_list_ptr);
SCM scm_from_AnchorPoints (SCM AnchorPoint_linked_list_ptr);
SCM scm_to_AnchorPoint (SCM AnchorClass_linked_list_ptr, SCM anchor_point);
SCM scm_to_AnchorPoints (SCM AnchorClass_linked_list_ptr, SCM anchor_points);
SCM scm_sort_anchor_points (SCM AnchorClass_linked_list_ptr, SCM anchor_points);
SCM scm_sort_AnchorPoints (SCM AnchorClass_linked_list_ptr,
                           SCM AnchorPoint_linked_list_ptr);

/*-----------------------------------------------------------------------*/

/* Some C functions convenient for work with anchor points. */

/* The next function is an alternative to scm_anchor_point_name. It
   may be faster, and it has different checking. Use
   scm_anchor_point_name if you are validating anchors before adding
   them to the anchor point list; but you might use
   scm_anchor_point_name_2 if you are sifting through the anchors that
   already are in the anchor point list. */
SCM scm_anchor_point_name_2 (SCM anchor_point);

void scm_c_anchor_point_coords (SCM anchor_point, double *x, double *y);

/*-----------------------------------------------------------------------*/

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_ANCHORS_H */
