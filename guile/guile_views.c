#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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

#include <sortsmillff/guile/views.h>

void init_guile_sortsmillff_views (void);

//-------------------------------------------------------------------------

#define _FF_MENU_ENTRY_VIEW_WRAPPER(VIEW)				\
									\
  static SCM pointer_to_##VIEW = SCM_UNDEFINED;				\
  static SCM VIEW##_to_pointer = SCM_UNDEFINED;				\
  static SCM VIEW##_predicate = SCM_UNDEFINED;				\
									\
  VISIBLE SCM								\
  scm_pointer_to_##VIEW (SCM pointer)					\
  {									\
    return scm_call_1 (pointer_to_##VIEW, pointer);			\
  }									\
									\
  VISIBLE SCM								\
  scm_c_pointer_to_##VIEW (void *p)					\
  {									\
    return scm_call_1 (pointer_to_##VIEW, scm_from_pointer (p, NULL));	\
  }									\
									\
  VISIBLE SCM								\
  scm_##VIEW##_to_pointer (SCM view)					\
  {									\
    return scm_call_1 (VIEW##_to_pointer, view);			\
  }									\
									\
  VISIBLE void *							\
  scm_c_##VIEW##_to_pointer (SCM view)					\
  {									\
    return scm_to_pointer (scm_call_1 (VIEW##_to_pointer, view));	\
  }									\
									\
  VISIBLE SCM								\
  scm_##VIEW##_p (SCM view)						\
  {									\
    return scm_call_1 (VIEW##_predicate, view);				\
  }									\
									\
  VISIBLE bool								\
  scm_is_##VIEW (SCM view)						\
  {									\
    return scm_is_true (scm_call_1 (VIEW##_predicate, view));		\
  }

_FF_MENU_ENTRY_VIEW_WRAPPER (font_view);
_FF_MENU_ENTRY_VIEW_WRAPPER (glyph_view);

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_views (void)
{
  pointer_to_font_view = scm_c_public_ref ("sortsmillff views", "pointer->font-view");
  font_view_to_pointer = scm_c_public_ref ("sortsmillff views", "font-view->pointer");
  font_view_predicate = scm_c_public_ref ("sortsmillff views", "font-view?");

  pointer_to_glyph_view = scm_c_public_ref ("sortsmillff views", "pointer->glyph-view");
  glyph_view_to_pointer = scm_c_public_ref ("sortsmillff views", "glyph-view->pointer");
  glyph_view_predicate = scm_c_public_ref ("sortsmillff views", "glyph-view?");
}

//-------------------------------------------------------------------------
