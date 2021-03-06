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

#ifndef _SORTSMILL_GUILE_FONTS_GLYPHS_H
#define _SORTSMILL_GUILE_FONTS_GLYPHS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_glyphlayer_to_glyph_view (SCM gl);
SCM scm_glyphlayer_to_layer (SCM gl);

SCM scm_glyphlayer_update_changed (SCM gl);
SCM scm_view_update_layer_palette (SCM view);

SCM scm_glyph_view_transform_by_psmat (SCM gv, SCM ps_matrix, SCM flags);

SCM scm_view_active_layer (SCM view);
SCM scm_view_active_layer_set_x (SCM view, SCM layer);

SCM scm_glyph_view_editable_layer (SCM gv);
SCM scm_glyph_view_editable_layer_set_x (SCM gv, SCM layer);

SCM scm_view_layer_names (SCM view);

SCM scm_glyph_view_width (SCM gv);
SCM scm_glyph_view_width_set_x (SCM gv, SCM width);

/* In the following group of functions, @var{pred} may be set to
   @code{SCM_UNDEFINED} to use a default predicate. */
size_t scm_c_view_glyph_count (SCM view, SCM pred);
SCM scm_view_glyph_count (SCM view, SCM pred);
SCM scm_view_glyphs (SCM view, SCM pred);

bool scm_glyph_view_is_worth_outputting (SCM gv);
SCM scm_glyph_view_worth_outputting_p (SCM gv);

SCM scm_layer_to_integer (SCM layer, SCM layer_names);
SCM scm_integer_to_layer (SCM i);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_GLYPHS_H */
