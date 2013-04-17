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
SCM scm_view_active_layer_set_p (SCM view, SCM layer);

SCM scm_glyph_view_editable_layer (SCM gv);
SCM scm_glyph_view_editable_layer_set_p (SCM gv, SCM layer);

SCM scm_view_layer_names (SCM view);

SCM scm_glyph_view_width (SCM gv);
SCM scm_glyph_view_width_set_x (SCM gv, SCM width);

size_t scm_c_view_glyph_count (SCM view);
SCM scm_view_glyph_count (SCM view);

SCM scm_layer_to_integer (SCM layer, SCM layer_names);
SCM scm_integer_to_layer (SCM i);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_GLYPHS_H */
