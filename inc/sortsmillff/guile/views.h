/*
 * Copyright (C) 2012 Barry Schwartz
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

#ifndef _SORTSMILLFF_GUILE_VIEWS_H
#define _SORTSMILLFF_GUILE_VIEWS_H

#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_pointer_to_font_view (SCM pointer);
SCM scm_c_pointer_to_font_view (void *p);

SCM scm_font_view_to_pointer (SCM view);
void *scm_c_font_view_to_pointer (SCM view);

SCM scm_font_view_p (SCM view);
bool scm_is_font_view (SCM view);

SCM scm_pointer_to_glyph_view (SCM pointer);
SCM scm_c_pointer_to_glyph_view (void *p);

SCM scm_glyph_view_to_pointer (SCM view);
void *scm_c_glyph_view_to_pointer (SCM view);

SCM scm_glyph_view_p (SCM view);
bool scm_is_glyph_view (SCM view);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILLFF_GUILE_VIEWS_H */
