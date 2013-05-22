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

#ifndef _SORTSMILL_GUILE_FONTS_VIEWS_H
#define _SORTSMILL_GUILE_FONTS_VIEWS_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

SCM scm_font_view_p (SCM);
SCM scm_pointer_to_font_view (SCM);
SCM scm_font_view_to_pointer (SCM);

SCM scm_glyph_view_p (SCM);
SCM scm_pointer_to_glyph_view (SCM);
SCM scm_glyph_view_to_pointer (SCM);

SCM scm_view_p (SCM);
SCM scm_pointer_to_view (SCM);
SCM scm_view_to_pointer (SCM);

SCM scm_glyph_view_to_CharViewBase (SCM);
SCM scm_CharViewBase_to_glyph_view (SCM);

SCM scm_font_view_to_FontViewBase (SCM);
SCM scm_FontViewBase_to_font_view (SCM);

SCM scm_glyph_view_to_ViewBase (SCM);
SCM scm_ViewBase_to_glyph_view (SCM);
SCM scm_font_view_to_ViewBase (SCM);
SCM scm_ViewBase_to_font_view (SCM);

SCM scm_ViewBase_to_CharViewBase (SCM);
SCM scm_CharViewBase_to_ViewBase (SCM);
SCM scm_ViewBase_to_FontViewBase (SCM);
SCM scm_FontViewBase_to_ViewBase (SCM);

SCM scm_glyph_view_to_SplineChar (SCM);
SCM scm_font_view_to_SplineFont (SCM);
SCM scm_view_to_SplineFont (SCM);
SCM scm_view_to_FontViewBase (SCM);

/* The following functions return void pointers so they can be
   included here without any reference to internal structures. */
void *scm_c_glyph_view_to_CharViewBase (SCM);
void *scm_c_glyph_view_to_SplineChar (SCM);
void *scm_c_font_view_to_SplineFont (SCM);
void *scm_c_view_to_SplineFont (SCM);
void *scm_c_view_to_FontViewBase (SCM);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_VIEWS_H */
