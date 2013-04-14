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

static const char my_module[] = "sortsmill fonts views";

VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_p, my_module, "font-view?");
VISIBLE C_WRAP_SCM_CALL_1 (scm_pointer_to_font_view, my_module,
                           "pointer->font-view");
VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_to_pointer, my_module,
                           "font-view->pointer");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_p, my_module, "glyph-view?");
VISIBLE C_WRAP_SCM_CALL_1 (scm_pointer_to_glyph_view, my_module,
                           "pointer->glyph-view");
VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_pointer, my_module,
                           "glyph-view->pointer");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_CharViewBase, my_module,
                           "glyph-view->CharViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_CharViewBase_to_glyph_view, my_module,
                           "CharViewBase->glyph-view");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_SplineChar, my_module,
                           "glyph-view->SplineChar");
VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_to_SplineFont, my_module,
                           "font-view->SplineFont");
VISIBLE C_WRAP_SCM_CALL_1 (scm_view_to_SplineFont, my_module,
                           "view->SplineFont");
