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
#include <usermenu.h>
#include <fontforge.h>
#include <splinefont.h>
#include <baseviews.h>
#include <intl.h>

static const char my_module[] = "sortsmill fonts views";

//-------------------------------------------------------------------------

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

VISIBLE C_WRAP_SCM_CALL_1 (scm_view_p, my_module, "view?");
VISIBLE C_WRAP_SCM_CALL_1 (scm_pointer_to_view, my_module, "pointer->view");
VISIBLE C_WRAP_SCM_CALL_1 (scm_view_to_pointer, my_module, "view->pointer");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_CharViewBase, my_module,
                           "glyph-view->CharViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_CharViewBase_to_glyph_view, my_module,
                           "CharViewBase->glyph-view");

VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_to_FontViewBase, my_module,
                           "font-view->FontViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_FontViewBase_to_font_view, my_module,
                           "FontViewBase->font-view");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_ViewBase, my_module,
                           "glyph-view->ViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_ViewBase_to_glyph_view, my_module,
                           "ViewBase->glyph-view");
VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_to_ViewBase, my_module,
                           "font-view->ViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_ViewBase_to_font_view, my_module,
                           "ViewBase->font-view");

VISIBLE C_WRAP_SCM_CALL_1 (scm_ViewBase_to_CharViewBase, my_module,
                           "ViewBase->CharViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_CharViewBase_to_ViewBase, my_module,
                           "CharViewBase->ViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_ViewBase_to_FontViewBase, my_module,
                           "ViewBase->FontViewBase");
VISIBLE C_WRAP_SCM_CALL_1 (scm_FontViewBase_to_ViewBase, my_module,
                           "FontViewBase->ViewBase");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyph_view_to_SplineChar, my_module,
                           "glyph-view->SplineChar");
VISIBLE C_WRAP_SCM_CALL_1 (scm_font_view_to_SplineFont, my_module,
                           "font-view->SplineFont");
VISIBLE C_WRAP_SCM_CALL_1 (scm_view_to_SplineFont, my_module,
                           "view->SplineFont");
VISIBLE C_WRAP_SCM_CALL_1 (scm_view_to_FontViewBase, my_module,
                           "view->FontViewBase");

//-------------------------------------------------------------------------

VISIBLE void *
scm_c_glyph_view_to_CharViewBase (SCM gv)
{
  return scm_to_pointer (SCM_FF_API_CALL_1 ("CharViewBase->pointer",
                                            scm_glyph_view_to_CharViewBase
                                            (gv)));
}

VISIBLE void *
scm_c_glyph_view_to_SplineChar (SCM gv)
{
  return scm_to_pointer (SCM_FF_API_CALL_1 ("SplineChar->pointer",
                                            scm_glyph_view_to_SplineChar (gv)));
}

VISIBLE void *
scm_c_font_view_to_SplineFont (SCM fv)
{
  return scm_to_pointer (SCM_FF_API_CALL_1 ("SplineFont->pointer",
                                            scm_font_view_to_SplineFont (fv)));
}

VISIBLE void *
scm_c_view_to_SplineFont (SCM v)
{
  return scm_to_pointer (SCM_FF_API_CALL_1 ("SplineFont->pointer",
                                            scm_view_to_SplineFont (v)));
}

VISIBLE void *
scm_c_view_to_FontViewBase (SCM v)
{
  return scm_to_pointer (SCM_FF_API_CALL_1 ("FontViewBase->pointer",
                                            scm_view_to_FontViewBase (v)));
}

//-------------------------------------------------------------------------

static FontViewBase *
SFAdd (SplineFont *sf, bool hide)
{
  if (sf->fv == NULL)
    {
      if (get_no_windowing_ui ())
        FVAppend (_FontViewCreate (sf));
      else
        FontViewCreate (sf, hide);
    }
  return sf->fv;
}

static SCM
scm_c_SFAdd (SplineFont *sf, bool hide)
{
  return scm_pointer_to_font_view (scm_from_pointer (SFAdd (sf, hide), NULL));
}

VISIBLE SCM
scm_c_make_font (const char *encoding, size_t foreground_degree,
                 size_t background_degree, size_t guide_layer_degree, bool hide)
{
  const char *who = "scm_c_make_font";

  Encoding *enc = FindOrMakeEncoding (encoding);
  if (enc == NULL)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("encoding not recognized")),
        rnrs_make_irritants_condition (scm_list_1
                                       (scm_from_utf8_string (encoding)))));

  if (foreground_degree < 2 || 3 < foreground_degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("unsupported foreground degree")),
        rnrs_make_irritants_condition (scm_list_1
                                       (scm_from_size_t (foreground_degree)))));

  if (background_degree < 2 || 3 < background_degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("unsupported background degree")),
        rnrs_make_irritants_condition (scm_list_1
                                       (scm_from_size_t (background_degree)))));

  if (guide_layer_degree < 2 || 3 < guide_layer_degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("unsupported guide layer degree")),
        rnrs_make_irritants_condition (scm_list_1
                                       (scm_from_size_t
                                        (guide_layer_degree)))));

  SCM font_view = scm_c_SFAdd (SplineFontNew_long_form (enc, foreground_degree,
                                                        background_degree,
                                                        guide_layer_degree),
                               hide);
  scm_view_private_dict_set_from_alist_x (font_view, SCM_EOL);
  return font_view;
}

VISIBLE SCM
scm_make_font (SCM encoding, SCM foreground_degree,
               SCM background_degree, SCM guide_layer_degree, SCM hide)
{
  const char *default_encoding = "ISO8859-1";

  const char *_encoding = (scm_is_false (encoding)) ?
    default_encoding : x_gc_grabstr (scm_to_utf8_stringn (encoding, NULL));
  const size_t _fg_degree = (scm_is_false (foreground_degree)) ?
    (new_fonts_are_order2 ? 2 : 3) : scm_to_size_t (foreground_degree);
  const size_t _bg_degree = (scm_is_false (background_degree)) ?
    (new_fonts_are_order2 ? 2 : 3) : scm_to_size_t (background_degree);
  const size_t _gl_degree = (scm_is_false (guide_layer_degree)) ?
    (new_fonts_are_order2 ? 2 : 3) : scm_to_size_t (guide_layer_degree);
  const bool _hide = scm_is_true (hide);
  return scm_c_make_font (_encoding, _fg_degree, _bg_degree, _gl_degree, _hide);
}

//-------------------------------------------------------------------------

void init_guile_fonts_views (void);

VISIBLE void
init_guile_fonts_views (void)
{
  scm_c_define ("font-view-flag", scm_from_int (FF_FONT_WINDOW));
  scm_c_define ("glyph-view-flag", scm_from_int (FF_GLYPH_WINDOW));

  scm_c_define_gsubr ("private:make-font", 5, 0, 0, scm_make_font);
}

//-------------------------------------------------------------------------
