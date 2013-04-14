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
#include <baseviews.h>
#include <splinefont.h>
#include <uiinterface.h>
#include <intl.h>
#include <assert.h>

static const char my_module[] = "sortsmill fonts glyphs";

//-------------------------------------------------------------------------

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyphlayer_to_glyph_view, my_module,
                           "glyph&layer->glyph-view");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyphlayer_to_layer, my_module,
                           "glyph&layer->layer");

VISIBLE C_WRAP_SCM_CALL_1 (scm_glyphlayer_update_changed, my_module,
                           "glyph&layer:update-changed");

VISIBLE C_WRAP_SCM_CALL_1 (scm_view_active_layer, my_module,
                           "view:active-layer");

VISIBLE C_WRAP_SCM_CALL_1 (scm_view_active_layer_set_x, my_module,
                           "view:active-layer-set!");

//-------------------------------------------------------------------------

static SCM
scm_SplineFont_preserve_guide_layer_as_undo (SCM splinefont)
{
  SplineFont *sf = scm_to_pointer (SCM_FF_API_CALL_1 ("SplineFont->pointer",
                                                      splinefont));
  _SFPreserveGuide (sf);
  return SCM_UNSPECIFIED;
}

static SCM
scm_SplineChar_preserve_layer_as_undo (SCM who, SCM splinechar, SCM layer, SCM layer_names, SCM hints_p)
{
  SCM i_layer = scm_layer_to_integer (layer, layer_names);
  if (scm_is_true (scm_negative_p (i_layer)))
    {
      const char *message = _("not a valid layer to preserve for undo");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_make_who_condition (who),
          rnrs_c_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_1 (layer))));
    }
  SplineChar *sc = scm_to_pointer (SCM_FF_API_CALL_1 ("SplineChar->pointer",
                                                      splinechar));
  _SCPreserveLayer (sc, scm_to_int (i_layer), scm_is_true (hints_p));
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

static SCM
scm_FVTrans (SCM fontviewbase, SCM splinechar, SCM ps_matrix, SCM sel,
             SCM flags)
{
  real transform[6];
  for (size_t i = 0; i < 6; i++)
    transform[i] =
      scm_to_double (scm_list_ref (ps_matrix, scm_from_size_t (i)));
  FVTrans (scm_to_pointer (SCM_FF_API_CALL_1 ("FontViewBase->pointer",
                                              fontviewbase)),
           scm_to_pointer (SCM_FF_API_CALL_1 ("SplineChar->pointer",
                                              splinechar)),
           transform, scm_to_pointer (sel), scm_to_int (flags));
  return SCM_UNSPECIFIED;
}

static SCM
scm_ffglyph_transform (SCM splinechar, SCM ps_matrix, SCM flags)
{
  SCM splinefont = SCM_FF_API_CALL_1 ("SplineChar:parent-dref", splinechar);
  SCM fontviewbase = SCM_FF_API_CALL_1 ("SplineFont:fv-dref", splinefont);
  SCM sel = scm_from_pointer (NULL, NULL);
  SCM flags2 = scm_logior (flags, scm_from_int (fvt_alllayers));
  return scm_FVTrans (fontviewbase, splinechar, ps_matrix, sel, flags2);
}

VISIBLE SCM
scm_glyph_view_transform_by_psmat (SCM gv, SCM ps_matrix, SCM flags)
{
  if (SCM_UNBNDP (flags))
    flags = scm_from_int (0);
  SCM sc = scm_glyph_view_to_SplineChar (gv);
  return scm_ffglyph_transform (sc, ps_matrix, flags);
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_layer_to_integer (SCM layer, SCM layer_names)
{
  const char *who = "scm_layer_to_integer";

  SCM i;
  if (scm_is_eq (layer, SCM_BOOL_F))
    i = scm_from_int (ly_none);
  else if (scm_is_true (scm_integer_p (layer))
           && scm_is_false (scm_negative_p (layer)))
    i = layer;
  else if (scm_is_eq (layer, scm_symbol__all ()))
    i = scm_from_int (ly_all);
  else if (scm_is_eq (layer, scm_symbol__grid ()))
    i = scm_from_int (ly_grid);
  else if (!SCM_UNBNDP (layer_names) && scm_is_string (layer))
    {
      SCM sublist = scm_member (layer, layer_names);
      if (scm_is_true (sublist))
        i = scm_difference (scm_length (layer_names), scm_length (sublist));
      else
        {
          const char *message = _("layer name not found");
          rnrs_raise_condition
            (scm_list_4
             (rnrs_make_assertion_violation (),
              rnrs_c_make_who_condition (who),
              rnrs_c_make_message_condition (message),
              rnrs_make_irritants_condition (scm_list_2 (layer, layer_names))));
        }
    }
  else
    {
      const char *message =
        (SCM_UNBNDP (layer_names)) ?
        _("expected a non-negative integer, 'all, 'grid, or #f") :
        _("expected a non-negative integer, string, 'all, 'grid, or #f");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_1 (layer))));
    }
  return i;
}

VISIBLE SCM
scm_integer_to_layer (SCM i)
{
  SCM layer;
  const int _i = scm_to_int (i);
  switch (_i)
    {
    case ly_none:
      layer = SCM_BOOL_F;
      break;
    case ly_all:
      layer = scm_symbol__all ();
      break;
    case ly_grid:
      layer = scm_symbol__grid ();
      break;
    default:
      if (0 <= _i)
        layer = i;
      else
        {
          const char *message = _("not a recognized layer index");
          rnrs_raise_condition
            (scm_list_4
             (rnrs_make_assertion_violation (),
              rnrs_c_make_who_condition ("scm_integer_to_layer"),
              rnrs_c_make_message_condition (message),
              rnrs_make_irritants_condition (scm_list_1 (i))));
        }
      break;
    }
  return layer;
}

VISIBLE SCM
scm_view_layer_names (SCM view)
{
  SCM splinefont = scm_view_to_SplineFont (view);
  SplineFont *sf = scm_to_pointer (SCM_FF_API_CALL_1 ("SplineFont->pointer",
                                                      splinefont));
  SCM names = SCM_EOL;
  for (size_t i = sf->layer_cnt; 0 < i; i--)
    names = scm_cons (scm_from_utf8_string (sf->layers[i - 1].name), names);
  return names;
}

VISIBLE SCM
scm_view_update_layer_palette (SCM view)
{
  SCM splinefont = scm_view_to_SplineFont (view);
  SplineFont *sf = scm_to_pointer (SCM_FF_API_CALL_1 ("SplineFont->pointer",
                                                      splinefont));
  CVLayerPaletteCheck (sf);
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_glyph_view_editable_layer (SCM gv)
{
  CharViewBase *cvb =
    scm_to_pointer (SCM_FF_API_CALL_1 ("CharViewBase->pointer",
                                       scm_glyph_view_to_CharViewBase (gv)));
  SCM layer = SCM_UNSPECIFIED;
  switch (cvb->drawmode)
    {
    case dm_grid:
      layer = scm_symbol__grid ();
      break;
    case dm_fore:
      layer = scm_from_int (1);
      break;
    case dm_back:
      {
        Layer *layer_ptr = cvb->layerheads[dm_back];
        if (layer_ptr == NULL)
          // MAYBE FIXME: I guess the following is safe, because layer
          // 0 always is there. But we also could treat this as an
          // error. It is discouraged to depend on the following
          // behavior. NOTE: Do not assume that ‘editable’ layer can
          // be set only in the GUI; we may well add support for an
          // ‘editable’ layer in scripts. We can do that pretty
          // easily, given that for us a glyph view is a CharViewBase,
          // and not, as in the legacy Python scripting, a ‘mere’
          // SplineChar.
          layer = scm_from_int (0);
        else
          layer = scm_from_size_t (layer_ptr - cvb->sc->layers);
      }
      break;
    default:
      assert (false);
      break;
    }
  return layer;
}

VISIBLE SCM
scm_glyph_view_editable_layer_set_x (SCM gv, SCM layer)
{
  const char *who = "scm_glyph_view_editable_layer_set_x";

  CharViewBase *cvb =
    scm_to_pointer (SCM_FF_API_CALL_1 ("CharViewBase->pointer",
                                       scm_glyph_view_to_CharViewBase (gv)));

  int i_layer =
    scm_to_int (scm_layer_to_integer (layer, scm_view_layer_names (gv)));
  switch (i_layer)
    {
    case ly_grid:
      if (cvb->drawmode != dm_grid)
        {
          cvb->drawmode = dm_grid;
          scm_glyphlayer_update_changed (gv);
          scm_view_update_layer_palette (gv);
        }
      break;

    case ly_fore:
      if (cvb->drawmode != dm_fore)
        {
          cvb->drawmode = dm_fore;
          scm_glyphlayer_update_changed (gv);
          scm_view_update_layer_palette (gv);
        }
      break;

    default:
      if (i_layer < 0)
        {
          const char *message = _("not a valid setting for `editable' layer");
          rnrs_raise_condition
            (scm_list_4
             (rnrs_make_assertion_violation (),
              rnrs_c_make_who_condition (who),
              rnrs_c_make_message_condition (message),
              rnrs_make_irritants_condition (scm_list_1 (layer))));
        }
      else if (cvb->drawmode != dm_back
               || cvb->layerheads[dm_back] != &cvb->sc->layers[i_layer])
        {
          cvb->drawmode = dm_back;
          cvb->layerheads[dm_back] = &cvb->sc->layers[i_layer];
          scm_glyphlayer_update_changed (gv);
          scm_view_update_layer_palette (gv);
        }
      break;
    }

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

void init_guile_fonts_glyphs (void);

VISIBLE void
init_guile_fonts_glyphs (void)
{
  scm_c_define_gsubr ("view:layer-names", 1, 0, 0, scm_view_layer_names);

  scm_c_define_gsubr ("view:update-layer-palette", 1, 0, 0,
                      scm_view_update_layer_palette);

  scm_c_define_gsubr ("glyph-view:editable-layer", 1, 0, 0,
                      scm_glyph_view_editable_layer);
  scm_c_define_gsubr ("glyph-view:editable-layer-set!", 2, 0, 0,
                      scm_glyph_view_editable_layer_set_x);

  scm_c_define_gsubr ("glyph-view:transform-by-psmat", 2, 1, 0,
                      scm_glyph_view_transform_by_psmat);

  scm_c_define_gsubr ("layer->integer", 1, 1, 0, scm_layer_to_integer);
  scm_c_define_gsubr ("integer->layer", 1, 0, 0, scm_integer_to_layer);

  // Unexported variables.
  scm_c_define_gsubr ("private:SplineFont-preserve-guide-layer-as-undo", 1, 0,
                      0, scm_SplineFont_preserve_guide_layer_as_undo);
  scm_c_define_gsubr ("private:SplineChar-preserve-layer-as-undo", 5, 0, 0,
                      scm_SplineChar_preserve_layer_as_undo);
}

//-------------------------------------------------------------------------
