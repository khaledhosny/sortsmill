#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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
#include <sortsmill/xgc.h>
#include <xalloc.h>
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

VISIBLE C_WRAP_SCM_CALL_2 (scm_view_active_layer_set_x, my_module,
                           "view:active-layer-set!");

//-------------------------------------------------------------------------

static SCM
scm_view_preserve_guide_layer_as_undo (SCM view)
{
  _SFPreserveGuide ((SplineFont *) scm_c_font_view_to_SplineFont (view));
  return SCM_UNSPECIFIED;
}

static SCM
scm_glyphlayer_preserve_nonguide_layer_as_undo (SCM who, SCM glyphlayer,
                                                SCM layer_names, SCM hints_p)
{
  SCM layer = scm_glyphlayer_to_layer (glyphlayer);
  int i_layer = scm_to_int (scm_layer_to_integer (layer, layer_names));
  if (i_layer < 0)
    {
      const char *message = _("not a valid layer to preserve for undo");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_make_who_condition (who),
          rnrs_c_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_1 (glyphlayer))));
    }

  SCM gv = scm_glyphlayer_to_glyph_view (glyphlayer);
  SplineChar *sc = (SplineChar *) scm_c_glyph_view_to_SplineChar (gv);

  _SCPreserveLayer (sc, i_layer, scm_is_true (hints_p));

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

static void
scm_psmat_to_real_array (SCM ps_matrix, real array[6])
{
  for (size_t i = 0; i < 6; i++)
    array[i] = scm_to_double (scm_list_ref (ps_matrix, scm_from_size_t (i)));
}

VISIBLE SCM
scm_glyph_view_transform_by_psmat (SCM gv, SCM ps_matrix, SCM flags)
{
  real transform[6];
  scm_psmat_to_real_array (ps_matrix, transform);

  int i_flags =
    (SCM_UNBNDP (flags)) ? fvt_alllayers : (scm_to_int (flags) | fvt_alllayers);

  FVTrans ((FontViewBase *) scm_c_view_to_FontViewBase (gv),
           (SplineChar *) scm_c_glyph_view_to_SplineChar (gv),
           transform, NULL, i_flags);

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_glyph_view_width (SCM gv)
{
  SplineChar *sc = (SplineChar *) scm_c_glyph_view_to_SplineChar (gv);
  return scm_from_intmax (sc->width);
}

VISIBLE SCM
scm_glyph_view_width_set_x (SCM gv, SCM width)
{
  SplineChar *sc = (SplineChar *) scm_c_glyph_view_to_SplineChar (gv);
  const intmax_t new_width = scm_to_intmax (scm_round_number (width));
  SCSynchronizeWidth (sc, new_width, sc->width, NULL);
  scm_glyphlayer_update_changed (gv);
  return SCM_UNSPECIFIED;
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
  else if (scm_is_eq (layer, scm_symbol__guide ()))
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
        _("expected a non-negative integer, 'all, 'guide, or #f") :
        _("expected a non-negative integer, string, 'all, 'guide, or #f");
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
      layer = scm_symbol__guide ();
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
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  SCM names = SCM_EOL;
  for (size_t i = sf->layer_cnt; 0 < i; i--)
    names = scm_cons (scm_from_utf8_string (sf->layers[i - 1].name), names);
  return names;
}

VISIBLE SCM
scm_view_update_layer_palette (SCM view)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  CVLayerPaletteCheck (sf);
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_glyph_view_editable_layer (SCM gv)
{
  CharViewBase *cvb = (CharViewBase *) scm_c_glyph_view_to_CharViewBase (gv);
  SCM layer = SCM_UNSPECIFIED;
  switch (cvb->drawmode)
    {
    case dm_grid:
      layer = scm_symbol__guide ();
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

  CharViewBase *cvb = (CharViewBase *) scm_c_glyph_view_to_CharViewBase (gv);

  int i_layer =
    scm_to_int (scm_layer_to_integer (layer, scm_view_layer_names (gv)));
  switch (i_layer)
    {
    case ly_grid:
      if (cvb->drawmode != dm_grid)
        {
          cvb->drawmode = dm_grid;
          scm_view_update_layer_palette (gv);
        }
      break;

    case ly_fore:
      if (cvb->drawmode != dm_fore)
        {
          cvb->drawmode = dm_fore;
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
          scm_view_update_layer_palette (gv);
        }
      break;
    }

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

static bool
scm_glyph_view_satisfies_predicate (SCM gv, SCM pred)
{
  SCM base_pred =
    scm_fluid_ref (scm_c_public_ref (my_module, "base-glyph-view-predicate"));
  bool result;
  if (scm_is_true (scm_call_1 (base_pred, gv)))
    {
      if (SCM_UNBNDP (pred))
        result = true;
      else
        result = scm_is_true (scm_call_1 (pred, gv));
    }
  else
    result = false;
  return result;
}

VISIBLE size_t
scm_c_view_glyph_count (SCM view, SCM pred)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  size_t count = 0;
  for (ssize_t i = 0; i < sf->glyphcnt; i++)
    if (sf->glyphs[i] != NULL)
      {
        CharViewBase *cvb = x_gc_malloc (sizeof (CharViewBase));
        cvb[0] = minimalist_CharViewBase (sf->glyphs[i]);
        SCM gv = scm_pointer_to_glyph_view (scm_from_pointer (cvb, NULL));
        if (scm_glyph_view_satisfies_predicate (gv, pred))
          count++;
      }
  return count;
}

VISIBLE SCM
scm_view_glyph_count (SCM view, SCM pred)
{
  return scm_from_size_t (scm_c_view_glyph_count (view, pred));
}

VISIBLE SCM
scm_view_glyphs (SCM view, SCM pred)
{
  const size_t n = scm_c_view_glyph_count (view, pred);
  SCM result = scm_c_make_vector (n, SCM_UNSPECIFIED);

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  size_t k = 0;
  for (ssize_t i = 0; i < sf->glyphcnt; i++)
    if (sf->glyphs[i] != NULL)
      {
        CharViewBase *cvb = x_gc_malloc (sizeof (CharViewBase));
        cvb[0] = minimalist_CharViewBase (sf->glyphs[i]);
        SCM gv = scm_pointer_to_glyph_view (scm_from_pointer (cvb, NULL));
        if (scm_glyph_view_satisfies_predicate (gv, pred))
          scm_c_vector_set_x (result, k, gv);
        k++;
      }

  return result;
}

VISIBLE bool
scm_glyph_view_is_worth_outputting (SCM gv)
{
  return (bool) SCWorthOutputting (scm_c_glyph_view_to_SplineChar (gv));
}

VISIBLE SCM
scm_glyph_view_worth_outputting_p (SCM gv)
{
  return scm_from_bool (scm_glyph_view_is_worth_outputting (gv));
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

  scm_c_define_gsubr ("glyph-view:width", 1, 0, 0, scm_glyph_view_width);
  scm_c_define_gsubr ("glyph-view:width-set!", 2, 0, 0,
                      scm_glyph_view_width_set_x);

  scm_c_define_gsubr ("view:glyph-count", 1, 1, 0, scm_view_glyph_count);
  scm_c_define_gsubr ("view:glyphs", 1, 1, 0, scm_view_glyphs);

  scm_c_define_gsubr ("glyph-view:worth-outputting?", 1, 0, 0,
                      scm_glyph_view_worth_outputting_p);

  scm_c_define_gsubr ("layer->integer", 1, 1, 0, scm_layer_to_integer);
  scm_c_define_gsubr ("integer->layer", 1, 0, 0, scm_integer_to_layer);

  // Unexported variables.
  scm_c_define_gsubr ("private:view:preserve-guide-layer-as-undo", 1, 0, 0,
                      scm_view_preserve_guide_layer_as_undo);
  scm_c_define_gsubr ("private:glyph&layer:preserve-nonguide-layer-as-undo", 4,
                      0, 0, scm_glyphlayer_preserve_nonguide_layer_as_undo);
}

//-------------------------------------------------------------------------
