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

#include <libguile.h>
#include <sortsmill/guile.h>
#include <splinefont.h>
#include <uiinterface.h>
#include <intl.h>

//-------------------------------------------------------------------------

static SCM
scm_layer_to_integer (SCM layer)
{
  SCM i;
  if (scm_is_true (scm_eq_p (layer, SCM_BOOL_F)))
    i = scm_from_int (ly_none);
  else if (scm_is_true (scm_integer_p (layer))
           && scm_is_false (scm_negative_p (layer)))
    i = layer;
  else if (scm_is_true (scm_eq_p (layer, scm_from_latin1_symbol ("all"))))
    i = scm_from_int (ly_all);
  else if (scm_is_true (scm_eq_p (layer, scm_from_latin1_symbol ("grid"))))
    i = scm_from_int (ly_grid);
  else
    {
      const char *message =
        _("expected a non-negative integer, 'all, 'grid, or #f");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition ("scm_layer_to_integer"),
          rnrs_c_make_message_condition (message),
          rnrs_make_irritants_condition (scm_list_1 (layer))));
    }
  return i;
}

static SCM
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
      layer = scm_from_latin1_symbol ("all");
      break;
    case ly_grid:
      layer = scm_from_latin1_symbol ("grid");
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

static SCM
scm_update_changed_SplineChar (SCM splinechar_ptr, SCM layer)
{
  SCCharChangedUpdate (scm_to_pointer (splinechar_ptr),
                       scm_to_int (scm_layer_to_integer (layer)));
  return SCM_UNSPECIFIED;
}

static SCM
scm_free_AnchorPoint_linked_list (SCM ap_ptr)
{
  AnchorPointsFree (scm_to_pointer (ap_ptr));
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

void init_guile_fontforge_internals (void);

VISIBLE void
init_guile_fontforge_internals (void)
{
  scm_c_define_gsubr ("layer->integer", 1, 0, 0, scm_layer_to_integer);
  scm_c_define_gsubr ("integer->layer", 1, 0, 0, scm_integer_to_layer);

  scm_c_define_gsubr ("update-changed-SplineChar", 2, 0, 0,
                      scm_update_changed_SplineChar);

  scm_c_define_gsubr ("free-AnchorPoint-linked-list", 1, 0, 0,
                      scm_free_AnchorPoint_linked_list);
}

//-------------------------------------------------------------------------
