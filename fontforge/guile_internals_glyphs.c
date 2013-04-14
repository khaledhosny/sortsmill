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
scm_c_SplineFont_layer_names (SplineFont *sf)
{
  SCM names = SCM_EOL;
  for (size_t i = sf->layer_cnt; 0 < i; i--)
    names = scm_cons (scm_from_utf8_string (sf->layers[i - 1].name), names);
  return names;
}

static SCM
scm_update_changed_SplineChar (SCM splinechar_ptr, SCM layer)
{
  SplineChar *sc = scm_to_pointer (splinechar_ptr);
  SCM layer_names = scm_c_SplineFont_layer_names (sc->parent);
  const int i_layer = scm_to_int (scm_layer_to_integer (layer, layer_names));
  SCCharChangedUpdate (sc, i_layer);
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

void init_guile_internals_glyphs (void);

VISIBLE void
init_guile_internals_glyphs (void)
{
  scm_c_define_gsubr ("update-changed-SplineChar", 2, 0, 0,
                      scm_update_changed_SplineChar);
}

//-------------------------------------------------------------------------
