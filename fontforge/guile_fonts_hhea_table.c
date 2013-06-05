#include <config.h>             // -*- coding: utf-8 -*-

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
#include <guile_fonts_os2_hhea.h>
#include <splinefont.h>
#include <intl.h>

static const char my_module[] = "sortsmill fonts hhea-table";

//-------------------------------------------------------------------------

typedef enum
{
  //_hhea_version, ← not supported here
  _hhea_Ascender,
  _hhea_Descender,
  _hhea_LineGap,
  //_hhea_advanceWidthMax, ← not supported here
  //_hhea_minLeftSideBearing, ← not supported here
  //_hhea_minRightSideBearing, ← not supported here
  //_hhea_xMaxExtent, ← not supported here
  //_hhea_caretSlopeRise, ← not supported here
  //_hhea_caretSlopeRun, ← not supported here
  //_hhea_caretOffset, ← not supported here
  //_hhea_metricDataFormat, ← not supported here
  //_hhea_numberOfHMetrics, ← not supported here
  _hhea_SENTINEL
} _hhea_key_t;

static const char *hhea_key_table[] = {
  //[_hhea_version] = "version", ← not supported here
  [_hhea_Ascender] = "Ascender",
  [_hhea_Descender] = "Descender",
  [_hhea_LineGap] = "LineGap"
    //[_hhea_advanceWidthMax] = "advanceWidthMax", ← not supported here
    //[_hhea_minLeftSideBearing] = "minLeftSideBearing", ← not supported here
    //[_hhea_minRightSideBearing] = "minRightSideBearing", ← not supported here
    //[_hhea_xMaxExtent] = "xMaxExtent", ← not supported here
    //[_hhea_caretSlopeRise] = "caretSlopeRise", ← not supported here
    //[_hhea_caretSlopeRun] = "caretSlopeRun", ← not supported here
    //[_hhea_caretOffset] = "caretOffset", ← not supported here
    //[_hhea_metricDataFormat] = "metricDataFormat", ← not supported here
    //[_hhea_numberOfHMetrics] = "numberOfHMetrics" ← not supported here
};

static void
raise_unsupported_key (const char *who, SCM key)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("unsupported hhea table key")),
      rnrs_make_irritants_condition (scm_list_1 (key))));
}

static _hhea_key_t
hhea_key (const char *who, const char *key_string)
{
  _hhea_key_t k = 0;
  while (k < _hhea_SENTINEL && strcmp (key_string, hhea_key_table[k]) != 0)
    k++;
  if (k == _hhea_SENTINEL)
    raise_unsupported_key (who, scm_from_utf8_string (key_string));
  return k;
}

VISIBLE void
scm_c_view_hhea_table_set_x (SCM view, const char *key, SCM value,
                             SCM value_is_offset)
{
  const char *who = "scm_c_view_hhea_table_set_x";

  if (SCM_UNBNDP (value_is_offset))
    value_is_offset = SCM_BOOL_F;

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  SFDefaultOS2 (sf);

  switch (hhea_key (who, key))
    {
    case _hhea_Ascender:
      sf->pfminfo.hhead_ascent =
        stored_int_considering_offsets (sf->pfminfo.hheadascent_add,
                                        value, value_is_offset,
                                        scm_from_double (sf_ymax (sf)),
                                        scm_ceiling);
      break;
    case _hhea_Descender:
      sf->pfminfo.hhead_descent =
        stored_int_considering_offsets (sf->pfminfo.hheaddescent_add,
                                        value, value_is_offset,
                                        scm_from_double (sf_ymin (sf)),
                                        scm_floor);
      break;
    case _hhea_LineGap:
      sf->pfminfo.linegap = scm_to_int (value);
      break;

    default:
      //// FIXME FIXME FIXME
      break;
    }
}

VISIBLE SCM
scm_view_hhea_table_set_x (SCM view, SCM key, SCM value, SCM value_is_offset)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  scm_c_view_hhea_table_set_x (view, _key, value, value_is_offset);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_c_view_hhea_table_ref (SCM view, const char *key, SCM value_is_offset)
{
  const char *who = "scm_c_view_hhea_table_ref";

  if (SCM_UNBNDP (value_is_offset))
    value_is_offset = SCM_BOOL_F;

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  SFDefaultOS2 (sf);

  SCM result = SCM_UNDEFINED;
  switch (hhea_key (who, key))
    {
    case _hhea_Ascender:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.hhead_ascent),
                                   sf->pfminfo.hheadascent_add,
                                   scm_from_double (sf_ymax (sf)));
      break;
    case _hhea_Descender:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.hhead_descent),
                                   sf->pfminfo.hheaddescent_add,
                                   scm_from_double (sf_ymin (sf)));
      break;
    case _hhea_LineGap:
      result = scm_from_int (sf->pfminfo.linegap);
      break;

    default:
      //// FIXME FIXME FIXME
      result = SCM_BOOL_F;
      break;
    }
  return result;
}

VISIBLE SCM
scm_view_hhea_table_ref (SCM view, SCM key, SCM value_is_offset)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM s = scm_c_view_hhea_table_ref (view, _key, value_is_offset);

  scm_dynwind_end ();

  return s;
}

VISIBLE SCM
scm_view_hhea_table_set_from_alist_x (SCM view, SCM lst)
{
  const char *who = "scm_view_hhea_table_set_from_alist_x";

  for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))
    {
      scm_c_assert_can_be_alist_link (who, lst, p);
      scm_view_hhea_table_set_x (view, SCM_CAAR (p), SCM_CDAR (p),
                                 SCM_UNDEFINED);
    }
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_hhea_table_to_alist (SCM view)
{
  SCM lst = SCM_EOL;
  for (_hhea_key_t k = 0; k < _hhea_SENTINEL; k++)
    {
      const char *key = hhea_key_table[_hhea_SENTINEL - 1 - k];
      lst = scm_acons (scm_from_latin1_string (key),
                       scm_c_view_hhea_table_ref (view, key, SCM_UNDEFINED),
                       lst);
    }
  return lst;
}

VISIBLE SCM
scm_view_hhea_table_keys (SCM view)
{
  SCM lst = SCM_EOL;
  for (_hhea_key_t k = 0; k < _hhea_SENTINEL; k++)
    {
      const char *key = hhea_key_table[_hhea_SENTINEL - 1 - k];
      lst = scm_cons (scm_from_latin1_string (key), lst);
    }
  return lst;
}

//-------------------------------------------------------------------------

void init_guile_fonts_hhea_table (void);

VISIBLE void
init_guile_fonts_hhea_table (void)
{
  scm_c_define_gsubr ("view:hhea-table-set!", 3, 1, 0,
                      scm_view_hhea_table_set_x);
  scm_c_define_gsubr ("view:hhea-table-ref", 2, 1, 0, scm_view_hhea_table_ref);
  scm_c_define_gsubr ("view:hhea-table-set-from-alist!", 2, 0, 0,
                      scm_view_hhea_table_set_from_alist_x);
  scm_c_define_gsubr ("view:hhea-table->alist", 1, 0, 0,
                      scm_view_hhea_table_to_alist);
  scm_c_define_gsubr ("view:hhea-table-keys", 1, 0, 0,
                      scm_view_hhea_table_keys);
}

//-------------------------------------------------------------------------
