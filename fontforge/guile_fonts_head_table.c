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
#include <guile_fonts_table.h>
#include <splinefont.h>
#include <intl.h>
#include <utype.h>

static const char my_module[] = "sortsmill fonts head-table";

//-------------------------------------------------------------------------

typedef enum
{
  //_head_version, ← not supported here
  //_head_fontRevision, ← not supported here
  //_head_checkSumAdjustment, ← not supported here
  //_head_magicNumber, ← not supported here
  _head_flags,
  _head_unitsPerEm,
  _head_created,
  _head_modified,
  //_head_xMin, ← not supported here
  //_head_yMin, ← not supported here
  //_head_xMax, ← not supported here
  //_head_yMax, ← not supported here
  //_head_macStyle, ← not supported here
  //_head_lowestRecPPEM, ← not supported here
  //_head_fontDirectionHint, ← not supported here
  //_head_indexToLocFormat, ← not supported here
  //_head_glyphDataFormat, ← not supported here
  _head_SENTINEL
} _head_key_t;

static const char *head_key_table[] = {
  //[_head_version] = "version", ← not supported here
  //[_head_fontRevision] = "fontRevision", ← not supported here
  //[_head_checkSumAdjustment] = "checkSumAdjustment", ← not supported here
  //[_head_magicNumber] = "magicNumber", ← not supported here
  [_head_flags] = "flags",
  [_head_unitsPerEm] = "unitsPerEm",
  [_head_created] = "created",
  [_head_modified] = "modified"
    //[_head_xMin] = "xMin", ← not supported here
    //[_head_yMin] = "yMin", ← not supported here
    //[_head_xMax] = "xMax", ← not supported here
    //[_head_yMax] = "yMax", ← not supported here
    //[_head_macStyle] = "macStyle", ← not supported here
    //[_head_lowestRecPPEM] = "lowestRecPPEM", ← not supported here
    //[_head_fontDirectionHint] = "fontDirectionHint", ← not supported here
    //[_head_indexToLocFormat] = "indexToLocFormat", ← not supported here
    //[_head_glyphDataFormat] = "glyphDataFormat" ← not supported here
};

static void
raise_unsupported_key (const char *who, SCM key)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("unsupported head table key")),
      rnrs_make_irritants_condition (scm_list_1 (key))));
}

static _head_key_t
head_key (const char *who, const char *key_string)
{
  _head_key_t k = 0;
  while (k < _head_SENTINEL && strcmp (key_string, head_key_table[k]) != 0)
    k++;
  if (k == _head_SENTINEL)
    raise_unsupported_key (who, scm_from_utf8_string (key_string));
  return k;
}

VISIBLE void
scm_c_view_head_table_set_x (SCM view, const char *key, SCM value)
{
  const char *who = "scm_c_view_head_table_set_x";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  switch (head_key (who, key))
    {
    case _head_flags:
      // Most bits are ignored here.
      scm_optimized_for_cleartype_set_x (view, scm_logbit_p (scm_from_int (13),
                                                             value));
      break;
    case _head_created:
      sf->creationtime = scm_to_intmax (value);
      break;
    case _head_modified:
      sf->modificationtime = scm_to_intmax (value);
      break;
    case _head_unitsPerEm:
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition
          (_("setting this value (in this way) is unsupported")),
          rnrs_make_irritants_condition
          (scm_list_1 (scm_from_latin1_string (key)))));
      break;
    }
}

static void
analyze_directionality (SplineFont *sf, bool *lr, bool *rl, bool *arabic)
{
  *lr = false;
  *rl = false;
  *arabic = false;
  for (size_t i = 0; i < sf->glyphcnt; i++)
    {
      SplineChar *sc = sf->glyphs[i];
      if (SCWorthOutputting (sc))
        {
          int uni = sc->unicodeenc;
          if (SCRightToLeft (sc))
            *rl = true;
          else if ((uni != -1 && uni < 0x10000 && islefttoright (uni))
                   || (uni >= 0x10300 && uni < 0x107ff))
            *lr = true;
          if (SCScriptFromUnicode (sc) == CHR ('a', 'r', 'a', 'b'))
            *arabic = true;
        }
    }
}

VISIBLE SCM
scm_c_view_head_table_ref (SCM view, const char *key)
{
  const char *who = "scm_c_view_head_table_ref";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  SCM result = SCM_UNDEFINED;
  switch (head_key (who, key))
    {
    case _head_flags:
      {
        bool lr;
        bool rl;
        bool arabic;
        analyze_directionality (sf, &lr, &rl, &arabic);
        result =
          scm_from_uint16 (head_table_flags (sf, ff_none, NULL, arabic, rl));
      }
      break;
    case _head_created:
      result = scm_from_intmax (sf->creationtime);
      break;
    case _head_modified:
      result = scm_from_intmax (sf->modificationtime);
      break;
    case _head_unitsPerEm:
      // This duplicates functionality of (sortsmill fonts general).
      result = scm_from_int (sf->ascent + sf->descent);
      break;
    }
  return result;
}

VISIBLE _SCM_VIEW_TABLE_SET (scm_view_head_table_set_x,
                             scm_c_view_head_table_set_x);

VISIBLE _SCM_VIEW_TABLE_REF (scm_view_head_table_ref,
                             scm_c_view_head_table_ref);

VISIBLE _SCM_VIEW_TABLE_SET_FROM_ALIST (scm_view_head_table_set_from_alist_x,
                                        scm_view_head_table_set_x);

VISIBLE _SCM_VIEW_TABLE_TO_ALIST (scm_view_head_table_to_alist,
                                  head_key_table, _head_key_t,
                                  _head_SENTINEL, scm_c_view_head_table_ref);

VISIBLE _SCM_VIEW_TABLE_KEYS (scm_view_head_table_keys, head_key_table,
                              _head_key_t, _head_SENTINEL);

VISIBLE SCM
scm_optimized_for_cleartype_set_x (SCM view, SCM value)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  sf->head_optimized_for_cleartype = scm_is_true (value);
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_optimized_for_cleartype_p (SCM view)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  return scm_from_bool (sf->head_optimized_for_cleartype);
}

//-------------------------------------------------------------------------

void init_guile_fonts_head_table (void);

VISIBLE void
init_guile_fonts_head_table (void)
{
  scm_c_define_gsubr ("view:head-table-set!", 3, 1, 0,
                      scm_view_head_table_set_x);
  scm_c_define_gsubr ("view:head-table-ref", 2, 1, 0, scm_view_head_table_ref);
  scm_c_define_gsubr ("view:head-table-set-from-alist!", 2, 0, 0,
                      scm_view_head_table_set_from_alist_x);
  scm_c_define_gsubr ("view:head-table->alist", 1, 0, 0,
                      scm_view_head_table_to_alist);
  scm_c_define_gsubr ("view:head-table-keys", 1, 0, 0,
                      scm_view_head_table_keys);
  scm_c_define_gsubr ("view:optimized-for-cleartype-set!", 2, 0, 0,
                      scm_optimized_for_cleartype_set_x);
  scm_c_define_gsubr ("view:optimized-for-cleartype?", 1, 0, 0,
                      scm_optimized_for_cleartype_p);
}

//-------------------------------------------------------------------------
