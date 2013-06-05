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

static const char my_module[] = "sortsmill fonts os2-table";

//-------------------------------------------------------------------------

typedef enum
{
  _os2_version = 0,
  //_os2_xAvgCharWidth, ← not supported here
  _os2_usWeightClass,
  _os2_usWidthClass,
  _os2_fsType,
  _os2_ySubscriptXSize,
  _os2_ySubscriptYSize,
  _os2_ySubscriptXOffset,
  _os2_ySubscriptYOffset,
  _os2_ySuperscriptXSize,
  _os2_ySuperscriptYSize,
  _os2_ySuperscriptXOffset,
  _os2_ySuperscriptYOffset,
  _os2_yStrikeoutSize,
  _os2_yStrikeoutPosition,
  _os2_sFamilyClass,
  _os2_panose,
  _os2_ulUnicodeRange,          /* Instead of _os2_ulUnicodeRange1,
                                   _os2_ulUnicodeRange2, _os2_ulUnicodeRange3,
                                   _os2_ulUnicodeRange4. */
  _os2_achVendID,
  //_os2_fsSelection, ← not supported here
  //_os2_usFirstCharIndex, ← not supported here
  //_os2_usLastCharIndex, ← not supported here
  _os2_sTypoAscender,
  _os2_sTypoDescender,
  _os2_sTypoLineGap,
  _os2_usWinAscent,
  _os2_usWinDescent,
  _os2_ulCodePageRange,         /* Instead of _os2_ulCodePageRange1,
                                   _os2_ulCodePageRange2. */
  //_os2_sxHeight, ← not supported here
  //_os2_sCapHeight, ← not supported here
  //_os2_usDefaultChar, ← not supported here
  //_os2_usBreakChar, ← not supported here
  //_os2_usMaxContext, ← not supported here

  // These ‘offset’ flags are FontForge/Sorts Mill Tools-specific.
  _os2_sTypoAscender_is_offset,
  _os2_sTypoDescender_is_offset,
  _os2_usWinAscent_is_offset,
  _os2_usWinDescent_is_offset,

  _os2_SENTINEL
} _os2_key_t;

static const char *os2_key_table[] = {
  [_os2_version] = "version",
  //[_os2_xAvgCharWidth] = "xAvgCharWidth", ← not supported here
  [_os2_usWeightClass] = "usWeightClass",
  [_os2_usWidthClass] = "usWidthClass",
  [_os2_fsType] = "fsType",
  [_os2_ySubscriptXSize] = "ySubscriptXSize",
  [_os2_ySubscriptYSize] = "ySubscriptYSize",
  [_os2_ySubscriptXOffset] = "ySubscriptXOffset",
  [_os2_ySubscriptYOffset] = "ySubscriptYOffset",
  [_os2_ySuperscriptXSize] = "ySuperscriptXSize",
  [_os2_ySuperscriptYSize] = "ySuperscriptYSize",
  [_os2_ySuperscriptXOffset] = "ySuperscriptXOffset",
  [_os2_ySuperscriptYOffset] = "ySuperscriptYOffset",
  [_os2_yStrikeoutSize] = "yStrikeoutSize",
  [_os2_yStrikeoutPosition] = "yStrikeoutPosition",
  [_os2_sFamilyClass] = "sFamilyClass",
  [_os2_panose] = "panose",
  [_os2_ulUnicodeRange] = "ulUnicodeRange",     /* Instead of
                                                   ulUnicodeRange1
                                                   ulUnicodeRange2,
                                                   ulUnicodeRange3,
                                                   ulUnicodeRange4. */
  [_os2_achVendID] = "achVendID",
  //[_os2_fsSelection] = "fsSelection", ← not supported here
  //[_os2_usFirstCharIndex] = "usFirstCharIndex", ← not supported here
  //[_os2_usLastCharIndex] = "usLastCharIndex", ← not supported here
  [_os2_sTypoAscender] = "sTypoAscender",
  [_os2_sTypoDescender] = "sTypoDescender",
  [_os2_sTypoLineGap] = "sTypoLineGap",
  [_os2_usWinAscent] = "usWinAscent",
  [_os2_usWinDescent] = "usWinDescent",
  [_os2_ulCodePageRange] = "ulCodePageRange",   /* Instead of
                                                   ulCodePageRange1,
                                                   ulCodePageRange2. */
  //[_os2_sxHeight] = "sxHeight", ← not supported here
  //[_os2_sCapHeight] = "sCapHeight", ← not supported here
  //[_os2_usDefaultChar] = "usDefaultChar", ← not supported here
  //[_os2_usBreakChar] = "usBreakChar", ← not supported here
  //[_os2_usMaxContext] = "usMaxContext" ← not supported here

  // These ‘offset’ flags are FontForge/Sorts Mill
  // Tools-specific. They correspond to the ‘Offset’ checkboxes for
  // the corresponding OS/2 table entries.
  [_os2_sTypoAscender_is_offset] = "sTypoAscender-is-offset",
  [_os2_sTypoDescender_is_offset] = "sTypoDescender-is-offset",
  [_os2_usWinAscent_is_offset] = "usWinAscent-is-offset",
  [_os2_usWinDescent_is_offset] = "usWinDescent-is-offset"
};

static void
raise_unsupported_key (const char *who, SCM key)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("unsupported OS/2 table key")),
      rnrs_make_irritants_condition (scm_list_1 (key))));
}

static _os2_key_t
os2_key (const char *who, const char *key_string)
{
  _os2_key_t k = 0;
  while (k < _os2_SENTINEL && strcmp (key_string, os2_key_table[k]) != 0)
    k++;
  if (k == _os2_SENTINEL)
    raise_unsupported_key (who, scm_from_utf8_string (key_string));
  return k;
}

static void
set_panose (const char *who, SplineFont *sf, SCM value)
{
  if (scm_is_false (scm_list_p (value))
      || scm_to_size_t (scm_length (value)) != 10)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected a list of length 10")),
        rnrs_make_irritants_condition (scm_list_1 (value))));
  SCM p = value;
  for (size_t i = 0; i < 10; i++)
    {
      sf->pfminfo.panose[i] = scm_to_int (SCM_CAR (p));
      p = SCM_CDR (p);
    }
}

static SCM
get_panose (SplineFont *sf)
{
  SCM p = SCM_EOL;
  for (size_t i = 0; i < 10; i++)
    p = scm_cons (scm_from_int (sf->pfminfo.panose[9 - i]), p);
  return p;
}

static void
set_unicode_range (const char *who, SplineFont *sf, SCM value)
{
  if (scm_is_false (value))
    sf->pfminfo.hasunicoderanges = false;
  else
    {
      if (scm_is_false (scm_list_p (value))
          || scm_to_size_t (scm_length (value)) != 4)
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_c_make_who_condition (who),
            rnrs_c_make_message_condition (_("expected #f or "
                                             "a list of length 4")),
            rnrs_make_irritants_condition (scm_list_1 (value))));
      SCM p = value;
      for (size_t i = 0; i < 4; i++)
        {
          sf->pfminfo.unicoderanges[i] = scm_to_uint32 (SCM_CAR (p));
          p = SCM_CDR (p);
        }
      sf->pfminfo.hasunicoderanges = true;
    }
}

static SCM
get_unicode_range (SplineFont *sf)
{
  if (!sf->pfminfo.hasunicoderanges)
    // Set the ranges, but _do not_ set sf->pfminfo.hasunicoderanges
    // as true. We will recompute the range each time, unless someone
    // has set them explicitly.
    OS2FigureUnicodeRanges (sf, sf->pfminfo.unicoderanges);

  SCM p = SCM_EOL;
  for (size_t i = 0; i < 4; i++)
    p = scm_cons (scm_from_uint32 (sf->pfminfo.unicoderanges[3 - i]), p);
  return p;
}

static void
set_code_page_range (const char *who, SplineFont *sf, SCM value)
{
  if (scm_is_false (value))
    sf->pfminfo.hascodepages = false;
  else
    {
      if (scm_is_false (scm_list_p (value))
          || scm_to_size_t (scm_length (value)) != 2)
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_c_make_who_condition (who),
            rnrs_c_make_message_condition (_("expected #f or "
                                             "a list of length 2")),
            rnrs_make_irritants_condition (scm_list_1 (value))));
      SCM p = value;
      for (size_t i = 0; i < 2; i++)
        {
          sf->pfminfo.codepages[i] = scm_to_uint32 (SCM_CAR (p));
          p = SCM_CDR (p);
        }
      sf->pfminfo.hascodepages = true;
    }
}

static SCM
get_code_page_range (SplineFont *sf)
{
  if (!sf->pfminfo.hascodepages)
    // Set the ranges, but _do not_ set sf->pfminfo.hascodepages as
    // true. We will recompute the range each time, unless someone has
    // set them explicitly.
    OS2FigureCodePages (sf, sf->pfminfo.codepages);

  SCM p = SCM_EOL;
  for (size_t i = 0; i < 2; i++)
    p = scm_cons (scm_from_uint32 (sf->pfminfo.codepages[1 - i]), p);
  return p;
}

static void
set_vendor_id (const char *who, SplineFont *sf, SCM value)
{
  value = scm_string_trim_right (value, SCM_UNDEFINED, SCM_UNDEFINED,
                                 SCM_UNDEFINED);

  scm_dynwind_begin (0);

  char *_value = scm_to_latin1_stringn (value, NULL);
  scm_dynwind_free (_value);

  const size_t n = strlen (_value);

  if (4 < n)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected a space-padded "
                                         "string of length "
                                         "at most 4")),
        rnrs_make_irritants_condition (scm_list_1 (value))));

  sf->pfminfo.os2_vendor[0] = (n < 1) ? '\0' : _value[0];
  sf->pfminfo.os2_vendor[1] = (n < 2) ? '\0' : _value[1];
  sf->pfminfo.os2_vendor[2] = (n < 3) ? '\0' : _value[2];
  sf->pfminfo.os2_vendor[3] = (n < 4) ? '\0' : _value[3];

  scm_dynwind_end ();
}

static int
nul_to_sp (int c)
{
  return (c == '\0') ? ' ' : c;
}

static SCM
get_vendor_id (SplineFont *sf)
{
  char buffer[4];
  buffer[0] = nul_to_sp (sf->pfminfo.os2_vendor[0]);
  buffer[1] = nul_to_sp (sf->pfminfo.os2_vendor[1]);
  buffer[2] = nul_to_sp (sf->pfminfo.os2_vendor[2]);
  buffer[3] = nul_to_sp (sf->pfminfo.os2_vendor[3]);
  return scm_from_latin1_stringn (buffer, 4);
}

VISIBLE void
scm_c_view_os2_table_set_x (SCM view, const char *key, SCM value,
                            SCM value_is_offset)
{
  const char *who = "scm_c_view_os2_table_set_x";

  if (SCM_UNBNDP (value_is_offset))
    value_is_offset = SCM_BOOL_F;

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  SFDefaultOS2 (sf);

  switch (os2_key (who, key))
    {
    case _os2_version:
      sf->os2_version = scm_to_int (value);
      break;
    case _os2_usWeightClass:
      sf->pfminfo.weight = scm_to_int (value);
      break;
    case _os2_usWidthClass:
      sf->pfminfo.width = scm_to_int (value);
      break;
    case _os2_fsType:
      sf->pfminfo.fstype = scm_to_int (value);
      break;
    case _os2_ySubscriptXSize:
      sf->pfminfo.os2_subxsize = scm_to_int (value);
      break;
    case _os2_ySubscriptYSize:
      sf->pfminfo.os2_subysize = scm_to_int (value);
      break;
    case _os2_ySubscriptXOffset:
      sf->pfminfo.os2_subxoff = scm_to_int (value);
      break;
    case _os2_ySubscriptYOffset:
      sf->pfminfo.os2_subyoff = scm_to_int (value);
      break;
    case _os2_ySuperscriptXSize:
      sf->pfminfo.os2_supxsize = scm_to_int (value);
      break;
    case _os2_ySuperscriptYSize:
      sf->pfminfo.os2_supysize = scm_to_int (value);
      break;
    case _os2_ySuperscriptXOffset:
      sf->pfminfo.os2_supxoff = scm_to_int (value);
      break;
    case _os2_ySuperscriptYOffset:
      sf->pfminfo.os2_supyoff = scm_to_int (value);
      break;
    case _os2_yStrikeoutSize:
      sf->pfminfo.os2_strikeysize = scm_to_int (value);
      break;
    case _os2_yStrikeoutPosition:
      sf->pfminfo.os2_strikeypos = scm_to_int (value);
      break;
    case _os2_sFamilyClass:
      sf->pfminfo.os2_family_class = scm_to_int (value);
      break;
    case _os2_panose:
      set_panose (who, sf, value);
      break;
    case _os2_ulUnicodeRange:
      set_unicode_range (who, sf, value);
      break;
    case _os2_achVendID:
      set_vendor_id (who, sf, value);
      break;
    case _os2_sTypoAscender:
      sf->pfminfo.os2_typoascent =
        stored_int_considering_offsets (sf->pfminfo.typoascent_add,
                                        value, value_is_offset,
                                        scm_from_int (sf->ascent), scm_ceiling);
      break;
    case _os2_sTypoDescender:
      sf->pfminfo.os2_typodescent =
        stored_int_considering_offsets (sf->pfminfo.typodescent_add,
                                        value, value_is_offset,
                                        scm_from_int (-sf->descent), scm_floor);
      break;
    case _os2_sTypoLineGap:
      sf->pfminfo.os2_typolinegap = scm_to_int (value);
      break;
    case _os2_usWinAscent:
      sf->pfminfo.os2_winascent =
        stored_int_considering_offsets (sf->pfminfo.winascent_add,
                                        value, value_is_offset,
                                        scm_from_double (sf_ymax (sf)),
                                        scm_ceiling);
      break;
    case _os2_usWinDescent:
      sf->pfminfo.os2_windescent =
        stored_int_considering_offsets (sf->pfminfo.windescent_add,
                                        value, value_is_offset,
                                        scm_from_double (-sf_ymin (sf)),
                                        scm_ceiling);
      break;
    case _os2_ulCodePageRange:
      set_code_page_range (who, sf, value);
      break;

    case _os2_sTypoAscender_is_offset:
      sf->pfminfo.typoascent_add = scm_is_true (value);
      break;
    case _os2_sTypoDescender_is_offset:
      sf->pfminfo.typodescent_add = scm_is_true (value);
      break;
    case _os2_usWinAscent_is_offset:
      sf->pfminfo.winascent_add = scm_is_true (value);
      break;
    case _os2_usWinDescent_is_offset:
      sf->pfminfo.windescent_add = scm_is_true (value);
      break;
    }
}

VISIBLE SCM
scm_view_os2_table_set_x (SCM view, SCM key, SCM value, SCM value_is_offset)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  scm_c_view_os2_table_set_x (view, _key, value, value_is_offset);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_c_view_os2_table_ref (SCM view, const char *key, SCM value_is_offset)
{
  const char *who = "scm_c_view_os2_table_ref";

  if (SCM_UNBNDP (value_is_offset))
    value_is_offset = SCM_BOOL_F;

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);

  SFDefaultOS2 (sf);

  SCM result = SCM_UNDEFINED;
  switch (os2_key (who, key))
    {
    case _os2_version:
      result = scm_from_int (sf->os2_version);
      break;
    case _os2_usWeightClass:
      result = scm_from_int (sf->pfminfo.weight);
      break;
    case _os2_usWidthClass:
      result = scm_from_int (sf->pfminfo.width);
      break;
    case _os2_fsType:
      result = scm_from_int (sf->pfminfo.fstype);
      break;
    case _os2_ySubscriptXSize:
      result = scm_from_int (sf->pfminfo.os2_subxsize);
      break;
    case _os2_ySubscriptYSize:
      result = scm_from_int (sf->pfminfo.os2_subysize);
      break;
    case _os2_ySubscriptXOffset:
      result = scm_from_int (sf->pfminfo.os2_subxoff);
      break;
    case _os2_ySubscriptYOffset:
      result = scm_from_int (sf->pfminfo.os2_subyoff);
      break;
    case _os2_ySuperscriptXSize:
      result = scm_from_int (sf->pfminfo.os2_supxsize);
      break;
    case _os2_ySuperscriptYSize:
      result = scm_from_int (sf->pfminfo.os2_supysize);
      break;
    case _os2_ySuperscriptXOffset:
      result = scm_from_int (sf->pfminfo.os2_supxoff);
      break;
    case _os2_ySuperscriptYOffset:
      result = scm_from_int (sf->pfminfo.os2_supyoff);
      break;
    case _os2_yStrikeoutSize:
      result = scm_from_int (sf->pfminfo.os2_strikeysize);
      break;
    case _os2_yStrikeoutPosition:
      result = scm_from_int (sf->pfminfo.os2_strikeypos);
      break;
    case _os2_sFamilyClass:
      result = scm_from_int (sf->pfminfo.os2_family_class);
      break;
    case _os2_panose:
      result = get_panose (sf);
      break;
    case _os2_ulUnicodeRange:
      result = get_unicode_range (sf);
      break;
    case _os2_achVendID:
      result = get_vendor_id (sf);
      break;
    case _os2_sTypoAscender:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.os2_typoascent),
                                   sf->pfminfo.typoascent_add,
                                   scm_from_int (sf->ascent));
      break;
    case _os2_sTypoDescender:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.os2_typodescent),
                                   sf->pfminfo.typodescent_add,
                                   scm_from_int (-sf->descent));
      break;
    case _os2_sTypoLineGap:
      result = scm_from_int (sf->pfminfo.os2_typolinegap);
      break;
    case _os2_usWinAscent:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.os2_winascent),
                                   sf->pfminfo.winascent_add,
                                   scm_from_double (sf_ymax (sf)));
      break;
    case _os2_usWinDescent:
      result =
        value_considering_offsets (value_is_offset,
                                   scm_from_int (sf->pfminfo.os2_windescent),
                                   sf->pfminfo.windescent_add,
                                   scm_from_double (-sf_ymin (sf)));
      break;
    case _os2_ulCodePageRange:
      result = get_code_page_range (sf);
      break;

    case _os2_sTypoAscender_is_offset:
      result = scm_from_bool (sf->pfminfo.typoascent_add);
      break;
    case _os2_sTypoDescender_is_offset:
      result = scm_from_bool (sf->pfminfo.typodescent_add);
      break;
    case _os2_usWinAscent_is_offset:
      result = scm_from_bool (sf->pfminfo.winascent_add);
      break;
    case _os2_usWinDescent_is_offset:
      result = scm_from_bool (sf->pfminfo.windescent_add);
      break;
    }
  return result;
}

VISIBLE SCM
scm_view_os2_table_ref (SCM view, SCM key, SCM value_is_offset)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM s = scm_c_view_os2_table_ref (view, _key, value_is_offset);

  scm_dynwind_end ();

  return s;
}

VISIBLE SCM
scm_view_os2_table_set_from_alist_x (SCM view, SCM lst)
{
  const char *who = "scm_view_os2_table_set_from_alist_x";

  for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))
    {
      scm_c_assert_can_be_alist_link (who, lst, p);
      scm_view_os2_table_set_x (view, SCM_CAAR (p), SCM_CDAR (p),
                                SCM_UNDEFINED);
    }
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_os2_table_to_alist (SCM view)
{
  SCM lst = SCM_EOL;
  for (_os2_key_t k = 0; k < _os2_SENTINEL; k++)
    {
      const char *key = os2_key_table[_os2_SENTINEL - 1 - k];
      lst = scm_acons (scm_from_latin1_string (key),
                       scm_c_view_os2_table_ref (view, key, SCM_UNDEFINED),
                       lst);
    }
  return lst;
}

VISIBLE SCM
scm_view_os2_table_keys (SCM view)
{
  SCM lst = SCM_EOL;
  for (_os2_key_t k = 0; k < _os2_SENTINEL; k++)
    {
      const char *key = os2_key_table[_os2_SENTINEL - 1 - k];
      lst = scm_cons (scm_from_latin1_string (key), lst);
    }
  return lst;
}

//-------------------------------------------------------------------------

void init_guile_fonts_os2_table (void);

VISIBLE void
init_guile_fonts_os2_table (void)
{
  scm_c_define_gsubr ("view:os2-table-set!", 3, 1, 0, scm_view_os2_table_set_x);
  scm_c_define_gsubr ("view:os2-table-ref", 2, 1, 0, scm_view_os2_table_ref);
  scm_c_define_gsubr ("view:os2-table-set-from-alist!", 2, 0, 0,
                      scm_view_os2_table_set_from_alist_x);
  scm_c_define_gsubr ("view:os2-table->alist", 1, 0, 0,
                      scm_view_os2_table_to_alist);
  scm_c_define_gsubr ("view:os2-table-keys", 1, 0, 0, scm_view_os2_table_keys);
}

//-------------------------------------------------------------------------
