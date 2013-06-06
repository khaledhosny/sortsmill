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
#include <guile_fonts_table.h>
#include <splinefont.h>
#include <intl.h>

static const char my_module[] = "sortsmill fonts t1font-dict";

//-------------------------------------------------------------------------

typedef enum
{
  _t1font_FontName = 0,
  _t1font_PaintType,
  _t1font_StrokeWidth,
  _t1font_SENTINEL
} _t1font_key_t;

static const char *t1font_key_table[] = {
  [_t1font_FontName] = "FontName",
  [_t1font_PaintType] = "PaintType",
  [_t1font_StrokeWidth] = "StrokeWidth"
};

static _t1font_key_t
t1font_key (const char *who, const char *key_string)
{
  _t1font_key_t k = 0;
  while (k < _t1font_SENTINEL && strcmp (key_string, t1font_key_table[k]) != 0)
    k++;
  if (k == _t1font_SENTINEL)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("unsupported Type1 font dict key")),
        rnrs_make_irritants_condition
        (scm_list_1 (scm_from_utf8_string (key_string)))));
  return k;
}

VISIBLE void
scm_c_view_t1font_dict_set_x (SCM view, const char *key, const char *value)
{
  const char *who = "scm_c_view_t1font_dict_set_x";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  switch (t1font_key (who, key))
    {
    case _t1font_FontName:
      free (sf->fontname);
      sf->fontname = xstrdup (value);
      break;
    case _t1font_PaintType:
      sf->strokedfont = (scm_to_int (scm_c_postscript_to_number (value)) == 2);
      break;
    case _t1font_StrokeWidth:
      sf->strokewidth = scm_to_double (scm_c_postscript_to_number (value));
      break;
    }
}

static SCM
scm_from_string_or_null (const char *s)
{
  return scm_from_latin1_string ((s == NULL) ? "" : s);
}

VISIBLE SCM
scm_c_view_t1font_dict_ref (SCM view, const char *key)
{
  const char *who = "scm_c_view_t1font_dict_ref";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  SCM result = SCM_UNDEFINED;
  switch (t1font_key (who, key))
    {
    case _t1font_FontName:
      result = scm_from_string_or_null (sf->fontname);
      break;
    case _t1font_PaintType:
      result = scm_number_to_string (scm_from_int (sf->strokedfont ? 2 : 0),
                                     scm_from_int (10));
      break;
    case _t1font_StrokeWidth:
      result = scm_number_to_string (scm_from_double (sf->strokewidth),
                                     scm_from_int (10));
      break;
    }
  return result;
}

VISIBLE _SCM_VIEW_TABLE_SET_PS (scm_view_t1font_dict_set_x,
                                scm_c_view_t1font_dict_set_x);

VISIBLE _SCM_VIEW_TABLE_REF (scm_view_t1font_dict_ref,
                             scm_c_view_t1font_dict_ref);

VISIBLE _SCM_VIEW_TABLE_SET_FROM_ALIST (scm_view_t1font_dict_set_from_alist_x,
                                        scm_view_t1font_dict_set_x);

VISIBLE _SCM_VIEW_TABLE_TO_ALIST (scm_view_t1font_dict_to_alist,
                                  t1font_key_table, _t1font_key_t,
                                  _t1font_SENTINEL, scm_c_view_t1font_dict_ref);

VISIBLE _SCM_VIEW_TABLE_KEYS (scm_view_t1font_dict_keys, t1font_key_table,
                              _t1font_key_t, _t1font_SENTINEL);

#define _SCM_T1FONT_FIELD_STRING_REF(FIELDNAME)                 \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    return (scm_view_t1font_dict_ref                            \
            (view, scm_from_latin1_string (#FIELDNAME)));       \
  }

#define _SCM_T1FONT_FIELD_NUMBER_REF(FIELDNAME)                 \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    SCM value = (scm_view_t1font_dict_ref                       \
                 (view, scm_from_latin1_string (#FIELDNAME)));  \
    return (scm_is_true (scm_postscript_number_p (value))) ?    \
      scm_postscript_to_number (value) : value;                 \
  }

VISIBLE _SCM_T1FONT_FIELD_STRING_REF (FontName);
VISIBLE _SCM_T1FONT_FIELD_NUMBER_REF (PaintType);
VISIBLE _SCM_T1FONT_FIELD_NUMBER_REF (StrokeWidth);

#define _SCM_T1FONT_FIELD_SET_X(FIELDNAME)                              \
  SCM                                                                   \
  scm_view_##FIELDNAME##_set_x (SCM view, SCM value)                    \
  {                                                                     \
    return (scm_view_t1font_dict_set_x                                  \
            (view, scm_from_latin1_string (#FIELDNAME), value));        \
  }

VISIBLE _SCM_T1FONT_FIELD_SET_X (FontName);
VISIBLE _SCM_T1FONT_FIELD_SET_X (PaintType);
VISIBLE _SCM_T1FONT_FIELD_SET_X (StrokeWidth);

VISIBLE SCM
scm_view_stroked_font_p (SCM view)
{
  return scm_num_eq_p (scm_view_PaintType_ref (view), scm_from_int (2));
}

VISIBLE SCM
scm_view_stroked_font_set_x (SCM view, SCM value)
{
  scm_view_PaintType_set_x (view, scm_from_int ((scm_is_true (value)) ? 2 : 0));
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

void init_guile_fonts_t1font_dict (void);

VISIBLE void
init_guile_fonts_t1font_dict (void)
{
  scm_c_define_gsubr ("view:t1font-dict-set!", 3, 0, 0,
                      scm_view_t1font_dict_set_x);
  scm_c_define_gsubr ("view:t1font-dict-ref", 2, 0, 0,
                      scm_view_t1font_dict_ref);
  scm_c_define_gsubr ("view:t1font-dict-set-from-alist!", 2, 0, 0,
                      scm_view_t1font_dict_set_from_alist_x);
  scm_c_define_gsubr ("view:t1font-dict->alist", 1, 0, 0,
                      scm_view_t1font_dict_to_alist);
  scm_c_define_gsubr ("view:t1font-dict-keys", 1, 0, 0,
                      scm_view_t1font_dict_keys);

  scm_c_define_gsubr ("view:FontName-ref", 1, 0, 0, scm_view_FontName_ref);
  scm_c_define_gsubr ("view:PaintType-ref", 1, 0, 0, scm_view_PaintType_ref);
  scm_c_define_gsubr ("view:StrokeWidth-ref", 1, 0, 0,
                      scm_view_StrokeWidth_ref);

  scm_c_define_gsubr ("view:FontName-set!", 2, 0, 0, scm_view_FontName_set_x);
  scm_c_define_gsubr ("view:PaintType-set!", 2, 0, 0, scm_view_PaintType_set_x);
  scm_c_define_gsubr ("view:StrokeWidth-set!", 2, 0, 0,
                      scm_view_StrokeWidth_set_x);

  scm_c_define_gsubr ("view:stroked-font?", 1, 0, 0, scm_view_stroked_font_p);
  scm_c_define_gsubr ("view:stroked-font-set!", 2, 0, 0,
                      scm_view_stroked_font_set_x);
}

//-------------------------------------------------------------------------
