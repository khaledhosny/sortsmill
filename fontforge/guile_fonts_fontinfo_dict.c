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
#include <splinefont.h>
#include <intl.h>

static const char my_module[] = "sortsmill fonts fontinfo-dict";

//-------------------------------------------------------------------------

typedef enum
{
  _fontinfo_version = 0,
  _fontinfo_Notice,
  _fontinfo_FullName,
  _fontinfo_FamilyName,
  _fontinfo_Weight,
  _fontinfo_ItalicAngle,
  _fontinfo_IsFixedPitch,
  _fontinfo_UnderlinePosition,
  _fontinfo_UnderlineThickness,
  _fontinfo_SENTINEL
} _fontinfo_key_t;

static const char *fontinfo_key_table[] = {
  [_fontinfo_version] = "version",
  [_fontinfo_Notice] = "Notice",
  [_fontinfo_FullName] = "FullName",
  [_fontinfo_FamilyName] = "FamilyName",
  [_fontinfo_Weight] = "Weight",
  [_fontinfo_ItalicAngle] = "ItalicAngle",
  [_fontinfo_IsFixedPitch] = "IsFixedPitch",
  [_fontinfo_UnderlinePosition] = "UnderlinePosition",
  [_fontinfo_UnderlineThickness] = "UnderlineThickness"
};

static _fontinfo_key_t
fontinfo_key (const char *who, const char *key_string)
{
  _fontinfo_key_t k = 0;
  while (k < _fontinfo_SENTINEL
         && strcmp (key_string, fontinfo_key_table[k]) != 0)
    k++;
  if (k == _fontinfo_SENTINEL)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("unsupported /FontInfo dict key")),
        rnrs_make_irritants_condition
        (scm_list_1 (scm_from_utf8_string (key_string)))));
  return k;
}

VISIBLE void
scm_c_view_fontinfo_dict_set_x (SCM view, const char *key, const char *value)
{
  const char *who = "scm_c_view_fontinfo_dict_set_x";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  switch (fontinfo_key (who, key))
    {
    case _fontinfo_version:
      free (sf->version);
      sf->version = xstrdup (value);
      break;
    case _fontinfo_Notice:
      free (sf->copyright);
      sf->copyright = xstrdup (value);
      break;
    case _fontinfo_FullName:
      free (sf->fullname);
      sf->fullname = xstrdup (value);
      break;
    case _fontinfo_FamilyName:
      free (sf->familyname);
      sf->familyname = xstrdup (value);
      break;
    case _fontinfo_Weight:
      free (sf->weight);
      sf->weight = xstrdup (value);
      break;
    case _fontinfo_ItalicAngle:
      sf->italicangle = scm_to_double (scm_c_postscript_to_number (value));
      break;
    case _fontinfo_IsFixedPitch:
      // Ignore this field. We determine fixed-pitchedness ourselves.
      break;
    case _fontinfo_UnderlinePosition:
      sf->upos = scm_to_double (scm_c_postscript_to_number (value));
      break;
    case _fontinfo_UnderlineThickness:
      sf->uwidth = scm_to_double (scm_c_postscript_to_number (value));
      break;
    }
}

VISIBLE SCM
scm_view_fontinfo_dict_set_x (SCM view, SCM key, SCM value)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM value_string = scm_to_postscript (value);
  char *_value = scm_to_latin1_stringn (value_string, NULL);
  scm_dynwind_free (_value);

  scm_c_view_fontinfo_dict_set_x (view, _key, _value);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

static SCM
scm_from_string_or_null (const char *s)
{
  return scm_from_latin1_string ((s == NULL) ? "" : s);
}

VISIBLE SCM
scm_c_view_fontinfo_dict_ref (SCM view, const char *key)
{
  const char *who = "scm_c_view_fontinfo_dict_ref";

  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  SCM result = SCM_UNDEFINED;
  switch (fontinfo_key (who, key))
    {
    case _fontinfo_version:
      result = scm_from_string_or_null (sf->version);
      break;
    case _fontinfo_Notice:
      result = scm_from_string_or_null (sf->copyright);
      break;
    case _fontinfo_FullName:
      result = scm_from_string_or_null (sf->fullname);
      break;
    case _fontinfo_FamilyName:
      result = scm_from_string_or_null (sf->familyname);
      break;
    case _fontinfo_Weight:
      result = scm_from_string_or_null (sf->weight);
      break;
    case _fontinfo_ItalicAngle:
      result = scm_number_to_string (scm_from_double (sf->italicangle),
                                     scm_from_int (10));
      break;
    case _fontinfo_IsFixedPitch:
      result =
        scm_from_latin1_string ((CIDOneWidth (sf) == -1) ? "false" : "true");
      break;
    case _fontinfo_UnderlinePosition:
      result = scm_number_to_string (scm_from_double (sf->upos),
                                     scm_from_int (10));
      break;
    case _fontinfo_UnderlineThickness:
      result = scm_number_to_string (scm_from_double (sf->uwidth),
                                     scm_from_int (10));
      break;
    }
  return result;
}

VISIBLE SCM
scm_view_fontinfo_dict_ref (SCM view, SCM key)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_latin1_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM s = scm_c_view_fontinfo_dict_ref (view, _key);

  scm_dynwind_end ();

  return s;
}

VISIBLE SCM
scm_view_fontinfo_dict_set_from_alist_x (SCM view, SCM lst)
{
  const char *who = "scm_view_fontinfo_dict_set_from_alist_x";

  for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))
    {
      scm_c_assert_can_be_alist_link (who, lst, p);
      scm_view_fontinfo_dict_set_x (view, SCM_CAAR (p), SCM_CDAR (p));
    }
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_fontinfo_dict_to_alist (SCM view)
{
  SCM lst = SCM_EOL;
  for (_fontinfo_key_t k = 0; k < _fontinfo_SENTINEL; k++)
    {
      const char *key = fontinfo_key_table[_fontinfo_SENTINEL - 1 - k];
      lst = scm_acons (scm_from_latin1_string (key),
                       scm_c_view_fontinfo_dict_ref (view, key), lst);
    }
  return lst;
}

VISIBLE SCM
scm_view_fontinfo_dict_keys (SCM view)
{
  SCM lst = SCM_EOL;
  for (_fontinfo_key_t k = 0; k < _fontinfo_SENTINEL; k++)
    {
      const char *key = fontinfo_key_table[_fontinfo_SENTINEL - 1 - k];
      lst = scm_cons (scm_from_latin1_string (key), lst);
    }
  return lst;
}

#define _SCM_FONTINFO_FIELD_STRING_REF(FIELDNAME)               \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    return (scm_view_fontinfo_dict_ref                          \
            (view, scm_from_latin1_string (#FIELDNAME)));       \
  }

#define _SCM_FONTINFO_FIELD_NUMBER_REF(FIELDNAME)               \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    SCM value = (scm_view_fontinfo_dict_ref                     \
                 (view, scm_from_latin1_string (#FIELDNAME)));  \
    return (scm_is_true (scm_postscript_number_p (value))) ?    \
      scm_postscript_to_number (value) : value;                 \
  }

// WARNING: This does not distinguish between a missing entry and an
// entry set to "false".
#define _SCM_FONTINFO_FIELD_BOOLEAN_REF(FIELDNAME)              \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    SCM value = (scm_view_fontinfo_dict_ref                     \
                 (view, scm_from_latin1_string (#FIELDNAME)));  \
    return (scm_is_true (scm_postscript_boolean_p (value))) ?   \
      scm_postscript_to_boolean (value) : value;                \
  }

VISIBLE _SCM_FONTINFO_FIELD_STRING_REF (version);
VISIBLE _SCM_FONTINFO_FIELD_STRING_REF (Notice);
VISIBLE _SCM_FONTINFO_FIELD_STRING_REF (FullName);
VISIBLE _SCM_FONTINFO_FIELD_STRING_REF (FamilyName);
VISIBLE _SCM_FONTINFO_FIELD_STRING_REF (Weight);

VISIBLE _SCM_FONTINFO_FIELD_NUMBER_REF (ItalicAngle);
VISIBLE _SCM_FONTINFO_FIELD_NUMBER_REF (UnderlinePosition);
VISIBLE _SCM_FONTINFO_FIELD_NUMBER_REF (UnderlineThickness);

VISIBLE _SCM_FONTINFO_FIELD_BOOLEAN_REF (IsFixedPitch);

#define _SCM_FONTINFO_FIELD_SET_X(FIELDNAME)                            \
  SCM                                                                   \
  scm_view_##FIELDNAME##_set_x (SCM view, SCM value)                    \
  {                                                                     \
    return (scm_view_fontinfo_dict_set_x                                \
            (view, scm_from_latin1_string (#FIELDNAME), value));        \
  }

VISIBLE _SCM_FONTINFO_FIELD_SET_X (version);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (Notice);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (FullName);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (FamilyName);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (Weight);

VISIBLE _SCM_FONTINFO_FIELD_SET_X (ItalicAngle);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (UnderlinePosition);
VISIBLE _SCM_FONTINFO_FIELD_SET_X (UnderlineThickness);

//-------------------------------------------------------------------------

void init_guile_fonts_fontinfo_dict (void);

VISIBLE void
init_guile_fonts_fontinfo_dict (void)
{
  scm_c_define_gsubr ("view:fontinfo-dict-set!", 3, 0, 0,
                      scm_view_fontinfo_dict_set_x);
  scm_c_define_gsubr ("view:fontinfo-dict-ref", 2, 0, 0,
                      scm_view_fontinfo_dict_ref);
  scm_c_define_gsubr ("view:fontinfo-dict-set-from-alist!", 2, 0, 0,
                      scm_view_fontinfo_dict_set_from_alist_x);
  scm_c_define_gsubr ("view:fontinfo-dict->alist", 1, 0, 0,
                      scm_view_fontinfo_dict_to_alist);
  scm_c_define_gsubr ("view:fontinfo-dict-keys", 1, 0, 0,
                      scm_view_fontinfo_dict_keys);

  scm_c_define_gsubr ("view:version-ref", 1, 0, 0, scm_view_version_ref);
  scm_c_define_gsubr ("view:Notice-ref", 1, 0, 0, scm_view_Notice_ref);
  scm_c_define_gsubr ("view:FullName-ref", 1, 0, 0, scm_view_FullName_ref);
  scm_c_define_gsubr ("view:FamilyName-ref", 1, 0, 0, scm_view_FamilyName_ref);
  scm_c_define_gsubr ("view:Weight-ref", 1, 0, 0, scm_view_Weight_ref);
  scm_c_define_gsubr ("view:ItalicAngle-ref", 1, 0, 0,
                      scm_view_ItalicAngle_ref);
  scm_c_define_gsubr ("view:UnderlinePosition-ref", 1, 0, 0,
                      scm_view_UnderlinePosition_ref);
  scm_c_define_gsubr ("view:UnderlineThickness-ref", 1, 0, 0,
                      scm_view_UnderlineThickness_ref);
  scm_c_define_gsubr ("view:IsFixedPitch-ref", 1, 0, 0,
                      scm_view_IsFixedPitch_ref);

  scm_c_define_gsubr ("view:version-set!", 2, 0, 0, scm_view_version_set_x);
  scm_c_define_gsubr ("view:Notice-set!", 2, 0, 0, scm_view_Notice_set_x);
  scm_c_define_gsubr ("view:FullName-set!", 2, 0, 0, scm_view_FullName_set_x);
  scm_c_define_gsubr ("view:FamilyName-set!", 2, 0, 0,
                      scm_view_FamilyName_set_x);
  scm_c_define_gsubr ("view:Weight-set!", 2, 0, 0, scm_view_Weight_set_x);
  scm_c_define_gsubr ("view:ItalicAngle-set!", 2, 0, 0,
                      scm_view_ItalicAngle_set_x);
  scm_c_define_gsubr ("view:UnderlinePosition-set!", 2, 0, 0,
                      scm_view_UnderlinePosition_set_x);
  scm_c_define_gsubr ("view:UnderlineThickness-set!", 2, 0, 0,
                      scm_view_UnderlineThickness_set_x);
}

//-------------------------------------------------------------------------
