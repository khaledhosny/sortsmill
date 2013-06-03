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

static const char my_module[] = "sortsmill fonts private-dict";

//-------------------------------------------------------------------------

VISIBLE void
scm_c_view_private_dict_set_x (SCM view, const char *key, const char *value)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  if (sf->private == NULL)
    sf->private = (struct psdict *) scm_calloc (sizeof (struct psdict));
  PSDictChangeEntry (sf->private, key, value);
}

VISIBLE SCM
scm_view_private_dict_set_x (SCM view, SCM key, SCM value)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_utf8_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM value_string = scm_to_postscript (value);
  char *_value = scm_to_utf8_stringn (value_string, NULL);
  scm_dynwind_free (_value);

  scm_c_view_private_dict_set_x (view, _key, _value);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE const char *
scm_c_view_private_dict_ref (SCM view, const char *key)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  return PSDictHasEntry (sf->private, key);
}

VISIBLE SCM
scm_view_private_dict_ref (SCM view, SCM key)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_utf8_stringn (key, NULL);
  scm_dynwind_free (_key);

  const char *s = scm_c_view_private_dict_ref (view, _key);

  scm_dynwind_end ();

  return (s == NULL) ? SCM_BOOL_F : scm_from_utf8_string (s);
}

VISIBLE void
scm_c_view_private_dict_remove_x (SCM view, const char *key)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  PSDictRemoveEntry (sf->private, key);
}

VISIBLE SCM
scm_view_private_dict_remove_x (SCM view, SCM key)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_utf8_stringn (key, NULL);
  scm_dynwind_free (_key);

  scm_c_view_private_dict_remove_x (view, _key);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_private_dict_clear_x (SCM view)
{
  SCM keys = scm_view_private_dict_keys (view);
  for (SCM p = keys; !scm_is_null (p); p = SCM_CDR (p))
    scm_view_private_dict_remove_x (view, SCM_CAR (p));
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_private_dict_set_from_alist_x (SCM view, SCM lst)
{
  const char *who = "scm_view_private_dict_set_from_alist_x";

  for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))
    {
      scm_c_assert_can_be_alist_link (who, lst, p);
      scm_view_private_dict_set_x (view, SCM_CAAR (p), SCM_CDAR (p));
    }
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_alist_to_private_dict_x (SCM view, SCM lst)
{
  scm_view_private_dict_clear_x (view);
  scm_view_private_dict_set_from_alist_x (view, lst);
  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_view_private_dict_to_alist (SCM view)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  SCM lst = SCM_EOL;
  if (sf->private != NULL)
    for (size_t i = 0; i < sf->private->next; ++i)
      {
        const char *key = sf->private->keys[sf->private->next - 1 - i];
        const char *value = sf->private->values[sf->private->next - 1 - i];
        lst = scm_acons (scm_from_utf8_string (key),
                         scm_from_utf8_string (value), lst);
      }
  return lst;
}

VISIBLE SCM
scm_view_private_dict_keys (SCM view)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  SCM lst = SCM_EOL;
  if (sf->private != NULL)
    for (size_t i = 0; i < sf->private->next; ++i)
      {
        const char *key = sf->private->keys[sf->private->next - 1 - i];
        lst = scm_cons (scm_from_utf8_string (key), lst);
      }
  return lst;
}

#define _SCM_PRIVATE_FIELD_NUMBER_LIST_REF(FIELDNAME)                   \
  SCM                                                                   \
  scm_view_##FIELDNAME##_ref (SCM view)                                 \
  {                                                                     \
    SCM value = (scm_view_private_dict_ref                              \
                 (view, scm_from_latin1_string (#FIELDNAME)));          \
    return (scm_is_true (scm_postscript_number_list_p (value))) ?       \
      scm_postscript_to_number_list (value) : value;                    \
  }

#define _SCM_PRIVATE_FIELD_NUMBER_REF(FIELDNAME)                \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    SCM value = (scm_view_private_dict_ref                      \
                 (view, scm_from_latin1_string (#FIELDNAME)));  \
    return (scm_is_true (scm_postscript_number_p (value))) ?    \
      scm_postscript_to_number (value) : value;                 \
  }

// WARNING: This does not distinguish between a missing entry and an
// entry set to "false".
#define _SCM_PRIVATE_FIELD_BOOLEAN_REF(FIELDNAME)               \
  SCM                                                           \
  scm_view_##FIELDNAME##_ref (SCM view)                         \
  {                                                             \
    SCM value = (scm_view_private_dict_ref                      \
                 (view, scm_from_latin1_string (#FIELDNAME)));  \
    return (scm_is_true (scm_postscript_boolean_p (value))) ?   \
      scm_postscript_to_boolean (value) : value;                \
  }

VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (BlueValues);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (OtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (FamilyBlues);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (FamilyOtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (StdHW);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (StdVW);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (StemSnapH);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_LIST_REF (StemSnapV);

VISIBLE _SCM_PRIVATE_FIELD_NUMBER_REF (BlueFuzz);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_REF (BlueScale);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_REF (BlueShift);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_REF (ExpansionFactor);
VISIBLE _SCM_PRIVATE_FIELD_NUMBER_REF (LanguageGroup);

// WARNING: These do not distinguish between a missing entry and an
// entry set to "false".
VISIBLE _SCM_PRIVATE_FIELD_BOOLEAN_REF (ForceBold);
VISIBLE _SCM_PRIVATE_FIELD_BOOLEAN_REF (RndStemUp);

#define _SCM_PRIVATE_FIELD_SET_X(FIELDNAME)                             \
  SCM                                                                   \
  scm_view_##FIELDNAME##_set_x (SCM view, SCM value)                    \
  {                                                                     \
    return (scm_view_private_dict_set_x                                 \
            (view, scm_from_latin1_string (#FIELDNAME), value));        \
  }

VISIBLE _SCM_PRIVATE_FIELD_SET_X (BlueValues);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (OtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (FamilyBlues);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (FamilyOtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (StdHW);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (StdVW);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (StemSnapH);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (StemSnapV);

VISIBLE _SCM_PRIVATE_FIELD_SET_X (BlueFuzz);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (BlueScale);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (BlueShift);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (ExpansionFactor);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (LanguageGroup);

VISIBLE _SCM_PRIVATE_FIELD_SET_X (ForceBold);
VISIBLE _SCM_PRIVATE_FIELD_SET_X (RndStemUp);

#define _SCM_PRIVATE_FIELD_REMOVE_X(FIELDNAME)                  \
  SCM                                                           \
  scm_view_##FIELDNAME##_remove_x (SCM view)                    \
  {                                                             \
    return (scm_view_private_dict_remove_x                      \
            (view, scm_from_latin1_string (#FIELDNAME)));       \
  }

VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (BlueValues);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (OtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (FamilyBlues);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (FamilyOtherBlues);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (StdHW);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (StdVW);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (StemSnapH);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (StemSnapV);

VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (BlueFuzz);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (BlueScale);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (BlueShift);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (ExpansionFactor);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (LanguageGroup);

VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (ForceBold);
VISIBLE _SCM_PRIVATE_FIELD_REMOVE_X (RndStemUp);

//-------------------------------------------------------------------------

void init_guile_fonts_private_dict (void);

VISIBLE void
init_guile_fonts_private_dict (void)
{
  scm_c_define_gsubr ("view:private-dict-set!", 3, 0, 0,
                      scm_view_private_dict_set_x);
  scm_c_define_gsubr ("view:private-dict-ref", 2, 0, 0,
                      scm_view_private_dict_ref);
  scm_c_define_gsubr ("view:private-dict-remove!", 2, 0, 0,
                      scm_view_private_dict_remove_x);
  scm_c_define_gsubr ("view:private-dict-clear!", 1, 0, 0,
                      scm_view_private_dict_clear_x);
  scm_c_define_gsubr ("view:private-dict-set-from-alist!", 2, 0, 0,
                      scm_view_private_dict_set_from_alist_x);
  scm_c_define_gsubr ("view:alist->private-dict!", 2, 0, 0,
                      scm_view_alist_to_private_dict_x);
  scm_c_define_gsubr ("view:private-dict->alist", 1, 0, 0,
                      scm_view_private_dict_to_alist);
  scm_c_define_gsubr ("view:private-dict-keys", 1, 0, 0,
                      scm_view_private_dict_keys);

  scm_c_define_gsubr ("view:BlueValues-ref", 1, 0, 0, scm_view_BlueValues_ref);
  scm_c_define_gsubr ("view:OtherBlues-ref", 1, 0, 0, scm_view_OtherBlues_ref);
  scm_c_define_gsubr ("view:FamilyBlues-ref", 1, 0, 0,
                      scm_view_FamilyBlues_ref);
  scm_c_define_gsubr ("view:FamilyOtherBlues-ref", 1, 0, 0,
                      scm_view_FamilyOtherBlues_ref);
  scm_c_define_gsubr ("view:StdHW-ref", 1, 0, 0, scm_view_StdHW_ref);
  scm_c_define_gsubr ("view:StdVW-ref", 1, 0, 0, scm_view_StdVW_ref);
  scm_c_define_gsubr ("view:StemSnapH-ref", 1, 0, 0, scm_view_StemSnapH_ref);
  scm_c_define_gsubr ("view:StemSnapV-ref", 1, 0, 0, scm_view_StemSnapV_ref);
  scm_c_define_gsubr ("view:BlueFuzz-ref", 1, 0, 0, scm_view_BlueFuzz_ref);
  scm_c_define_gsubr ("view:BlueScale-ref", 1, 0, 0, scm_view_BlueScale_ref);
  scm_c_define_gsubr ("view:BlueShift-ref", 1, 0, 0, scm_view_BlueShift_ref);
  scm_c_define_gsubr ("view:ExpansionFactor-ref", 1, 0, 0,
                      scm_view_ExpansionFactor_ref);
  scm_c_define_gsubr ("view:LanguageGroup-ref", 1, 0, 0,
                      scm_view_LanguageGroup_ref);
  scm_c_define_gsubr ("view:ForceBold-ref", 1, 0, 0, scm_view_ForceBold_ref);
  scm_c_define_gsubr ("view:RndStemUp-ref", 1, 0, 0, scm_view_RndStemUp_ref);

  scm_c_define_gsubr ("view:BlueValues-set!", 2, 0, 0,
                      scm_view_BlueValues_set_x);
  scm_c_define_gsubr ("view:OtherBlues-set!", 2, 0, 0,
                      scm_view_OtherBlues_set_x);
  scm_c_define_gsubr ("view:FamilyBlues-set!", 2, 0, 0,
                      scm_view_FamilyBlues_set_x);
  scm_c_define_gsubr ("view:FamilyOtherBlues-set!", 2, 0, 0,
                      scm_view_FamilyOtherBlues_set_x);
  scm_c_define_gsubr ("view:StdHW-set!", 2, 0, 0, scm_view_StdHW_set_x);
  scm_c_define_gsubr ("view:StdVW-set!", 2, 0, 0, scm_view_StdVW_set_x);
  scm_c_define_gsubr ("view:StemSnapH-set!", 2, 0, 0, scm_view_StemSnapH_set_x);
  scm_c_define_gsubr ("view:StemSnapV-set!", 2, 0, 0, scm_view_StemSnapV_set_x);
  scm_c_define_gsubr ("view:BlueFuzz-set!", 2, 0, 0, scm_view_BlueFuzz_set_x);
  scm_c_define_gsubr ("view:BlueScale-set!", 2, 0, 0, scm_view_BlueScale_set_x);
  scm_c_define_gsubr ("view:BlueShift-set!", 2, 0, 0, scm_view_BlueShift_set_x);
  scm_c_define_gsubr ("view:ExpansionFactor-set!", 2, 0, 0,
                      scm_view_ExpansionFactor_set_x);
  scm_c_define_gsubr ("view:LanguageGroup-set!", 2, 0, 0,
                      scm_view_LanguageGroup_set_x);
  scm_c_define_gsubr ("view:ForceBold-set!", 2, 0, 0, scm_view_ForceBold_set_x);
  scm_c_define_gsubr ("view:RndStemUp-set!", 2, 0, 0, scm_view_RndStemUp_set_x);

  scm_c_define_gsubr ("view:BlueValues-remove!", 1, 0, 0,
                      scm_view_BlueValues_remove_x);
  scm_c_define_gsubr ("view:OtherBlues-remove!", 1, 0, 0,
                      scm_view_OtherBlues_remove_x);
  scm_c_define_gsubr ("view:FamilyBlues-remove!", 1, 0, 0,
                      scm_view_FamilyBlues_remove_x);
  scm_c_define_gsubr ("view:FamilyOtherBlues-remove!", 1, 0, 0,
                      scm_view_FamilyOtherBlues_remove_x);
  scm_c_define_gsubr ("view:StdHW-remove!", 1, 0, 0, scm_view_StdHW_remove_x);
  scm_c_define_gsubr ("view:StdVW-remove!", 1, 0, 0, scm_view_StdVW_remove_x);
  scm_c_define_gsubr ("view:StemSnapH-remove!", 1, 0, 0,
                      scm_view_StemSnapH_remove_x);
  scm_c_define_gsubr ("view:StemSnapV-remove!", 1, 0, 0,
                      scm_view_StemSnapV_remove_x);
  scm_c_define_gsubr ("view:BlueFuzz-remove!", 1, 0, 0,
                      scm_view_BlueFuzz_remove_x);
  scm_c_define_gsubr ("view:BlueScale-remove!", 1, 0, 0,
                      scm_view_BlueScale_remove_x);
  scm_c_define_gsubr ("view:BlueShift-remove!", 1, 0, 0,
                      scm_view_BlueShift_remove_x);
  scm_c_define_gsubr ("view:ExpansionFactor-remove!", 1, 0, 0,
                      scm_view_ExpansionFactor_remove_x);
  scm_c_define_gsubr ("view:LanguageGroup-remove!", 1, 0, 0,
                      scm_view_LanguageGroup_remove_x);
  scm_c_define_gsubr ("view:ForceBold-remove!", 1, 0, 0,
                      scm_view_ForceBold_remove_x);
  scm_c_define_gsubr ("view:RndStemUp-remove!", 1, 0, 0,
                      scm_view_RndStemUp_remove_x);
}

//-------------------------------------------------------------------------
