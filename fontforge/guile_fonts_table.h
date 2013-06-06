// -*- coding: utf-8 -*-

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

#ifndef _GUILE_FONTS_TABLE_H
#define _GUILE_FONTS_TABLE_H

// Support for Postscript dictionary-related and OT table-related
// modules.


#define _SCM_VIEW_TABLE_SET(NAME, C_SET_X)              \
  SCM                                                   \
  NAME (SCM view, SCM key, SCM value)                   \
  {                                                     \
    scm_dynwind_begin (0);                              \
                                                        \
    char *_key = scm_to_latin1_stringn (key, NULL);     \
    scm_dynwind_free (_key);                            \
                                                        \
    C_SET_X (view, _key, value);                        \
                                                        \
    scm_dynwind_end ();                                 \
                                                        \
    return SCM_UNSPECIFIED;                             \
  }

#define _SCM_VIEW_TABLE_SET_PS(NAME, C_SET_X)                   \
  SCM                                                           \
  NAME (SCM view, SCM key, SCM value)                           \
  {                                                             \
    scm_dynwind_begin (0);                                      \
                                                                \
    char *_key = scm_to_latin1_stringn (key, NULL);             \
    scm_dynwind_free (_key);                                    \
                                                                \
    SCM value_string = scm_to_postscript (value);               \
    char *_value = scm_to_latin1_stringn (value_string, NULL);  \
    scm_dynwind_free (_value);                                  \
                                                                \
    C_SET_X (view, _key, _value);                               \
                                                                \
    scm_dynwind_end ();                                         \
                                                                \
    return SCM_UNSPECIFIED;                                     \
  }

#define _SCM_VIEW_TABLE_SET2(NAME, C_SET_X)                     \
  SCM                                                           \
  NAME (SCM view, SCM key, SCM value, SCM value_is_offset)      \
  {                                                             \
    scm_dynwind_begin (0);                                      \
                                                                \
    char *_key = scm_to_latin1_stringn (key, NULL);             \
    scm_dynwind_free (_key);                                    \
                                                                \
    C_SET_X (view, _key, value, value_is_offset);               \
                                                                \
    scm_dynwind_end ();                                         \
                                                                \
    return SCM_UNSPECIFIED;                                     \
  }

#define _SCM_VIEW_TABLE_REF(NAME, C_REF)                \
  SCM                                                   \
  NAME (SCM view, SCM key)                              \
  {                                                     \
    scm_dynwind_begin (0);                              \
                                                        \
    char *_key = scm_to_latin1_stringn (key, NULL);     \
    scm_dynwind_free (_key);                            \
                                                        \
    SCM s = C_REF (view, _key);                         \
                                                        \
    scm_dynwind_end ();                                 \
                                                        \
    return s;                                           \
  }

#define _SCM_VIEW_TABLE_REF2(NAME, C_REF)               \
  SCM                                                   \
  NAME (SCM view, SCM key, SCM value_is_offset)         \
  {                                                     \
    scm_dynwind_begin (0);                              \
                                                        \
    char *_key = scm_to_latin1_stringn (key, NULL);     \
    scm_dynwind_free (_key);                            \
                                                        \
    SCM s = C_REF (view, _key, value_is_offset);        \
                                                        \
    scm_dynwind_end ();                                 \
                                                        \
    return s;                                           \
  }

#define _SCM_VIEW_TABLE_SET_FROM_ALIST(NAME, SET_X)             \
  SCM                                                           \
  NAME (SCM view, SCM lst)                                      \
  {                                                             \
    const char *who = #NAME;                                    \
                                                                \
    for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))        \
      {                                                         \
        scm_c_assert_can_be_alist_link (who, lst, p);           \
        SET_X (view, SCM_CAAR (p), SCM_CDAR (p));               \
      }                                                         \
    return SCM_UNSPECIFIED;                                     \
  }

#define _SCM_VIEW_TABLE_SET_FROM_ALIST2(NAME, SET_X)                    \
  SCM                                                                   \
  NAME (SCM view, SCM lst)                                              \
  {                                                                     \
    const char *who = #NAME;                                            \
                                                                        \
    for (SCM p = lst; !scm_is_null (p); p = SCM_CDR (p))                \
      {                                                                 \
        scm_c_assert_can_be_alist_link (who, lst, p);                   \
        SET_X (view, SCM_CAAR (p), SCM_CDAR (p), SCM_UNDEFINED);        \
      }                                                                 \
    return SCM_UNSPECIFIED;                                             \
  }

#define _SCM_VIEW_TABLE_TO_ALIST(NAME, TABLE, KEY_T, SENTINEL, REF)     \
  SCM                                                                   \
  NAME (SCM view)                                                       \
  {                                                                     \
    SCM lst = SCM_EOL;                                                  \
    for (KEY_T k = 0; k < SENTINEL; k++)                                \
      {                                                                 \
        const char *key = TABLE[SENTINEL - 1 - k];                      \
        lst = scm_acons (scm_from_latin1_string (key),                  \
                         REF (view, key), lst);                         \
      }                                                                 \
    return lst;                                                         \
  }

#define _SCM_VIEW_TABLE_TO_ALIST2(NAME, TABLE, KEY_T, SENTINEL, REF)    \
  SCM                                                                   \
  NAME (SCM view)                                                       \
  {                                                                     \
    SCM lst = SCM_EOL;                                                  \
    for (KEY_T k = 0; k < SENTINEL; k++)                                \
      {                                                                 \
        const char *key = TABLE[SENTINEL - 1 - k];                      \
        lst = scm_acons (scm_from_latin1_string (key),                  \
                         REF (view, key, SCM_UNDEFINED), lst);          \
      }                                                                 \
    return lst;                                                         \
  }

#define _SCM_VIEW_TABLE_KEYS(NAME, TABLE, KEY_T, SENTINEL)      \
  SCM                                                           \
  NAME (SCM view)                                               \
  {                                                             \
    SCM lst = SCM_EOL;                                          \
    for (KEY_T k = 0; k < SENTINEL; k++)                        \
      {                                                         \
        const char *key = TABLE[SENTINEL - 1 - k];              \
        lst = scm_cons (scm_from_latin1_string (key), lst);     \
      }                                                         \
    return lst;                                                 \
  }

#endif  // _GUILE_FONTS_TABLE_H
