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
scm_c_private_dict_set_x (SCM view, const char *key, const char *value)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  if (sf->private == NULL)
    sf->private = (struct psdict *) scm_calloc (sizeof (struct psdict));
  PSDictChangeEntry (sf->private, key, value);
}

VISIBLE SCM
scm_private_dict_set_x (SCM view, SCM key, SCM value)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_utf8_stringn (key, NULL);
  scm_dynwind_free (_key);

  SCM value_string = scm_private_dict_value_to_string (value);
  char *_value = scm_to_utf8_stringn (value_string, NULL);
  scm_dynwind_free (_value);

  scm_c_private_dict_set_x (view, _key, _value);

  scm_dynwind_end ();

  return SCM_UNSPECIFIED;
}

VISIBLE SCM
scm_private_dict_value_to_string (SCM value)
{
  return scm_call_1 (scm_c_public_ref (my_module, "private-dict-value->string"),
                     value);
}

VISIBLE const char *
scm_c_private_dict_ref (SCM view, const char *key)
{
  SplineFont *sf = (SplineFont *) scm_c_view_to_SplineFont (view);
  return PSDictHasEntry (sf->private, key);
}

VISIBLE SCM
scm_private_dict_ref (SCM view, SCM key)
{
  scm_dynwind_begin (0);

  char *_key = scm_to_utf8_stringn (key, NULL);
  scm_dynwind_free (_key);

  const char *s = scm_c_private_dict_ref (view, _key);

  scm_dynwind_end ();

  return (s == NULL) ? SCM_BOOL_F : scm_from_utf8_string (s);
}

//-------------------------------------------------------------------------

void init_guile_fonts_private_dict (void);

VISIBLE void
init_guile_fonts_private_dict (void)
{
  scm_c_define_gsubr ("private-dict-set!", 3, 0, 0, scm_private_dict_set_x);
  scm_c_define_gsubr ("private-dict-ref", 2, 0, 0, scm_private_dict_ref);
}

//-------------------------------------------------------------------------
