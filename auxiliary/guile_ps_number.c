#include <config.h>

// Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

#include <c-ctype.h>
#include <sortsmill/core.h>
#include <sortsmill/guile.h>
#include <intl.h>

//-------------------------------------------------------------------------

static SCM
scm_c_from_postscript_integer (const char *s)
{
  return scm_string_to_number (scm_from_utf8_string (s), scm_from_int (10));
}

static SCM
scm_c_from_postscript_real (const char *s)
{
  return scm_string_to_number (scm_from_utf8_string (s), scm_from_int (10));
}

static SCM
scm_c_from_postscript_radix_number (const char *s)
{
  size_t i = 0;
  do
    i++;
  while (c_isdigit (s[i]));
  SCM radix = scm_string_to_number (scm_from_utf8_stringn (s, i),
                                    scm_from_int (10));
  SCM numeral = scm_from_utf8_string (&s[i + 1]);
  return scm_string_to_number (numeral, radix);
}

VISIBLE SCM
scm_c_postscript_to_number (const char *s)
{
  SCM number = SCM_UNDEFINED;
  if (is_postscript_integer (s))
    number = scm_c_from_postscript_integer (s);
  else if (is_postscript_real (s))
    number = scm_c_from_postscript_real (s);
  else if (is_postscript_radix_number (s))
    number = scm_c_from_postscript_radix_number (s);
  else
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition ("scm_c_from_postscript_number"),
        rnrs_c_make_message_condition (_("not a valid PostScript numeral")),
        rnrs_make_irritants_condition (scm_list_1 (scm_from_utf8_string (s)))));
  return number;
}

VISIBLE SCM
scm_postscript_to_number (SCM s)
{
  scm_dynwind_begin (0);

  char *_s = scm_to_utf8_stringn (s, NULL);
  scm_dynwind_free (_s);

  SCM number = scm_c_postscript_to_number (_s);

  scm_dynwind_end ();

  return number;
}

VISIBLE SCM
scm_postscript_number_p (SCM s)
{
  SCM result = SCM_BOOL_F;
  if (scm_is_string (s))
    {
      scm_dynwind_begin (0);

      char *_s = scm_to_utf8_stringn (s, NULL);
      scm_dynwind_free (_s);

      result = scm_from_bool (is_postscript_number (_s));

      scm_dynwind_end ();
    }
  return result;
}

//-------------------------------------------------------------------------

void init_guile_ps_number (void);

VISIBLE void
init_guile_ps_number (void)
{
  scm_c_define_gsubr ("postscript->number", 1, 0, 0, scm_postscript_to_number);
  scm_c_define_gsubr ("postscript-number?", 1, 0, 0, scm_postscript_number_p);
}

//-------------------------------------------------------------------------
