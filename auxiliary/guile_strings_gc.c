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

#include <sortsmill/guile/strings/gc.h>
#include <sortsmill/xgc.h>

//-------------------------------------------------------------------------

VISIBLE char *
scm_c_gc_grabstr_utf8 (SCM string)
{
  char *s = scm_to_utf8_stringn (string, NULL);
  return x_gc_grabstr (s);
}

VISIBLE SCM
scm_gc_grabstr_utf8 (SCM string)
{
  return scm_from_pointer (scm_c_gc_grabstr_utf8 (string), NULL);
}

//-------------------------------------------------------------------------

void init_sortsmill_guile_strings_gc (void);

VISIBLE void
init_sortsmill_guile_strings_gc (void)
{
  scm_c_define_gsubr ("gc-grabstr-utf8", 1, 0, 0, scm_gc_grabstr_utf8);
}

//-------------------------------------------------------------------------
