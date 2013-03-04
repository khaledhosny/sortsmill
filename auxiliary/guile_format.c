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

#include <sortsmill/guile/format.h>

VISIBLE SCM
scm_format (SCM destination, SCM message, SCM args)
{
  return scm_apply_2 (scm_c_public_ref ("ice-9 format", "format"),
                      destination, message, args);
}

VISIBLE
SCM scm_c_utf8_format (SCM destination, const char *message, SCM args)
{
  return scm_format (destination, scm_from_utf8_string (message), args);
}

VISIBLE
SCM scm_c_locale_format (SCM destination, const char *message, SCM args)
{
  return scm_format (destination, scm_from_locale_string (message), args);
}

VISIBLE SCM
scm_sformat (SCM message, SCM args)
{
  return scm_format (SCM_BOOL_F, message, args);
}

VISIBLE
SCM scm_c_utf8_sformat (const char *message, SCM args)
{
  return scm_sformat (scm_from_utf8_string (message), args);
}

VISIBLE
SCM scm_c_locale_sformat (const char *message, SCM args)
{
  return scm_sformat (scm_from_locale_string (message), args);
}
