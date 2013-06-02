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

#include <sortsmill/guile/postscript.h>
#include <sortsmill/initialized_global_constants.h>

static const char *my_module = "sortsmill postscript";

//-------------------------------------------------------------------------

VISIBLE SCM
scm_to_postscript (SCM value)
{
  return scm_call_1 (scm_c_public_ref (my_module, "scm->postscript"), value);
}

VISIBLE SCM
scm_postscript_boolean_p (SCM s)
{
  return scm_call_1 (scm_c_public_ref (my_module, "postscript-boolean?"), s);
}

VISIBLE SCM
scm_postscript_to_boolean (SCM s)
{
  return scm_call_1 (scm_c_public_ref (my_module, "postscript->boolean"), s);
}

VISIBLE SCM
scm_postscript_number_list_p (SCM s)
{
  return scm_call_1 (scm_c_public_ref (my_module, "postscript-number-list?"),
                     s);
}

VISIBLE SCM
scm_postscript_to_number_list (SCM s)
{
  return scm_call_1 (scm_c_public_ref (my_module, "postscript->number-list"),
                     s);
}
