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

#include <libguile.h>
#include <sortsmill/initialized_global_constants.h>

VISIBLE void
scm_c_initialize_from_eval_string (SCM *proc, const char *s)
{
  *proc = scm_call_3 (scm_c_public_ref ("ice-9 eval-string", "eval-string"),
                      scm_from_utf8_string (s),
                      scm_from_latin1_keyword ("compile?"), SCM_BOOL_T);
}
