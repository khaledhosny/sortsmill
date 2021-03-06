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

#include <sortsmill/guile/math/gsl/error.h>

VISIBLE SCM
scm_gsl_errno_to_symbol (SCM errval)
{
  return
    scm_call_1 (scm_c_public_ref ("sortsmill math gsl", "gsl-errno->symbol"),
                errval);
}

VISIBLE SCM
scm_c_gsl_errno_to_symbol (int errval)
{
  return scm_gsl_errno_to_symbol (scm_from_int (errval));
}

VISIBLE SCM
scm_raise_gsl_error (SCM arguments)
{
  return
    scm_apply_0 (scm_c_public_ref ("sortsmill math gsl", "raise-gsl-error"),
                 arguments);
}

VISIBLE void
scm_gsl_error_handler_for_raising_a_gsl_error (const char *reason,
                                               const char *file,
                                               int line, int gsl_errno)
{
  scm_raise_gsl_error (scm_list_4 (scm_from_locale_string (reason),
                                   scm_from_locale_string (file),
                                   scm_from_int (line),
                                   scm_from_int (gsl_errno)));
}
