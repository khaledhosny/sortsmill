#include <config.h>             // -*- coding: utf-8 -*-

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

#include <sortsmill/math.h>
#include <sortsmill/guile.h>
#include <intl.h>

//-------------------------------------------------------------------------

/*
static VISIBLE SCM
scm_c_bezout_matrix (SCM poly1, SCM poly2,
                     SCM sum (SCM, SCM),
                     SCM product (SCM, SCM))
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("array is not hypercubic")),
      rnrs_make_irritants_condition (scm_list_1 (array))));
}
*/

//-------------------------------------------------------------------------

void init_math_polyspline_implicit (void);

VISIBLE void
init_math_polyspline_implicit (void)
{
  // Nothing here yet.
}

//-------------------------------------------------------------------------
