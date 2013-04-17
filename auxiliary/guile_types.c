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

#include <sortsmill/guile.h>
#include <intl.h>

// Generate instances of inline functions.
VISIBLE void scm_c_assert_can_be_list_link (const char *who, SCM lst, SCM p);
VISIBLE void scm_c_assert_can_be_alist_link (const char *who, SCM lst, SCM p);
VISIBLE void scm_c_assert_list_does_not_end_here (const char *who, SCM lst,
                                                  SCM p);

VISIBLE void
scm_c_raise_is_not_a_list (const char *who, SCM lst)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("expected a list")),
      rnrs_make_irritants_condition (scm_list_1 (lst))));
}

VISIBLE void
scm_c_raise_is_not_an_alist (const char *who, SCM lst)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("expected an association list")),
      rnrs_make_irritants_condition (scm_list_1 (lst))));
}

VISIBLE void
scm_c_raise_list_ends_prematurely (const char *who, SCM lst)
{
  rnrs_raise_condition
    (scm_list_4
     (rnrs_make_assertion_violation (),
      rnrs_c_make_who_condition (who),
      rnrs_c_make_message_condition (_("list ends prematurely")),
      rnrs_make_irritants_condition (scm_list_1 (lst))));
}
