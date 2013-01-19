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

#include <sortsmillff/guile/rnrs_conditions.h>
#include <intl.h>

VISIBLE SCM
rnrs_condition (SCM condition_list)
{
  return scm_apply_0 (scm_c_public_ref ("rnrs conditions", "condition"),
                      condition_list);
}

VISIBLE SCM
rnrs_raise_condition (SCM condition_list)
{
  return scm_call_1 (scm_c_public_ref ("rnrs exceptions", "raise"),
                     rnrs_condition (condition_list));
}

VISIBLE SCM
rnrs_make_error (void)
{
  return scm_call_0 (scm_c_public_ref ("rnrs conditions", "make-error"));
}

VISIBLE SCM
rnrs_make_assertion_violation (void)
{
  return
    scm_call_0 (scm_c_public_ref ("rnrs conditions",
                                  "make-assertion-violation"));
}

VISIBLE SCM
rnrs_make_who_condition (SCM who)
{
  return scm_call_1 (scm_c_public_ref ("rnrs conditions", "make-who-condition"),
                     who);
}

VISIBLE SCM
rnrs_c_make_who_condition (const char *who)
{
  return rnrs_make_who_condition (scm_from_utf8_string (who));
}

VISIBLE SCM
rnrs_make_message_condition (SCM message)
{
  return
    scm_call_1 (scm_c_public_ref ("rnrs conditions", "make-message-condition"),
                message);
}

VISIBLE SCM
rnrs_c_make_message_condition (const char *message)
{
  return rnrs_make_message_condition (scm_from_utf8_string (message));
}

VISIBLE SCM
rnrs_make_irritants_condition (SCM irritants)
{
  return scm_call_1 (scm_c_public_ref ("rnrs conditions",
                                       "make-irritants-condition"), irritants);
}
