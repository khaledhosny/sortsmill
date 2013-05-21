#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <stdint.h>
#include <libguile.h>
#include <splinefont.h>
#include <intl.h>
#include <sortsmill/guile.h>

//-------------------------------------------------------------------------

static SCM
scm_AnchorPointType_to_type_symbol (SCM type, SCM who)
{
  SCM symb = SCM_UNSPECIFIED;
  const int _type = scm_to_int (type);
  switch (_type)
    {
    case at_mark:
      symb = scm_symbol__mark ();
      break;
    case at_basechar:
      symb = scm_symbol__base ();
      break;
    case at_baselig:
      symb = scm_symbol__ligature ();
      break;
    case at_basemark:
      symb = scm_symbol__base_mark ();
      break;
    case at_centry:
      symb = scm_symbol__entry ();
      break;
    case at_cexit:
      symb = scm_symbol__exit ();
      break;

    default:
      {
        if (SCM_UNBNDP (who))
          who = scm_from_latin1_string ("scm_AnchorPointType_to_type_symbol");
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_make_who_condition (who),
            rnrs_c_make_message_condition (_("unrecognized AnchorPointType "
                                             "value")),
            rnrs_make_irritants_condition (scm_list_1 (type))));
      }
      break;
    }

  return symb;
}

static SCM
scm_type_symbol_to_AnchorPointType (SCM symb, SCM who)
{
  int type = INT_MIN;
  if (scm_is_eq (symb, scm_symbol__mark ()))
    type = at_mark;
  else if (scm_is_eq (symb, scm_symbol__base ()))
    type = at_basechar;
  else if (scm_is_eq (symb, scm_symbol__ligature ()))
    type = at_baselig;
  else if (scm_is_eq (symb, scm_symbol__base_mark ()))
    type = at_basemark;
  else if (scm_is_eq (symb, scm_symbol__entry ()))
    type = at_centry;
  else if (scm_is_eq (symb, scm_symbol__exit ()))
    type = at_cexit;
  else
    {
      if (SCM_UNBNDP (who))
        who = scm_from_latin1_string ("scm_type_symbol_to_AnchorPointType");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_make_who_condition (who),
          rnrs_c_make_message_condition (_("unrecognized anchor point type")),
          rnrs_make_irritants_condition (scm_list_1 (symb))));
    }
  return scm_from_int (type);
}

// FIXME: Try to use garbage-collected allocation for the management
// of anchor point lists. Note: after this is done, we can get rid of
// the ‘catch’ calls in the Guile code.
static SCM
scm_free_AnchorPoint_linked_list (SCM ap_ptr)
{
  AnchorPointsFree (scm_to_pointer (ap_ptr));
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

void init_guile_internals_anchors (void);

VISIBLE void
init_guile_internals_anchors (void)
{
  scm_c_define_gsubr ("AnchorPointType->type-symbol", 1, 1, 0,
                      scm_AnchorPointType_to_type_symbol);
  scm_c_define_gsubr ("type-symbol->AnchorPointType", 1, 1, 0,
                      scm_type_symbol_to_AnchorPointType);
  scm_c_define_gsubr ("free-AnchorPoint-linked-list", 1, 0, 0,
                      scm_free_AnchorPoint_linked_list);
}

//-------------------------------------------------------------------------
