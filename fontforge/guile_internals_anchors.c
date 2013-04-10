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

#include <stdint.h>
#include <libguile.h>
#include <splinefont.h>
#include <intl.h>
#include <sortsmill/guile.h>
#include <sortsmill/initialized_global_constants.h>

//-------------------------------------------------------------------------

// FIXME: This seems quite reusable, though perhaps it could have a
// better name.
static void
scm_c_initialize_from_eval_string (SCM *proc, const char *s)
{
  *proc = scm_call_3 (scm_c_public_ref ("ice-9 eval-string", "eval-string"),
                      scm_from_utf8_string (s),
                      scm_from_latin1_keyword ("compile?"), SCM_BOOL_T);
}

//-------------------------------------------------------------------------

#define _MY_SCM_SYMBOL(C_NAME, SCM_NAME)                        \
  INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, C_NAME, \
                        scm_c_initialize_from_eval_string,      \
                        "(quote " SCM_NAME ")");

_MY_SCM_SYMBOL (scm_symbol_mark, "mark");
_MY_SCM_SYMBOL (scm_symbol_base, "base");
_MY_SCM_SYMBOL (scm_symbol_ligature, "ligature");
_MY_SCM_SYMBOL (scm_symbol_base_mark, "base-mark");
_MY_SCM_SYMBOL (scm_symbol_entry, "entry");
_MY_SCM_SYMBOL (scm_symbol_exit, "exit");

static SCM
scm_AnchorPointType_to_type_symbol (SCM type, SCM who)
{
  SCM symb = SCM_UNSPECIFIED;
  const int _type = scm_to_int (type);
  switch (_type)
    {
    case at_mark:
      symb = scm_symbol_mark ();
      break;
    case at_basechar:
      symb = scm_symbol_base ();
      break;
    case at_baselig:
      symb = scm_symbol_ligature ();
      break;
    case at_basemark:
      symb = scm_symbol_base_mark ();
      break;
    case at_centry:
      symb = scm_symbol_entry ();
      break;
    case at_cexit:
      symb = scm_symbol_exit ();
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
  if (scm_is_true (scm_eq_p (symb, scm_symbol_mark ())))
    type = at_mark;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_base ())))
    type = at_basechar;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_ligature ())))
    type = at_baselig;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_base_mark ())))
    type = at_basemark;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_entry ())))
    type = at_centry;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_exit ())))
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
