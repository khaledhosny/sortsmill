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

_MY_SCM_SYMBOL (scm_symbol_gsub_single, "gsub-single");
_MY_SCM_SYMBOL (scm_symbol_gsub_multiple, "gsub-multiple");
_MY_SCM_SYMBOL (scm_symbol_gsub_alternate, "gsub-alternate");
_MY_SCM_SYMBOL (scm_symbol_gsub_ligature, "gsub-ligature");
_MY_SCM_SYMBOL (scm_symbol_gsub_context, "gsub-context");
_MY_SCM_SYMBOL (scm_symbol_gsub_chaining_contextual,
                "gsub-chaining-contextual");
_MY_SCM_SYMBOL (scm_symbol_gsub_extension, "gsub-extension");
_MY_SCM_SYMBOL (scm_symbol_gsub_reverse_chaining_contextual,
                "gsub-reverse-chaining-contextual");
_MY_SCM_SYMBOL (scm_symbol_gpos_single, "gpos-single");
_MY_SCM_SYMBOL (scm_symbol_gpos_pair, "gpos-pair");
_MY_SCM_SYMBOL (scm_symbol_gpos_cursive, "gpos-cursive");
_MY_SCM_SYMBOL (scm_symbol_gpos_mark_to_base, "gpos-mark-to-base");
_MY_SCM_SYMBOL (scm_symbol_gpos_mark_to_ligature, "gpos-mark-to-ligature");
_MY_SCM_SYMBOL (scm_symbol_gpos_mark_to_mark, "gpos-mark-to-mark");
_MY_SCM_SYMBOL (scm_symbol_gpos_context, "gpos-context");
_MY_SCM_SYMBOL (scm_symbol_gpos_chaining_contextual,
                "gpos-chaining-contextual");
_MY_SCM_SYMBOL (scm_symbol_gpos_extension, "gpos-extension");

static SCM
scm_OTLookupType_to_type_symbol (SCM type, SCM who)
{
  SCM symb = SCM_UNSPECIFIED;

  const int _type = scm_to_int (type);
  switch (_type)
    {
    case gsub_single:
      symb = scm_symbol_gsub_single ();
      break;
    case gsub_multiple:
      symb = scm_symbol_gsub_multiple ();
      break;
    case gsub_alternate:
      symb = scm_symbol_gsub_alternate ();
      break;
    case gsub_ligature:
      symb = scm_symbol_gsub_ligature ();
      break;
    case gsub_context:
      symb = scm_symbol_gsub_context ();
      break;
    case gsub_contextchain:
      symb = scm_symbol_gsub_chaining_contextual ();
      break;
    case gsub_extension:
      symb = scm_symbol_gsub_extension ();
      break;
    case gsub_reversecchain:
      symb = scm_symbol_gsub_reverse_chaining_contextual ();
      break;

    case gpos_single:
      symb = scm_symbol_gpos_single ();
      break;
    case gpos_pair:
      symb = scm_symbol_gpos_pair ();
      break;
    case gpos_cursive:
      symb = scm_symbol_gpos_cursive ();
      break;
    case gpos_mark2base:
      symb = scm_symbol_gpos_mark_to_base ();
      break;
    case gpos_mark2ligature:
      symb = scm_symbol_gpos_mark_to_ligature ();
      break;
    case gpos_mark2mark:
      symb = scm_symbol_gpos_mark_to_mark ();
      break;
    case gpos_context:
      symb = scm_symbol_gpos_context ();
      break;
    case gpos_contextchain:
      symb = scm_symbol_gpos_chaining_contextual ();
      break;
    case gpos_extension:
      symb = scm_symbol_gpos_extension ();
      break;

    default:
      {
        if (SCM_UNBNDP (who))
          who = scm_from_latin1_string ("scm_OTLookupType_to_type_symbol");
        rnrs_raise_condition
          (scm_list_4
           (rnrs_make_assertion_violation (),
            rnrs_make_who_condition (who),
            rnrs_c_make_message_condition (_("unrecognized OTLookupType "
                                             "value")),
            rnrs_make_irritants_condition (scm_list_1 (type))));
      }
      break;
    }

  return symb;
}

static SCM
scm_type_symbol_to_OTLookupType (SCM symb, SCM who)
{
  int type = INT_MIN;
  if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_single ())))
    type = gsub_single;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_multiple ())))
    type = gsub_multiple;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_alternate ())))
    type = gsub_alternate;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_ligature ())))
    type = gsub_ligature;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_context ())))
    type = gsub_context;
  else if (scm_is_true
           (scm_eq_p (symb, scm_symbol_gsub_chaining_contextual ())))
    type = gsub_contextchain;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gsub_extension ())))
    type = gsub_extension;
  else if (scm_is_true
           (scm_eq_p (symb, scm_symbol_gsub_reverse_chaining_contextual ())))
    type = gsub_reversecchain;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_single ())))
    type = gpos_single;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_pair ())))
    type = gpos_pair;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_cursive ())))
    type = gpos_cursive;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_mark_to_base ())))
    type = gpos_mark2base;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_mark_to_ligature ())))
    type = gpos_mark2ligature;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_mark_to_mark ())))
    type = gpos_mark2mark;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_context ())))
    type = gpos_context;
  else if (scm_is_true
           (scm_eq_p (symb, scm_symbol_gpos_chaining_contextual ())))
    type = gpos_contextchain;
  else if (scm_is_true (scm_eq_p (symb, scm_symbol_gpos_extension ())))
    type = gpos_extension;
  else
    {
      if (SCM_UNBNDP (who))
        who = scm_from_latin1_string ("scm_type_symbol_to_OTLookupType");
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_make_who_condition (who),
          rnrs_c_make_message_condition (_("unrecognized lookup type")),
          rnrs_make_irritants_condition (scm_list_1 (symb))));
    }
  return scm_from_int (type);
}

//-------------------------------------------------------------------------

void init_guile_internals_lookups (void);

VISIBLE void
init_guile_internals_lookups (void)
{
  scm_c_define_gsubr ("OTLookupType->type-symbol", 1, 1, 0,
                      scm_OTLookupType_to_type_symbol);
  scm_c_define_gsubr ("type-symbol->OTLookupType", 1, 1, 0,
                      scm_type_symbol_to_OTLookupType);
}

//-------------------------------------------------------------------------
