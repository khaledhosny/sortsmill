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
#include <sortsmill/rexp.h>
#include <sortsmill/initialized_global_constants.h>
#include <assert.h>

static const char my_module[] = "sortsmill fonts peg_spacing";

//-------------------------------------------------------------------------

static void
initialize_u8_re (rexp_t *re, rexp_buffer_t *re_buf, const char *re_string)
{
  *re = u8_rexp_compile_once_study (re_buf, re_string);
}

// Examples:
//
//   r;bottom-serif
//   l;k;my-peg-identifier
//
// The characters legal in identifiers are similar to those for
// identifiers and symbols in R⁵RS.
static const uint8_t peg_name_re_string[] =
  "^([lr]);([ks];|)([[:alnum:]!$%&*+-./:<=>?@^_~]+)$";

static rexp_buffer_t peg_name_re_buf = REXP_BUFFER_T_INITIALIZER;

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, rexp_t,
                      peg_name_re, initialize_u8_re, &peg_name_re_buf,
                      peg_name_re_string);

#define _MY_SCM_SYMBOL(C_NAME, SCM_NAME)                        \
  INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, C_NAME, \
                        scm_c_initialize_from_eval_string,      \
                        "(quote " SCM_NAME ")");

_MY_SCM_SYMBOL (scm_symbol_left, "left");
_MY_SCM_SYMBOL (scm_symbol_right, "right");
_MY_SCM_SYMBOL (scm_symbol_kerning_only, "kerning-only");
_MY_SCM_SYMBOL (scm_symbol_special, "special");

static rexp_match_t
peg_name_match (uint8_t *name)
{
  return u8_rexp_match (peg_name_re (), name);
}

VISIBLE bool
scm_is_spacing_peg_name (SCM name)
{
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  const bool result = (bool) peg_name_match (s);
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_name_p (SCM name)
{
  return scm_from_bool (scm_is_spacing_peg_name (name));
}

VISIBLE SCM
scm_spacing_peg_side (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 1);
      switch (s[interv.i_start])
        {
        case 'l':
          result = scm_symbol_left ();
          break;
        case 'r':
          result = scm_symbol_right ();
          break;
        default:
          assert (false);
          break;
        }
    }
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_modifier (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 2);
      if (interv.i_start != interv.i_end)
        switch (s[interv.i_start])
          {
          case 'k':
            result = scm_symbol_kerning_only ();
            break;
          case 's':
            result = scm_symbol_special ();
            break;
          default:
            break;
          }
    }
  free (s);
  return result;
}

VISIBLE SCM
scm_spacing_peg_identifier (SCM name)
{
  SCM result = SCM_BOOL_F;
  uint8_t *s = (uint8_t *) scm_to_utf8_stringn (name, NULL);
  rexp_match_t m = peg_name_match (s);
  if (m)
    {
      const rexp_interval_t interv = rexp_interval (m, 3);
      result = scm_from_utf8_stringn (s + interv.i_start,
                                      interv.i_end - interv.i_start);
    }
  free (s);
  return result;
}

//-------------------------------------------------------------------------

void init_guile_fonts_peg_spacing (void);

VISIBLE void
init_guile_fonts_peg_spacing (void)
{
  scm_c_define_gsubr ("spacing-peg-name?", 1, 0, 0, scm_spacing_peg_name_p);
  scm_c_define_gsubr ("spacing-peg-side", 1, 0, 0, scm_spacing_peg_side);
  scm_c_define_gsubr ("spacing-peg-modifier", 1, 0, 0, scm_spacing_peg_modifier);
  scm_c_define_gsubr ("spacing-peg-identifier", 1, 0, 0, scm_spacing_peg_identifier);
}

//-------------------------------------------------------------------------
