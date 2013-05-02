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

#include <sortsmill/guile/strings/rexp.h>
#include <sortsmill/initialized_global_constants.h>
#include <sortsmill/attributes.h>

//-------------------------------------------------------------------------

#define _REXP_MODULE_NAME "sortsmill strings rexp"

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _pointer_to_rexp,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _REXP_MODULE_NAME ") pointer->rexp)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _rexp_to_pointer,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _REXP_MODULE_NAME ") rexp->pointer)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _pointer_to_rexp_match,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _REXP_MODULE_NAME ") pointer->rexp-match)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _rexp_match_to_pointer,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _REXP_MODULE_NAME ") rexp-match->pointer)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _rexp_buffers,
                      scm_c_initialize_from_eval_string,
                      "(make-fluid (make-hash-table))");

VISIBLE SCM
scm_from_rexp_t (rexp_t _re)
{
  return scm_call_1 (_pointer_to_rexp (), scm_from_pointer (_re, NULL));
}

VISIBLE rexp_t
scm_to_rexp_t (SCM re)
{
  return (rexp_t) scm_to_pointer (scm_call_1 (_rexp_to_pointer (), re));
}

VISIBLE SCM
scm_from_rexp_match_t (rexp_match_t _m)
{
  return scm_call_1 (_pointer_to_rexp_match (), scm_from_pointer (_m, NULL));
}

VISIBLE rexp_match_t
scm_to_rexp_match_t (SCM m)
{
  return (rexp_match_t)
    scm_to_pointer (scm_call_1 (_rexp_match_to_pointer (), m));
}

//-------------------------------------------------------------------------

static inline SCM
scm_rexp_general_compile (rexp_t (*compile) (const uint8_t *pattern),
                          SCM pattern)
{
  uint8_t *_pattern = (uint8_t *) scm_to_utf8_stringn (pattern, NULL);
  rexp_t _re = compile (_pattern);
  free (_pattern);
  return (_re == NULL) ? SCM_BOOL_F : scm_from_rexp_t (_re);
}

static SCM
scm_rexp_general_compile_once (rexp_t (*compile) (const uint8_t *pattern),
                               SCM pattern)
{
  SCM buffers = scm_fluid_ref (_rexp_buffers ());
  SCM my_rexp = scm_hash_ref (buffers, pattern, SCM_BOOL_F);
  if (scm_is_false (my_rexp))
    {
      uint8_t *_pattern = (uint8_t *) scm_to_utf8_stringn (pattern, NULL);
      rexp_t _re = compile (_pattern);
      free (_pattern);
      my_rexp = (_re == NULL) ? SCM_BOOL_F : scm_from_rexp_t (_re);
      scm_hash_set_x (buffers, pattern, my_rexp);
    }
  return my_rexp;
}

VISIBLE SCM
scm_rexp_compile (SCM pattern)
{
  return scm_rexp_general_compile (u8_rexp_compile, pattern);
}

VISIBLE SCM
scm_rexp_compile_study (SCM pattern)
{
  return scm_rexp_general_compile (u8_rexp_compile_study, pattern);
}

VISIBLE SCM
scm_rexp_compile_jit (SCM pattern)
{
  return scm_rexp_general_compile (u8_rexp_compile_jit, pattern);
}

VISIBLE SCM
scm_rexp_compile_once (SCM pattern)
{
  return scm_rexp_general_compile_once (u8_rexp_compile, pattern);
}

VISIBLE SCM
scm_rexp_compile_once_study (SCM pattern)
{
  return scm_rexp_general_compile_once (u8_rexp_compile_study, pattern);
}

VISIBLE SCM
scm_rexp_compile_once_jit (SCM pattern)
{
  return scm_rexp_general_compile_once (u8_rexp_compile_jit, pattern);
}

VISIBLE SCM
scm_rexp_match (SCM re, SCM string)
{
  uint8_t *_string = (uint8_t *) scm_to_utf8_stringn (string, NULL);
  rexp_t _re = scm_to_rexp_t (re);
  rexp_match_t _match = u8_rexp_match (_re, _string);
  free (_string);
  return (_match == NULL) ? SCM_BOOL_F : scm_from_rexp_match_t (_match);
}

VISIBLE SCM
scm_rexp_search (SCM re, SCM string)
{
  uint8_t *_string = (uint8_t *) scm_to_utf8_stringn (string, NULL);
  rexp_t _re = scm_to_rexp_t (re);
  rexp_match_t _match = u8_rexp_search (_re, _string);
  free (_string);
  return (_match == NULL) ? SCM_BOOL_F : scm_from_rexp_match_t (_match);
}

VISIBLE SCM
scm_rexp_interval (SCM match, SCM subexpression)
{
  rexp_interval_t interv = rexp_interval (scm_to_rexp_match_t (match),
                                          scm_to_size_t (subexpression));
  return ((interv.i_start == -1) ?
          SCM_BOOL_F : scm_list_2 (scm_from_int (interv.i_start),
                                   scm_from_int (interv.i_end)));
}

VISIBLE SCM
scm_rexp_substring (SCM match, SCM string, SCM subexpression)
{
  uint8_t *_string = (uint8_t *) scm_to_utf8_stringn (string, NULL);
  uint8_t *_substring = u8_rexp_substr (scm_to_rexp_match_t (match), _string,
                                        scm_to_size_t (subexpression));
  free (_string);
  return (_substring == NULL) ?
    SCM_BOOL_F : scm_from_utf8_string ((const char *) _substring);
}

//-------------------------------------------------------------------------

void init_sortsmill_guile_strings_rexp (void);

VISIBLE void
init_sortsmill_guile_strings_rexp (void)
{
  scm_c_define_gsubr ("rexp:compile", 1, 0, 0, scm_rexp_compile);
  scm_c_define_gsubr ("rexp:compile-study", 1, 0, 0, scm_rexp_compile_study);
  scm_c_define_gsubr ("rexp:compile-jit", 1, 0, 0, scm_rexp_compile_jit);
  scm_c_define_gsubr ("rexp:compile-once", 1, 0, 0, scm_rexp_compile_once);
  scm_c_define_gsubr ("rexp:compile-once-study", 1, 0, 0,
                      scm_rexp_compile_once_study);
  scm_c_define_gsubr ("rexp:compile-once-jit", 1, 0, 0,
                      scm_rexp_compile_once_jit);
  scm_c_define_gsubr ("rexp:match", 2, 0, 0, scm_rexp_match);
  scm_c_define_gsubr ("rexp:search", 2, 0, 0, scm_rexp_search);
  scm_c_define_gsubr ("rexp:interval", 2, 0, 0, scm_rexp_interval);
  scm_c_define_gsubr ("rexp:substring", 3, 0, 0, scm_rexp_substring);
}

//-------------------------------------------------------------------------
