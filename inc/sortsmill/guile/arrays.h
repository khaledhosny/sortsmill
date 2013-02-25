/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_GUILE_ARRAYS_H
#define _SORTSMILL_GUILE_ARRAYS_H

#include <libguile.h>
#include <gsl/gsl_matrix.h>
#include <sortsmill/guile/math/gsl.h>
#include <atomic_ops.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*-----------------------------------------------------------------------*/

void scm_array_handle_unwind_handler (void *handlep);
void scm_dynwind_array_handle_release (scm_t_array_handle *handlep);

/*-----------------------------------------------------------------------*/

/* Support for using Guile array types to index C arrays. */

typedef enum
{
  _FF_INDEX_NOT_AN_ARRAY = 0,
  _FF_INDEX_ARRAY_NONUNIFORM = 1,
  _FF_INDEX_ARRAY_U8 = 2,
  _FF_INDEX_ARRAY_S8 = 3,
  _FF_INDEX_ARRAY_U16 = 4,
  _FF_INDEX_ARRAY_S16 = 5,
  _FF_INDEX_ARRAY_U32 = 6,
  _FF_INDEX_ARRAY_S32 = 7,
  _FF_INDEX_ARRAY_U64 = 8,
  _FF_INDEX_ARRAY_S64 = 9,
  _FF_INDEX_ARRAY_F32 = 10,
  _FF_INDEX_ARRAY_F64 = 11,
  _FF_INDEX_ARRAY_C32 = 12,
  _FF_INDEX_ARRAY_C64 = 13
} scm_t_array_type_index;

scm_t_array_type_index scm_array_handle_to_array_type_index (scm_t_array_handle
                                                             *handlep);
scm_t_array_type_index scm_to_array_type_index (SCM obj);

/*-----------------------------------------------------------------------*/

#define _FF_GUILE_VECTAG_DECL(NAME)                             \
  static inline SCM                                             \
  NAME (void)                                                   \
  {                                                             \
    extern SCM _##NAME;                                         \
    extern volatile AO_t _##NAME##_is_initialized;              \
    extern void _initialize_##NAME (void);                      \
    if (!AO_load_acquire_read (&_##NAME##_is_initialized))      \
      _initialize_##NAME ();                                    \
    return _##NAME;                                             \
  }

_FF_GUILE_VECTAG_DECL (scm_symbol_u8);
_FF_GUILE_VECTAG_DECL (scm_symbol_s8);
_FF_GUILE_VECTAG_DECL (scm_symbol_u16);
_FF_GUILE_VECTAG_DECL (scm_symbol_s16);
_FF_GUILE_VECTAG_DECL (scm_symbol_u32);
_FF_GUILE_VECTAG_DECL (scm_symbol_s32);
_FF_GUILE_VECTAG_DECL (scm_symbol_u64);
_FF_GUILE_VECTAG_DECL (scm_symbol_s64);
_FF_GUILE_VECTAG_DECL (scm_symbol_f32);
_FF_GUILE_VECTAG_DECL (scm_symbol_f64);
_FF_GUILE_VECTAG_DECL (scm_symbol_c32);
_FF_GUILE_VECTAG_DECL (scm_symbol_c64);

bool scm_array_handle_is_uniform_array (scm_t_array_handle *handlep);

bool scm_is_uniform_array (SCM obj);
bool scm_is_uniform_signed_integer_array (SCM obj);
bool scm_is_uniform_unsigned_integer_array (SCM obj);
bool scm_is_uniform_integer_array (SCM obj);
bool scm_is_uniform_real_float_array (SCM obj);
bool scm_is_uniform_complex_float_array (SCM obj);
bool scm_is_integer_array (SCM obj);
bool scm_is_real_array (SCM obj);
bool scm_is_exact_array (SCM obj);
bool scm_is_number_array (SCM obj);

static inline SCM
scm_uniform_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_array (obj));
}

static inline SCM
scm_uniform_signed_integer_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_signed_integer_array (obj));
}

static inline SCM
scm_uniform_unsigned_integer_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_unsigned_integer_array (obj));
}

static inline SCM
scm_uniform_integer_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_integer_array (obj));
}

static inline SCM
scm_uniform_real_float_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_real_float_array (obj));
}

static inline SCM
scm_uniform_complex_float_array_p (SCM obj)
{
  return scm_from_bool (scm_is_uniform_complex_float_array (obj));
}

static inline SCM
scm_integer_array_p (SCM obj)
{
  return scm_from_bool (scm_is_integer_array (obj));
}

static inline SCM
scm_real_array_p (SCM obj)
{
  return scm_from_bool (scm_is_real_array (obj));
}

static inline SCM
scm_exact_array_p (SCM obj)
{
  return scm_from_bool (scm_is_exact_array (obj));
}

static inline SCM
scm_number_array_p (SCM obj)
{
  return scm_from_bool (scm_is_number_array (obj));
}

/*-----------------------------------------------------------------------*/

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_ARRAYS_H */
