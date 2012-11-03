/*
 * Copyright (C) 2012 Barry Schwartz.
 * Based in part on code from guile-2.0.6, which is
 *   Copyright (C) 1995-2012 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */

#ifndef _LIBGUILE_WRAPPER_H
#define _LIBGUILE_WRAPPER_H 1

#include <config.h>

#include <stdlib.h>
#include <string.h>
#include <xgc.h>
#include <libguile.h>

#undef scm_short2num
#undef scm_ushort2num
#undef scm_int2num
#undef scm_uint2num
#undef scm_long2num
#undef scm_ulong2num
#undef scm_size2num
#undef scm_ptrdiff2num

#undef scm_num2short
#undef scm_num2ushort
#undef scm_num2int
#undef scm_num2uint
#undef scm_num2long
#undef scm_num2ulong
#undef scm_num2size
#undef scm_num2ptrdiff

#undef scm_make_real
#undef scm_num2dbl
#undef scm_float2num
#undef scm_double2num
#undef scm_make_complex

#undef scm_mem2symbol
#undef scm_mem2uninterned_symbol
#undef scm_str2symbol
#undef scm_take_str
#undef scm_take0str
#undef scm_mem2string
#undef scm_str2string
#undef scm_makfrom0str
#undef scm_makfrom0str_opt
#undef scm_c_make_keyword

#undef scm_must_malloc
#undef scm_must_realloc
#undef scm_must_strndup
#undef scm_must_strdup
#undef scm_must_free
#undef scm_done_malloc
#undef scm_done_free

#define scm_short2num repl_scm_short2num
#define scm_ushort2num repl_scm_ushort2num
#define scm_int2num repl_scm_int2num
#define scm_uint2num repl_scm_uint2num
#define scm_long2num repl_scm_long2num
#define scm_ulong2num repl_scm_ulong2num
#define scm_size2num repl_scm_size2num
#define scm_ptrdiff2num repl_scm_ptrdiff2num

#define scm_num2short repl_scm_num2short
#define scm_num2ushort repl_scm_num2ushort
#define scm_num2int repl_scm_num2int
#define scm_num2uint repl_scm_num2uint
#define scm_num2long repl_scm_num2long
#define scm_num2ulong repl_scm_num2ulong
#define scm_num2size repl_scm_num2size
#define scm_num2ptrdiff repl_scm_num2ptrdiff

#define scm_make_real repl_scm_make_real
#define scm_num2dbl repl_scm_num2dbl
#define scm_float2num repl_scm_float2num
#define scm_double2num repl_scm_double2num
#define scm_make_complex repl_scm_make_complex

#define scm_mem2symbol repl_scm_mem2symbol
#define scm_mem2uninterned_symbol repl_scm_mem2uninterned_symbol
#define scm_str2symbol repl_scm_str2symbol
#define scm_take_str repl_scm_take_str
#define scm_take0str repl_scm_take0str
#define scm_mem2string repl_scm_mem2string
#define scm_str2string repl_scm_str2string
#define scm_makfrom0str repl_scm_makfrom0str
#define scm_makfrom0str_opt repl_scm_makfrom0str_opt
#define scm_c_make_keyword repl_scm_c_make_keyword

#define scm_must_malloc repl_scm_must_malloc
#define scm_must_realloc repl_scm_must_realloc
#define scm_must_strndup repl_scm_must_strndup
#define scm_must_strdup repl_scm_must_strdup
#define scm_must_free repl_scm_must_free
#define scm_done_malloc repl_scm_done_malloc
#define scm_done_free repl_scm_done_free

// FIXME: This is kind of big to be static inline.
static inline char *
repl_scm_string_chars (SCM str)
{
  size_t length = scm_c_string_length (str);
  size_t len = length;
  char *s = scm_to_latin1_stringn (str, &len);
  char *p = x_gc_malloc_atomic ((length + 1) * sizeof (char));
  memset (p, 0, (length + 1) * sizeof (char));
  memcpy (p, s, len * sizeof (char));
  return p;
}

static inline SCM
repl_scm_short2num (short int x)
{
  return scm_from_short (x);
}

static inline SCM
repl_scm_ushort2num (unsigned short int x)
{
  return scm_from_ushort (x);
}

static inline SCM
repl_scm_int2num (int x)
{
  return scm_from_int (x);
}

static inline SCM
repl_scm_uint2num (unsigned int x)
{
  return scm_from_uint (x);
}

static inline SCM
repl_scm_long2num (long int x)
{
  return scm_from_long (x);
}

static inline SCM
repl_scm_ulong2num (unsigned long int x)
{
  return scm_from_ulong (x);
}

static inline SCM
repl_scm_size2num (size_t x)
{
  return scm_from_size_t (x);
}

static inline SCM
repl_scm_ptrdiff2num (ptrdiff_t x)
{
  return scm_from_ssize_t (x);
}

static inline short int
repl_scm_num2short (SCM x, unsigned long int UNUSED (pos),
                    const char *UNUSED (s_caller))
{
  return scm_to_short (x);
}

static inline unsigned short int
repl_scm_num2ushort (SCM x, unsigned long int UNUSED (pos),
                     const char *UNUSED (s_caller))
{
  return scm_to_ushort (x);
}

static inline int
repl_scm_num2int (SCM x, unsigned long int UNUSED (pos),
                  const char *UNUSED (s_caller))
{
  return scm_to_int (x);
}

static inline unsigned int
repl_scm_num2uint (SCM x, unsigned long int UNUSED (pos),
                   const char *UNUSED (s_caller))
{
  return scm_to_uint (x);
}

static inline long int
repl_scm_num2long (SCM x, unsigned long int UNUSED (pos),
                   const char *UNUSED (s_caller))
{
  return scm_to_long (x);
}

static inline unsigned long int
repl_scm_num2ulong (SCM x, unsigned long int UNUSED (pos),
                    const char *UNUSED (s_caller))
{
  return scm_to_ulong (x);
}

static inline size_t
repl_scm_num2size (SCM x, unsigned long int UNUSED (pos),
                   const char *UNUSED (s_caller))
{
  return scm_to_size_t (x);
}

static inline ptrdiff_t
repl_scm_num2ptrdiff (SCM x, unsigned long int UNUSED (pos),
                      const char *UNUSED (s_caller))
{
  return scm_to_ssize_t (x);
}

static inline SCM
repl_scm_make_real (double x)
{
  return scm_from_double (x);
}

static inline double
repl_scm_num2dbl (SCM a, const char *why)
{
  return scm_to_double (a);
}

static inline SCM
repl_scm_float2num (float n)
{
  return scm_from_double ((double) n);
}

static inline SCM
repl_scm_double2num (double n)
{
  return scm_from_double (n);
}

static inline SCM
repl_scm_make_complex (double x, double y)
{
  return scm_c_make_rectangular (x, y);
}

static inline SCM
repl_scm_mem2symbol (const char *mem, size_t len)
{
  return scm_from_locale_symboln (mem, len);
}

static inline SCM
repl_scm_mem2uninterned_symbol (const char *mem, size_t len)
{
  return scm_make_symbol (scm_from_locale_stringn (mem, len));
}

static inline SCM
repl_scm_str2symbol (const char *str)
{
  return scm_from_locale_symbol (str);
}

static inline SCM
repl_scm_take_str (char *s, size_t len)
{
  return scm_take_locale_stringn (s, len);
}

static inline SCM
repl_scm_take0str (char *s)
{
  return scm_take_locale_string (s);
}

static inline SCM
repl_scm_mem2string (const char *src, size_t len)
{
  return scm_from_locale_stringn (src, len);
}

static inline SCM
repl_scm_str2string (const char *src)
{
  return scm_from_locale_string (src);
}

static inline SCM
repl_scm_makfrom0str (const char *src)
{
  if (!src)
    return SCM_BOOL_F;
  return scm_from_locale_string (src);
}

static inline SCM
repl_scm_makfrom0str_opt (const char *src)
{
  return scm_makfrom0str (src);
}

static inline SCM
repl_scm_c_make_keyword (const char *s)
{
  return scm_from_locale_keyword (s);
}

static inline void *
repl_scm_must_malloc (size_t size, const char *what)
{
  return scm_gc_malloc (size, what);
}

static inline void *
repl_scm_must_realloc (void *where,
                       size_t old_size, size_t size, const char *what)
{
  return scm_gc_realloc (where, old_size, size, what);
}

static inline char *
repl_scm_must_strndup (const char *str, size_t length)
{
  return scm_gc_strndup (str, length, "string");
}

static inline char *
repl_scm_must_strdup (const char *str)
{
  return scm_gc_strdup (str, "string");
}

static inline void
repl_scm_must_free (void *obj)
{
  // In Guile 2.0 the allocations are wrappers around Boehm GC.
  GC_FREE (obj);
}

static inline void
repl_scm_done_malloc (long int size)
{
  if (size >= 0)
    scm_gc_register_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_unregister_collectable_memory (NULL, -size, "foreign mallocs");
}

static inline void
repl_scm_done_free (long int size)
{
  if (size >= 0)
    scm_gc_unregister_collectable_memory (NULL, size, "foreign mallocs");
  else
    scm_gc_register_collectable_memory (NULL, -size, "foreign mallocs");
}

#endif // _LIBGUILE_WRAPPER_H
