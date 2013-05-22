/*
 * Copyright (C) 2012 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_XGC_H
#define _SORTSMILL_XGC_H

/*
 * Boehm GC routines that die on NULL return, like those in GNU
 * xalloc.h or libiberty.h
 */

#ifndef _FF_DO_NOT_INCLUDE_PTHREAD_H
/* pthread.h should be included _before_ gc.h, if included at all, so
   let’s play it a little safer and do so here. */
#include <pthread.h>
#endif
#include <gc.h>

#include <stdlib.h>
#include <stdint.h>
#include <stdarg.h>
#include <sortsmill/xdie_on_null.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc (size_t sz);
_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc_atomic (size_t sz);
_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc_uncollectable (size_t sz);
_FF_ATTRIBUTE_WARN_UNUSED_RESULT inline void *x_gc_realloc (void *old_pointer,
                                                            size_t sz);
_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc_ignore_off_page (size_t sz);
_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc_atomic_ignore_off_page (size_t
                                                                      sz);
_FF_ATTRIBUTE_MALLOC inline void *x_gc_malloc_stubborn (size_t sz);
_FF_ATTRIBUTE_MALLOC inline char *x_gc_strdup (const char *s);

_FF_ATTRIBUTE_MALLOC char *x_gc_strndup (const char *s, size_t n);

_FF_ATTRIBUTE_SENTINEL
  _FF_ATTRIBUTE_MALLOC char *x_gc_strjoin (const char *s1, ...);
_FF_ATTRIBUTE_MALLOC char *x_gc_vstrjoin (const char *s1, va_list ap);

/* In the current implementation, the 'x_gc_grabstr' functions can be
   marked as __attribute__((__malloc__)), because they copy to freshly
   allocated memory. */
_FF_ATTRIBUTE_MALLOC inline char *x_gc_grabstr (char *s);
_FF_ATTRIBUTE_MALLOC uint8_t *x_gc_u8_grabstr (uint8_t *s);
_FF_ATTRIBUTE_MALLOC uint16_t *x_gc_u16_grabstr (uint16_t *s);
_FF_ATTRIBUTE_MALLOC uint32_t *x_gc_u32_grabstr (uint32_t *s);

/*
 * For example:
 *
 *    char *s;
 *    X_GC_STRJOIN_ITER (s, (int i = 0; i < n; i++),
 *                       (i == 0 ? a[i] : x_gc_strjoin (", ", a[i], NULL)));
 *
 */
#define X_GC_STRJOIN_ITER(VAR, ITERATOR, STRING_FORMULA)        \
  do                                                            \
    {                                                           \
      VAR = "";                                                 \
      for ITERATOR                                              \
        VAR = x_gc_strjoin ((VAR), (STRING_FORMULA), NULL);     \
    }                                                           \
  while (0)

inline void *
x_gc_malloc (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC (sz));
}

inline void *
x_gc_malloc_atomic (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC_ATOMIC (sz));
}

inline void *
x_gc_malloc_uncollectable (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC_UNCOLLECTABLE (sz));
}

inline void *
x_gc_realloc (void *old_pointer, size_t sz)
{
  return XDIE_ON_NULL (GC_REALLOC (old_pointer, sz));
}

inline void *
x_gc_malloc_ignore_off_page (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC_IGNORE_OFF_PAGE (sz));
}

inline void *
x_gc_malloc_atomic_ignore_off_page (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC_ATOMIC_IGNORE_OFF_PAGE (sz));
}

inline void *
x_gc_malloc_stubborn (size_t sz)
{
  return XDIE_ON_NULL (GC_MALLOC_STUBBORN (sz));
}

inline char *
x_gc_strdup (const char *s)
{
  return XDIE_ON_NULL (GC_STRDUP (s));
}

/* Replace an allocated string with a garbage-collected copy of it. */
inline char *
x_gc_grabstr (char *s)
{
  char *p = NULL;
  if (s != NULL)
    {
      /* Make the garbage-collected copy. Do not try to free this! Or,
       * if you must, use GC_FREE(), not free(). */
      p = x_gc_strdup (s);

      /* Now free the memory in which the original string was
         stored. Do not use it anymore! */
      free (s);
    }
  return p;
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_XGC_H */
