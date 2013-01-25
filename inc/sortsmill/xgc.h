/*
 * Copyright (C) 2012 by Barry Schwartz
  
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
extern "C" {
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
