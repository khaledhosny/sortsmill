/*
 * Copyright (C) 2000-2012 by Barry Schwartz
  
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

#ifndef _FONTFORGE_XGC_H
#define _FONTFORGE_XGC_H

//
// Boehm GC routines combined with xalloc.
//

#include <config.h>

// pthread.h should be included _before_ gc.h, if included at all, so
// let’s play it a little safer and do so here.
#include <pthread.h>
#include <gc.h>

#include <stdlib.h>
#include <stdint.h>
#include <xalloc.h>
#include <xdie_on_null.h>

VISIBLE inline void *x_gc_malloc (size_t sz);
VISIBLE inline void *x_gc_malloc_atomic (size_t sz);
VISIBLE inline void *x_gc_malloc_uncollectable (size_t sz);
VISIBLE inline void *x_gc_realloc (void *old_pointer, size_t sz);
VISIBLE inline void *x_gc_malloc_ignore_off_page (size_t sz);
VISIBLE inline void *x_gc_malloc_atomic_ignore_off_page (size_t sz);
VISIBLE inline void *x_gc_malloc_stubborn (size_t sz);
VISIBLE inline char *x_gc_strdup (const char *s);
VISIBLE inline char *x_gc_grabstr (char *s);
VISIBLE uint8_t *x_gc_u8_grabstr (uint8_t *s);
VISIBLE uint16_t *x_gc_u16_grabstr (uint16_t *s);
VISIBLE uint32_t *x_gc_u32_grabstr (uint32_t *s);

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

// Replace an allocated string with a garbage-collected copy of it.
inline char *
x_gc_grabstr (char *s)
{
  char *p = NULL;
  if (s != NULL)
    {
      // Make the garbage-collected copy. Do not try to free this! Or,
      // if you must, use GC_FREE(), not free().
      p = x_gc_strdup (s);

      // Now free the memory in which the original string was
      // stored. Do not use it anymore!
      free (s);
    }
  return p;
}

#endif // _FONTFORGE_XGC_H