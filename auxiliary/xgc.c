#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz */
/*
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

#include <xgc.h>
#include <unistr.h>

// The following bunch of declarations should result in non-inline
// versions being generated.
void *x_gc_malloc (size_t sz);
void *x_gc_malloc_atomic (size_t sz);
void *x_gc_malloc_uncollectable (size_t sz);
void *x_gc_realloc (void *old_pointer, size_t sz);
void *x_gc_malloc_ignore_off_page (size_t sz);
void *x_gc_malloc_atomic_ignore_off_page (size_t sz);
void *x_gc_malloc_stubborn (size_t sz);
char *x_gc_strdup (const char *s);
char *x_gc_grabstr (char *s);

uint8_t *x_gc_u8_grabstr (uint8_t *s)
{
  uint8_t *copy =
    x_gc_malloc_atomic ((u8_strlen (s) + 1) * sizeof (uint8_t));
  u8_strcpy (copy, s);
  free (s);
  return copy;
}

uint16_t *x_gc_u16_grabstr (uint16_t *s)
{
  uint16_t *copy =
    x_gc_malloc_atomic ((u16_strlen (s) + 1) * sizeof (uint16_t));
  u16_strcpy (copy, s);
  free (s);
  return copy;
}

uint32_t *x_gc_u32_grabstr (uint32_t *s)
{
  uint32_t *copy =
    x_gc_malloc_atomic ((u32_strlen (s) + 1) * sizeof (uint32_t));
  u32_strcpy (copy, s);
  free (s);
  return copy;
}
