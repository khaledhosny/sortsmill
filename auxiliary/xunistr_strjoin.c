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

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <xunistring.h>

//-------------------------------------------------------------------------

VISIBLE uint8_t *
x_gc_u8_strjoin (const uint8_t *s1, ...)
{
  va_list ap;

  va_start (ap, s1);
  uint8_t *p = x_gc_u8_vstrjoin (s1, ap);
  va_end (ap);
  return p;
}

VISIBLE uint16_t *
x_gc_u16_strjoin (const uint16_t *s1, ...)
{
  va_list ap;

  va_start (ap, s1);
  uint16_t *p = x_gc_u16_vstrjoin (s1, ap);
  va_end (ap);
  return p;
}

VISIBLE uint32_t *
x_gc_u32_strjoin (const uint32_t *s1, ...)
{
  va_list ap;

  va_start (ap, s1);
  uint32_t *p = x_gc_u32_vstrjoin (s1, ap);
  va_end (ap);
  return p;
}

//-------------------------------------------------------------------------

#define VSTRJOIN_FUNC(NAME, SIZE)				\
  uint##SIZE##_t *						\
  NAME (const uint##SIZE##_t *s1, va_list ap)			\
  {								\
    uint##SIZE##_t *p;						\
								\
    if (s1 == NULL)						\
      {								\
	uint##SIZE##_t empty_string[] = { 0 };			\
	p = x_gc_u##SIZE##_strdup (empty_string);		\
      }								\
    else							\
      {								\
	va_list aq;						\
								\
	size_t length = u##SIZE##_strlen (s1);			\
								\
	size_t total_length = length;				\
	va_copy (aq, ap);					\
	uint##SIZE##_t *s = va_arg (aq, uint##SIZE##_t *);	\
	while (s != NULL)					\
	  {							\
	    total_length += u##SIZE##_strlen (s);		\
	    s = va_arg (aq, uint##SIZE##_t *);			\
	  }							\
	va_end (aq);						\
								\
	p = (uint##SIZE##_t * ) x_gc_malloc_atomic		\
	  ((total_length + 1) * sizeof (uint##SIZE##_t));	\
								\
	memcpy (p, s1, length * sizeof (uint##SIZE##_t));	\
	total_length = length;					\
	va_copy (aq, ap);					\
	s = va_arg (aq, uint##SIZE##_t *);			\
	while (s != NULL)					\
	  {							\
	    length = u##SIZE##_strlen (s);			\
	    memcpy (p + total_length, s,			\
		    length * sizeof (uint##SIZE##_t));		\
	    total_length += length;				\
	    s = va_arg (aq, uint##SIZE##_t *);			\
	  }							\
	p[total_length] = 0;					\
	va_end (aq);						\
      }								\
								\
    return p;							\
  }

VISIBLE VSTRJOIN_FUNC (x_gc_u8_vstrjoin, 8);
VISIBLE VSTRJOIN_FUNC (x_gc_u16_vstrjoin, 16);
VISIBLE VSTRJOIN_FUNC (x_gc_u32_vstrjoin, 32);

//-------------------------------------------------------------------------
