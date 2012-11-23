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
#include <string.h>
#include <xunistring.h>

// Generate non-inline versions of these functions.
VISIBLE bool u8_valid (const uint8_t *string);
VISIBLE bool u16_valid (const uint16_t *string);
VISIBLE bool u32_valid (const uint32_t *string);

//-------------------------------------------------------------------------

#define U_TO_U_FUNC(NAME, SIZE1, SIZE2, BUFSIZE_FACTOR, ALLOCATOR,	\
		    REALLOCATOR)					\
  uint##SIZE2##_t *							\
  NAME (const uint##SIZE1##_t *string)					\
  {									\
    size_t n = u##SIZE1##_strlen (string);				\
    assert (u##SIZE1##_check (string, n) == NULL);			\
									\
    size_t length = BUFSIZE_FACTOR * n;					\
    uint##SIZE2##_t *buffer =						\
      ALLOCATOR ((length + 1) * sizeof (uint##SIZE2##_t));		\
    memset (buffer, 0, (length + 1) * sizeof (uint##SIZE2##_t));	\
									\
    (void) u##SIZE1##_to_u##SIZE2 (string, n, buffer, &length);		\
    return REALLOCATOR (buffer, (u##SIZE2##_strlen (buffer) + 1)	\
			* sizeof (uint##SIZE2##_t));			\
  }

VISIBLE U_TO_U_FUNC (x_u8_to_u16, 8, 16, 1, xmalloc, xrealloc);
VISIBLE U_TO_U_FUNC (x_u8_to_u32, 8, 32, 1, xmalloc, xrealloc);
VISIBLE U_TO_U_FUNC (x_u16_to_u8, 16, 8, 3, xmalloc, xrealloc);
VISIBLE U_TO_U_FUNC (x_u16_to_u32, 16, 32, 1, xmalloc, xrealloc);
VISIBLE U_TO_U_FUNC (x_u32_to_u8, 32, 8, 4, xmalloc, xrealloc);
VISIBLE U_TO_U_FUNC (x_u32_to_u16, 32, 16, 2, xmalloc, xrealloc);

VISIBLE U_TO_U_FUNC (x_gc_u8_to_u16, 8, 16, 1, x_gc_malloc, x_gc_realloc);
VISIBLE U_TO_U_FUNC (x_gc_u8_to_u32, 8, 32, 1, x_gc_malloc, x_gc_realloc);
VISIBLE U_TO_U_FUNC (x_gc_u16_to_u8, 16, 8, 3, x_gc_malloc, x_gc_realloc);
VISIBLE U_TO_U_FUNC (x_gc_u16_to_u32, 16, 32, 1, x_gc_malloc, x_gc_realloc);
VISIBLE U_TO_U_FUNC (x_gc_u32_to_u8, 32, 8, 4, x_gc_malloc, x_gc_realloc);
VISIBLE U_TO_U_FUNC (x_gc_u32_to_u16, 32, 16, 2, x_gc_malloc, x_gc_realloc);

//-------------------------------------------------------------------------
//
// These functions force a valid string in a way that should be
// considered arbitrary. (The current implementation returns an empty
// string in place of an invalid one.)

VISIBLE const uint8_t *
u8_force_valid (const uint8_t *string)
{
  static const uint8_t empty_string[1] = { 0 };
  return (u8_valid (string)) ? string : empty_string;
}

VISIBLE const uint16_t *
u16_force_valid (const uint16_t *string)
{
  static const uint16_t empty_string[1] = { 0 };
  return (u16_valid (string)) ? string : empty_string;
}

VISIBLE const uint32_t *
u32_force_valid (const uint32_t *string)
{
  static const uint32_t empty_string[1] = { 0 };
  return (u32_valid (string)) ? string : empty_string;
}

//-------------------------------------------------------------------------

VISIBLE void
u8_trim_invalid_suffix (uint8_t *string)
{
  uint8_t *p = (uint8_t *) u8_check (string, u8_strlen (string));
  if (p != NULL)
    *p = 0;
}

VISIBLE void
u16_trim_invalid_suffix (uint16_t *string)
{
  uint16_t *p = (uint16_t *) u16_check (string, u16_strlen (string));
  if (p != NULL)
    *p = 0;
}

VISIBLE void
u32_trim_invalid_suffix (uint32_t *string)
{
  uint32_t *p = (uint32_t *) u32_check (string, u32_strlen (string));
  if (p != NULL)
    *p = 0;
}

//-------------------------------------------------------------------------

#define VALID_PREFIX_FUNC(NAME, SIZE, ALLOCATOR)			\
  uint##SIZE##_t *							\
  NAME (const uint##SIZE##_t *string)					\
  {									\
    uint##SIZE##_t *prefix;						\
    size_t length = u##SIZE##_strlen (string);				\
    const uint##SIZE##_t *p = u##SIZE##_check (string, length);		\
    if (p != NULL)							\
      length = p - string;						\
    prefix = ALLOCATOR ((length + 1) * sizeof (uint##SIZE##_t));	\
    prefix[0] = 0;							\
    u##SIZE##_strncat (prefix, string, length);				\
    return prefix;							\
  }

VISIBLE VALID_PREFIX_FUNC (x_u8_valid_prefix, 8, xmalloc);
VISIBLE VALID_PREFIX_FUNC (x_u16_valid_prefix, 16, xmalloc);
VISIBLE VALID_PREFIX_FUNC (x_u32_valid_prefix, 32, xmalloc);

VISIBLE VALID_PREFIX_FUNC (x_gc_u8_valid_prefix, 8, x_gc_malloc);
VISIBLE VALID_PREFIX_FUNC (x_gc_u16_valid_prefix, 16, x_gc_malloc);
VISIBLE VALID_PREFIX_FUNC (x_gc_u32_valid_prefix, 32, x_gc_malloc);

//-------------------------------------------------------------------------

#define STRMBNDUP_FUNC(NAME, SIZE, ALLOCATOR)				\
  uint##SIZE##_t *							\
  NAME (const uint##SIZE##_t *string, size_t n)				\
  {									\
    size_t num_units = 0;						\
    size_t i = 0;							\
    int len = u##SIZE##_strmblen (&string[0]);				\
    while (i < n && 0 < len)						\
      {									\
	num_units += len;						\
	i++;								\
	if (i < n)							\
	  len = u##SIZE##_strmblen (&string[num_units]);		\
      }									\
									\
    uint##SIZE##_t *p;							\
    if (len < 0)							\
      p = NULL; /* Unicode error. */					\
    else								\
      {									\
	p = ALLOCATOR ((num_units + 1) * sizeof (uint##SIZE##_t));	\
	memcpy (p, string, (num_units + 1) * sizeof (uint##SIZE##_t));	\
	p[num_units] = 0;						\
      }									\
    return p;								\
  }

VISIBLE STRMBNDUP_FUNC (x_u8_strmbndup, 8, xmalloc);
VISIBLE STRMBNDUP_FUNC (x_u16_strmbndup, 16, xmalloc);
VISIBLE STRMBNDUP_FUNC (x_u32_strmbndup, 32, xmalloc);

VISIBLE STRMBNDUP_FUNC (x_gc_u8_strmbndup, 8, x_gc_malloc);
VISIBLE STRMBNDUP_FUNC (x_gc_u16_strmbndup, 16, x_gc_malloc);
VISIBLE STRMBNDUP_FUNC (x_gc_u32_strmbndup, 32, x_gc_malloc);

//-------------------------------------------------------------------------

// NOTE: This implementation copies the entire pure-ASCII prefix of
// the string, so you should not use it directly on a very long
// string.

#define STRTOI_FUNC(NAME, SIZE, ITYPE, STRTOI, CONVERSION)		\
  ITYPE									\
  NAME (const uint##SIZE##_t *nptr, uint##SIZE##_t **endptr, int base)	\
  {									\
    ucs4_t c;								\
									\
    size_t i = 0;							\
    const uint##SIZE##_t *p = u##SIZE##_next (&c, nptr);		\
    while (p != NULL && c < 128)					\
      {									\
	i++;								\
	p = u##SIZE##_next (&c, p);					\
      }									\
									\
    uint##SIZE##_t *substring = x_gc_u##SIZE##_strmbndup (nptr, i);	\
    char *csubstring = (char *) CONVERSION (substring);			\
									\
    char *endp;								\
    ITYPE value = STRTOI (csubstring, &endp, base);			\
									\
    if (endptr != NULL)							\
      {									\
	size_t n = endp - csubstring;					\
	const uint##SIZE##_t *p = nptr;					\
	for (size_t j = 0; j < n; j++)					\
	  p = u##SIZE##_next (&c, p);					\
	*endptr = (uint##SIZE##_t *) p;					\
      }									\
									\
    return value;							\
  }

VISIBLE STRTOI_FUNC (u8_strtol, 8, long int, strtol, /* no conversion */ );
VISIBLE STRTOI_FUNC (u16_strtol, 16, long int, strtol, x_gc_u16_to_u8);
VISIBLE STRTOI_FUNC (u32_strtol, 32, long int, strtol, x_gc_u32_to_u8);

VISIBLE STRTOI_FUNC (u8_strtoul, 8, unsigned long int, strtoul, /* no conversion */ );
VISIBLE STRTOI_FUNC (u16_strtoul, 16, unsigned long int, strtoul, x_gc_u16_to_u8);
VISIBLE STRTOI_FUNC (u32_strtoul, 32, unsigned long int, strtoul, x_gc_u32_to_u8);

//-------------------------------------------------------------------------

// NOTE: This implementation copies the entire pure-ASCII prefix of
// the string, so you should not use it directly on a very long
// string.

#define STRTOF_FUNC(NAME, SIZE, FTYPE, STRTOF, CONVERSION)		\
  FTYPE									\
  NAME (const uint##SIZE##_t *nptr, uint##SIZE##_t **endptr)		\
  {									\
    ucs4_t c;								\
									\
    size_t i = 0;							\
    const uint##SIZE##_t *p = u##SIZE##_next (&c, nptr);		\
    while (p != NULL && c < 128)					\
      {									\
	i++;								\
	p = u##SIZE##_next (&c, p);					\
      }									\
									\
    uint##SIZE##_t *substring = x_gc_u##SIZE##_strmbndup (nptr, i);	\
    char *csubstring = (char *) CONVERSION (substring);			\
									\
    char *endp;								\
    FTYPE value = STRTOF (csubstring, &endp);				\
									\
    if (endptr != NULL)							\
      {									\
	size_t n = endp - csubstring;					\
	const uint##SIZE##_t *p = nptr;					\
	for (size_t j = 0; j < n; j++)					\
	  p = u##SIZE##_next (&c, p);					\
	*endptr = (uint##SIZE##_t *) p;					\
      }									\
									\
    return value;							\
  }

VISIBLE STRTOF_FUNC (u8_strtod, 8, double, strtod, /* no conversion */ );
VISIBLE STRTOF_FUNC (u16_strtod, 16, double, strtod, x_gc_u16_to_u8);
VISIBLE STRTOF_FUNC (u32_strtod, 32, double, strtod, x_gc_u32_to_u8);

//-------------------------------------------------------------------------

// A replacement for the 'u8_get_next' function that used to be in
// FontForge. New code should use 'u8_next', instead. It is a standard
// part of libunistring, and it avoids using signed integers to
// represent UCS-4 values.

#define GETNEXT_FUNC(NAME, SIZE)			\
  int							\
  NAME (const uint##SIZE##_t **sptrptr)			\
  {							\
    int c;						\
    int len = u##SIZE##_strmbtouc (&c, *sptrptr);	\
    if (len < 0 || 0x10FFFF < c)			\
      c = -1;						\
    else if (len == 0)					\
      (*sptrptr) += 1;					\
    else						\
      (*sptrptr) += len;				\
    return c;						\
  }

VISIBLE GETNEXT_FUNC (u8_get_next, 8);
VISIBLE GETNEXT_FUNC (u16_get_next, 16);
VISIBLE GETNEXT_FUNC (u32_get_next, 32);

//-------------------------------------------------------------------------
