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

#ifndef _FONTFORGE_XUNISTR_H
#define _FONTFORGE_XUNISTR_H

//
// libunistring conversion routines combined with xalloc and the Boehm
// garbage collector.
//

#include <config.h>

#include <stdio.h>
#include <stdbool.h>
#include <unistr.h>
#include <xalloc.h>
#include <xdie_on_null.h>
#include <null_passthru.h>
#include <xgc.h>

VISIBLE uint16_t *x_u8_to_u16 (const uint8_t *string);
VISIBLE uint32_t *x_u8_to_u32 (const uint8_t *string);
VISIBLE uint8_t *x_u16_to_u8 (const uint16_t *string);
VISIBLE uint32_t *x_u16_to_u32 (const uint16_t *string);
VISIBLE uint8_t *x_u32_to_u8 (const uint32_t *string);
VISIBLE uint16_t *x_u32_to_u16 (const uint32_t *string);
VISIBLE inline uint16_t *x_gc_u8_to_u16 (const uint8_t *string);
VISIBLE inline uint32_t *x_gc_u8_to_u32 (const uint8_t *string);
VISIBLE inline uint8_t *x_gc_u16_to_u8 (const uint16_t *string);
VISIBLE inline uint32_t *x_gc_u16_to_u32 (const uint16_t *string);
VISIBLE inline uint8_t *x_gc_u32_to_u8 (const uint32_t *string);
VISIBLE inline uint16_t *x_gc_u32_to_u16 (const uint32_t *string);

VISIBLE inline bool u8_valid (const uint8_t *string);
VISIBLE inline bool u16_valid (const uint16_t *string);
VISIBLE inline bool u32_valid (const uint32_t *string);
VISIBLE const uint8_t *u8_force_valid (const uint8_t *string);
VISIBLE const uint16_t *u16_force_valid (const uint16_t *string);
VISIBLE const uint32_t *u32_force_valid (const uint32_t *string);
VISIBLE void u8_trim_invalid_suffix (uint8_t *string);
VISIBLE void u16_trim_invalid_suffix (uint16_t *string);
VISIBLE void u32_trim_invalid_suffix (uint32_t *string);
VISIBLE uint8_t *x_u8_valid_prefix (const uint8_t *string);
VISIBLE uint16_t *x_u16_valid_prefix (const uint16_t *string);
VISIBLE uint32_t *x_u32_valid_prefix (const uint32_t *string);
VISIBLE uint8_t *x_gc_u8_valid_prefix (const uint8_t *string);
VISIBLE uint16_t *x_gc_u16_valid_prefix (const uint16_t *string);
VISIBLE uint32_t *x_gc_u32_valid_prefix (const uint32_t *string);

// Copy up to n characters (as opposed to storage units).
VISIBLE uint8_t *x_u8_strmbndup (const uint8_t *string, size_t n);
VISIBLE uint16_t *x_u16_strmbndup (const uint16_t *string, size_t n);
VISIBLE uint32_t *x_u32_strmbndup (const uint32_t *string, size_t n);
VISIBLE uint8_t *x_gc_u8_strmbndup (const uint8_t *string, size_t n);
VISIBLE uint16_t *x_gc_u16_strmbndup (const uint16_t *string, size_t n);
VISIBLE uint32_t *x_gc_u32_strmbndup (const uint32_t *string, size_t n);

VISIBLE uint8_t *x_gc_u8_strjoin (const uint8_t *s1, ...);
VISIBLE uint16_t *x_gc_u16_strjoin (const uint16_t *s1, ...);
VISIBLE uint32_t *x_gc_u32_strjoin (const uint32_t *s1, ...);
VISIBLE uint8_t *x_gc_u8_vstrjoin (const uint8_t *s1, va_list ap);
VISIBLE uint16_t *x_gc_u16_vstrjoin (const uint16_t *s1, va_list ap);
VISIBLE uint32_t *x_gc_u32_vstrjoin (const uint32_t *s1, va_list ap);

VISIBLE long int u8_strtol (const uint8_t *nptr, uint8_t **endptr, int base);
VISIBLE long int u16_strtol (const uint16_t *nptr, uint16_t **endptr,
                             int base);
VISIBLE long int u32_strtol (const uint32_t *nptr, uint32_t **endptr,
                             int base);
VISIBLE unsigned long int u8_strtoul (const uint8_t *nptr, uint8_t **endptr,
                                      int base);
VISIBLE unsigned long int u16_strtoul (const uint16_t *nptr,
                                       uint16_t **endptr, int base);
VISIBLE unsigned long int u32_strtoul (const uint32_t *nptr,
                                       uint32_t **endptr, int base);
VISIBLE double u8_strtod (const uint8_t *nptr, uint8_t **endptr);
VISIBLE double u16_strtod (const uint16_t *nptr, uint16_t **endptr);
VISIBLE double u32_strtod (const uint32_t *nptr, uint32_t **endptr);

VISIBLE int u8_get_next (const uint8_t **sptrptr);
VISIBLE int u16_get_next (const uint16_t **sptrptr);
VISIBLE int u32_get_next (const uint32_t **sptrptr);

static inline uint8_t *
x_u8_strdup (const uint8_t *string)
{
  return XDIE_ON_ENOMEM (u8_strdup (string));
}

static inline uint16_t *
x_u16_strdup (const uint16_t *string)
{
  return XDIE_ON_ENOMEM (u16_strdup (string));
}

static inline uint32_t *
x_u32_strdup (const uint32_t *string)
{
  return XDIE_ON_ENOMEM (u32_strdup (string));
}

static inline uint8_t *
x_gc_u8_strdup (const uint8_t *string)
{
  return x_gc_u8_grabstr (x_u8_strdup (string));
}

static inline uint16_t *
x_gc_u16_strdup (const uint16_t *string)
{
  return x_gc_u16_grabstr (x_u16_strdup (string));
}

static inline uint32_t *
x_gc_u32_strdup (const uint32_t *string)
{
  return x_gc_u32_grabstr (x_u32_strdup (string));
}

static inline uint8_t *
x_u8_strdup_or_null (const uint8_t *string)
{
  return NULL_PASSTHRU (string, x_u8_strdup (string));
}

static inline uint16_t *
x_u16_strdup_or_null (const uint16_t *string)
{
  return NULL_PASSTHRU (string, x_u16_strdup (string));
}

static inline uint32_t *
x_u32_strdup_or_null (const uint32_t *string)
{
  return NULL_PASSTHRU (string, x_u32_strdup (string));
}

inline uint16_t *
x_gc_u8_to_u16 (const uint8_t *string)
{
  return x_gc_u16_grabstr (x_u8_to_u16 (string));
}

inline uint32_t *
x_gc_u8_to_u32 (const uint8_t *string)
{
  return x_gc_u32_grabstr (x_u8_to_u32 (string));
}

inline uint8_t *
x_gc_u16_to_u8 (const uint16_t *string)
{
  return x_gc_u8_grabstr (x_u16_to_u8 (string));
}

inline uint32_t *
x_gc_u16_to_u32 (const uint16_t *string)
{
  return x_gc_u32_grabstr (x_u16_to_u32 (string));
}

inline uint8_t *
x_gc_u32_to_u8 (const uint32_t *string)
{
  return x_gc_u8_grabstr (x_u32_to_u8 (string));
}

inline uint16_t *
x_gc_u32_to_u16 (const uint32_t *string)
{
  return x_gc_u16_grabstr (x_u32_to_u16 (string));
}

inline bool
u8_valid (const uint8_t *string)
{
  return (u8_check (string, u8_strlen (string)) == NULL);
}

inline bool
u16_valid (const uint16_t *string)
{
  return (u16_check (string, u16_strlen (string)) == NULL);
}

inline bool
u32_valid (const uint32_t *string)
{
  return (u32_check (string, u32_strlen (string)) == NULL);
}

#endif // _FONTFORGE_XUNISTR_H
