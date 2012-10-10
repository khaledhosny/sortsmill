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
uint16_t *x_gc_u8_to_u16 (const uint8_t *string);
uint32_t *x_gc_u8_to_u32 (const uint8_t *string);
uint8_t *x_gc_u16_to_u8 (const uint16_t *string);
uint32_t *x_gc_u16_to_u32 (const uint16_t *string);
uint8_t *x_gc_u32_to_u8 (const uint32_t *string);
uint16_t *x_gc_u32_to_u16 (const uint32_t *string);
bool u8_valid (const uint8_t *string);
bool u16_valid (const uint16_t *string);
bool u32_valid (const uint32_t *string);

uint16_t *
x_u8_to_u16 (const uint8_t *string)
{
  size_t n = u8_strlen (string);
  assert (u8_check (string, n) == NULL);

  size_t length = n;            // Enough for UTF-16.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint16_t *buffer = xcalloc (length + 1, sizeof (uint16_t));

  (void) u8_to_u16 (string, n, buffer, &length);
  uint16_t *result = XDIE_ON_ENOMEM (u16_cpy_alloc (buffer, length));
  free (buffer);

  return result;
}

uint32_t *
x_u8_to_u32 (const uint8_t *string)
{
  size_t n = u8_strlen (string);
  assert (u8_check (string, n) == NULL);

  size_t length = n;            // Enough for UTF-32.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint32_t *buffer = xcalloc (length + 1, sizeof (uint32_t));

  (void) u8_to_u32 (string, n, buffer, &length);
  uint32_t *result = XDIE_ON_ENOMEM (u32_cpy_alloc (buffer, length + 1));
  free (buffer);

  return result;
}

uint8_t *
x_u16_to_u8 (const uint16_t *string)
{
  size_t n = u16_strlen (string);
  assert (u16_check (string, n) == NULL);

  size_t length = 2 * n;        // Enough for UTF-8.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint8_t *buffer = xcalloc (length + 1, sizeof (uint8_t));

  (void) u16_to_u8 (string, n, buffer, &length);
  uint8_t *result = XDIE_ON_ENOMEM (u8_cpy_alloc (buffer, length + 1));
  free (buffer);

  return result;
}

uint32_t *
x_u16_to_u32 (const uint16_t *string)
{
  size_t n = u16_strlen (string);
  assert (u16_check (string, n) == NULL);

  size_t length = n;            // Enough for UTF-32.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint32_t *buffer = xcalloc (length + 1, sizeof (uint32_t));

  (void) u16_to_u32 (string, n, buffer, &length);
  uint32_t *result = XDIE_ON_ENOMEM (u32_cpy_alloc (buffer, length + 1));
  free (buffer);

  return result;
}

uint8_t *
x_u32_to_u8 (const uint32_t *string)
{
  size_t n = u32_strlen (string);
  assert (u32_check (string, n) == NULL);

  size_t length = 4 * n;        // Enough for UTF-8.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint8_t *buffer = xcalloc (length + 1, sizeof (uint8_t));

  (void) u32_to_u8 (string, n, buffer, &length);
  uint8_t *result = XDIE_ON_ENOMEM (u8_cpy_alloc (buffer, length + 1));
  free (buffer);

  return result;
}

uint16_t *
x_u32_to_u16 (const uint32_t *string)
{
  size_t n = u32_strlen (string);
  assert (u32_check (string, n) == NULL);

  size_t length = 2 * n;        // Enough for UTF-16.

  // Use alloc’d space rather than a variable-length array (which may
  // be on the stack), so longer strings can be handled.
  uint16_t *buffer = xcalloc (length + 1, sizeof (uint16_t));

  (void) u32_to_u16 (string, n, buffer, &length);
  uint16_t *result = XDIE_ON_ENOMEM (u16_cpy_alloc (buffer, length + 1));
  free (buffer);

  return result;
}

const uint8_t *
u8_force_valid (const uint8_t *string)
{
  static const uint8_t empty_string[1] = { 0 };
  return (u8_valid (string)) ? string : empty_string;
}

const uint16_t *
u16_force_valid (const uint16_t *string)
{
  static const uint16_t empty_string[1] = { 0 };
  return (u16_valid (string)) ? string : empty_string;
}

const uint32_t *
u32_force_valid (const uint32_t *string)
{
  static const uint32_t empty_string[1] = { 0 };
  return (u32_valid (string)) ? string : empty_string;
}

uint8_t *x_u8_mbstrndup (const uint8_t *string, size_t n)
{
  // FIXME? This may not be the fastest implementation using
  // libunistring, although it is simple and likely will suffice.
  uint32_t *s32 = x_u8_to_u32 (string);
  uint32_t *s32_copy = x_u32_mbstrndup (s32, n);
  free (s32);
  uint8_t *s8_copy = x_u32_to_u8 (s32_copy);
  free (s32_copy);
  return s8_copy;
}

uint16_t *x_u16_mbstrndup (const uint16_t *string, size_t n)
{
  // FIXME? This may not be the fastest implementation using
  // libunistring, although it is simple and likely will suffice.
  uint32_t *s32 = x_u16_to_u32 (string);
  uint32_t *s32_copy = x_u32_mbstrndup (s32, n);
  free (s32);
  uint16_t *s16_copy = x_u32_to_u16 (s32_copy);
  free (s32_copy);
  return s16_copy;
}

uint32_t *x_u32_mbstrndup (const uint32_t *string, size_t n)
{
  uint32_t *copy;

  size_t i = 0;
  while (i < n && string[i] != 0)
    i++;
  if (string[i] == 0)
    copy = x_u32_strdup (string);
  else
    {
      copy = xmalloc ((n + 1) * sizeof (uint32_t));
      memcpy (copy, string, n * sizeof (uint32_t));
      copy[n] = 0;
    }
  return copy;
}
