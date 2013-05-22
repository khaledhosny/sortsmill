#include <config.h>

// Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <sortsmill/xgc.h>
#include <unistr.h>
#include <string.h>

// The following bunch of declarations should result in non-inline
// versions being generated.
VISIBLE void *x_gc_malloc (size_t sz);
VISIBLE void *x_gc_malloc_atomic (size_t sz);
VISIBLE void *x_gc_malloc_uncollectable (size_t sz);
VISIBLE void *x_gc_realloc (void *old_pointer, size_t sz);
VISIBLE void *x_gc_malloc_ignore_off_page (size_t sz);
VISIBLE void *x_gc_malloc_atomic_ignore_off_page (size_t sz);
VISIBLE void *x_gc_malloc_stubborn (size_t sz);
VISIBLE char *x_gc_strdup (const char *s);
VISIBLE char *x_gc_grabstr (char *s);

VISIBLE uint8_t *
x_gc_u8_grabstr (uint8_t *s)
{
  uint8_t *copy = x_gc_malloc_atomic ((u8_strlen (s) + 1) * sizeof (uint8_t));
  u8_strcpy (copy, s);
  free (s);
  return copy;
}

VISIBLE uint16_t *
x_gc_u16_grabstr (uint16_t *s)
{
  uint16_t *copy =
    x_gc_malloc_atomic ((u16_strlen (s) + 1) * sizeof (uint16_t));
  u16_strcpy (copy, s);
  free (s);
  return copy;
}

VISIBLE uint32_t *
x_gc_u32_grabstr (uint32_t *s)
{
  uint32_t *copy =
    x_gc_malloc_atomic ((u32_strlen (s) + 1) * sizeof (uint32_t));
  u32_strcpy (copy, s);
  free (s);
  return copy;
}

VISIBLE char *
x_gc_strndup (const char *s, size_t n)
{
  size_t i = 0;
  while (i < n && s[i] != '\0')
    i++;
  char *p = x_gc_malloc_atomic ((i + 1) * sizeof (char));
  p[0] = '\0';
  strncat (p, s, i);
  return p;
}
