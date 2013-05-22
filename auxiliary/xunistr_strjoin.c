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
