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

#ifndef _SORTSMILL_XUNICONV_H
#define _SORTSMILL_XUNICONV_H

/*
 * Enhancements to libunistring conversion routines.
 */

/* *INDENT-OFF* */
#ifdef __cplusplus
extern "C" {
#endif
/* *INDENT-ON* */

#include <stdio.h>
#include <uniconv.h>
#include <sortsmill/xdie_on_null.h>
#include <sortsmill/xgc.h>

inline uint8_t *x_u8_strconv_from_locale (const char *string);
inline uint16_t *x_u16_strconv_from_locale (const char *string);
inline uint32_t *x_u32_strconv_from_locale (const char *string);
inline char *x_u8_strconv_to_locale (const uint8_t *string);
inline char *x_u16_strconv_to_locale (const uint16_t *string);
inline char *x_u32_strconv_to_locale (const uint32_t *string);
inline uint8_t *x_gc_u8_strconv_from_locale (const char *string);
inline uint16_t *x_gc_u16_strconv_from_locale (const char *string);
inline uint32_t *x_gc_u32_strconv_from_locale (const char *string);
inline char *x_gc_u8_strconv_to_locale (const uint8_t *string);
inline char *x_gc_u16_strconv_to_locale (const uint16_t *string);
inline char *x_gc_u32_strconv_to_locale (const uint32_t *string);

inline uint8_t *
x_u8_strconv_from_locale (const char *string)
{
  return XDIE_ON_ENOMEM (u8_strconv_from_locale (string));
}

inline uint16_t *
x_u16_strconv_from_locale (const char *string)
{
  return XDIE_ON_ENOMEM (u16_strconv_from_locale (string));
}

inline uint32_t *
x_u32_strconv_from_locale (const char *string)
{
  return XDIE_ON_ENOMEM (u32_strconv_from_locale (string));
}

inline char *
x_u8_strconv_to_locale (const uint8_t *string)
{
  return XDIE_ON_ENOMEM (u8_strconv_to_locale (string));
}

inline char *
x_u16_strconv_to_locale (const uint16_t *string)
{
  return XDIE_ON_ENOMEM (u16_strconv_to_locale (string));
}

inline char *
x_u32_strconv_to_locale (const uint32_t *string)
{
  return XDIE_ON_ENOMEM (u32_strconv_to_locale (string));
}

inline uint8_t *
x_gc_u8_strconv_from_locale (const char *string)
{
  return x_gc_u8_grabstr (x_u8_strconv_from_locale (string));
}

inline uint16_t *
x_gc_u16_strconv_from_locale (const char *string)
{
  return x_gc_u16_grabstr (x_u16_strconv_from_locale (string));
}

inline uint32_t *
x_gc_u32_strconv_from_locale (const char *string)
{
  return x_gc_u32_grabstr (x_u32_strconv_from_locale (string));
}

inline char *
x_gc_u8_strconv_to_locale (const uint8_t *string)
{
  return x_gc_grabstr (x_u8_strconv_to_locale (string));
}

inline char *
x_gc_u16_strconv_to_locale (const uint16_t *string)
{
  return x_gc_grabstr (x_u16_strconv_to_locale (string));
}

inline char *
x_gc_u32_strconv_to_locale (const uint32_t *string)
{
  return x_gc_grabstr (x_u32_strconv_to_locale (string));
}

/* *INDENT-OFF* */
#ifdef __cplusplus
}
#endif
/* *INDENT-ON* */

#endif /* _SORTSMILL_XUNICONV_H */
