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

#include <xunistring.h>

// Generate non-inline versions of these functions.
VISIBLE uint8_t *x_u8_strconv_from_locale (const char *string);
VISIBLE uint16_t *x_u16_strconv_from_locale (const char *string);
VISIBLE uint32_t *x_u32_strconv_from_locale (const char *string);
VISIBLE char *x_u8_strconv_to_locale (const uint8_t *string);
VISIBLE char *x_u16_strconv_to_locale (const uint16_t *string);
VISIBLE char *x_u32_strconv_to_locale (const uint32_t *string);
VISIBLE uint8_t *x_gc_u8_strconv_from_locale (const char *string);
VISIBLE uint16_t *x_gc_u16_strconv_from_locale (const char *string);
VISIBLE uint32_t *x_gc_u32_strconv_from_locale (const char *string);
VISIBLE char *x_gc_u8_strconv_to_locale (const uint8_t *string);
VISIBLE char *x_gc_u16_strconv_to_locale (const uint16_t *string);
VISIBLE char *x_gc_u32_strconv_to_locale (const uint32_t *string);
