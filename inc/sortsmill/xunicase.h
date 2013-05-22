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

#ifndef _SORTSMILL_XUNICASE_H
#define _SORTSMILL_XUNICASE_H

#include <unicase.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

int u8_casecompare (const uint8_t *s1, const uint8_t *s2);
int u16_casecompare (const uint16_t *s1, const uint16_t *s2);
int u32_casecompare (const uint32_t *s1, const uint32_t *s2);

/* The following routines count numbers of storage units rather than
   multibyte characters. (They are appropriate, for example, where 'n'
   equals the difference of two pointers.) */
int u8_ncasecompare (const uint8_t *s1, const uint8_t *s2, size_t n);
int u16_ncasecompare (const uint16_t *s1, const uint16_t *s2, size_t n);
int u32_ncasecompare (const uint32_t *s1, const uint32_t *s2, size_t n);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_XUNICASE_H */
