/*
 * Copyright (C) 2012 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
*/

#ifndef _SORTSMILL_GMP_GC_H
#define _SORTSMILL_GMP_GC_H

/*
 * Garbage collection for GMP objects.
 */

#include <gmp.h>
#include <sortsmill/attributes.h>

#ifdef __cplusplus
extern "C" {
#endif
#if 0
}
#endif

void mpz_gc_init (mpz_t x);
void mpq_gc_init (mpq_t x);

_FF_ATTRIBUTE_SENTINEL void mpz_gc_inits (mpz_t x, ...);
_FF_ATTRIBUTE_SENTINEL void mpq_gc_inits (mpq_t x, ...);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GMP_GC_H */