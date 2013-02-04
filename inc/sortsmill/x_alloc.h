/*
 * Copyright (C) 2013 Barry Schwartz
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

#ifndef _SORTSMILL_X_ALLOC_H
#define _SORTSMILL_X_ALLOC_H

/*
 * Exported wrappers around otherwise hidden xalloc routines.
 */

#include <stdlib.h>
#include <sortsmill/attributes.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

_FF_ATTRIBUTE_NORETURN void x_alloc_die (void);

/* *INDENT-OFF* */

void *x_malloc (size_t s)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((1));

void *x_zalloc (size_t s)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((1));

void *x_calloc (size_t n, size_t s)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((1, 2));

void *x_realloc (void *p, size_t s) _FF_ATTRIBUTE_ALLOC_SIZE ((2));

void *x_2realloc (void *p, size_t *pn);

void *x_memdup (const void *p, size_t s)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((2));

char *x_strdup (const char *str) _FF_ATTRIBUTE_MALLOC;

void *x_nmalloc (size_t n, size_t s)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((1, 2));

void *x_nrealloc (void *p, size_t n, size_t s)
     _FF_ATTRIBUTE_ALLOC_SIZE ((2, 3));

void *x_2nrealloc (void *p, size_t *pn, size_t s);

char *x_charalloc (size_t n)
     _FF_ATTRIBUTE_MALLOC _FF_ATTRIBUTE_ALLOC_SIZE ((1));

/* *INDENT-ON* */

#define X_MALLOC(t) ((t *) x_malloc (sizeof (t)))
#define X_NMALLOC(n, t) ((t *) (sizeof (t) == 1 ? x_malloc (n) : x_nmalloc ((n), sizeof (t))))
#define X_ZALLOC(t) ((t *) x_zalloc (sizeof (t)))
#define X_CALLOC(n, t) ((t *) (sizeof (t) == 1 ? x_zalloc (n) : x_calloc ((n), sizeof (t))))

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_X_ALLOC_H */
