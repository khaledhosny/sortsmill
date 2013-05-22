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

#ifndef _SORTSMILL_XDIE_ON_NULL_H
#define _SORTSMILL_XDIE_ON_NULL_H

#include <assert.h>
#include <stddef.h>
#include <errno.h>
#include <sortsmill/attributes.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

_FF_ATTRIBUTE_PURE inline void *xdie_on_null (void *p);
_FF_ATTRIBUTE_PURE inline void *xdie_on_enomem (void *p);
void ff_xalloc_die (void);

inline void *
xdie_on_null (void *p)
{
  if (p == NULL)
    ff_xalloc_die ();
  return p;
}

inline void *
xdie_on_enomem (void *p)
{
  if (p == NULL && errno == ENOMEM)
    ff_xalloc_die ();
  assert (p != NULL);           /* May fail if strings have not been validated. */
  return p;
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

/* The macro XDIE_ON_NULL tries to avoid implicit type-casting between
   the type of p and (void *). This works with gcc, in particular. */
#define XDIE_ON_NULL(p) (_FF_CAST_TYPEOF (p) xdie_on_null ((void *) (p)))

/* The macro XDIE_ON_ENOMEM tries to avoid implicit type-casting
   between the type of p and (void *). This works with gcc, in
   particular. */
#define XDIE_ON_ENOMEM(p) (_FF_CAST_TYPEOF (p) xdie_on_enomem ((void *) (p)))

#endif /* _SORTSMILL_XDIE_ON_NULL_H */
