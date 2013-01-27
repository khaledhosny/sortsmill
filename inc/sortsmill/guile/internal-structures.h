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

#ifndef _SORTSMILL_GUILE_INTERNAL_STRUCTURES_H
#define _SORTSMILL_GUILE_INTERNAL_STRUCTURES_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

#define _FF_INTERNAL_STRUCTURES_POINTER_DECL(TYPE)	\
  SCM scm_ff_##TYPE##_p (SCM obj);			\
  int scm_is_ff_##TYPE (SCM obj)

_FF_INTERNAL_STRUCTURES_POINTER_DECL (FontViewBase);
_FF_INTERNAL_STRUCTURES_POINTER_DECL (SplineFont);
_FF_INTERNAL_STRUCTURES_POINTER_DECL (SplineChar);
_FF_INTERNAL_STRUCTURES_POINTER_DECL (EncMap);
_FF_INTERNAL_STRUCTURES_POINTER_DECL (BDFFont);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_INTERNAL_STRUCTURES_H */
