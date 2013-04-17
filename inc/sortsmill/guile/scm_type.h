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

#ifndef _SORTSMILL_GUILE_FONTS_SCM_TYPE_H
#define _SORTSMILL_GUILE_FONTS_SCM_TYPE_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void init_sortsmill_guile_scm_type (void);

inline SCM scm_c_SCM_ref (void *p);
inline SCM scm_SCM_ref (SCM ptr);
inline SCM scm_c_SCM_set_x (void *p, SCM value);
inline SCM scm_SCM_set_x (SCM ptr, SCM value);

SCM scm_c_bytevector_SCM_ref (SCM bv, size_t index);
inline SCM scm_bytevector_SCM_ref (SCM bv, SCM index);
SCM scm_c_bytevector_SCM_set_x (SCM bv, size_t index, SCM value);
inline SCM scm_bytevector_SCM_set_x (SCM bv, SCM index, SCM value);

inline SCM
scm_c_SCM_ref (void *p)
{
  return *(SCM *) p;
}

inline SCM
scm_SCM_ref (SCM ptr)
{
  return scm_c_SCM_ref (scm_to_pointer (ptr));
}

inline SCM
scm_c_SCM_set_x (void *p, SCM value)
{
  *(SCM *) p = value;
  return SCM_UNSPECIFIED;
}

inline SCM
scm_SCM_set_x (SCM ptr, SCM value)
{
  return scm_c_SCM_set_x (scm_to_pointer (ptr), value);
}

inline SCM
scm_bytevector_SCM_ref (SCM bv, SCM index)
{
  return scm_c_bytevector_SCM_ref (bv, scm_to_size_t (index));
}


inline SCM
scm_bytevector_SCM_set_x (SCM bv, SCM index, SCM value)
{
  return scm_c_bytevector_SCM_set_x (bv, scm_to_size_t (index), value);
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_SCM_TYPE_H */
