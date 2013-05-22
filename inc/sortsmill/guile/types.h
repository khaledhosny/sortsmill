/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#ifndef _SORTSMILL_GUILE_FONTS_TYPES_H
#define _SORTSMILL_GUILE_FONTS_TYPES_H

#include <libguile.h>

/* Include <sortsmill/guile/scm_type.h>, mainly because the name may
   be similar enough to create confusion if we do not include it. */
#include <sortsmill/guile/scm_type.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_c_raise_is_not_a_list (const char *who, SCM lst);
void scm_c_raise_is_not_an_alist (const char *who, SCM lst);
void scm_c_raise_list_ends_prematurely (const char *who, SCM lst);

inline void scm_c_assert_can_be_list_link (const char *who, SCM lst, SCM p);
inline void scm_c_assert_can_be_alist_link (const char *who, SCM lst, SCM p);
inline void scm_c_assert_list_does_not_end_here (const char *who, SCM lst,
                                                 SCM p);

inline void
scm_c_assert_can_be_list_link (const char *who, SCM lst, SCM p)
{
  if (!scm_is_pair (p))
    scm_c_raise_is_not_a_list (who, lst);
}

inline void
scm_c_assert_can_be_alist_link (const char *who, SCM lst, SCM p)
{
  if (!scm_is_pair (p) || !scm_is_pair (SCM_CAR (p)))
    scm_c_raise_is_not_an_alist (who, lst);
}

inline void
scm_c_assert_list_does_not_end_here (const char *who, SCM lst, SCM p)
{
  if (scm_is_null (p))
    scm_c_raise_list_ends_prematurely (who, lst);
}

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_TYPES_H */
