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

#ifndef _SORTSMILL_GUILE_FONTS_OS2_TABLE_H
#define _SORTSMILL_GUILE_FONTS_OS2_TABLE_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_c_view_os2_table_set_x (SCM view, const char *key, SCM value,
                                 SCM value_is_offset);
SCM scm_view_os2_table_set_x (SCM view, SCM key, SCM value,
                              SCM value_is_offset);

SCM scm_c_view_os2_table_ref (SCM view, const char *key, SCM value_is_offset);
SCM scm_view_os2_table_ref (SCM view, SCM key, SCM value_is_offset);

SCM scm_view_os2_table_set_from_alist_x (SCM view, SCM lst);
SCM scm_view_os2_table_to_alist (SCM view);
SCM scm_view_os2_table_keys (SCM view);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_OS2_TABLE_H */
