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

#ifndef _SORTSMILLFF_USERMENU_H
#define _SORTSMILLFF_USERMENU_H

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

enum
{
  menu_fv = 0x01,
  menu_cv = 0x02
};

typedef void (*menu_info_func) (void *, void *);
typedef int (*menu_info_check) (void *, void *);
typedef void *menu_info_data;

void RegisterMenuItem (menu_info_func func, menu_info_check check,
                       menu_info_data data, int flags,
                       const char *shortcut_str, const char **submenu_names);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILLFF_USERMENU_H */