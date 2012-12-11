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

#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

enum {
  /* Values chosen so they can be used in bitarrays. */
  FF_FONT_WINDOW = 0x01,
  FF_GLYPH_WINDOW = 0x02,
  FF_CHAR_WINDOW = FF_GLYPH_WINDOW
};

typedef void (*ff_menu_entry_action_t) (void *obj, void *data);
typedef bool (*ff_menu_entry_enabled_t) (void *obj, void *data);

void register_fontforge_menu_entry (int window,
				    const char **menu_path,
				    ff_menu_entry_action_t action,
				    ff_menu_entry_enabled_t enabled,
				    const char *shortcut,
				    void *data);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILLFF_USERMENU_H */
