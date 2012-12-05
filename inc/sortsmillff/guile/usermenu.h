// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#ifndef _SORTSMILLFF_GUILE_USERMENU_H
#define _SORTSMILLFF_GUILE_USERMENU_H

#include <libguile.h>

SCM scm_register_fontforge_menu_item (SCM window, SCM menu_path, SCM action,
                                      SCM enabled, SCM shortcut);

#endif /* _SORTSMILLFF_GUILE_USERMENU_H */
