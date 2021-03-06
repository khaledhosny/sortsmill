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

#ifndef _SORTSMILL_GUILE_FONTS_FONTINFO_DICT_H
#define _SORTSMILL_GUILE_FONTS_FONTINFO_DICT_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_c_view_fontinfo_dict_set_x (SCM view, const char *key,
                                     const char *value);
SCM scm_view_fontinfo_dict_set_x (SCM view, SCM key, SCM value);

SCM scm_c_view_fontinfo_dict_ref (SCM view, const char *key);
SCM scm_view_fontinfo_dict_ref (SCM view, SCM key);

SCM scm_view_fontinfo_dict_set_from_alist_x (SCM view, SCM lst);
SCM scm_view_fontinfo_dict_to_alist (SCM view);
SCM scm_view_fontinfo_dict_keys (SCM view);

SCM scm_view_version_ref (SCM view);
SCM scm_view_Notice_ref (SCM view);
SCM scm_view_FullName_ref (SCM view);
SCM scm_view_FamilyName_ref (SCM view);
SCM scm_view_Weight_ref (SCM view);
SCM scm_view_ItalicAngle_ref (SCM view);
SCM scm_view_UnderlinePosition_ref (SCM view);
SCM scm_view_UnderlineThickness_ref (SCM view);
SCM scm_view_IsFixedPitch_ref (SCM view);       /* Does not distinguish a
                                                   missing entry from an
                                                   entry set to "false". */

SCM scm_view_version_set_x (SCM view, SCM value);
SCM scm_view_Notice_set_x (SCM view, SCM value);
SCM scm_view_FullName_set_x (SCM view, SCM value);
SCM scm_view_FamilyName_set_x (SCM view, SCM value);
SCM scm_view_Weight_set_x (SCM view, SCM value);
SCM scm_view_ItalicAngle_set_x (SCM view, SCM value);
SCM scm_view_UnderlinePosition_set_x (SCM view, SCM value);
SCM scm_view_UnderlineThickness_set_x (SCM view, SCM value);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_FONTINFO_DICT_H */
