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

#ifndef _SORTSMILL_GUILE_FONTS_PRIVATE_DICT_H
#define _SORTSMILL_GUILE_FONTS_PRIVATE_DICT_H

#include <libguile.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

void scm_c_view_private_dict_set_x (SCM view, const char *key,
                                    const char *value);
SCM scm_view_private_dict_set_x (SCM view, SCM key, SCM value);

const char *scm_c_view_private_dict_ref (SCM view, const char *key);
SCM scm_view_private_dict_ref (SCM view, SCM key);

void scm_c_view_private_dict_remove_x (SCM view, const char *key);
SCM scm_view_private_dict_remove_x (SCM view, SCM key);

SCM scm_view_private_dict_clear_x (SCM view);
SCM scm_view_private_dict_set_from_alist_x (SCM view, SCM lst);
SCM scm_view_alist_to_private_dict_x (SCM view, SCM lst);
SCM scm_view_private_dict_to_alist (SCM view);
SCM scm_view_private_dict_keys (SCM view);

SCM scm_view_BlueValues_ref (SCM view);
SCM scm_view_OtherBlues_ref (SCM view);
SCM scm_view_FamilyBlues_ref (SCM view);
SCM scm_view_FamilyOtherBlues_ref (SCM view);
SCM scm_view_StdHW_ref (SCM view);
SCM scm_view_StdVW_ref (SCM view);
SCM scm_view_StemSnapH_ref (SCM view);
SCM scm_view_StemSnapV_ref (SCM view);
SCM scm_view_BlueFuzz_ref (SCM view);
SCM scm_view_BlueScale_ref (SCM view);
SCM scm_view_BlueShift_ref (SCM view);
SCM scm_view_ExpansionFactor_ref (SCM view);
SCM scm_view_LanguageGroup_ref (SCM view);
SCM scm_view_ForceBold_ref (SCM view);  /* Does not distinguish a
                                           missing entry from an entry
                                           set to "false". */
SCM scm_view_RndStemUp_ref (SCM view);  /* Does not distinguish a
                                           missing entry from an entry
                                           set to "false". */

SCM scm_view_BlueValues_set_x (SCM view, SCM value);
SCM scm_view_OtherBlues_set_x (SCM view, SCM value);
SCM scm_view_FamilyBlues_set_x (SCM view, SCM value);
SCM scm_view_FamilyOtherBlues_set_x (SCM view, SCM value);
SCM scm_view_StdHW_set_x (SCM view, SCM value);
SCM scm_view_StdVW_set_x (SCM view, SCM value);
SCM scm_view_StemSnapH_set_x (SCM view, SCM value);
SCM scm_view_StemSnapV_set_x (SCM view, SCM value);
SCM scm_view_BlueFuzz_set_x (SCM view, SCM value);
SCM scm_view_BlueScale_set_x (SCM view, SCM value);
SCM scm_view_BlueShift_set_x (SCM view, SCM value);
SCM scm_view_ExpansionFactor_set_x (SCM view, SCM value);
SCM scm_view_LanguageGroup_set_x (SCM view, SCM value);
SCM scm_view_ForceBold_set_x (SCM view, SCM value);
SCM scm_view_RndStemUp_set_x (SCM view, SCM value);

SCM scm_view_BlueValues_remove_x (SCM view);
SCM scm_view_OtherBlues_remove_x (SCM view);
SCM scm_view_FamilyBlues_remove_x (SCM view);
SCM scm_view_FamilyOtherBlues_remove_x (SCM view);
SCM scm_view_StdHW_remove_x (SCM view);
SCM scm_view_StdVW_remove_x (SCM view);
SCM scm_view_StemSnapH_remove_x (SCM view);
SCM scm_view_StemSnapV_remove_x (SCM view);
SCM scm_view_BlueFuzz_remove_x (SCM view);
SCM scm_view_BlueScale_remove_x (SCM view);
SCM scm_view_BlueShift_remove_x (SCM view);
SCM scm_view_ExpansionFactor_remove_x (SCM view);
SCM scm_view_LanguageGroup_remove_x (SCM view);
SCM scm_view_ForceBold_remove_x (SCM view);
SCM scm_view_RndStemUp_remove_x (SCM view);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_PRIVATE_DICT_H */
