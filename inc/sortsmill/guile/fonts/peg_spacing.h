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

#ifndef _SORTSMILL_GUILE_FONTS_PEG_SPACING_H
#define _SORTSMILL_GUILE_FONTS_PEG_SPACING_H

#include <libguile.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

bool scm_is_spacing_peg_name (SCM name);
SCM scm_spacing_peg_name_p (SCM name);
SCM scm_spacing_peg_side (SCM name);
SCM scm_spacing_peg_modifier (SCM name);
SCM scm_spacing_peg_identifier (SCM name);

SCM scm_spacing_pegs (SCM anchor_points);
SCM scm_left_spacing_pegs (SCM anchor_points);
SCM scm_right_spacing_pegs (SCM anchor_points);
SCM scm_ordinary_spacing_pegs (SCM anchor_points);
SCM scm_kerning_only_spacing_pegs (SCM anchor_points);
SCM scm_special_spacing_pegs (SCM anchor_points);
SCM scm_nonspecial_spacing_pegs (SCM anchor_points);
SCM scm_left_ordinary_spacing_pegs (SCM anchor_points);
SCM scm_right_ordinary_spacing_pegs (SCM anchor_points);

SCM scm_within_peg_spacing_tolerance_p (SCM a, SCM b);
SCM scm_peg_spacing_left_spacing (SCM anchor_points);
SCM scm_peg_spacing_right_spacing (SCM anchor_points);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_GUILE_FONTS_PEG_SPACING_H */
