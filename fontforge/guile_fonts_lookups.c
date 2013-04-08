#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <libguile.h>
#include <splinefont.h>
#include <guile_fontforge_internals.h>

void
init_guile_fonts_lookups (void)
{
  scm_c_define ("lookup-type:ot-undef", scm_from_int (ot_undef));
  scm_c_define ("lookup-type:gsub-start", scm_from_int (gsub_start));
  scm_c_define ("lookup-type:gsub-single", scm_from_int (gsub_single));
  scm_c_define ("lookup-type:gsub-multiple", scm_from_int (gsub_multiple));
  scm_c_define ("lookup-type:gsub-alternate", scm_from_int (gsub_alternate));
  scm_c_define ("lookup-type:gsub-ligature", scm_from_int (gsub_ligature));
  scm_c_define ("lookup-type:gsub-contextual", scm_from_int (gsub_context));
  scm_c_define ("lookup-type:gsub-chaining-contextual", scm_from_int (gsub_contextchain));
  scm_c_define ("lookup-type:gsub-reverse-chaining-contextual", scm_from_int (gsub_reversecchain));
  scm_c_define ("lookup-type:gpos-start", scm_from_int (gpos_start));
  scm_c_define ("lookup-type:gpos-single", scm_from_int (gpos_single));
  scm_c_define ("lookup-type:gpos-pair", scm_from_int (gpos_pair));
  scm_c_define ("lookup-type:gpos-cursive", scm_from_int (gpos_cursive));
  scm_c_define ("lookup-type:gpos-mark-to-base", scm_from_int (gpos_mark2base));
  scm_c_define ("lookup-type:gpos-mark-to-ligature", scm_from_int (gpos_mark2ligature));
  scm_c_define ("lookup-type:gpos-mark-to-mark", scm_from_int (gpos_mark2mark));
  scm_c_define ("lookup-type:gpos-contextual", scm_from_int (gpos_context));
  scm_c_define ("lookup-type:gpos-chaining-contextual", scm_from_int (gpos_contextchain));
}
