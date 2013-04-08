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
init_guile_fonts_anchors (void)
{
  // FIXME: GET RID OF THESE. They are redundant data. Use the lookup
  // type instead.
  scm_c_define ("anchor-class-type:mark-to-base", scm_from_int (act_mark));
  scm_c_define ("anchor-class-type:mark-to-mark", scm_from_int (act_mkmk));
  scm_c_define ("anchor-class-type:cursive", scm_from_int (act_curs));
  scm_c_define ("anchor-class-type:mark-to-ligature", scm_from_int (act_mklg));

  scm_c_define ("anchor-type:mark", scm_from_int (at_mark));
  scm_c_define ("anchor-type:base", scm_from_int (at_basechar));
  scm_c_define ("anchor-type:ligature", scm_from_int (at_baselig));
  scm_c_define ("anchor-type:base-mark", scm_from_int (at_basemark));
  scm_c_define ("anchor-type:entry", scm_from_int (at_centry));
  scm_c_define ("anchor-type:exit", scm_from_int (at_cexit));
  scm_c_define ("anchor-type:max", scm_from_int (at_max));
  scm_c_define ("anchor-type:illegal", scm_from_int (at_illegal));
}
