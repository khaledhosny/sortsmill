#include <config.h>

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

#include <sortsmillff/guile/usermenu.h>
#include <sortsmillff/usermenu.h>
#include <sortsmillff/xgc.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

void init_guile_sortsmillff_usermenu (void);

//-------------------------------------------------------------------------

static void
raise_window_error (SCM window)
{
  scm_misc_error ("register_fontforge_menu_entry",
                  "Expected #:window 'font, 'glyph, or 'char, but got: ~S",
                  scm_list_1 (window));
}

static int
window_to_flag (SCM window)
{
  int flag = 0;
  if (scm_is_eq (window, scm_from_latin1_symbol ("font")))
    flag = FF_FONT_WINDOW;
  else if (scm_is_eq (window, scm_from_latin1_symbol ("glyph"))
           || scm_is_eq (window, scm_from_latin1_symbol ("char")))
    flag = FF_GLYPH_WINDOW;
  else
    raise_window_error (window);
  return flag;
}

VISIBLE SCM
scm_register_fontforge_menu_entry (SCM window, SCM menu_path, SCM action,
                                   SCM enabled, SCM shortcut)
{
  SCM_ASSERT_TYPE (scm_is_symbol (window), window, SCM_ARG1,
                   "scm_register_fontforge_menu_entry", "symbol");
  int flag = window_to_flag (window);

  SCM_ASSERT_TYPE (scm_is_pair (menu_path), menu_path, SCM_ARG2,
                   "scm_register_fontforge_menu_entry", "string list");
  size_t path_length = scm_to_size_t (scm_length (menu_path));
  if (0 < path_length)
    {
      const char **c_menu_path =
        (const char **) x_gc_malloc ((path_length + 1) *
                                     sizeof (const char *));
      size_t i = 0;
      for (SCM ls = menu_path; scm_is_pair (ls); ls = SCM_CDR (ls))
        {
          SCM entry = SCM_CAR (ls);
          SCM_ASSERT_TYPE (scm_is_string (entry), menu_path, SCM_ARG2,
                           "scm_register_fontforge_menu_entry",
                           "string list");
          c_menu_path[i] = x_gc_grabstr (scm_to_locale_string (entry));
          i++;
        }
      c_menu_path[i] = NULL;

      char *c_shortcut = NULL;
      if (scm_is_true (shortcut))
        {
          SCM_ASSERT (scm_is_string (shortcut), shortcut, SCM_ARG5,
                      "scm_register_fontforge_menu_entry");
          c_shortcut = x_gc_grabstr (scm_to_locale_string (shortcut));
        }

      register_fontforge_menu_entry (flag, c_menu_path, action, enabled,
                                     c_shortcut);
    }

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_usermenu (void)
{
  scm_c_define_gsubr ("internal:register-fontforge-menu-entry", 5, 0, 0,
                      scm_register_fontforge_menu_entry);
}

//-------------------------------------------------------------------------
