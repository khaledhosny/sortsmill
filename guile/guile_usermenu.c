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

typedef struct
{
  int flag;
  SCM action;
  SCM enabled;
} menu_entry_data;

static SCM font_view_wrapper = SCM_UNDEFINED;
static SCM glyph_view_wrapper = SCM_UNDEFINED;

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
    flag = menu_fv;
  else if (scm_is_eq (window, scm_from_latin1_symbol ("glyph"))
           || scm_is_eq (window, scm_from_latin1_symbol ("char")))
    flag = menu_cv;
  else
    raise_window_error (window);
  return flag;
}

static SCM
wrap_view_pointer (int flag, void *p)
{
  SCM view = SCM_UNDEFINED;
  switch (flag)
    {
    case menu_fv:
      view = scm_call_1 (font_view_wrapper, scm_from_pointer (p, NULL));
      break;
    case menu_cv:
      view = scm_call_1 (glyph_view_wrapper, scm_from_pointer (p, NULL));
      break;
    default:
      assert (false);
    }
  return view;
}

static void
call_func (void *vdata, void *p)
{
  menu_entry_data *data = (menu_entry_data *) vdata;
  SCM view = wrap_view_pointer (data->flag, p);
  SCM wrapper =
    scm_c_private_ref ("sortsmillff usermenu", "menu-entry-error-handling");
  (void) scm_call_2 (wrapper, data->action, view);
}

static int
call_check (void *vdata, void *p)
{
  menu_entry_data *data = (menu_entry_data *) vdata;
  SCM view = wrap_view_pointer (data->flag, p);
  SCM wrapper =
    scm_c_private_ref ("sortsmillff usermenu", "menu-entry-error-handling");
  SCM retval = scm_call_2 (wrapper, data->enabled, view);
  return scm_is_true (retval);
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
      const char **submenu_names =
        (const char **) x_gc_malloc ((path_length + 1) *
                                     sizeof (const char *));
      size_t i = 0;
      for (SCM ls = menu_path; scm_is_pair (ls); ls = SCM_CDR (ls))
        {
          SCM entry = SCM_CAR (ls);
          SCM_ASSERT_TYPE (scm_is_string (entry), menu_path, SCM_ARG2,
                           "scm_register_fontforge_menu_entry",
                           "string list");
          submenu_names[i] = x_gc_grabstr (scm_to_locale_string (entry));
          i++;
        }
      submenu_names[i] = NULL;

      char *shortcut_str = NULL;
      if (scm_is_true (shortcut))
        {
          SCM_ASSERT (scm_is_string (shortcut), shortcut, SCM_ARG5,
                      "scm_register_fontforge_menu_entry");
          shortcut_str = x_gc_grabstr (scm_to_locale_string (shortcut));
        }

      menu_entry_data *data =
        (menu_entry_data *) x_gc_malloc (sizeof (menu_entry_data));
      data->flag = flag;
      data->action = action;
      data->enabled = enabled;

      RegisterMenuItem (call_func, call_check, data, data->flag, shortcut_str,
                        submenu_names);
    }

  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_usermenu (void)
{
  font_view_wrapper =
    scm_c_public_ref ("sortsmillff views", "wrap-font-view");
  glyph_view_wrapper =
    scm_c_public_ref ("sortsmillff views", "wrap-glyph-view");
  scm_c_define_gsubr ("internal:register-fontforge-menu-entry", 5, 0, 0,
                      scm_register_fontforge_menu_entry);
}

//-------------------------------------------------------------------------
