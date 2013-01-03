#include <config.h>

/* Copyright (C) 2007-2012 by George Williams */
/*
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <sortsmillff/guile/views.h>
#include <fontforgeui.h>
#include <usermenu.h>
#include <ustring.h>
#include <stdio.h>
#include <stdint.h>

GMenuItem *cv_menu = NULL;
GMenuItem *fv_menu = NULL;

void
cv_tools_list_check (GWindow gw, GMenuItem *mi, GEvent *e)
{
  CharViewBase *cvb = (CharViewBase *) GDrawGetUserData (gw);
  SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views", "pointer->glyph-view"),
			 scm_from_pointer (cvb, NULL));
  SCM menu_info = scm_c_private_ref ("sortsmillff usermenu", "cv-menu-info");
  scm_call_3 (scm_c_private_ref ("sortsmillff usermenu", "tools-list-check"),
	      scm_from_pointer (mi, NULL), view, menu_info);
}

void
fv_tools_list_check (GWindow gw, GMenuItem *mi, GEvent *e)
{
  FontViewBase *fvb = (FontViewBase *) GDrawGetUserData (gw);
  SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views", "pointer->font-view"),
			 scm_from_pointer (fvb, NULL));
  SCM menu_info = scm_c_private_ref ("sortsmillff usermenu", "fv-menu-info");
  scm_call_3 (scm_c_private_ref ("sortsmillff usermenu", "tools-list-check"),
	      scm_from_pointer (mi, NULL), view, menu_info);
}

static void
cv_do_action (GWindow gw, GMenuItem *mi, GEvent *e)
{
  CharViewBase *cvb = (CharViewBase *) GDrawGetUserData (gw);
  SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views", "pointer->glyph-view"),
			 scm_from_pointer (cvb, NULL));
  SCM menu_info = scm_c_private_ref ("sortsmillff usermenu", "cv-menu-info");
  scm_call_3 (scm_c_private_ref ("sortsmillff usermenu", "do-action"),
	      scm_from_pointer (mi, NULL), view, menu_info);
}

static void
fv_do_action (GWindow gw, GMenuItem *mi, GEvent *e)
{
  FontViewBase *fvb = (FontViewBase *) GDrawGetUserData (gw);
  SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views", "pointer->font-view"),
			 scm_from_pointer (fvb, NULL));
  SCM menu_info = scm_c_private_ref ("sortsmillff usermenu", "fv-menu-info");
  scm_call_3 (scm_c_private_ref ("sortsmillff usermenu", "do-action"),
	      scm_from_pointer (mi, NULL), view, menu_info);
}

static int
menu_info_add (int window, SCM action, SCM enabled)
{
  SCM menu_info = SCM_UNSPECIFIED;
  switch (window)
    {
    case FF_GLYPH_WINDOW:
      menu_info = scm_c_private_ref ("sortsmillff usermenu", "cv-menu-info");
      break;
    case FF_FONT_WINDOW:
      menu_info = scm_c_private_ref ("sortsmillff usermenu", "fv-menu-info");
      break;
    default:
      assert (false);
    }
  SCM menu_info_entry = scm_list_2 (action, enabled);
  SCM mid = scm_call_2 (scm_c_private_ref ("sortsmillff usermenu", "menu-info-add!"),
			menu_info, menu_info_entry);
  return scm_to_int (mid);
}

static gmenuitem_moveto_t
moveto_func (int window)
{
  gmenuitem_moveto_t result = NULL;
  switch (window)
    {
    case FF_GLYPH_WINDOW:
      result = cv_tools_list_check;
      break;
    case FF_FONT_WINDOW:
      result = fv_tools_list_check;
      break;
    default:
      assert (false);
    }
  return result;
}

static gmenuitem_invoke_t
invoke_func (int window)
{
  gmenuitem_invoke_t result = NULL;
  switch (window)
    {
    case FF_GLYPH_WINDOW:
      result = cv_do_action;
      break;
    case FF_FONT_WINDOW:
      result = fv_do_action;
      break;
    default:
      assert (false);
    }
  return result;
}

static int
find_sub_menu (GMenuItem **mn, uint32_t *submenuu)
{
  int j = 0;
  bool submenu_found = false;
  while (!submenu_found && ((*mn)[j].ti.text != NULL || (*mn)[j].ti.line))
    {
      if ((*mn)[j].ti.text != NULL
          // FIXME: Should this be a normalized comparison?
          && u32_strcmp ((*mn)[j].ti.text, submenuu) == 0)
        submenu_found = true;
      else
        j++;
    }
  return j;
}

static void
insert_sub_menus (int window, const char **menu_path, SCM action, SCM enabled,
                  const char *shortcut, GMenuItem **mn)
{
  int i;
  int j;
  GMenuItem *mmn;

  for (i = 0; menu_path[i] != NULL; i++)
    {
      uint32_t *submenuu = utf82u_copy (menu_path[i]);

      j = 0;
      if (*mn != NULL)
        j = find_sub_menu (mn, submenuu);

      if (*mn == NULL || (*mn)[j].ti.text == NULL)
        {
          *mn = xrealloc (*mn, (j + 2) * sizeof (GMenuItem));
          memset (*mn + j, 0, 2 * sizeof (GMenuItem));
        }
      mmn = *mn;
      if (mmn[j].ti.text == NULL)
        {
          mmn[j].ti.text = submenuu;
          mmn[j].ti.fg = COLOR_DEFAULT;
          mmn[j].ti.bg = COLOR_DEFAULT;
          if (menu_path[i + 1] != NULL)
            {
              mmn[j].mid = -1;
              mmn[j].moveto = moveto_func (window);
              mn = &mmn[j].sub;
            }
          else
            {
              mmn[j].shortcut = xstrdup_or_null (shortcut);
              mmn[j].invoke = invoke_func (window);
              mmn[j].mid = menu_info_add (window, action, enabled);
            }
        }
      else
        {
          if (menu_path[i + 1] != NULL)
            mn = &mmn[j].sub;
          else
            {
              mmn[j].shortcut = xstrdup_or_null (shortcut);
              mmn[j].invoke = invoke_func (window);
              mmn[j].mid = menu_info_add (window, action, enabled);
              fprintf (stderr, _("Redefining menu entry %s\n"), menu_path[i]);
              free (submenuu);
            }
        }
    }
}

VISIBLE void
register_fontforge_menu_entry (int window, const char **menu_path, SCM action,
                               SCM enabled, const char *shortcut)
{
  if (!no_windowing_ui)
    switch (window)
      {
      case FF_FONT_WINDOW:
        insert_sub_menus (window, menu_path, action, enabled, shortcut,
                          &fv_menu);
        break;
      case FF_GLYPH_WINDOW:
        insert_sub_menus (window, menu_path, action, enabled, shortcut,
                          &cv_menu);
        break;
      }
}
