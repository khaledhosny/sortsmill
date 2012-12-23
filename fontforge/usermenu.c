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

GMenuItem2 *cv_menu = NULL;
GMenuItem2 *fv_menu = NULL;

typedef struct
{
  SCM action;
  SCM enabled;
} menu_info;

static menu_info *cv_menu_info = NULL;
static menu_info *fv_menu_info = NULL;
static int cv_menu_size = 0;
static int cv_menu_max_size = 0;
static int fv_menu_size = 0;
static int fv_menu_max_size = 0;

//-------------------------------------------------------------------------

static void
tools_list_check (struct gmenuitem *mi, SCM owner,
                  menu_info *info, int menu_size)
{
  if (info != NULL)
    for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; mi++)
      if (mi->mid == -1)        /* Submenu */
        ;
      else if (mi->mid < 0 || menu_size <= mi->mid)
        {
          fprintf (stderr, _("Bad Menu ID in menu %d\n"), mi->mid);
          mi->ti.disabled = true;
        }
      else if (info[mi->mid].enabled == NULL)
        mi->ti.disabled = false;
      else
        mi->ti.disabled =
          scm_is_false (scm_call_2
                        (scm_c_private_ref ("sortsmillff usermenu",
                                            "menu-entry-error-handling"),
                         info[mi->mid].enabled, owner));
}

void
cv_tools_list_check (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  CharViewBase *cvb = (CharViewBase *) GDrawGetUserData (gw);
  if (cv_menu_info != NULL)
    {
      sc_active_in_ui = cvb->sc;
      layer_active_in_ui = CVLayer (cvb);
      SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views",
                                               "pointer->glyph-view"),
                             scm_from_pointer (cvb, NULL));
      tools_list_check (mi, view, cv_menu_info, cv_menu_size);
      sc_active_in_ui = NULL;
      layer_active_in_ui = ly_fore;
    }
}

void
fv_tools_list_check (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  FontViewBase *fvb = (FontViewBase *) GDrawGetUserData (gw);
  if (fv_menu_info != NULL)
    {
      fv_active_in_ui = fvb;
      layer_active_in_ui = fvb->active_layer;
      SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views",
                                               "pointer->font-view"),
                             scm_from_pointer (fvb, NULL));
      tools_list_check (mi, view, fv_menu_info, fv_menu_size);
      fv_active_in_ui = NULL;
    }
}

//-------------------------------------------------------------------------

static void
do_action (struct gmenuitem *mi, SCM owner, menu_info *info, int menu_size)
{
  if (mi->mid == -1)            /* Submenu */
    ;
  else if (mi->mid < 0 || menu_size <= mi->mid)
    fprintf (stderr, _("Bad Menu ID in menu %d\n"), mi->mid);
  else if (info[mi->mid].action == NULL)
    ;
  else
    scm_call_2 (scm_c_private_ref ("sortsmillff usermenu",
                                   "menu-entry-error-handling"),
                info[mi->mid].action, owner);
}

static void
cv_do_action (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  CharViewBase *cvb = (CharViewBase *) GDrawGetUserData (gw);
  if (cv_menu_info != NULL)
    {
      sc_active_in_ui = cvb->sc;
      layer_active_in_ui = CVLayer (cvb);
      SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views",
                                               "pointer->glyph-view"),
                             scm_from_pointer (cvb, NULL));
      do_action (mi, view, cv_menu_info, cv_menu_size);
      sc_active_in_ui = NULL;
      layer_active_in_ui = ly_fore;
    }
}

static void
fv_do_action (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  FontViewBase *fvb = (FontViewBase *) GDrawGetUserData (gw);
  if (fv_menu_info != NULL)
    {
      fv_active_in_ui = fvb;
      layer_active_in_ui = fvb->active_layer;
      SCM view = scm_call_1 (scm_c_public_ref ("sortsmillff views",
                                               "pointer->font-view"),
                             scm_from_pointer (fvb, NULL));
      do_action (mi, view, fv_menu_info, fv_menu_size);
      fv_active_in_ui = NULL;
    }
}

//-------------------------------------------------------------------------

static int
menu_info_add (int window, SCM action, SCM enabled)
{
  int index = INT_MIN;

  switch (window)
    {
    case FF_GLYPH_WINDOW:
      if (cv_menu_max_size <= cv_menu_size)
        {
          cv_menu_max_size += 10;
          cv_menu_info =
            xrealloc (cv_menu_info, cv_menu_max_size * sizeof (menu_info));
        }
      cv_menu_info[cv_menu_size].action = action;
      cv_menu_info[cv_menu_size].enabled = enabled;
      index = cv_menu_size;
      cv_menu_size++;
      break;

    case FF_FONT_WINDOW:
      if (fv_menu_max_size <= fv_menu_size)
        {
          fv_menu_max_size += 10;
          fv_menu_info =
            xrealloc (fv_menu_info, fv_menu_max_size * sizeof (menu_info));
        }
      fv_menu_info[fv_menu_size].action = action;
      fv_menu_info[fv_menu_size].enabled = enabled;
      index = fv_menu_size;
      fv_menu_size++;
      break;

    default:
      assert (false);
    }

  return index;
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
find_sub_menu (GMenuItem2 **mn, uint32_t *submenuu)
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
                  const char *shortcut, GMenuItem2 **mn)
{
  int i;
  int j;
  GMenuItem2 *mmn;

  for (i = 0; menu_path[i] != NULL; i++)
    {
      uint32_t *submenuu = utf82u_copy (menu_path[i]);

      j = 0;
      if (*mn != NULL)
        j = find_sub_menu (mn, submenuu);

      if (*mn == NULL || (*mn)[j].ti.text == NULL)
        {
          *mn = xrealloc (*mn, (j + 2) * sizeof (GMenuItem2));
          memset (*mn + j, 0, 2 * sizeof (GMenuItem2));
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
