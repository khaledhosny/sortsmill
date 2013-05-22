#include <config.h>

// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

/* Copyright (C) 2000-2012 by George Williams */
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

#include <stdlib.h>
#include <gdraw.h>
#include "ggadgetP.h"
#include <gwidget.h>
#include <ustring.h>
#include <gkeysym.h>
#include <utype.h>
#include <gresource.h>

static GBox menubar_box = GBOX_EMPTY;   /* Don't initialize here */
static GBox menu_box = GBOX_EMPTY;      /* Don't initialize here */

static FontInstance *menu_font = NULL;
static FontInstance *menubar_font = NULL;

static int gmenubar_inited = false;

#ifdef __Mac
static int mac_menu_icons = true;
#else
static int mac_menu_icons = false;
#endif

static int mask_set = 0;

/* These are the modifier masks expected in menus. Will be overridden
   by what's actually there. */
static int menumask = ksm_control | ksm_meta | ksm_shift;

#ifndef _Keyboard
#define _Keyboard 0
#endif

static enum
{
  kb_ibm,
  kb_mac,
  kb_sun,
  kb_ppc
} keyboard = _Keyboard;
/* Sigh. In old XonX the command key is mapped to 0x20 and Option to 0x8 (meta) */
/*  the option key conversions (option-c => ccedilla) are not done */
/*  In the next X, the command key is mapped to 0x10 and Option to 0x2000 */
/*  (again option key conversion are not done) */
/*  In 10.3, the command key is mapped to 0x10 and Option to 0x8 */
/*  In 10.5 the command key is mapped to 0x10 and Option to 0x8 */
/*   (and option conversions are done) */
/*  While in Suse PPC X, the command key is 0x8 (meta) and option is 0x2000 */
/*  and the standard mac option conversions are done */

static GResInfo gmenu_ri;

static GResInfo gmenubar_ri = {
  &gmenu_ri,
  &ggadget_ri,
  &gmenu_ri,
  NULL,
  &menubar_box,
  &menubar_font,
  NULL,
  NULL,
  N_("Menu Bar"),
  N_("Menu Bar"),
  "GMenuBar",
  "Gdraw",
  false,
  omf_border_shape | omf_border_width | box_foreground_border_outer,
  NULL,
  GBOX_EMPTY,
  NULL,
  NULL,
  NULL
};

static struct resed menu_re[] = {
  {N_("MacIcons"), "MacIcons", rt_bool, &mac_menu_icons,
   N_
   ("Whether to use mac-like icons to indicate modifiers (for instance ^ for Control)\nor to use an abbreviation (for instance \"Cnt-\")"),
   NULL, {0}, 0, 0},
  RESED_EMPTY
};

static GResInfo gmenu_ri = {
  NULL, &ggadget_ri, &gmenubar_ri, NULL,
  &menu_box,
  &menu_font,
  NULL,
  menu_re,
  N_("Menu"),
  N_("Menu"),
  "GMenu",
  "Gdraw",
  false,
  omf_border_shape | omf_padding | box_foreground_border_outer,
  NULL,
  GBOX_EMPTY,
  NULL,
  NULL,
  NULL
};

static void GMenuBarChangeSelection (GMenuBar * mb, int newsel, GEvent *);
static struct gmenu *GMenuCreateSubMenu (struct gmenu *parent, GMenuItem *mi,
                                         int disable);
static struct gmenu *GMenuCreatePulldownMenu (GMenuBar * mb, GMenuItem *mi,
                                              int disabled);

static int menu_grabs = true;
static struct gmenu *most_recent_popup_menu = NULL;

static bool
GTextInfo_nonempty (GTextInfo *ti)
{
  return (ti->text != NULL || ti->image != NULL || ti->line);
}

VISIBLE bool
GMenuItem_nonempty (GMenuItem *mi)
{
  return GTextInfo_nonempty (&mi->ti);
}

static void
GMenuInit ()
{
  char *keystr, *end;

  GGadgetInit ();
  menu_font = menubar_font = _ggadget_default_font;
  _GGadgetCopyDefaultBox (&menubar_box);
  _GGadgetCopyDefaultBox (&menu_box);
  menubar_box.active_border = 0xffffff;
  menu_box.padding = 3;
  menu_box.main_background = 0xffffff;
  menu_box.border_outer = menu_box.disabled_foreground;
  menu_box.flags |= box_foreground_border_outer;
  menubar_font =
    _GGadgetInitDefaultBox ("GMenuBar.", &menubar_box, menubar_font);
  menu_font = _GGadgetInitDefaultBox ("GMenu.", &menu_box, menubar_font);
  keystr = GResourceFindString ("Keyboard");
  if (keystr != NULL)
    {
      if (strcasecmp (keystr, "mac") == 0)
        keyboard = kb_mac;
      else if (strcasecmp (keystr, "sun") == 0)
        keyboard = kb_sun;
      else if (strcasecmp (keystr, "ppc") == 0)
        keyboard = kb_ppc;
      else if (strcasecmp (keystr, "ibm") == 0
               || strcasecmp (keystr, "pc") == 0)
        keyboard = kb_ibm;
      else if (strtol (keystr, &end, 10), *end == '\0')
        keyboard = strtol (keystr, NULL, 10);
    }
  menu_grabs = GResourceFindBool ("GMenu.Grab", menu_grabs);
  mac_menu_icons = GResourceFindBool ("GMenu.MacIcons", mac_menu_icons);
  gmenubar_inited = true;
  _GGroup_Init ();
}

typedef struct gmenu
{
  bool hasticks;
  bool pressed;
  bool initial_press;
  bool scrollup;
  bool freemi;
  bool disabled;
  bool dying;
  bool hidden;
  bool any_unmasked_shortcuts;  /* Only set for popup menus. Else info in menubar. */
  int bp;
  int tickoff;
  int tioff;
  int rightedge;
  int width;
  int height;
  int line_with_mouse;
  int offtop;
  int lcnt;
  int mcnt;
  GMenuItem *mi;
  int fh;
  int as;
  GWindow w;
  GBox *box;
  struct gmenu *parent;
  struct gmenu *child;
  struct gmenubar *menubar;
  GWindow owner;
  FontInstance *font;
  void (*donecallback) (GWindow owner);
  GIC *gic;
  GGadget *vsb;
} GMenu;

static void
translate_shortcut (int i, char *modifier)
{
  char buffer[32];
  char *temp;

  sprintf (buffer, "Flag0x%02x", 1 << i);
  temp = dgettext (GMenuGetShortcutDomain (), buffer);

  if (strcmp (temp, buffer) != 0)
    modifier = temp;
  else
    modifier = dgettext (GMenuGetShortcutDomain (), modifier);
}

static void
shorttext (GMenuItem *gi, uint32_t *buf)
{
  uint32_t *pt = buf;
  static int initted = false;
  struct
  {
    int mask;
    char *modifier;
  } mods[8] =
  {
    {
    ksm_shift, H_ ("Shft+")},
    {
    ksm_capslock, H_ ("CapsLk+")},
    {
    ksm_control, H_ ("Ctl+")},
    {
    ksm_meta, H_ ("Alt+")},
    {
    0x10, H_ ("Flag0x10+")},
    {
    0x20, H_ ("Flag0x20+")},
    {
    0x40, H_ ("Flag0x40+")},
    {
    0x80, H_ ("Flag0x80+")}
  };
  int i;
  char buffer[32];

  if (!initted)
    {
      for (i = 0; i < 8; ++i)
        {
          if (mac_menu_icons)
            {
              if (mods[i].mask == ksm_cmdmacosx)
                mods[i].modifier = "⌘";
              else if (mods[i].mask == ksm_control)
                mods[i].modifier = "⌃";
              else if (mods[i].mask == ksm_meta)
                mods[i].modifier = "⎇";
              else if (mods[i].mask == ksm_shift)
                mods[i].modifier = "⇧";
              else
                translate_shortcut (i, mods[i].modifier);
            }
          else
            {
              translate_shortcut (i, mods[i].modifier);
            }
        }

      /* It used to be that the Command key was available to X on the mac */
      /*  but no longer. So we used to use it, but we can't now */
      /* It's sort of available. X11->Preferences->Input->Enable Keyboard shortcuts under X11 needs to be OFF */
      /* if ( strcmp(mods[2].modifier,"Ctl+")==0 ) */
      /* mods[2].modifier = keyboard!=kb_mac?"Ctl+":"Cmd+"; */
      if (strcmp (mods[3].modifier, "Alt+") == 0)
        mods[3].modifier =
          keyboard == kb_ibm ? "Alt+" : keyboard ==
          kb_mac ? "Opt+" : keyboard == kb_ppc ? "Cmd+" : "Meta+";
    }

  if (gi->shortcut_char == 0)
    {
      *pt = '\0';
      return;
    }

  for (i = 7; i >= 0; --i)
    {
      if (gi->short_mask & (1 << i))
        {
          u32_strcpy (pt, x_gc_u8_to_u32 (mods[i].modifier));
          pt += u32_strlen (pt);
        }
    }

  if (gi->shortcut_char >= 0xff00 && GDrawKeysyms[gi->shortcut_char - 0xff00])
    {
      u8_strcpy (buffer, x_gc_u32_to_u8 (GDrawKeysyms[gi->shortcut_char - 0xff00]));
      utf82u_strcpy (pt, dgettext (GMenuGetShortcutDomain (), buffer));
    }
  else
    {
      *pt++ = islower (gi->shortcut_char) ? toupper (gi->shortcut_char) : gi->shortcut_char;
      *pt = '\0';
    }
}

static void
GMenuDrawCheckMark (struct gmenu *m, Color fg, int ybase)
{
  int pt = GDrawPointsToPixels (m->w, 1);
  int as = m->as;
  int x = m->tickoff;
  cairo_t *cr = GDrawGetCairo (m->w);

  cairo_new_path (cr);
  cairo_set_line_width (cr, 2 * pt);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_source_rgba (cr,
                         COLOR_RED (fg) / 255.0,
                         COLOR_GREEN (fg) / 255.0,
                         COLOR_BLUE (fg) / 255.0, 1.0);
  cairo_move_to (cr, x + pt, ybase - as / 2);
  cairo_line_to (cr, x + as / 3, ybase - 2 * pt);
  cairo_line_to (cr, x + as, ybase - 8 * pt);
  cairo_stroke (cr);
}

static void
GMenuDrawUncheckMark (struct gmenu *m, Color fg, int ybase)
{
}

static void
GMenuDrawArrow (struct gmenu *m, Color fg, int ybase)
{
  int pt = GDrawPointsToPixels (m->w, 1);
  int as = 2 * (m->as / 3);
  int x = m->rightedge - 2 * pt;
  cairo_t *cr = GDrawGetCairo (m->w);

  cairo_new_path (cr);
  cairo_set_line_width (cr, 2 * pt);
  cairo_set_line_cap (cr, CAIRO_LINE_CAP_ROUND);
  cairo_set_source_rgba (cr,
                         COLOR_RED (fg) / 255.0,
                         COLOR_GREEN (fg) / 255.0,
                         COLOR_BLUE (fg) / 255.0, 1.0);
  cairo_move_to (cr, x - 1 * (as / 2), ybase - as);
  cairo_line_to (cr, x, ybase - as / 2);
  cairo_line_to (cr, x - 1 * (as / 2), ybase);
  cairo_stroke (cr);
}

static int
GMenuDrawMenuLine (struct gmenu *m, GMenuItem *mi, int y, GWindow pixmap)
{
  uint32_t shortbuf[300];
  int as = GTextInfoGetAs (m->w, &mi->ti, m->font);
  int h, width;
  Color fg = m->box->main_foreground;
  GRect old, new;
  int ybase = y + as;
  int x;
  GTextInfo *ti = GTextInfoCopy (&mi->ti);

  new.x = m->tickoff;
  new.width = m->rightedge - m->tickoff;
  new.y = y;
  new.height = GTextInfoGetHeight (pixmap, &mi->ti, m->font);
  GDrawPushClip (pixmap, &new, &old);

  if (ti->fg != COLOR_DEFAULT && ti->fg != COLOR_UNKNOWN)
    fg = ti->fg;
  if (ti->disabled || m->disabled)
    fg = m->box->disabled_foreground;
  if (fg == COLOR_DEFAULT)
    fg = GDrawGetDefaultForeground (GDrawGetDisplayOfWindow (pixmap));

  if (ti->checkable)
    {
      if (ti->checked)
        {
          GMenuDrawCheckMark (m, fg, ybase);
          /* we don't want to draw the image if check mark is drawn */
          ti->image = NULL;
        }
      else
        {
          GMenuDrawUncheckMark (m, fg, ybase);
        }
    }

  /* draw the menu items with image at the base point, so the actual text
   * offset is always the same */
  if (ti->image != NULL)
    x = m->bp;
  else
    x = m->tioff;

  h = GTextInfoDraw (pixmap, x, y, ti, m->font,
                     (ti->disabled
                      || m->disabled) ? m->box->disabled_foreground : fg,
                     m->box->active_border, new.y + new.height);

  if (mi->sub != NULL)
    GMenuDrawArrow (m, fg, ybase);
  else if (mi->shortcut_char != 0)
    {
      shorttext (mi, shortbuf);

      width = GDrawGetTextWidth (pixmap, shortbuf, -1);
      GDrawDrawText (pixmap, m->rightedge - width, ybase, shortbuf, -1, fg);
    }

  GDrawPopClip (pixmap, &old);

  return (y + h);
}

static int
gmenu_expose (struct gmenu *m, GEvent *event, GWindow pixmap)
{
  GRect old1, old2;
  GRect r;
  int i;

  GDrawPushClip (pixmap, &event->u.expose.rect, &old1);
  r.x = 0;
  r.width = m->width;
  r.y = 0;
  r.height = m->height;
  GBoxDrawBackground (pixmap, &r, m->box, gs_active, false);
  GBoxDrawBorder (pixmap, &r, m->box, gs_active, false);
  r.x = m->tickoff;
  r.width = m->rightedge - m->tickoff;
  r.y = m->bp;
  r.height = m->height - 2 * m->bp;
  GDrawPushClip (pixmap, &r, &old2);
  for (i = event->u.expose.rect.y / m->fh + m->offtop;
       i < m->mcnt
       && i <=
       (event->u.expose.rect.y + event->u.expose.rect.height) / m->fh +
       m->offtop; ++i)
    {
      if (m->lcnt != m->mcnt && i == m->lcnt + m->offtop - 1
          && i != m->mcnt - 1)
        {
          GMenuDrawMenuLine (m, &m->mi[i], m->bp + (i - m->offtop) * m->fh,
                             pixmap);
          break;                /* Otherwise we get bits of the line after the last */
        }
      else
        GMenuDrawMenuLine (m, &m->mi[i], m->bp + (i - m->offtop) * m->fh,
                           pixmap);
    }
  GDrawPopClip (pixmap, &old2);
  GDrawPopClip (pixmap, &old1);
  return (true);
}

static void
GMenuDrawLines (struct gmenu *m, int ln, int cnt)
{
  GRect r, old1, old2, winrect;

  winrect.x = 0;
  winrect.width = m->width;
  winrect.y = 0;
  winrect.height = m->height;
  r = winrect;
  r.height = cnt * m->fh;
  r.y = (ln - m->offtop) * m->fh + m->bp;
  GDrawPushClip (m->w, &r, &old1);
  GBoxDrawBackground (m->w, &winrect, m->box, gs_active, false);
  GBoxDrawBorder (m->w, &winrect, m->box, gs_active, false);
  r.x = m->tickoff;
  r.width = m->rightedge - r.x;
  GDrawPushClip (m->w, &r, &old2);
  cnt += ln;
  for (; ln < cnt; ++ln)
    GMenuDrawMenuLine (m, &m->mi[ln], m->bp + (ln - m->offtop) * m->fh, m->w);
  GDrawPopClip (m->w, &old2);
  GDrawPopClip (m->w, &old1);
}

static void
GMenuSetPressed (struct gmenu *m, int pressed)
{
  while (m->child != NULL)
    m = m->child;
  while (m->parent != NULL)
    {
      m->pressed = pressed;
      m = m->parent;
    }
  m->pressed = pressed;
  if (m->menubar != NULL)
    m->menubar->pressed = pressed;
}

static void
_GMenuDestroy (struct gmenu *m)
{
  if (m->dying)
    return;
  m->dying = true;
  if (m->line_with_mouse != -1)
    m->mi[m->line_with_mouse].ti.selected = false;
  if (m->child != NULL)
    _GMenuDestroy (m->child);
  if (m->parent != NULL)
    m->parent->child = NULL;
  else if (m->menubar != NULL)
    {
      m->menubar->child = NULL;
      m->menubar->pressed = false;
      _GWidget_ClearPopupOwner ((GGadget *) (m->menubar));
      _GWidget_ClearGrabGadget ((GGadget *) (m->menubar));
      GMenuBarChangeSelection (m->menubar, -1, NULL);
    }
  GDrawDestroyWindow (m->w);
  /* data are freed when we get the destroy event !!!! */
}

static void
GMenuDestroy (struct gmenu *m)
{
  GDrawPointerUngrab (GDrawGetDisplayOfWindow (m->w));
  if (menu_grabs && m->parent != NULL)
    GDrawPointerGrab (m->parent->w);
  _GMenuDestroy (m);
}

static void
GMenuHideAll (struct gmenu *m)
{
  if (m != NULL)
    {
      struct gmenu *s = m;
      GDrawPointerUngrab (GDrawGetDisplayOfWindow (m->w));
      while (m->parent)
        m = m->parent;
      while (m)
        {
          m->hidden = true;
          GDrawSetVisible (m->w, false);
          m = m->child;
        }
      GDrawSync (GDrawGetDisplayOfWindow (s->w));
      GDrawProcessPendingEvents (GDrawGetDisplayOfWindow (s->w));
    }
}

static void
GMenuDismissAll (struct gmenu *m)
{
  if (m != NULL)
    {
      while (m->parent)
        m = m->parent;
      GMenuDestroy (m);
    }
}

static void
UnsetInitialPress (struct gmenu *m)
{
  while (m != NULL)
    {
      m->initial_press = false;
      if (m->menubar != NULL)
        m->menubar->initial_press = false;
      m = m->parent;
    }
}

static void
GMenuChangeSelection (struct gmenu *m, int newsel, GEvent *event)
{
  int old = m->line_with_mouse;

  if (old == newsel)
    return;
  if (newsel == m->mcnt)
    return;

  if (m->child != NULL)
    {
      GMenuDestroy (m->child);
      m->child = NULL;
    }
  UnsetInitialPress (m);
  m->line_with_mouse = newsel;
  if (newsel != -1)
    m->mi[newsel].ti.selected = true;
  if (old != -1)
    m->mi[old].ti.selected = false;

  if (newsel == old + 1 && old != -1)
    GMenuDrawLines (m, old, 2);
  else if (old == newsel + 1 && newsel != -1)
    GMenuDrawLines (m, newsel, 2);
  else
    {
      if (newsel != -1)
        GMenuDrawLines (m, newsel, 1);
      if (old != -1)
        GMenuDrawLines (m, old, 1);
    }
  if (newsel != -1)
    {
      if (m->mi[newsel].moveto != NULL)
        (m->mi[newsel].moveto) (m->owner, &m->mi[newsel], event);
      if (m->mi[newsel].sub != NULL)
        m->child = GMenuCreateSubMenu (m, m->mi[newsel].sub,
                                       m->disabled
                                       || m->mi[newsel].ti.disabled);
    }
}

static void
GMenuBarChangeSelection (GMenuBar * mb, int newsel, GEvent *event)
{
  int old = mb->entry_with_mouse;
  GMenuItem *mi;

  if (old == newsel)
    return;
  if (mb->child != NULL)
    {
      int waspressed = mb->pressed;
      GMenuDestroy (mb->child);
      mb->child = NULL;
      mb->pressed = waspressed;
    }
  mb->entry_with_mouse = newsel;
  if (newsel != -1)
    mb->mi[newsel].ti.selected = true;
  if (old != -1)
    mb->mi[old].ti.selected = false;

  _ggadget_redraw (&mb->g);
  if (newsel != -1)
    {
      mi = newsel == mb->lastmi ? mb->fake : &mb->mi[newsel];
      if (mi->moveto != NULL)
        (mi->moveto) (mb->g.base, mi, event);
      if (mi->sub != NULL)
        mb->child = GMenuCreatePulldownMenu (mb, mi->sub, mi->ti.disabled);
    }
}

static int
MParentInitialPress (struct gmenu *m)
{
  if (m->parent != NULL)
    return (m->parent->initial_press);
  else if (m->menubar != NULL)
    return (m->menubar->initial_press);

  return (false);
}

static int
gmenu_mouse (struct gmenu *m, GEvent *event)
{
  GPoint p;
  struct gmenu *testm;

  if (m->hidden || (m->child != NULL && m->child->hidden))
    return (true);

  if (event->type == et_crossing)
    {
      if (!event->u.crossing.entered)
        UnsetInitialPress (m);
      return (true);
    }

  p.x = event->u.mouse.x;
  p.y = event->u.mouse.y;

  for (testm = m; testm->child != NULL; testm = testm->child);
  for (; testm != NULL; testm = testm->parent)
    if (GDrawEventInWindow (testm->w, event))
      break;

  if (testm != m && testm != NULL)
    {
      GDrawPointerGrab (testm->w);
      GDrawTranslateCoordinates (m->w, testm->w, &p);
      m = testm;
    }
  else if (testm == NULL /*&& event->u.mouse.y<0 */ )
    {                           /* menubars can be below the menu if no room on screen */
      for (testm = m; testm->parent != NULL; testm = testm->parent);
      if (testm->menubar != NULL)
        {
          GDrawTranslateCoordinates (m->w, testm->menubar->g.base, &p);
          if (p.x >= 0 && p.y >= 0 &&
              p.x < testm->menubar->g.inner.x + testm->menubar->g.inner.width
              && p.y <
              testm->menubar->g.inner.y + testm->menubar->g.inner.height)
            {
              /*GDrawPointerGrab(testm->menubar->g.base); *//* Don't do this */
              event->u.mouse.x = p.x;
              event->u.mouse.y = p.y;
              return ((GDrawGetEH (testm->menubar->g.base))
                      (testm->menubar->g.base, event));
            }
        }
      testm = NULL;
    }
  if (testm == NULL)
    {
      if (event->type == et_mousedown)
        GMenuDismissAll (m);
      else if (event->type == et_mouseup)
        GMenuSetPressed (m, false);
      else if (m->pressed)
        GMenuChangeSelection (m, -1, event);
      return (true);
    }

  event->u.mouse.x = p.x;
  event->u.mouse.y = p.y;
  event->w = m->w;
  if ((m->pressed && event->type == et_mousemove) ||
      event->type == et_mousedown)
    {
      int l = (event->u.mouse.y - m->bp) / m->fh;
      int i = l + m->offtop;
      if (event->u.mouse.y < m->bp && event->type == et_mousedown)
        GMenuDismissAll (m);
      else if (event->type == et_mousedown && m->child != NULL &&
               i == m->line_with_mouse)
        GMenuChangeSelection (m, -1, event);
      else if (i >= m->mcnt)
        GMenuChangeSelection (m, -1, event);
      else
        GMenuChangeSelection (m, i, event);
      if (event->type == et_mousedown)
        {
          GMenuSetPressed (m, true);
          if (m->child != NULL)
            m->initial_press = true;
        }
    }
  else if (event->type == et_mouseup && m->child == NULL)
    {
      if (event->u.mouse.y >= m->bp && event->u.mouse.x >= 0 &&
          event->u.mouse.y < m->height - m->bp &&
          event->u.mouse.x < m->width && !MParentInitialPress (m))
        {
          int l = (event->u.mouse.y - m->bp) / m->fh;
          int i = l + m->offtop;
          if (!(l == 0 && m->offtop != 0 && m->vsb == NULL) &&
              !(l == m->lcnt - 1 && m->offtop + m->lcnt < m->mcnt
                && m->vsb == NULL) && !m->disabled && !m->mi[i].ti.disabled
              && !m->mi[i].ti.line)
            {
              if (m->mi[i].ti.checkable)
                m->mi[i].ti.checked = !m->mi[i].ti.checked;
              GMenuHideAll (m);
              GMenuDismissAll (m);
              if (m->mi[i].invoke != NULL)
                (m->mi[i].invoke) (m->owner, &m->mi[i], event);
            }
        }
    }
  else if (event->type == et_mouseup)
    {
      UnsetInitialPress (m);
      GMenuSetPressed (m, false);
    }
  else
    return (false);

  return (true);
}

static int
gmenu_timer (struct gmenu *m, GEvent *event)
{
  if (m->scrollup)
    {
      if (m->offtop == 0)
        return (true);
      if (--m->offtop < 0)
        m->offtop = 0;
    }
  else
    {
      if (m->offtop == m->mcnt - m->lcnt)
        return (true);
      ++m->offtop;
      if (m->offtop + m->lcnt > m->mcnt)
        m->offtop = m->mcnt - m->lcnt;
    }
  GDrawRequestExpose (m->w, NULL, false);
  return (true);
}

static int
GMenuKeyInvoke (struct gmenu *m, int i)
{
  GMenuChangeSelection (m, i, NULL);
  if (m->mi[i].ti.checkable)
    m->mi[i].ti.checked = !m->mi[i].ti.checked;
  if (m->mi[i].sub == NULL)
    GMenuHideAll (m);
  if (m->mi[i].invoke != NULL)
    (m->mi[i].invoke) (m->owner, &m->mi[i], NULL);
  if (m->mi[i].sub == NULL)
    GMenuDismissAll (m);
  return (true);
}

static int
GMenuBarKeyInvoke (struct gmenubar *mb, int i)
{
  GMenuBarChangeSelection (mb, i, NULL);
  if (mb->mi[i].invoke != NULL)
    (mb->mi[i].invoke) (mb->g.base, &mb->mi[i], NULL);
  return (true);
}

static GMenuItem *
GMenuSearchShortcut (GWindow gw, GMenuItem *mi, GEvent *event,
                     bool call_moveto)
{
  int i;
  uint32_t keysym = event->u.chr.keysym;

  if (keysym < GK_Special && islower (keysym))
    keysym = toupper (keysym);  /*getkey(keysym,event->u.chr.state&0x2000 ); */
  for (i = 0; GMenuItem_nonempty (&mi[i]); ++i)
    {
      if (call_moveto && mi[i].moveto != NULL)
        (mi[i].moveto) (gw, &(mi[i]), event);
      if (mi[i].sub == NULL && mi[i].shortcut_char == keysym &&
          (menumask & event->u.chr.state) == mi[i].short_mask)
        return (&mi[i]);
      else if (mi[i].sub != NULL)
        {
          GMenuItem *ret =
            GMenuSearchShortcut (gw, mi[i].sub, event, call_moveto);
          if (ret != NULL)
            return (ret);
        }
    }
  return (NULL);
}

static int
GMenuSpecialKeys (struct gmenu *m, uint32_t keysym, GEvent *event)
{
  switch (keysym)
    {
    case GK_Escape:
      GMenuDestroy (m);
      return (true);
    case GK_Return:
      if (m->line_with_mouse == -1)
        {
          int ns = 0;
          while (ns < m->mcnt && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
            ++ns;
          if (ns < m->mcnt)
            GMenuChangeSelection (m, ns, event);
        }
      else if (m->mi[m->line_with_mouse].sub != NULL && m->child == NULL)
        {
          m->child =
            GMenuCreateSubMenu (m, m->mi[m->line_with_mouse].sub,
                                (m->disabled
                                 || m->mi[m->line_with_mouse].ti.disabled));
        }
      else
        {
          int i = m->line_with_mouse;
          if (!m->disabled && !m->mi[i].ti.disabled && !m->mi[i].ti.line)
            {
              if (m->mi[i].ti.checkable)
                m->mi[i].ti.checked = !m->mi[i].ti.checked;
              GMenuDismissAll (m);
              if (m->mi[i].invoke != NULL)
                (m->mi[i].invoke) (m->owner, &m->mi[i], event);
            }
          else
            GMenuDismissAll (m);
        }
      return (true);
    case GK_Left:
    case GK_KP_Left:
      if (m->parent != NULL)
        {
          GMenuDestroy (m);
          return (true);
        }
      else if (m->menubar != NULL)
        {
          GMenuBar *mb = m->menubar;
          int en = mb->entry_with_mouse;
          int lastmi = mb->fake[0].sub != NULL ? mb->lastmi + 1 : mb->lastmi;
          if (en > 0)
            GMenuBarChangeSelection (mb, en - 1, event);
          else
            GMenuBarChangeSelection (mb, lastmi - 1, event);
          return (true);
        }
      /* Else fall into the "Up" case */
    case GK_Up:
    case GK_KP_Up:
    case GK_Page_Up:
    case GK_KP_Page_Up:
      {
        int ns;
        if (keysym != GK_Left && keysym != GK_KP_Left)
          {
            while (m->line_with_mouse == -1 && m->parent != NULL)
              {
                GMenu *p = m->parent;
                GMenuDestroy (m);
                m = p;
              }
          }
        ns = m->line_with_mouse - 1;
        while (ns >= 0 && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
          --ns;
        if (ns < 0)
          {
            ns = m->mcnt - 1;
            while (ns >= 0 && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
              --ns;
          }
        if (ns < 0 && m->line_with_mouse == -1)
          {                     /* Nothing selectable? get rid of menu */
            GMenuDestroy (m);
            return (true);
          }
        if (ns < 0)
          ns = -1;
        GMenuChangeSelection (m, ns, NULL);
        return (true);
      }
    case GK_Right:
    case GK_KP_Right:
      if (m->line_with_mouse != -1 &&
          m->mi[m->line_with_mouse].sub != NULL && m->child == NULL)
        {
          m->child =
            GMenuCreateSubMenu (m, m->mi[m->line_with_mouse].sub,
                                (m->disabled
                                 || m->mi[m->line_with_mouse].ti.disabled));
          return (true);
        }
      else if (m->parent == NULL && m->menubar != NULL)
        {
          GMenuBar *mb = m->menubar;
          int en = mb->entry_with_mouse;
          int lastmi = mb->fake[0].sub != NULL ? mb->lastmi + 1 : mb->lastmi;
          if (en + 1 < lastmi)
            GMenuBarChangeSelection (mb, en + 1, event);
          else
            GMenuBarChangeSelection (mb, 0, event);
          return (true);
        }
      /* Fall through into the "Down" case */
    case GK_Down:
    case GK_KP_Down:
    case GK_Page_Down:
    case GK_KP_Page_Down:
      {
        int ns;
        if (keysym != GK_Right && keysym != GK_KP_Right)
          {
            while (m->line_with_mouse == -1 && m->parent != NULL)
              {
                GMenu *p = m->parent;
                GMenuDestroy (m);
                m = p;
              }
          }
        ns = m->line_with_mouse + 1;
        while (ns < m->mcnt && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
          ++ns;
        if (ns >= m->mcnt)
          {
            ns = 0;
            while (ns < m->mcnt
                   && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
              ++ns;
          }
        if (ns >= m->mcnt && m->line_with_mouse == -1)
          {                     /* Nothing selectable? get rid of menu */
            GMenuDestroy (m);
            return (true);
          }
        GMenuChangeSelection (m, ns, event);
        return (true);
      }
    case GK_Home:
    case GK_KP_Home:
      {
        int ns = 0;
        while (ns < m->mcnt && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
          ++ns;
        if (ns != m->mcnt)
          GMenuChangeSelection (m, ns, event);
        return (true);
      }
    case GK_End:
    case GK_KP_End:
      {
        int ns = m->mcnt - 1;
        while (ns >= 0 && (m->mi[ns].ti.disabled || m->mi[ns].ti.line))
          --ns;
        if (ns >= 0)
          GMenuChangeSelection (m, ns, event);
        return (true);
      }
    }
  return (false);
}

static int
gmenu_key (struct gmenu *m, GEvent *event)
{
  int i;
  GMenuItem *mi;
  GMenu *top;
  uint32_t keysym = event->u.chr.keysym;

  if (islower (keysym))
    keysym = toupper (keysym);
  if (event->u.chr.state & ksm_meta
      && !(event->u.chr.state & (menumask & ~(ksm_meta | ksm_shift))))
    {
      /* Look for mnemonics only in the child. */
      while (m->child != NULL)
        m = m->child;
      for (i = 0; i < m->mcnt; ++i)
        {
          if (m->mi[i].ti.mnemonic == keysym &&
              !m->disabled && !m->mi[i].ti.disabled)
            {
              GMenuKeyInvoke (m, i);
              return (true);
            }
        }
    }

  /* then look for shortcuts everywhere */
  if ((event->u.chr.state & (menumask & ~ksm_shift)) ||
      event->u.chr.keysym >= GK_Special)
    {
      for (top = m; top->parent != NULL; top = top->parent);
      if (top->menubar != NULL)
        mi = GMenuSearchShortcut (top->owner, top->menubar->mi, event, false);
      else
        mi = GMenuSearchShortcut (top->owner, top->mi, event, false);
      if (mi != NULL)
        {
          if (mi->ti.checkable)
            mi->ti.checked = !mi->ti.checked;
          GMenuHideAll (top);
          if (mi->invoke != NULL)
            (mi->invoke) (m->owner, mi, event);
          GMenuDestroy (m);
          return (true);
        }
      for (; m->child != NULL; m = m->child);
      return (GMenuSpecialKeys (m, event->u.chr.keysym, event));
    }

  return (false);
}

static int
gmenu_destroy (struct gmenu *m)
{
  if (most_recent_popup_menu == m)
    most_recent_popup_menu = NULL;
  if (m->donecallback)
    (m->donecallback) (m->owner);
  if (m->freemi)
    GMenuItemArrayFree (m->mi);
  free (m);
  return (true);
}

static int
gmenu_eh (GWindow w, GEvent *ge)
{
  GMenu *m = (GMenu *) GDrawGetUserData (w);

  switch (ge->type)
    {
    case et_map:
      /* I need to initialize the input context, but I can't do that until */
      /*  the menu pops up */
      if (ge->u.map.is_visible && m->gic != NULL)
        GDrawSetGIC (w, m->gic, 0, 20);
      return (true);
    case et_expose:
      return (gmenu_expose (m, ge, w));
    case et_char:
      return (gmenu_key (m, ge));
    case et_mousemove:
    case et_mousedown:
    case et_mouseup:
    case et_crossing:
      return (gmenu_mouse (m, ge));
    case et_timer:
      return (gmenu_timer (m, ge));
    case et_destroy:
      return (gmenu_destroy (m));
    case et_close:
      GMenuDestroy (m);
      return (true);
    }
  return (false);
}

static int
gmenu_scroll (GGadget *g, GEvent *event)
{
  enum sb sbt = event->u.control.u.sb.type;
  GMenu *m = (GMenu *) (g->data);
  int newpos = m->offtop;

  if (sbt == et_sb_top)
    newpos = 0;
  else if (sbt == et_sb_bottom)
    newpos = m->mcnt - m->lcnt;
  else if (sbt == et_sb_up)
    --newpos;
  else if (sbt == et_sb_down)
    ++newpos;
  else if (sbt == et_sb_uppage)
    {
      if (m->lcnt != 1)         /* Normally we leave one line in window from before, except if only one line fits */
        newpos -= m->lcnt - 1;
      else
        newpos -= 1;
    }
  else if (sbt == et_sb_downpage)
    {
      if (m->lcnt != 1)         /* Normally we leave one line in window from before, except if only one line fits */
        newpos += m->lcnt - 1;
      else
        newpos += 1;
    }
  else                          /* if ( sbt==et_sb_thumb || sbt==et_sb_thumbrelease ) */
    {
      newpos = event->u.control.u.sb.pos;
    }
  if (newpos + m->lcnt > m->mcnt)
    newpos = m->mcnt - m->lcnt;
  if (newpos < 0)
    newpos = 0;
  if (newpos != m->offtop)
    {
      m->offtop = newpos;
      GScrollBarSetPos (m->vsb, newpos);
      GDrawRequestExpose (m->w, NULL, false);
    }
  return (true);
}

static GMenu *
_GMenu_Create (GWindow owner, GMenuItem *mi, GPoint *where,
               int awidth, int aheight, GFont * font, int disable)
{
  GMenu *m = xcalloc (1, sizeof (GMenu));
  GRect pos;
  GDisplay *disp = GDrawGetDisplayOfWindow (owner);
  GWindowAttrs pattrs;
  int i, width, keywidth;
  uint32_t buffer[300];
  extern int _GScrollBar_Width;
  int ds, ld, temp, lh;
  int sbwidth = 0;
  int ticklen;
  GRect screen;

  m->owner = owner;
  m->mi = mi;
  m->disabled = disable;
  m->font = font;
  m->box = &menu_box;
  m->tickoff = m->tioff = m->bp = GBoxBorderWidth (owner, m->box);
  m->line_with_mouse = -1;

  /* Mnemonics in menus don't work under gnome. Turning off nodecor
     makes them work, but that seems a high price to pay. */
  pattrs.mask =
    wam_events | wam_nodecor | wam_positioned | wam_cursor | wam_transient |
    wam_verytransient;
  pattrs.event_masks = -1;
  pattrs.nodecoration = true;
  pattrs.positioned = true;
  pattrs.cursor = ct_pointer;
  pattrs.transient = GWidgetGetTopWidget (owner);

  pos.x = pos.y = 0;
  pos.width = pos.height = 100;

  m->w = GDrawCreateTopWindow (disp, &pos, gmenu_eh, m, &pattrs);
  m->gic = GDrawCreateInputContext (m->w, gic_root | gic_orlesser);
  GDrawGetFontMetrics (m->w, m->font, &m->as, &ds, &ld);
  m->fh = m->as + ds + 1;       /* I need some extra space, else mnemonic
                                   underlines look bad. */
  lh = m->fh;

  GDrawSetFont (m->w, m->font);
  m->hasticks = false;
  width = 0;
  keywidth = 0;
  for (i = 0; GMenuItem_nonempty (&mi[i]); i++)
    {
      if (mi[i].ti.checkable)
        m->hasticks = true;
      temp = GTextInfoGetWidth (m->w, &mi[i].ti, m->font);

      if (temp > width)
        width = temp;

      shorttext (&mi[i], buffer);
      temp = GDrawGetTextWidth (m->w, buffer, -1);

      if (temp > keywidth)
        keywidth = temp;

      if (mi[i].sub != NULL && 3 * m->as > keywidth)
        keywidth = 3 * m->as;

      temp = GTextInfoGetHeight (m->w, &mi[i].ti, m->font);
      if (temp > lh)
        {
          if (temp > 3 * m->fh / 2)
            temp = 3 * m->fh / 2;
          lh = temp;
        }
    }
  m->fh = lh;
  m->mcnt = m->lcnt = i;
  if (keywidth != 0)
    width += keywidth + GDrawPointsToPixels (m->w, 8);

  /* reseve space used by icons, even if we don't have any */
  ticklen = MENU_ICON_SIZE + GDrawPointsToPixels (m->w, MENU_ICON_SEP);
  width += ticklen;
  m->tioff += ticklen;

  m->width = pos.width = width + 2 * m->bp;
  m->rightedge = m->width - m->bp;
  m->height = pos.height = i * m->fh + 2 * m->bp;
  GDrawGetSize (GDrawGetRoot (disp), &screen);

  sbwidth = 0;
/* On the mac, the menu bar takes up the top twenty pixels or so of screen */
/*  so never put a menu that high */
#define MAC_MENUBAR	20
  if (pos.height > screen.height - MAC_MENUBAR - m->fh)
    {
      GGadgetData gd;

      m->lcnt = (screen.height - MAC_MENUBAR - m->fh - 2 * m->bp) / m->fh;
      m->height = pos.height = m->lcnt * m->fh + 2 * m->bp;

      /* It's too long, so add a scrollbar */
      sbwidth = GDrawPointsToPixels (owner, _GScrollBar_Width);
      pos.width += sbwidth;
      memset (&gd, '\0', sizeof (gd));
      gd.pos.y = 0;
      gd.pos.height = pos.height;
      gd.pos.width = sbwidth;
      gd.pos.x = m->width;
      gd.flags =
        gg_visible | gg_enabled | gg_pos_in_pixels | gg_sb_vert | gg_pos_use0;
      gd.handle_controlevent = gmenu_scroll;
      m->vsb = GScrollBarCreate (m->w, &gd, m);
      GScrollBarSetBounds (m->vsb, 0, m->mcnt, m->lcnt);
    }

  pos.x = where->x;
  pos.y = where->y;
  if (pos.y + pos.height > screen.height - MAC_MENUBAR)
    {
      if (where->y + aheight - pos.height >= MAC_MENUBAR)
        pos.y = where->y + aheight - pos.height;
      else
        {
          pos.y = MAC_MENUBAR;
          /* Ok, it's going to overlap the press point if we got here */
          /*  let's see if we can shift it left/right a bit so it won't */
          if (awidth < 0)
            /* Oh, well, I guess it won't. It's a submenu and we've already */
            /*  moved off to the left */ ;
          else if (pos.x + awidth + pos.width + 3 < screen.width)
            pos.x += awidth + 3;
          else if (pos.x - pos.width - 3 >= 0)
            pos.x -= pos.width + 3;
          else
            {
              /* There doesn't seem much we can do in this case */
              ;
            }
        }
    }
  if (pos.x + pos.width > screen.width)
    {
      if (where->x + awidth - pos.width >= 0)
        pos.x = where->x + awidth - pos.width - 3;
      else
        pos.x = 0;
    }
  GDrawResize (m->w, pos.width, pos.height);
  GDrawMove (m->w, pos.x, pos.y);

  GDrawSetVisible (m->w, true);
  if (menu_grabs)
    GDrawPointerGrab (m->w);
  return (m);
}

static GMenu *
GMenuCreateSubMenu (GMenu * parent, GMenuItem *mi, int disable)
{
  GPoint p;
  GMenu *m;

  p.x = parent->width;
  p.y = (parent->line_with_mouse - parent->offtop) * parent->fh + parent->bp;
  GDrawTranslateCoordinates (parent->w,
                             GDrawGetRoot (GDrawGetDisplayOfWindow
                                           (parent->w)), &p);
  m =
    _GMenu_Create (parent->owner, mi, &p, -parent->width, parent->fh,
                   parent->font, disable);
  m->parent = parent;
  m->pressed = parent->pressed;
  return (m);
}

static GMenu *
GMenuCreatePulldownMenu (GMenuBar * mb, GMenuItem *mi, int disabled)
{
  GPoint p;
  GMenu *m;

  p.x = mb->g.inner.x + mb->xs[mb->entry_with_mouse] -
    GBoxDrawnWidth (mb->g.base, &menu_box);
  p.y = mb->g.r.y + mb->g.r.height;
  GDrawTranslateCoordinates (mb->g.base,
                             GDrawGetRoot (GDrawGetDisplayOfWindow
                                           (mb->g.base)), &p);
  m =
    _GMenu_Create (mb->g.base, mi, &p,
                   mb->xs[mb->entry_with_mouse + 1] -
                   mb->xs[mb->entry_with_mouse], -mb->g.r.height, mb->font,
                   disabled);
  m->menubar = mb;
  m->pressed = mb->pressed;
  _GWidget_SetPopupOwner ((GGadget *) mb);
  return (m);
}

GWindow
_GMenuCreatePopupMenu (GWindow owner, GEvent *event, GMenuItem *mi,
                       void (*donecallback) (GWindow))
{
  GPoint p;
  GMenu *m;
  GEvent e;

  if (!gmenubar_inited)
    GMenuInit ();

  p.x = event->u.mouse.x;
  p.y = event->u.mouse.y;
  GDrawTranslateCoordinates (owner,
                             GDrawGetRoot (GDrawGetDisplayOfWindow (owner)),
                             &p);
  m =
    _GMenu_Create (owner, GMenuItemArrayCopy (mi, NULL), &p, 0, 0, menu_font,
                   false);
  m->any_unmasked_shortcuts = GMenuItemArrayAnyUnmasked (m->mi);
  GDrawPointerUngrab (GDrawGetDisplayOfWindow (owner));
  GDrawPointerGrab (m->w);
  GDrawGetPointerPosition (m->w, &e);
  if (e.u.mouse.state & (ksm_button1 | ksm_button2 | ksm_button3))
    m->pressed = m->initial_press = true;
  m->donecallback = donecallback;
  m->freemi = true;
  most_recent_popup_menu = m;
  return (m->w);
}

GWindow
GMenuCreatePopupMenu (GWindow owner, GEvent *event, GMenuItem *mi)
{
  return (_GMenuCreatePopupMenu (owner, event, mi, NULL));
}

int
GMenuPopupCheckKey (GEvent *event)
{

  if (most_recent_popup_menu == NULL)
    return (false);

  return (gmenu_key (most_recent_popup_menu, event));
}

/* ************************************************************************** */

int
GGadgetUndoMacEnglishOptionCombinations (GEvent *event)
{
  int keysym = event->u.chr.keysym;

  switch (keysym)
    {
    case 0xba:
      keysym = '0';
      break;
    case 0xa1:
      keysym = '1';
      break;
    case 0x2122:
      keysym = '2';
      break;
    case 0xa3:
      keysym = '3';
      break;
    case 0xa2:
      keysym = '4';
      break;
    case 0x221e:
      keysym = '5';
      break;
    case 0xa7:
      keysym = '6';
      break;
    case 0xb6:
      keysym = '7';
      break;
    case 0x2022:
      keysym = '8';
      break;
    case 0xaa:
      keysym = '9';
      break;
    case 0xe5:
      keysym = 'a';
      break;
    case 0x222b:
      keysym = 'b';
      break;
    case 0xe7:
      keysym = 'c';
      break;
    case 0x2202:
      keysym = 'd';
      break;
      /* e is a modifier */
    case 0x192:
      keysym = 'f';
      break;
    case 0xa9:
      keysym = 'g';
      break;
    case 0x2d9:
      keysym = 'h';
      break;
      /* i is a modifier */
    case 0x2206:
      keysym = 'j';
      break;
    case 0x2da:
      keysym = 'k';
      break;
    case 0xac:
      keysym = 'l';
      break;
    case 0xb5:
      keysym = 'm';
      break;
      /* n is a modifier */
    case 0xf8:
      keysym = 'o';
      break;
    case 0x3c0:
      keysym = 'p';
      break;
    case 0x153:
      keysym = 'q';
      break;
    case 0xae:
      keysym = 'r';
      break;
    case 0x2020:
      keysym = 's';
      break;
    case 0xee:
      keysym = 't';
      break;
      /* u is a modifier */
    case 0x221a:
      keysym = 'v';
      break;
    case 0x2211:
      keysym = 'w';
      break;
    case 0x2248:
      keysym = 'x';
      break;
    case 0xa5:
      keysym = 'y';
      break;
    case 0x3a9:
      keysym = 'z';
      break;
    }
  return (keysym);
}

int
GMenuBarCheckKey (GGadget *g, GEvent *event)
{
  int i;
  GMenuBar *mb = (GMenuBar *) g;
  GMenuItem *mi;
  uint32_t keysym = event->u.chr.keysym;

  if (g == NULL)
    return (false);
  if (keysym == 0)
    return (false);

  if ((menumask & ksm_cmdmacosx) && keysym > 0x7f &&
      (event->u.chr.state & ksm_meta) &&
      !(event->u.chr.state & menumask & (ksm_control | ksm_cmdmacosx)))
    keysym = GGadgetUndoMacEnglishOptionCombinations (event);

  if (keysym < GK_Special && islower (keysym))
    keysym = toupper (keysym);
  if (event->u.chr.state & ksm_meta
      && !(event->u.chr.state & (menumask & ~(ksm_meta | ksm_shift))))
    {
      /* Only look for mneumonics in the leaf of the displayed menu structure */
      if (mb->child != NULL)
        return (gmenu_key (mb->child, event));  /* this routine will do shortcuts too */

      for (i = 0; i < mb->mtot; ++i)
        {
          if (mb->mi[i].ti.mnemonic == keysym && !mb->mi[i].ti.disabled)
            {
              GMenuBarKeyInvoke (mb, i);
              return (true);
            }
        }
    }

  /* then look for shortcuts everywhere */
  if (event->u.chr.state & (menumask & ~ksm_shift) ||
      event->u.chr.keysym >= GK_Special || mb->any_unmasked_shortcuts)
    {
      mi = GMenuSearchShortcut (mb->g.base, mb->mi, event, mb->child == NULL);
      if (mi != NULL)
        {
          if (mi->ti.checkable && !mi->ti.disabled)
            mi->ti.checked = !mi->ti.checked;
          if (mi->invoke != NULL && !mi->ti.disabled)
            (mi->invoke) (mb->g.base, mi, NULL);
          if (mb->child != NULL)
            GMenuDestroy (mb->child);
          return (true);
        }
    }
  if (mb->child != NULL)
    {
      GMenu *m;
      for (m = mb->child; m->child != NULL; m = m->child);
      return (GMenuSpecialKeys (m, event->u.chr.keysym, event));
    }

  if (event->u.chr.keysym == GK_Menu)
    GMenuCreatePopupMenu (event->w, event, mb->mi);

  return (false);
}

static void
GMenuBarDrawDownArrow (GWindow pixmap, GMenuBar * mb, int x)
{
  int pt = GDrawPointsToPixels (pixmap, 1);
  int size = 2 * (mb->g.inner.height / 3);
  int ybase = mb->g.inner.y + size + (mb->g.inner.height - size) / 2;
  GPoint p[3];

  p[0].x = x + size;
  p[0].y = ybase;
  p[1].x = x;
  p[1].y = ybase - size;
  p[2].x = x + 2 * size;
  p[2].y = ybase - size;

  GDrawSetLineWidth (pixmap, pt);
  GDrawDrawLine (pixmap, p[0].x, p[0].y, p[1].x, p[1].y,
                 mb->g.box->border_darker);
  GDrawDrawLine (pixmap, p[0].x, p[0].y + pt, p[1].x + pt, p[1].y,
                 mb->g.box->border_darker);
  GDrawDrawLine (pixmap, p[1].x, p[1].y, p[2].x, p[2].y,
                 mb->g.box->border_brightest);
  GDrawDrawLine (pixmap, p[1].x + pt, p[1].y, p[2].x - pt, p[2].y,
                 mb->g.box->border_brightest);
  GDrawDrawLine (pixmap, p[2].x, p[2].y, p[0].x, p[0].y,
                 mb->g.box->border_darkest);
  GDrawDrawLine (pixmap, p[2].x - pt, p[2].y, p[0].x, p[0].y + pt,
                 mb->g.box->border_darkest);
}

static int
gmenubar_expose (GWindow pixmap, GGadget *g, GEvent *expose)
{
  GMenuBar *mb = (GMenuBar *) g;
  GRect r, old1, old2, old3;
  Color fg = (g->state == gs_disabled) ? g->box->disabled_foreground :
    ((g->box->main_foreground == COLOR_DEFAULT) ?
     GDrawGetDefaultForeground (GDrawGetDisplayOfWindow (pixmap))
     : g->box->main_foreground);
  int i;

  if (fg == COLOR_DEFAULT)
    fg = GDrawGetDefaultForeground (GDrawGetDisplayOfWindow (mb->g.base));

  GDrawPushClip (pixmap, &g->r, &old1);

  GBoxDrawBackground (pixmap, &g->r, g->box, g->state, false);
  GBoxDrawBorder (pixmap, &g->r, g->box, g->state, false);
  GDrawPushClip (pixmap, &g->inner, &old2);
  GDrawSetFont (pixmap, mb->font);

  r = g->inner;
  for (i = 0; i < mb->lastmi; ++i)
    {
      r.x = mb->xs[i] + mb->g.inner.x;
      r.width = mb->xs[i + 1] - mb->xs[i];
      GDrawPushClip (pixmap, &r, &old3);
      GTextInfoDraw (pixmap, r.x, r.y, &mb->mi[i].ti, mb->font,
                     (mb->mi[i].ti.disabled ?
                      mb->g.box->disabled_foreground : fg),
                     mb->g.box->active_border, r.y + r.height);
      GDrawPopClip (pixmap, &old3);
    }
  if (i < mb->mtot)
    {
      GMenuBarDrawDownArrow (pixmap, mb, mb->xs[i] + mb->g.inner.x);
    }

  GDrawPopClip (pixmap, &old2);
  GDrawPopClip (pixmap, &old1);
  return (true);
}

static int
GMenuBarIndex (GMenuBar * mb, int x)
{
  int i;

  if (x < 0)
    return (-1);
  for (i = 0; i < mb->lastmi; ++i)
    if (x < mb->g.inner.x + mb->xs[i + 1])
      return (i);
  if (mb->lastmi != mb->mtot)
    return (mb->lastmi);

  return (-1);
}

static int
gmenubar_mouse (GGadget *g, GEvent *event)
{
  GMenuBar *mb = (GMenuBar *) g;
  int which;

  if (mb->child != NULL && mb->child->hidden)
    return (true);

  if (event->type == et_mousedown)
    {
      mb->pressed = true;
      if (mb->child != NULL)
        GMenuSetPressed (mb->child, true);
      which = GMenuBarIndex (mb, event->u.mouse.x);
      if (which == mb->entry_with_mouse && mb->child != NULL)
        GMenuDestroy (mb->child);
      else
        {
          mb->initial_press = true;
          GMenuBarChangeSelection (mb, which, event);
        }
    }
  else if (event->type == et_mousemove && mb->pressed)
    {
      if (GGadgetWithin (g, event->u.mouse.x, event->u.mouse.y))
        GMenuBarChangeSelection (mb, GMenuBarIndex (mb, event->u.mouse.x),
                                 event);
      else if (mb->child != NULL)
        {
          GPoint p;

          p.x = event->u.mouse.x;
          p.y = event->u.mouse.y;
          GDrawTranslateCoordinates (mb->g.base, mb->child->w, &p);
          if (p.x >= 0 && p.y >= 0 && p.x < mb->child->width
              && p.y < mb->child->height)
            {
              GDrawPointerUngrab (GDrawGetDisplayOfWindow (mb->g.base));
              GDrawPointerGrab (mb->child->w);
              event->u.mouse.x = p.x;
              event->u.mouse.y = p.y;
              event->w = mb->child->w;
              gmenu_mouse (mb->child, event);
            }
        }
    }
  else if (event->type == et_mouseup &&
           (!mb->initial_press ||
            !GGadgetWithin (g, event->u.mouse.x, event->u.mouse.y)))
    {
      GMenuBarChangeSelection (mb, -1, event);
      mb->pressed = false;
    }
  else if (event->type == et_mouseup)
    {
      mb->initial_press = mb->pressed = false;
      if (mb->child != NULL)
        GMenuSetPressed (mb->child, false);
    }
  return (true);
}

static void
gmenubar_destroy (GGadget *g)
{
  GMenuBar *mb = (GMenuBar *) g;
  if (g == NULL)
    return;
  if (mb->child != NULL)
    {
      GMenuDestroy (mb->child);
      GDrawSync (NULL);
      GDrawProcessPendingEvents (NULL); /* popup's destroy routine must execute before we die */
    }
  GMenuItemArrayFree (mb->mi);
  free (mb->xs);
  _ggadget_destroy (g);
}

static void
GMenuBarSetFont (GGadget *g, FontInstance * new)
{
  GMenuBar *b = (GMenuBar *) g;
  b->font = new;
}

static FontInstance *
GMenuBarGetFont (GGadget *g)
{
  GMenuBar *b = (GMenuBar *) g;
  return (b->font);
}

static void
GMenuBarTestSize (GMenuBar * mb)
{
  int arrow_size = mb->g.inner.height;
  int i;

  if (mb->xs[mb->mtot] <= mb->g.inner.width + 4)
    mb->lastmi = mb->mtot;
  else
    {
      for (i = mb->mtot - 1;
           i > 0 && mb->xs[i] > mb->g.inner.width - arrow_size; --i);
      mb->lastmi = i;
      memset (&mb->fake, 0, sizeof (GMenuItem));
      mb->fake[0].sub = mb->mi + mb->lastmi;
    }
}

static void
GMenuBarResize (GGadget *g, int32_t width, int32_t height)
{
  _ggadget_resize (g, width, height);
  GMenuBarTestSize ((GMenuBar *) g);
}

struct gfuncs gmenubar_funcs = {
  0,
  sizeof (struct gfuncs),

  gmenubar_expose,
  gmenubar_mouse,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,

  _ggadget_redraw,
  _ggadget_move,
  GMenuBarResize,
  _ggadget_setvisible,
  _ggadget_setenabled,
  _ggadget_getsize,
  _ggadget_getinnersize,

  gmenubar_destroy,

  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  GMenuBarSetFont,
  GMenuBarGetFont,

  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,
  NULL,

  NULL,
  NULL,
  NULL,
  NULL
};

static void
GMenuBarFit (GMenuBar * mb, GGadgetData * gd)
{
  int bp = GBoxBorderWidth (mb->g.base, mb->g.box);
  GRect r;

  if (gd->pos.x <= 0)
    mb->g.r.x = 0;
  if (gd->pos.y <= 0)
    mb->g.r.y = 0;
  if (mb->g.r.width == 0)
    {
      GDrawGetSize (mb->g.base, &r);
      mb->g.r.width = r.width - mb->g.r.x;
    }
  if (mb->g.r.height == 0)
    {
      int as, ds, ld;
      GDrawGetFontMetrics (mb->g.base, mb->font, &as, &ds, &ld);
      mb->g.r.height = as + ds + 2 * bp;
    }
  mb->g.inner.x = mb->g.r.x + bp;
  mb->g.inner.y = mb->g.r.y + bp;
  mb->g.inner.width = mb->g.r.width - 2 * bp;
  mb->g.inner.height = mb->g.r.height - 2 * bp;
}

static void
GMenuBarFindXs (GMenuBar * mb)
{
  int i, wid;

  GDrawSetFont (mb->g.base, mb->font);
  wid = GDrawPointsToPixels (mb->g.base, 8);
  mb->xs[0] = GDrawPointsToPixels (mb->g.base, 2);
  for (i = 0; i < mb->mtot; ++i)
    mb->xs[i + 1] =
      mb->xs[i] + wid + GTextInfoGetWidth (mb->g.base, &mb->mi[i].ti, NULL);
  GMenuBarTestSize (mb);
}

static void
MenuMaskInit (GMenuItem *mi)
{
  int mask = GMenuItemArrayMask (mi);
  if (mask_set)
    menumask |= mask;
  else
    {
      menumask = mask;
      mask_set = true;
    }
}

GGadget *
GMenuBarCreate (struct gwindow *base, GGadgetData * gd, void *data)
{
  GMenuBar *mb = xcalloc (1, sizeof (GMenuBar));

  if (!gmenubar_inited)
    GMenuInit ();
  mb->g.funcs = &gmenubar_funcs;
  _GGadget_Create (&mb->g, base, gd, data, &menubar_box);

  mb->mi = GMenuItemArrayCopy (gd->u.menu, &mb->mtot);
  mb->xs = xmalloc ((mb->mtot + 1) * sizeof (uint16_t));
  mb->entry_with_mouse = -1;
  mb->font = menubar_font;

  GMenuBarFit (mb, gd);
  GMenuBarFindXs (mb);

  MenuMaskInit (mb->mi);
  mb->any_unmasked_shortcuts = GMenuItemArrayAnyUnmasked (mb->mi);

  if (gd->flags & gg_group_end)
    _GGadgetCloseGroup (&mb->g);
  _GWidget_SetMenuBar (&mb->g);

  mb->g.takes_input = true;
  return (&mb->g);
}

GGadget *
GMenu2BarCreate (struct gwindow *base, GGadgetData * gd, void *data)
{
  GMenuBar *mb = xcalloc (1, sizeof (GMenuBar));

  if (!gmenubar_inited)
    GMenuInit ();
  mb->g.funcs = &gmenubar_funcs;
  _GGadget_Create (&mb->g, base, gd, data, &menubar_box);

  mb->mi = GMenuItemArrayCopy (gd->u.menu2, &mb->mtot);
  mb->xs = xmalloc ((mb->mtot + 1) * sizeof (uint16_t));
  mb->entry_with_mouse = -1;
  mb->font = menubar_font;

  GMenuBarFit (mb, gd);
  GMenuBarFindXs (mb);

  MenuMaskInit (mb->mi);
  mb->any_unmasked_shortcuts = GMenuItemArrayAnyUnmasked (mb->mi);

  if (gd->flags & gg_group_end)
    _GGadgetCloseGroup (&mb->g);
  _GWidget_SetMenuBar (&mb->g);

  mb->g.takes_input = true;
  return (&mb->g);
}

/* ************************************************************************** */

static GMenuItem *
GMenuBarFindMid (GMenuItem *mi, int mid)
{
  GMenuItem *retval = NULL;
  int i = 0;
  while (retval == NULL && GMenuItem_nonempty (&mi[i]))
    {
      if (mi[i].mid == mid)
        retval = &mi[i];
      else if (mi[i].sub != NULL)
        retval = GMenuBarFindMid (mi[i].sub, mid);
      i++;
    }
  return retval;
}

void
GMenuBarSetItemChecked (GGadget *g, int mid, int check)
{
  GMenuBar *mb = (GMenuBar *) g;
  GMenuItem *item;

  item = GMenuBarFindMid (mb->mi, mid);
  if (item != NULL)
    item->ti.checked = check;
}

void
GMenuBarSetItemEnabled (GGadget *g, int mid, int enabled)
{
  GMenuBar *mb = (GMenuBar *) g;
  GMenuItem *item;

  item = GMenuBarFindMid (mb->mi, mid);
  if (item != NULL)
    item->ti.disabled = !enabled;
}

void
GMenuBarSetItemName (GGadget *g, int mid, const uint32_t *name)
{
  GMenuBar *mb = (GMenuBar *) g;
  GMenuItem *item;

  item = GMenuBarFindMid (mb->mi, mid);
  if (item != NULL)
    {
      free (item->ti.text);
      item->ti.text = x_u32_strdup_or_null (name);
    }
}

/* Check to see if event matches the given shortcut, expressed in our
   standard syntax and subject to gettext translation. */
VISIBLE bool
GMenuIsCommand (GEvent *event, char *shortcut)
{
  GMenuItem foo;
  uint32_t keysym = event->u.chr.keysym;

  bool result = false;

  if (event->type == et_char)
    {
      if (keysym < GK_Special && islower (keysym))
        keysym = toupper (keysym);

      memset (&foo, 0, sizeof (foo));

      GMenuItemParseShortCut (&foo, shortcut);

      result = ((menumask & event->u.chr.state) == foo.short_mask
                && foo.shortcut_char == keysym);
    }
  return result;
}

VISIBLE int
GMenuMask (void)
{
  return menumask;
}

GResInfo *
_GMenuRIHead (void)
{
  if (!gmenubar_inited)
    GMenuInit ();
  return &gmenubar_ri;
}

int
GMenuAnyUnmaskedShortcuts (GGadget *mb1, GGadget *mb2)
{

  return ((most_recent_popup_menu != NULL
           && most_recent_popup_menu->any_unmasked_shortcuts)
          || (mb1 != NULL && ((GMenuBar *) mb1)->any_unmasked_shortcuts)
          || (mb2 != NULL && ((GMenuBar *) mb2)->any_unmasked_shortcuts));
}
