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

#ifndef DOCDIR
#error You must define DOCDIR.
#endif

#ifndef BROWSER_DISPATCHER
#define BROWSER_DISPATCHER "xdg-open"
#endif

#include "fontforgeui.h"
#include <gfile.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <unistd.h>
#include <utype.h>
#include <ustring.h>
#include <sys/time.h>
#include <gkeysym.h>
#include <sys/types.h>
#include <sortsmill/rexp.h>
#include <invoke_funcs.h>
#include <trim.h>
#include <filename.h>
#include <filenamecat.h>
#include <findprog.h>

#define ACTIVE_BORDER   (_ggadget_Default_Box.active_border)
#define MAIN_FOREGROUND (_ggadget_Default_Box.main_foreground)

char *helpdir = NULL;

#if 0                           // Japanese documentation code.

// FIXME: Support the Japanese documentation, if it is adequately up
// to date and assuming we do not make massive changes to the
// docs (which we definitely might do).
//
// Also, do not use this code to do it. I have left it here for
// reference.

static int
SupportedLocale (char *fullspec, char *locale)
{
  static char *supported[] = { "ja", NULL };
  int i;

  for (i = 0; supported[i] != NULL; ++i)
    {
      if (strcmp (locale, supported[i]) == 0)
        {
          strcat (fullspec, locale);
          strcat (fullspec, "/");
          return (true);
        }
    }
  return (false);
}

// FIXME: Are these docs maintained?
static void
AppendSupportedLocale (char *fullspec)
{
  /* KANOU has provided a japanese translation of the docs */
  /* Edward Lee is working on traditional chinese */
  const char *loc = getenv ("LC_ALL");
  char buffer[40], *pt;

  if (loc == NULL)
    loc = getenv ("LC_MESSAGES");
  if (loc == NULL)
    loc = getenv ("LANG");
  if (loc == NULL)
    return;
  strncpy (buffer, loc, sizeof (buffer));
  if (SupportedLocale (fullspec, buffer))
    return;
  pt = strchr (buffer, '.');
  if (pt != NULL)
    {
      *pt = '\0';
      if (SupportedLocale (fullspec, buffer))
        return;
    }
  pt = strchr (buffer, '_');
  if (pt != NULL)
    {
      *pt = '\0';
      if (SupportedLocale (fullspec, buffer))
        return;
    }
}

#endif // Japanese documentation code.

// See http://en.wikipedia.org/wiki/URI_scheme
static const char *uri_pattern = "^([[:alpha:]][-+.[:alnum:]]*):(.*)$";

// FIXME: Put this in an URI-processing module.
static bool
looks_like_uri (const char *url)
{
  rexp_match_t m = rexp_search (rexp_compile (uri_pattern), url);
  return (bool) m;
}

static char *
absolute_help_file (const char *file)
{
  // FIXME: Maybe add support for the Japanese documentation.

  char *abs_file;
  if (IS_ABSOLUTE_PATH (file))
    abs_file = xstrdup (file);
  else
    abs_file = file_name_concat (DOCDIR, file, NULL);
  return abs_file;
}

static char *
make_help_uri (char *uri_or_file)
{
  char *uri = trim (uri_or_file);
  if (!looks_like_uri (uri))
    {
      char *abs_file = absolute_help_file (uri);
      const char *scheme = "file://";
      char *new_uri =
        xcalloc (strlen (scheme) + strlen (abs_file) + 1, sizeof (char));
      strcat (new_uri, scheme);
      strcat (new_uri, abs_file);
      free (abs_file);
      free (uri);
      uri = new_uri;
    }
  return uri;
}

static const char *browser_dispatcher = BROWSER_DISPATCHER;

static char *
find_browser (void)
{
  return (char *) find_in_path (browser_dispatcher);
}

void
help (char *file)
{
  char *uri = make_help_uri (file);
  char *browser = find_browser ();
  char *command =
    xcalloc (strlen (browser) + strlen (uri) + 10, sizeof (char));
  strcat (command, browser);
  strcat (command, " ");
  strcat (command, uri);
  strcat (command, " &");
  system (command);
  free (command);
  free (browser);
  free (uri);
}

//-------------------------------------------------------------------------

static void
UI_IError (const char *format, ...)
{
  va_list ap;
  char buffer[300];
  va_start (ap, format);
  vsnprintf (buffer, sizeof (buffer), format, ap);
  GDrawIError ("%s", buffer);
  va_end (ap);
}

#define MAX_ERR_LINES	400
static struct errordata
{
  char *errlines[MAX_ERR_LINES];
  GFont *font;
  int fh, as;
  GGadget *vsb;
  GWindow gw, v;
  int cnt, linecnt;
  int offtop;
  int showing;
  int start_l, start_c, end_l, end_c;
  int down;
} errdata;

static void
ErrHide (void)
{
  GDrawSetVisible (errdata.gw, false);
  errdata.showing = false;
}

static void
ErrScroll (struct sbevent *sb)
{
  int newpos = errdata.offtop;

  switch (sb->type)
    {
    case et_sb_top:
      newpos = 0;
      break;
    case et_sb_uppage:
      newpos -= errdata.linecnt;
      break;
    case et_sb_up:
      --newpos;
      break;
    case et_sb_down:
      ++newpos;
      break;
    case et_sb_downpage:
      newpos += errdata.linecnt;
      break;
    case et_sb_bottom:
      newpos = errdata.cnt - errdata.linecnt;
      break;
    case et_sb_thumb:
    case et_sb_thumbrelease:
      newpos = sb->pos;
      break;
    }
  if (newpos > errdata.cnt - errdata.linecnt)
    newpos = errdata.cnt - errdata.linecnt;
  if (newpos < 0)
    newpos = 0;
  if (newpos != errdata.offtop)
    {
      errdata.offtop = newpos;
      GScrollBarSetPos (errdata.vsb, errdata.offtop);
      GDrawRequestExpose (errdata.v, NULL, false);
    }
}

static int
ErrChar (GEvent *e)
{
  int newpos = errdata.offtop;

  switch (e->u.chr.keysym)
    {
    case GK_Home:
      newpos = 0;
      break;
    case GK_End:
      newpos = errdata.cnt - errdata.linecnt;
      break;
    case GK_Page_Up:
    case GK_KP_Page_Up:
      newpos -= errdata.linecnt;
      break;
    case GK_Page_Down:
    case GK_KP_Page_Down:
      newpos += errdata.linecnt;
      break;
    case GK_Up:
    case GK_KP_Up:
      --newpos;
      break;
    case GK_Down:
    case GK_KP_Down:
      ++newpos;
      break;
    }
  if (newpos > errdata.cnt - errdata.linecnt)
    newpos = errdata.cnt - errdata.linecnt;
  if (newpos < 0)
    newpos = 0;
  if (newpos != errdata.offtop)
    {
      errdata.offtop = newpos;
      GScrollBarSetPos (errdata.vsb, errdata.offtop);
      GDrawRequestExpose (errdata.v, NULL, false);
      return (true);
    }
  return (false);
}

static int
warnings_e_h (GWindow gw, GEvent *event)
{

  if ((event->type == et_mouseup || event->type == et_mousedown) &&
      (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return (GGadgetDispatchEvent (errdata.vsb, event));
    }

  switch (event->type)
    {
    case et_char:
      return (ErrChar (event));
      break;
    case et_expose:
      break;
    case et_resize:
      {
        GRect size, sbsize;
        GDrawGetSize (gw, &size);
        GGadgetGetSize (errdata.vsb, &sbsize);
        GGadgetMove (errdata.vsb, size.width - sbsize.width, 0);
        GGadgetResize (errdata.vsb, sbsize.width, size.height);
        GDrawResize (errdata.v, size.width - sbsize.width, size.height);
        errdata.linecnt = size.height / errdata.fh;
        GScrollBarSetBounds (errdata.vsb, 0, errdata.cnt, errdata.linecnt);
        if (errdata.offtop + errdata.linecnt > errdata.cnt)
          {
            errdata.offtop = errdata.cnt - errdata.linecnt;
            if (errdata.offtop < 0)
              errdata.offtop = 0;
            GScrollBarSetPos (errdata.vsb, errdata.offtop);
          }
        GDrawRequestExpose (errdata.v, NULL, false);
      }
      break;
    case et_controlevent:
      switch (event->u.control.subtype)
        {
        case et_scrollbarchange:
          ErrScroll (&event->u.control.u.sb);
          break;
        }
      break;
    case et_close:
      ErrHide ();
      break;
    case et_create:
      break;
    case et_destroy:
      break;
    }
  return (true);
}

static void
noop (void *_ed)
{
}

static void *
genutf8data (void *_ed, int32_t *len)
{
  int cnt, l;
  int s_l = errdata.start_l, s_c = errdata.start_c, e_l = errdata.end_l, e_c =
    errdata.end_c;
  char *ret, *pt;

  if (s_l > e_l)
    {
      s_l = e_l;
      s_c = e_c;
      e_l = errdata.start_l;
      e_c = errdata.start_c;
    }

  if (s_l == -1)
    {
      *len = 0;
      return (xstrdup (""));
    }

  l = s_l;
  if (e_l == l)
    {
      *len = e_c - s_c;
      return (xstrndup_or_null (errdata.errlines[l] + s_c, e_c - s_c));
    }

  cnt = strlen (errdata.errlines[l] + s_c) + 1;
  for (++l; l < e_l; ++l)
    cnt += strlen (errdata.errlines[l]) + 1;
  cnt += e_c;

  ret = xmalloc (cnt + 1);
  strcpy (ret, errdata.errlines[s_l] + s_c);
  pt = ret + strlen (ret);
  *pt++ = '\n';
  for (l = s_l + 1; l < e_l; ++l)
    {
      strcpy (pt, errdata.errlines[l]);
      pt += strlen (pt);
      *pt++ = '\n';
    }
  strncpy (pt, errdata.errlines[l], e_c);
  *len = cnt;
  return (ret);
}

static void
MouseToPos (GEvent *event, int *_l, int *_c)
{
  int l, c = 0;

  GDrawSetFont (errdata.v, errdata.font);
  l = event->u.mouse.y / errdata.fh + errdata.offtop;
  if (l >= errdata.cnt)
    {
      l = errdata.cnt - 1;
      if (l >= 0)
        c = strlen (errdata.errlines[l]);
    }
  else if (l >= 0)
    {
      GDrawLayoutInit (errdata.v, errdata.errlines[l], -1, NULL);
      c = GDrawLayoutXYToIndex (errdata.v, event->u.mouse.x - 3, 4);
    }
  *_l = l;
  *_c = c;
}

VISIBLE void
WarnMenuCopy (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  GDrawGrabSelection (gw, sn_clipboard);
  GDrawAddSelectionType (gw, sn_clipboard, "UTF8_STRING", &errdata, 1,
                         sizeof (char), genutf8data, noop);
  GDrawAddSelectionType (gw, sn_clipboard, "STRING", &errdata, 1,
                         sizeof (char), genutf8data, noop);
}

VISIBLE void
WarnMenuClear (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  int i;

  for (i = 0; i < errdata.cnt; ++i)
    {
      free (errdata.errlines[i]);
      errdata.errlines[i] = NULL;
    }
  errdata.cnt = 0;
  GDrawRequestExpose (gw, NULL, false);
}

#define MID_Copy	1
#define MID_Clear	2

// *INDENT-OFF*

GMenuItem warnpopupmenu[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Cu_t"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = 1,
      .text_is_1byte = 1,
      .text_has_mnemonic = 1,
      .mnemonic = 't'},
    .shortcut_char = '\0',
    .short_mask = ksm_control
  },
  {
    .ti = {
      .text = (uint32_t *) N_("_Copy"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = 0,
      .text_is_1byte = 1,
      .text_has_mnemonic = 1,
      .mnemonic = 'C'},
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = WarnMenuCopy,
    .mid = MID_Copy
  },
  {
    .ti = {
      .text = (uint32_t *) N_("_Paste"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
            .disabled = 1,
      .text_is_1byte = 1,
      .text_has_mnemonic = 1,
      .mnemonic = 'P'},
    .shortcut_char = '\0',
    .short_mask = ksm_control
  },
  {
    .ti = {
      .text = (uint32_t *) N_("C_lear"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = 0,
      .text_is_1byte = 1,
      .text_has_mnemonic = 1,
      .mnemonic = 'l'
    },
    .invoke = WarnMenuClear,
    .mid = MID_Clear
  },
  GMENUITEM_EMPTY
};

// *INDENT-ON*

static int
warningsv_e_h (GWindow gw, GEvent *event)
{
  int i;

  if ((event->type == et_mouseup || event->type == et_mousedown) &&
      (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return (GGadgetDispatchEvent (errdata.vsb, event));
    }

  switch (event->type)
    {
    case et_expose:
      /*GDrawFillRect(gw,&event->u.expose.rect,GDrawGetDefaultBackground(NULL)); */
      GDrawSetFont (gw, errdata.font);
      for (i = 0; i < errdata.linecnt && i + errdata.offtop < errdata.cnt;
           ++i)
        {
          int xs, xe;
          int s_l = errdata.start_l, s_c = errdata.start_c, e_l =
            errdata.end_l, e_c = errdata.end_c;
          GRect r;
          if (s_l > e_l)
            {
              s_l = e_l;
              s_c = e_c;
              e_l = errdata.start_l;
              e_c = errdata.start_c;
            }
          GDrawLayoutInit (gw, errdata.errlines[i + errdata.offtop], -1,
                           NULL);
          if (i + errdata.offtop >= s_l && i + errdata.offtop <= e_l)
            {
              if (i + errdata.offtop > s_l)
                xs = 0;
              else
                {
                  GRect pos;
                  GDrawLayoutIndexToPos (gw, s_c, &pos);
                  xs = pos.x + 3;
                }
              if (i + errdata.offtop < e_l)
                xe = 3000;
              else
                {
                  GRect pos;
                  GDrawLayoutIndexToPos (gw, s_c, &pos);
                  xe = pos.x + pos.width + 3;
                }
              r.x = xs + 3;
              r.width = xe - xs;
              r.y = i * errdata.fh;
              r.height = errdata.fh;
              GDrawFillRect (gw, &r, ACTIVE_BORDER);
            }
          GDrawLayoutDraw (gw, 3, i * errdata.fh + errdata.as,
                           MAIN_FOREGROUND);
        }
      break;
    case et_char:
      return (ErrChar (event));
      break;
    case et_mousedown:
      if (event->u.mouse.button == 3)
        {
          warnpopupmenu[1].ti.disabled = errdata.start_l == -1;
          warnpopupmenu[3].ti.disabled = errdata.cnt == 0;
          GMenuCreatePopupMenu (gw, event, warnpopupmenu);
        }
      else
        {
          if (errdata.down)
            return (true);
          MouseToPos (event, &errdata.start_l, &errdata.start_c);
          errdata.down = true;
        }
    case et_mousemove:
    case et_mouseup:
      if (!errdata.down)
        return (true);
      MouseToPos (event, &errdata.end_l, &errdata.end_c);
      GDrawRequestExpose (gw, NULL, false);
      if (event->type == et_mouseup)
        {
          errdata.down = false;
          if (errdata.start_l == errdata.end_l
              && errdata.start_c == errdata.end_c)
            {
              errdata.start_l = errdata.end_l = -1;
            }
          else
            {
              GDrawGrabSelection (gw, sn_primary);
              GDrawAddSelectionType (gw, sn_primary, "UTF8_STRING", &errdata,
                                     1, sizeof (char), genutf8data, noop);
              GDrawAddSelectionType (gw, sn_primary, "STRING", &errdata, 1,
                                     sizeof (char), genutf8data, noop);
            }
        }
      break;
    case et_selclear:
      errdata.start_l = errdata.end_l = -1;
      GDrawRequestExpose (gw, NULL, false);
      break;
    case et_timer:
      break;
    case et_focus:
      break;
    }
  return (true);
}

static void
CreateErrorWindow (void)
{
  GWindowAttrs wattrs;
  GRect pos, size;
  int as, ds, ld;
  GWindow gw;
  GGadgetData gd;
  extern int _GScrollBar_Width;

  GDrawGetSize (GDrawGetRoot (NULL), &size);

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_isdlg | wam_positioned;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.is_dlg = true;
  wattrs.cursor = ct_pointer;
  wattrs.positioned = true;
  wattrs.utf8_window_title = _("Warnings");
  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (400));
  pos.height = GDrawPointsToPixels (NULL, GGadgetScale (100));
  pos.x = size.width - pos.width - 10;
  pos.y = size.height - pos.height - 30;
  errdata.gw = gw =
    GDrawCreateTopWindow (NULL, &pos, warnings_e_h, &errdata, &wattrs);

  errdata.font = GDrawNewFont (NULL, "sans-serif", 10, 400, fs_none);
  errdata.font = GResourceFindFont ("Warnings.Font", errdata.font);
  GDrawGetFontMetrics (errdata.gw, errdata.font, &as, &ds, &ld);
  errdata.as = as;
  errdata.fh = as + ds;

  memset (&gd, 0, sizeof (gd));
  gd.pos.y = 0;
  gd.pos.height = pos.height;
  gd.pos.width = GDrawPointsToPixels (gw, _GScrollBar_Width);
  gd.pos.x = pos.width - gd.pos.width;
  gd.flags = gg_visible | gg_enabled | gg_pos_in_pixels | gg_sb_vert;
  errdata.vsb = GScrollBarCreate (gw, &gd, &errdata);

  pos.width -= gd.pos.width;
  pos.x = pos.y = 0;
  wattrs.mask = wam_events | wam_cursor;
  errdata.v =
    GWidgetCreateSubWindow (gw, &pos, warningsv_e_h, &errdata, &wattrs);
  GDrawSetVisible (errdata.v, true);

  errdata.linecnt = pos.height / errdata.fh;
  errdata.start_l = errdata.end_l = -1;
}

static void
AppendToErrorWindow (char *buffer)
{
  int i, linecnt;
  char *pt, *end;

  if (buffer[strlen (buffer) - 1] == '\n')
    buffer[strlen (buffer) - 1] = '\0';

  for (linecnt = 1, pt = buffer; (pt = strchr (pt, '\n')) != NULL; ++linecnt)
    ++pt;
  if (errdata.cnt + linecnt > MAX_ERR_LINES)
    {
      int off = errdata.cnt + linecnt - MAX_ERR_LINES;
      for (i = 0; i < off; ++i)
        free (errdata.errlines[i]);
      for ( /*i=off */ ; i < errdata.cnt; ++i)
        errdata.errlines[i - off] = errdata.errlines[i];
      for (; i < MAX_ERR_LINES + off; ++i)
        errdata.errlines[i - off] = NULL;
      errdata.cnt -= off;
      if ((errdata.start_l -= off) < 0)
        errdata.start_l = errdata.start_c = 0;
      if ((errdata.end_l -= off) < 0)
        errdata.end_l = errdata.start_l = -1;
    }
  for (i = errdata.cnt, pt = buffer; i < MAX_ERR_LINES; ++i)
    {
      end = strchr (pt, '\n');
      if (end == NULL)
        end = pt + strlen (pt);
      errdata.errlines[i] = xstrndup (pt, end - pt);
      pt = end;
      if (*pt == '\0')
        {
          ++i;
          break;
        }
      ++pt;
    }
  errdata.cnt = i;

  errdata.offtop = errdata.cnt - errdata.linecnt;
  if (errdata.offtop < 0)
    errdata.offtop = 0;
  GScrollBarSetBounds (errdata.vsb, 0, errdata.cnt, errdata.linecnt);
  GScrollBarSetPos (errdata.vsb, errdata.offtop);
}

int
ErrorWindowExists (void)
{
  return (errdata.gw != NULL);
}

void
ShowErrorWindow (void)
{
  if (errdata.gw == NULL)
    return;
  GDrawSetVisible (errdata.gw, true);
  GDrawRaise (errdata.gw);
  if (errdata.showing)
    GDrawRequestExpose (errdata.v, NULL, false);
  errdata.showing = true;
}

static void
_LogError (const char *format, va_list ap)
{
  // Old settings:
  //char buffer[500];
  //char nbuffer[600];

  // FIXME: Come up with a better buffering scheme than this.
  char buffer[2000];
  char nbuffer[2400];

  char *str;
  char *pt;
  char *npt;

  vsnprintf (buffer, sizeof (buffer), format, ap);
  for (pt = buffer, npt = nbuffer;
       *pt != '\0' && npt < nbuffer + sizeof (nbuffer) - 2;)
    {
      *npt++ = *pt++;
      if (pt[-1] == '\n' && *pt != '\0')
        {
          /* Force an indent of at least two spaces on secondary lines of a warning */
          if (npt < nbuffer + sizeof (nbuffer) - 2)
            {
              *npt++ = ' ';
              if (*pt == ' ')
                ++pt;
            }
          if (npt < nbuffer + sizeof (nbuffer) - 2)
            {
              *npt++ = ' ';
              if (*pt == ' ')
                ++pt;
            }
        }
    }
  *npt = '\0';

  if (no_windowing_ui || screen_display == NULL)
    {
      str = utf82def_copy (nbuffer);
      fprintf (stderr, "%s", str);
      if (str[strlen (str) - 1] != '\n')
        putc ('\n', stderr);
      free (str);
    }
  else
    {
      if (!ErrorWindowExists ())
        CreateErrorWindow ();
      AppendToErrorWindow (nbuffer);
      ShowErrorWindow ();
    }
}

static void
UI_LogError (const char *format, ...)
{
  va_list ap;

  va_start (ap, format);
  _LogError (format, ap);
  va_end (ap);
}

static void
UI_post_notice (const char *title, const char *statement, ...)
{
  va_list ap;
  va_start (ap, statement);
  if (no_windowing_ui)
    {
      _LogError (statement, ap);
    }
  else
    {
      if (GWidgetPostNoticeActive8 (title))
        _LogError (statement, ap);
      else
        _GWidgetPostNotice8 (title, statement, ap, 40);
    }
  va_end (ap);
}

static char *
UI_open_file (const char *title, const char *defaultfile,
              const char *initial_filter)
{
  return (gwwv_open_filename (title, defaultfile, initial_filter, NULL));
}

static char *
UI_saveas_file (const char *title, const char *defaultfile,
                const char *initial_filter)
{
  return (gwwv_save_filename (title, defaultfile, initial_filter));
}

static void
tinysleep (int microsecs)
{
#if !defined(__MINGW32__)
  fd_set none;
  struct timeval timeout;

  FD_ZERO (&none);
  memset (&timeout, 0, sizeof (timeout));
  timeout.tv_usec = microsecs;

  select (1, &none, &none, &none, &timeout);
#endif
}

static void
allow_events (void)
{
  GDrawSync (NULL);
  tinysleep (100);
  GDrawProcessPendingEvents (NULL);
}


VISIBLE ui_interface_t gdraw_ui_interface = {
  UI_IError,
  gwwv_post_error,
  UI_LogError,
  UI_post_notice,
  gwwv_ask_centered,
  gwwv_choose,
  gwwv_choose_multiple,
  gwwv_ask_string,
  gwwv_ask_password,
  UI_open_file,
  UI_saveas_file,
  gwwv_progress_start_indicator,
  gwwv_progress_end_indicator,
  gwwv_progress_show,
  gwwv_progress_next,
  gwwv_progress_next_stage,
  gwwv_progress_increment,
  gwwv_progress_change_line1,
  gwwv_progress_change_line2,
  gwwv_progress_pause_timer,
  gwwv_progress_resume_timer,
  gwwv_progress_change_stages,
  gwwv_progress_change_total,
  gwwv_progress_reset,

  allow_events,

  UI_TTFNameIds,
  UI_MSLangString,
  (int (*)(void)) Ps_StrokeFlagsDlg
};
