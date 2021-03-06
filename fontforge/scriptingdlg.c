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

/* Copyright (C) 2002-2012 by George Williams */
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

/*
 * FIXME: There is no need to have special, compiled-in support for a
 * Python scripting dialog. It can be implemented as an extension,
 * perhaps _in_ Python. (REPL dialogs can be, also.)
 */

static int
UNUSED (make_sure_there_is_some_code_in_this_module_even_if_there_is_no_python)
= 0;

#if !defined(_NO_PYTHON)

#include <Python.h>
#include "fontforgeui.h"
#include <gresource.h>
#include <utype.h>
#include <ustring.h>
#include <gkeysym.h>

struct sd_data
{
  int done;
  FontView *fv;
  SplineChar *sc;
  int layer;
  GWindow gw;
  int oldh;
};

#define SD_Width	250
#define SD_Height	270
#define CID_Script	1001
#define CID_Box		1002
#define CID_OK		1003
#define CID_Call	1004
#define CID_Cancel	1005
#define CID_Python	1006
#define CID_FF		1007

static int
SD_Call (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      char *fn;
      uint32_t *insert;

      fn = gwwv_open_filename (_("Call Script"), NULL, "*", NULL);
      if (fn == NULL)
        return true;
      insert = xmalloc ((strlen (fn) + 10) * sizeof (uint32_t));
      *insert = '"';
      utf82u_strcpy (insert + 1, fn);
      u32_strcat (insert, x_gc_u8_to_u32 ("\"()"));
      GTextFieldReplace (GWidgetGetControl (GGadgetGetWindow (g), CID_Script),
                         insert);
      free (insert);
      free (fn);
    }
  return true;
}

static void
ExecPython (GGadget *g, GEvent *e)
{
  struct sd_data *sd = GDrawGetUserData (GGadgetGetWindow (g));
  char *str;

  running_script = true;

  str = GGadgetGetTitle8 (GWidgetGetControl (sd->gw, CID_Script));
  PyFF_ScriptString ((FontViewBase *) sd->fv, sd->sc, sd->layer, str);
  free (str);
  running_script = false;
}

static int
SD_OK (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct sd_data *sd = GDrawGetUserData (GGadgetGetWindow (g));
      ExecPython (g, e);
      sd->done = true;
    }
  return true;
}

static void
SD_DoCancel (struct sd_data *sd)
{
  sd->done = true;
}

static int
SD_Cancel (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      SD_DoCancel (GDrawGetUserData (GGadgetGetWindow (g)));
    }
  return true;
}

static int
sd_e_h (GWindow gw, GEvent *event)
{
  struct sd_data *sd = GDrawGetUserData (gw);

  if (sd == NULL)
    return true;

  if (event->type == et_close)
    {
      SD_DoCancel (sd);
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("scripting.html");
          return true;
        }
      return false;
    }
  else if (event->type == et_map)
    {
      /* Above palettes */
      GDrawRaise (gw);
    }
  else if (event->type == et_resize)
    {
      GDrawRequestExpose (gw, NULL, false);
    }
  return true;
}

static void
script_dialog (FontView *fv, CharView *cv)
{
  GRect pos;
  static GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[12], boxes[5], *barray[4][8], *hvarray[4][2];
  GTextInfo label[12];
  struct sd_data sd;
  FontView *list;
  int i, l;

  memset (&sd, 0, sizeof (sd));
  sd.fv = fv;
  sd.sc = cv == NULL ? NULL : cv->b.sc;
  sd.layer = cv == NULL ? ly_fore : CVLayer ((CharViewBase *) cv);
  sd.oldh = pos.height = GDrawPointsToPixels (NULL, SD_Height);

  if (gw == NULL)
    {
      memset (&wattrs, 0, sizeof (wattrs));
      wattrs.mask =
        wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor |
        wam_restrict | wam_isdlg;
      wattrs.event_masks = ~(1 << et_charup);
      wattrs.restrict_input_to_me = 1;
      wattrs.undercursor = 1;
      wattrs.cursor = ct_pointer;
      wattrs.utf8_window_title = _("Execute Script");
      wattrs.is_dlg = true;
      pos.x = pos.y = 0;
      pos.width = GDrawPointsToPixels (NULL, GGadgetScale (SD_Width));
      gw = GDrawCreateTopWindow (NULL, &pos, sd_e_h, &sd, &wattrs);

      memset (&boxes, 0, sizeof (boxes));
      memset (&gcd, 0, sizeof (gcd));
      memset (&label, 0, sizeof (label));

      i = l = 0;
      gcd[i].gd.pos.x = 10;
      gcd[i].gd.pos.y = 10;
      gcd[i].gd.pos.width = SD_Width - 20;
      gcd[i].gd.pos.height = SD_Height - 54;
      gcd[i].gd.flags = gg_visible | gg_enabled | gg_textarea_wrap;
      gcd[i].gd.cid = CID_Script;
      gcd[i++].creator = GTextAreaCreate;
      hvarray[l][0] = &gcd[i - 1];
      hvarray[l++][1] = NULL;

      barray[0][0] = barray[1][0] = barray[0][6] = barray[1][6] = GCD_Glue;
      barray[0][2] = barray[1][2] = barray[0][4] = barray[1][4] = GCD_Glue;
      barray[0][1] = barray[0][5] = GCD_RowSpan;
      barray[0][7] = barray[1][7] = barray[2][0] = NULL;
      gcd[i].gd.pos.x = 25 - 3;
      gcd[i].gd.pos.y = SD_Height - 32 - 3;
      gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
      label[i].text = (uint32_t *) _("_OK");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.handle_controlevent = SD_OK;
      gcd[i].gd.cid = CID_OK;
      gcd[i++].creator = GButtonCreate;
      barray[1][1] = &gcd[i - 1];

      gcd[i].gd.pos.x = -25;
      gcd[i].gd.pos.y = SD_Height - 32;
      gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
      label[i].text = (uint32_t *) _("_Cancel");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.handle_controlevent = SD_Cancel;
      gcd[i].gd.cid = CID_Cancel;
      gcd[i++].creator = GButtonCreate;
      barray[1][5] = &gcd[i - 1];

      gcd[i].gd.pos.x =
        (SD_Width -
         GIntGetResource (_NUM_Buttonsize) * 100 /
         GIntGetResource (_NUM_ScaleFactor)) / 2;
      gcd[i].gd.pos.y = SD_Height - 40;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      label[i].text = (uint32_t *) _("C_all...");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.handle_controlevent = SD_Call;
      gcd[i].gd.cid = CID_Call;
      gcd[i++].creator = GButtonCreate;
      barray[0][3] = &gcd[i - 1];
      barray[1][3] = GCD_RowSpan;
      barray[3][0] = NULL;

      boxes[3].gd.flags = gg_enabled | gg_visible;
      boxes[3].gd.u.boxelements = barray[0];
      boxes[3].creator = GHVBoxCreate;
      hvarray[l][0] = &boxes[3];
      hvarray[l++][1] = NULL;
      hvarray[l][0] = NULL;

      boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
      boxes[0].gd.flags = gg_enabled | gg_visible;
      boxes[0].gd.u.boxelements = hvarray[0];
      boxes[0].creator = GHVGroupCreate;

      GGadgetsCreate (gw, boxes);
      if (boxes[2].ret != NULL)
        GHVBoxSetExpandableCol (boxes[2].ret, gb_expandglue);
      GHVBoxSetExpandableCol (boxes[3].ret, gb_expandgluesame);
      GHVBoxSetExpandableRow (boxes[0].ret, 0);
      GHVBoxFitWindow (boxes[0].ret);
    }
  sd.gw = gw;
  GDrawSetUserData (gw, &sd);
  GWidgetIndicateFocusGadget (GWidgetGetControl (gw, CID_Script));
  GDrawSetVisible (gw, true);
  while (!sd.done)
    GDrawProcessOneEvent (NULL);
  GDrawSetVisible (gw, false);

  /* Selection may be out of date, force a refresh */
  for (list = fv_list; list != NULL; list = (FontView *) list->b.next)
    GDrawRequestExpose (list->v, NULL, false);
  GDrawSync (NULL);
  GDrawProcessPendingEvents (NULL);
  GDrawSetUserData (gw, NULL);
}

void
ScriptDlg (FontView *fv, CharView *cv)
{
  if (Py_IsInitialized ())
    script_dialog (fv, cv);
}

#endif /* Python */
