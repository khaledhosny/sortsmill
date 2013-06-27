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

/* Copyright (C) 2003-2012 by George Williams */
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
#include "fontforgeui.h"
#include <ustring.h>
#include <math.h>
#include <gkeysym.h>
#include <locale.h>
#include <utype.h>
#include <xunistring.h>
#include "ttf.h"
#include "mm.h"

/* As far as I can tell, the CDV in AdobeSansMM is half gibberish */
/* This is disturbing */
/* But at least the CDV in Type1_supp.pdf for Myriad appears correct */
static char *standard_cdvs[5] = {
/* 0 axes? Impossible */
  "{}",
/* 1 axis */
  "{\n" "  1 1 index sub 2 1 roll\n" "  0 index 2 1 roll\n" "  pop\n" "}",
/* 2 axes */
  "{\n"
    "  1 2 index sub 1 2 index sub mul 3 1 roll\n"
    "  1 index 1 2 index sub mul 3 1 roll\n"
    "  1 2 index sub 1 index mul 3 1 roll\n"
    "  1 index 1 index mul 3 1 roll\n" "  pop pop\n" "}",
/* 3 axes */
  "{\n"
    "  1 3 index sub 1 3 index sub mul 1 2 index sub mul 4 1 roll\n"
    "  2 index 1 3 index sub mul 1 2 index sub mul 4 1 roll\n"
    "  1 3 index sub 2 index mul 1 2 index sub mul 4 1 roll\n"
    "  2 index 2 index mul 1 2 index sub mul 4 1 roll\n"
    "  1 3 index sub 1 3 index sub mul 1 index mul 4 1 roll\n"
    "  2 index 1 3 index sub mul 1 index mul 4 1 roll\n"
    "  1 3 index sub 2 index mul 1 index mul 4 1 roll\n"
    "  2 index 2 index mul 1 index mul 4 1 roll\n" "  pop pop pop\n" "}",
/* 4 axes */
/* This requires too big a string. We must build it at runtime */
  NULL
};

static char *cdv_4axis[3] = {
  "{\n"
    "  1 4 index sub 1 4 index sub mul 1 3 index sub mul 1 2 index sub mul 5 1 roll\n"
    "  3 index 1 4 index sub mul 1 3 index sub mul 1 2 index sub mul 5 1 roll\n"
    "  1 4 index sub 3 index mul 1 3 index sub mul 1 2 index sub mul 5 1 roll\n"
    "  3 index 3 index mul 1 3 index sub mul 1 2 index sub mul 5 1 roll\n"
    "  1 4 index sub 1 4 index sub mul 2 index mul 1 2 index sub mul 5 1 roll\n"
    "  3 index 1 4 index sub mul 2 index mul 1 2 index sub mul 5 1 roll\n",
  "  1 4 index sub 3 index mul 2 index mul 1 2 index sub mul 5 1 roll\n"
    "  3 index 3 index mul 2 index mul 1 2 index sub mul 5 1 roll\n"
    "  1 4 index sub 1 4 index sub mul 1 3 index sub mul 1 index mul 5 1 roll\n"
    "  3 index 1 4 index sub mul 1 3 index sub mul 1 index mul 5 1 roll\n"
    "  1 4 index sub 3 index mul 1 3 index sub mul 1 index mul 5 1 roll\n",
  "  3 index 3 index mul 1 3 index sub mul 1 index mul 5 1 roll\n"
    "  1 4 index sub 1 4 index sub mul 2 index mul 1 index mul 5 1 roll\n"
    "  3 index 1 4 index sub mul 2 index mul 1 index mul 5 1 roll\n"
    "  1 4 index sub 3 index mul 2 index mul 1 index mul 5 1 roll\n"
    "  3 index 3 index mul 2 index mul 1 index mul 5 1 roll\n"
    "  pop pop pop pop\n" "}"
};

static char *axistablab[] =
  { N_("Axis 1"), N_("Axis 2"), N_("Axis 3"), N_("Axis 4") };

static int
ExecConvertDesignVector (real *designs, int dcnt, char *ndv, char *cdv,
                         real *stack)
{
  char *temp, dv[101];
  int j, len, cnt;
  char oldloc[24];

  /* PostScript parses things in "C" locale too */
  strcpy (oldloc, setlocale (LC_NUMERIC, NULL));
  setlocale (LC_NUMERIC, "C");
  len = 0;
  for (j = 0; j < dcnt; ++j)
    {
      sprintf (dv + len, "%g ", (double) designs[j]);
      len += strlen (dv + len);
    }
  setlocale (LC_NUMERIC, oldloc);

  temp = xmalloc (len + strlen (ndv) + strlen (cdv) + 20);
  strcpy (temp, dv);
  /*strcpy(temp+len++," "); *//* dv always will end in a space */

  while (isspace (*ndv))
    ++ndv;
  if (*ndv == '{')
    ++ndv;
  strcpy (temp + len, ndv);
  len += strlen (temp + len);
  while (len > 0 && (temp[len - 1] == ' ' || temp[len - 1] == '\n'))
    --len;
  if (len > 0 && temp[len - 1] == '}')
    --len;

  while (isspace (*cdv))
    ++cdv;
  if (*cdv == '{')
    ++cdv;
  strcpy (temp + len, cdv);
  len += strlen (temp + len);
  while (len > 0 && (temp[len - 1] == ' ' || temp[len - 1] == '\n'))
    --len;
  if (len > 0 && temp[len - 1] == '}')
    --len;

  cnt = EvaluatePS (temp, stack, MmMax);
  free (temp);
  return cnt;
}

static int
StandardPositions (MMSet *mm, int instance_count, int axis_count)
{
  for (int i = 0; i < instance_count; ++i)
    {
      for (int j = 0; j < axis_count; ++j)
        if (mm->positions[i * mm->axis_count + j] !=
            ((i & (1 << j)) ? 1 : 0))
          return false;
    }
  return true;
}

static int
OrderedPositions (MMSet *mm, int instance_count)
{
  /* For a 1 axis system, check that the positions are ordered */
  int i;

  if (mm->positions[0] != 0)     /* must start at 0 */
    return false;
  if (mm->positions[(instance_count - 1) * 4] != 1)     /* and end at 1 */
    return false;
  for (i = 1; i < mm->instance_count; ++i)
    if (mm->positions[i * mm->axis_count] <=
        mm->positions[(i - 1) * mm->axis_count])
      return false;

  return true;
}

static uint32_t *
MMDesignCoords (MMSet *mm)
{
  char buffer[32 * mm->axis_count];
  char *pt;
  int i;
  real axiscoords[4];

  if (mm->instance_count != (1 << mm->axis_count) ||
      !StandardPositions (mm, mm->instance_count, mm->axis_count))
    return x_u8_to_u32 ("");
  MMWeightsUnMap (mm->defweights, axiscoords, mm->axis_count);
  pt = buffer;
  for (i = 0; i < mm->axis_count; ++i)
    {
      sprintf (pt, "%g ", (double) MMAxisUnmap (mm, i, axiscoords[i]));
      pt += strlen (pt);
    }
  pt[-1] = ' ';
  return x_u8_to_u32 (buffer);
}

struct mmcb
{
  int done;
  GWindow gw;
  MMSet *mm;
  FontView *fv;
  int tonew;
};

#define CID_Explicit		6001
#define	CID_ByDesign		6002
#define CID_NewBlends		6003
#define CID_NewDesign		6004
#define CID_Knowns		6005

static int
MMCB_Changed (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_radiochanged)
    {
      GWindow gw = GGadgetGetWindow (g);
      int explicitblends =
        GGadgetIsChecked (GWidgetGetControl (gw, CID_Explicit));
      GGadgetSetEnabled (GWidgetGetControl (gw, CID_NewBlends),
                         explicitblends);
      GGadgetSetEnabled (GWidgetGetControl (gw, CID_NewDesign),
                         !explicitblends);
    }
  return true;
}

static int
GetWeights (GWindow gw, real blends[MmMax], MMSet *mm,
            int instance_count, int axis_count)
{
  int explicitblends =
    GGadgetIsChecked (GWidgetGetControl (gw, CID_Explicit));
  const uint32_t *ret =
    _GGadgetGetTitle (GWidgetGetControl (gw, (explicitblends ?
                                              CID_NewBlends :
                                              CID_NewDesign)));
  const uint32_t *upt;
  uint32_t *uend;
  int i;
  real sum;

  sum = 0;
  for (i = 0, upt = ret; i < instance_count && *upt; ++i)
    {
      blends[i] = u32_strtod (upt, &uend);
      sum += blends[i];
      if (upt == uend)
        break;
      upt = uend;
      while (*upt == ',' || *upt == ' ')
        ++upt;
    }
  if ((explicitblends && i != instance_count) ||
      (!explicitblends && i != axis_count) || *upt != '\0')
    {
      ff_post_error (_("Bad MM Weights"),
                     _
                     ("Incorrect number of instances weights, or illegal numbers"));
      return false;
    }
  if (explicitblends)
    {
      if (sum < .99 || sum > 1.01)
        {
          ff_post_error (_("Bad MM Weights"),
                         _
                         ("The weights for the default version of the font must sum to 1.0"));
          return false;
        }
    }
  else
    {
      i = ExecConvertDesignVector (blends, i, mm->ndv, mm->cdv, blends);
      if (i != instance_count)
        {
          ff_post_error (_("Bad MM Weights"),
                         _
                         ("The results produced by applying the NormalizeDesignVector and ConvertDesignVector functions were not the results expected. You may need to change these functions"));
          return false;
        }
    }
  return true;
}

static int
MMCB_OK (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct mmcb *mmcb = GDrawGetUserData (GGadgetGetWindow (g));
      real blends[MmMax];

      if (!GetWeights
          (mmcb->gw, blends, mmcb->mm, mmcb->mm->instance_count,
           mmcb->mm->axis_count))
        return true;
      MMCreateBlendedFont (mmcb->mm, (FontViewBase *) mmcb->fv, blends,
                           mmcb->tonew);
    }
  return true;
}

static int
MMCB_Cancel (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct mmcb *mmcb = GDrawGetUserData (GGadgetGetWindow (g));
      mmcb->done = true;
    }
  return true;
}

static int
mmcb_e_h (GWindow gw, GEvent * event)
{
  if (event->type == et_close)
    {
      struct mmcb *mmcb = GDrawGetUserData (gw);
      mmcb->done = true;
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("mmmenu.html");
          return true;
        }
      return false;
    }
  return true;
}

void
MMChangeBlend (MMSet *mm, FontView * fv, int tonew)
{
  char buffer[MmMax * 20], *pt;
  uint32_t ubuf[MmMax * 20];
  int i, k;
  struct mmcb mmcb;
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[14];
  GTextInfo label[14];
  uint32_t *utemp;

  if (mm == NULL)
    return;

  memset (&mmcb, 0, sizeof (mmcb));
  mmcb.mm = mm;
  mmcb.fv = fv;
  mmcb.tonew = tonew;

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.is_dlg = true;
  wattrs.restrict_input_to_me = true;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title =
    tonew ? _("Blend to New Font") : _("MM Change Def Weights");
  pos.x = pos.y = 0;

  pt = buffer;
  for (i = 0; i < mm->instance_count; ++i)
    {
      sprintf (pt, "%g ", (double) mm->defweights[i]);
      pt += strlen (pt);
    }
  if (pt > buffer)
    pt[-2] = '\0';
  u32_strcpy (ubuf, x_gc_u8_to_u32 (buffer));

  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (270));
  pos.height = GDrawPointsToPixels (NULL, 200);
  mmcb.gw = gw =
    GDrawCreateTopWindow (NULL, &pos, mmcb_e_h, &mmcb, &wattrs);

  memset (&gcd, 0, sizeof (gcd));
  memset (&label, 0, sizeof (label));

  k = 0;
/* TRANSLATORS: The following strings should be concatenated together, the result */
/* translated, and then broken into lines by hand. I'm sure it would */
/* be better to specify this all as one string, but my widgets won't support */
/* that */
  label[k].text =
    (uint32_t *) (tonew ?
                   _("You may specify the new instance of this font") :
                   _("You may change the default instance of this font"));
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;

  label[k].text =
    (uint32_t *) _("either by explicitly entering the contribution");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 13;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;

  label[k].text =
    (uint32_t *) _("of each master design, or by entering the design");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 13;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;

  label[k].text = (uint32_t *) _("values for each axis");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 13;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;

  label[k].text = (uint32_t *) _("Contribution of each master design");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 16;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_cb_on;
  gcd[k].gd.cid = CID_Explicit;
  gcd[k].gd.handle_controlevent = MMCB_Changed;
  gcd[k++].creator = GRadioCreate;

  label[k].text = (uint32_t *) _("Design Axis Values");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 45;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k].gd.cid = CID_ByDesign;
  gcd[k].gd.handle_controlevent = MMCB_Changed;
  gcd[k++].creator = GRadioCreate;

  label[k].text = ubuf;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 15;
  gcd[k].gd.pos.y = gcd[k - 2].gd.pos.y + 18;
  gcd[k].gd.pos.width = 240;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k].gd.cid = CID_NewBlends;
  gcd[k++].creator = GTextFieldCreate;

  label[k].text = utemp = MMDesignCoords (mm);
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 15;
  gcd[k].gd.pos.y = gcd[k - 2].gd.pos.y + 18;
  gcd[k].gd.pos.width = 240;
  gcd[k].gd.flags = gg_visible;
  gcd[k].gd.cid = CID_NewDesign;
  gcd[k++].creator = GTextFieldCreate;

  gcd[k].gd.pos.x = 30 - 3;
  gcd[k].gd.pos.y = GDrawPixelsToPoints (NULL, pos.height) - 35 - 3;
  gcd[k].gd.pos.width = -1;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.handle_controlevent = MMCB_OK;
  gcd[k++].creator = GButtonCreate;

  gcd[k].gd.pos.x = -30;
  gcd[k].gd.pos.y = gcd[k - 1].gd.pos.y + 3;
  gcd[k].gd.pos.width = -1;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.handle_controlevent = MMCB_Cancel;
  gcd[k++].creator = GButtonCreate;

  gcd[k].gd.pos.x = 2;
  gcd[k].gd.pos.y = 2;
  gcd[k].gd.pos.width = pos.width - 4;
  gcd[k].gd.pos.height = pos.height - 4;
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_pos_in_pixels;
  gcd[k].creator = GGroupCreate;

  GGadgetsCreate (gw, gcd);
  free (utemp);
  
  GDrawSetVisible (gw, true);

  while (!mmcb.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);
}

GTextInfo axiscounts[] = {
  {(uint32_t *) "1", NULL, 0, 0, (void *) 1, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "2", NULL, 0, 0, (void *) 2, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "3", NULL, 0, 0, (void *) 3, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "4", NULL, 0, 0, (void *) 4, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  GTEXTINFO_EMPTY
};

/* These names are fixed by Adobe & Apple and are not subject to translation */
GTextInfo axistypes[] = {
  {(uint32_t *) "Weight", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "Width", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "OpticalSize", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1,
   0, 0, '\0'},
  {(uint32_t *) "Slant", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  GTEXTINFO_EMPTY
};

GTextInfo mastercounts[] = {
  {(uint32_t *) "1", NULL, 0, 0, (void *) 1, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "2", NULL, 0, 0, (void *) 2, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "3", NULL, 0, 0, (void *) 3, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "4", NULL, 0, 0, (void *) 4, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "5", NULL, 0, 0, (void *) 5, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "6", NULL, 0, 0, (void *) 6, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "7", NULL, 0, 0, (void *) 7, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "8", NULL, 0, 0, (void *) 8, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "9", NULL, 0, 0, (void *) 9, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "10", NULL, 0, 0, (void *) 10, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "11", NULL, 0, 0, (void *) 11, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "12", NULL, 0, 0, (void *) 12, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "13", NULL, 0, 0, (void *) 13, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "14", NULL, 0, 0, (void *) 14, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "15", NULL, 0, 0, (void *) 15, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "16", NULL, 0, 0, (void *) 16, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
  {(uint32_t *) "17", NULL, 0, 0, (void *) 17, NULL, 0, 0, 0, 0, 0, 0, 1, 0,
   0, '\0'},
#if MmMax!=16
#error "The mastercounts array needs to be expanded to match MmMax"
  /* Actually it should be one bigger than MmMax */
#endif
  GTEXTINFO_EMPTY
};

enum mmw_state
{ mmw_counts, mmw_axes, mmw_designs, mmw_named, mmw_funcs,
  mmw_others
};

typedef struct mmw
{
  GWindow gw;
  enum mmw_state state;
  GWindow subwins[mmw_others + 1];
  MMSet *mm, *old;
  int isnew;
  int done;
  int old_axis_count, old_adobe;
  int axis_count, instance_count;       /* The data in mm are set to the max for each */
  int last_instance_count, last_axis_count, lastw_instance_count;
  struct axismap last_axismaps[4];
  int canceldrop, subheightdiff;
  int lcnt, lmax;
  SplineFont **loaded;
} MMW;

#define MMW_Width	340
#define MMW_Height	300
#define ESD_Width	262
#define ESD_Height	316

#define CID_OK		1001
#define CID_Prev	1002
#define CID_Next	1003
#define CID_Cancel	1004
#define CID_Group	1005

#define CID_AxisCount	2001
#define CID_MasterCount	2002

#define CID_WhichAxis			3000
#define CID_AxisType			3001    /* +[0,3]*100 */
#define CID_AxisBegin			3002    /* +[0,3]*100 */
#define CID_AxisDefault			3003    /* +[0,3]*100 */
#define CID_AxisDefaultLabel		3004    /* +[0,3]*100 */
#define CID_AxisEnd			3005    /* +[0,3]*100 */
#define CID_IntermediateDesign		3006    /* +[0,3]*100 */
#define CID_IntermediateNormalized	3007    /* +[0,3]*100 */

#define DesignScaleFactor	20

#define CID_WhichDesign	4000
#define CID_DesignFonts	4001    /* +[0,26]*DesignScaleFactor */
#define CID_AxisWeights	4002    /* +[0,26]*DesignScaleFactor */

#define CID_NDV			5002
#define CID_CDV			5003

/* CID_Explicit-CID_NewDesign already defined */
#define CID_ForceBoldThreshold	6005
#define CID_FamilyName		6006

static void
SetMasterToAxis (MMW * mmw, int initial)
{
  int i, cnt, def;

  cnt =
    GGadgetGetFirstListSelectedItem (GWidgetGetControl
                                     (mmw->subwins[mmw_counts],
                                      CID_AxisCount)) + 1;
  if (cnt != mmw->old_axis_count)
    {
      GGadget *list =
        GWidgetGetControl (mmw->subwins[mmw_counts], CID_MasterCount);
      int32_t len;
      GTextInfo **ti = GGadgetGetList (list, &len);
      for (i = 0; i < MmMax; ++i)
        ti[i]->disabled = (i + 1) < (1 << cnt);
      def = (1 << cnt);
      if (!initial)
        GGadgetSelectOneListItem (list, def - 1);
      mmw->old_axis_count = cnt;
    }
}

static int
MMW_AxisCntChanged (GGadget * g, GEvent * e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_listselected)
    {
      SetMasterToAxis (GDrawGetUserData (GGadgetGetWindow (g)), false);
    }
  return true;
}

static void
MMUsurpNew (SplineFont *sf)
{
  /* This is a font that wasn't in the original MMSet */
  /* We ARE going to use it in the final result */
  /* So if it is attached to a fontview, we must close that window and */
  /*  claim the splinefont for ourselves */
  FontView *fv, *nextfv;

  if (sf->fv != NULL)
    {
      if (sf->kcld != NULL)
        KCLD_End (sf->kcld);
      if (sf->vkcld != NULL)
        KCLD_End (sf->vkcld);
      sf->kcld = sf->vkcld = NULL;

      for (fv = (FontView *) sf->fv; fv != NULL; fv = nextfv)
        {
          nextfv = (FontView *) (fv->b.nextsame);
          fv->b.nextsame = NULL;
          _FVCloseWindows (fv);
          fv->b.sf = NULL;
          GDrawDestroyWindow (fv->gw);
        }
      sf->fv = NULL;
      SFClearAutoSave (sf);
    }
}

static void
MMDetachNew (SplineFont *sf)
{
  /* This is a font that wasn't in the original MMSet */
  /* We aren't going to use it in the final result */
  /* If it is attached to a fontview, then the fontview retains control */
  /* If not, then free it */
  if (sf->fv == NULL)
    SplineFontFree (sf);
}

static void
MMDetachOld (SplineFont *sf)
{
  /* This is a font that was in the original MMSet */
  /* We aren't going to use it in the final result */
  /* So then free it */
  sf->mm = NULL;
  SplineFontFree (sf);
}

static void
MMW_Close (MMW * mmw)
{
  int i;

  for (i = 0; i < mmw->lcnt; ++i)
    MMDetachNew (mmw->loaded[i]);
  free (mmw->loaded);
  MMSetFreeContents (mmw->mm);
  free (mmw->mm);
  mmw->done = true;
}

static int
MMW_Cancel (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      MMW_Close (mmw);
    }
  return true;
}

static void
MMW_SetState (MMW * mmw)
{
  unsigned int i;

  GDrawSetVisible (mmw->subwins[mmw->state], true);
  for (i = mmw_counts; i <= mmw_others; ++i)
    if (i != mmw->state)
      GDrawSetVisible (mmw->subwins[i], false);

  GGadgetSetEnabled (GWidgetGetControl (mmw->gw, CID_Prev),
                     mmw->state != mmw_counts);
  GGadgetSetEnabled (GWidgetGetControl (mmw->gw, CID_Next),
                     mmw->state != mmw_others && mmw->state != mmw_named);
  GGadgetSetEnabled (GWidgetGetControl (mmw->gw, CID_OK),
                     mmw->state == mmw_others || mmw->state == mmw_named);
}

static int
ParseWeights (GWindow gw, int cid, char *str,
              real *list, int expected, int tabset_cid, int aspect)
{
  int cnt = 0;
  const uint32_t *ret, *pt;
  uint32_t *endpt;

  ret = _GGadgetGetTitle (GWidgetGetControl (gw, cid));

  for (pt = ret; *pt == ' '; ++pt);
  for (; *pt;)
    {
      list[cnt++] = u32_strtod (pt, &endpt);
      if (pt == endpt || (*endpt != '\0' && *endpt != ' '))
        {
          if (tabset_cid != -1)
            GTabSetSetSel (GWidgetGetControl (gw, tabset_cid), aspect);
          ff_post_error (_("Bad Axis"), _("Bad Number in %s"), str);
          return 0;
        }
      for (pt = endpt; *pt == ' '; ++pt);
    }
  if (cnt != expected && expected != -1)
    {
      if (tabset_cid != -1)
        GTabSetSetSel (GWidgetGetControl (gw, tabset_cid), aspect);
      ff_post_error (_("Bad Axis"), _("Wrong number of entries in %s"), str);
      return 0;
    }

  return cnt;
}

static int
ParseList (GWindow gw, int cid, char *str8, int *err, real start,
           real def, real end, real **_list, int tabset_cid, int aspect)
{
  int i, cnt;
  const uint32_t *ret, *pt;
  uint32_t *endpt;
  real *list, val;
  int defdone = false;

  *_list = NULL;

  ret = _GGadgetGetTitle (GWidgetGetControl (gw, cid));
  for (pt = ret; *pt == ' '; ++pt);
  cnt = *pt == '\0' ? 0 : 1;
  for (; *pt; ++pt)
    {
      if (*pt == ' ')
        ++cnt;
      while (*pt == ' ')
        ++pt;
    }
  if (start != end)
    cnt += 2;
  else
    defdone = true;
  list = xmalloc (cnt * sizeof (real));
  if (start == end)
    cnt = 0;
  else
    {
      list[0] = start;
      cnt = 1;
    }

  for (pt = ret; *pt == ' '; ++pt);
  for (; *pt;)
    {
      val = u32_strtod (pt, &endpt);
      if (!defdone && val > def)
        {
          list[cnt++] = def;
          defdone = true;
        }
      list[cnt++] = val;
      if (pt == endpt || (*endpt != '\0' && *endpt != ' '))
        {
          GTabSetSetSel (GWidgetGetControl (gw, tabset_cid), aspect);
          free (list);
          ff_post_error (_("Bad Axis"), _("Bad Number in %s"), str8);
          *err = true;
          return 0;
        }
      for (pt = endpt; *pt == ' '; ++pt);
    }
  if (start != end)
    list[cnt++] = end;
  for (i = 1; i < cnt; ++i)
    if (list[i - 1] > list[i])
      {
        GTabSetSetSel (GWidgetGetControl (gw, tabset_cid), aspect);
        ff_post_error (_("Bad Axis"), _("The %s list is not ordered"), str8);
        free (list);
        *err = true;
        return 0;
      }

  *_list = list;
  return cnt;
}

static char *
_ChooseFonts (char *buffer, SplineFont **sfs, real *positions, int i, int cnt)
{
  char *elsepart = NULL, *ret;
  int pos;
  int k;

  if (i < cnt - 2)
    elsepart = _ChooseFonts (buffer, sfs, positions, i + 1, cnt);

  pos = 0;
  if (positions[i] != 0)
    {
      sprintf (buffer, "%g sub ", (double) positions[i]);
      pos += strlen (buffer);
    }
  sprintf (buffer + pos, "%g div dup 1 sub exch ",
           (double) (positions[i + 1] - positions[i]));
  pos += strlen (buffer + pos);
  for (k = 0; k < i; ++k)
    {
      strcpy (buffer + pos, "0 ");
      pos += 2;
    }
  if (i != 0)
    {
      sprintf (buffer + pos, "%d -2 roll ", i + 2);
      pos += strlen (buffer + pos);
    }
  for (k = i + 2; k < cnt; ++k)
    {
      strcpy (buffer + pos, "0 ");
      pos += 2;
    }

  if (elsepart == NULL)
    return xstrdup_or_null (buffer);

  ret = xmalloc (strlen (buffer) + strlen (elsepart) + 40);
  sprintf (ret, "dup %g le {%s} {%s} ifelse", (double) positions[i + 1],
           buffer, elsepart);
  free (elsepart);
  return ret;
}

static uint32_t *
Figure1AxisCDV (MMW * mmw)
{
  real positions[MmMax];
  SplineFont *sfs[MmMax];
  int i;
  char *temp;
  uint32_t *ret;
  char buffer[400];

  if (mmw->axis_count != 1)
    return x_u8_to_u32 ("");
  if (mmw->instance_count == 2)
    return x_u8_to_u32 (u8_force_valid (standard_cdvs[1]));

  for (i = 0; i < mmw->instance_count; ++i)
    {
      positions[i] = mmw->mm->positions[4 * i];
      sfs[i] = mmw->mm->instances[i];
      if (i > 0 && positions[i - 1] >= positions[i])
        return x_u8_to_u32 ("");
    }
  temp = _ChooseFonts (buffer, sfs, positions, 0, mmw->instance_count);
  ret = x_u8_to_u32 (u8_force_valid (temp));
  free (temp);
  return ret;
}

static char *
_NormalizeAxis (char *buffer, struct axismap *axis, int i)
{
  char *elsepart = NULL, *ret;
  int pos;

  if (i < axis->points - 2)
    elsepart = _NormalizeAxis (buffer, axis, i + 1);

  pos = 0;
  if (axis->blends[i + 1] == axis->blends[i])
    {
      sprintf (buffer, "%g ", (double) axis->blends[i]);
      pos = strlen (buffer);
    }
  else
    {
      if (axis->designs[i] != 0)
        {
          sprintf (buffer, "%g sub ", (double) axis->designs[i]);
          pos += strlen (buffer);
        }
      sprintf (buffer + pos, "%g div ",
               (double) ((axis->designs[i + 1] -
                          axis->designs[i]) / (axis->blends[i + 1] -
                                               axis->blends[i])));
      pos += strlen (buffer + pos);
      if (axis->blends[i] != 0)
        {
          sprintf (buffer + pos, "%g add ", (double) axis->blends[i]);
          pos += strlen (buffer + pos);
        }
    }

  if (elsepart == NULL)
    return xstrdup_or_null (buffer);

  ret = xmalloc (strlen (buffer) + strlen (elsepart) + 40);
  sprintf (ret, "dup %g le {%s} {%s} ifelse", (double) axis->designs[i + 1],
           buffer, elsepart);
  free (elsepart);
  return ret;
}

static char *
NormalizeAxis (char *header, struct axismap *axis)
{
  char *ret;
  char buffer[200];

  ret = _NormalizeAxis (buffer, axis, 0);
  if (*header)
    {
      char *temp;
      temp = xmalloc (strlen (header) + strlen (ret) + 2);
      strcpy (temp, header);
      strcat (temp, ret);
      strcat (temp, "\n");
      free (ret);
      ret = temp;
    }
  return ret;
}

static int
SameAxes (int cnt1, int cnt2, struct axismap *axismaps1,
          struct axismap *axismaps2)
{
  int i, j;

  if (cnt1 != cnt2)
    return false;
  for (i = 0; i < cnt1; ++i)
    {
      if (axismaps1[i].points != axismaps2[i].points)
        return false;
      for (j = 0; j < axismaps1[i].points; ++j)
        {
          if (axismaps1[i].designs[j] >= axismaps2[i].designs[j] + .01 ||
              axismaps1[i].designs[j] <= axismaps2[i].designs[j] - .01)
            return false;
          if (axismaps1[i].blends[j] >= axismaps2[i].blends[j] + .001 ||
              axismaps1[i].blends[j] <= axismaps2[i].blends[j] - .001)
            return false;
        }
    }
  return true;
}

static void
AxisDataCopyFree (struct axismap *into, struct axismap *from, int count)
{
  int i;

  for (i = 0; i < 4; ++i)
    {
      free (into->blends);
      free (into->designs);
      into->blends = NULL;
      into->designs = NULL;
      into->points = 0;
    }
  for (i = 0; i < count; ++i)
    {
      into[i].points = from[i].points;
      into[i].blends = xmalloc (into[i].points * sizeof (real));
      memcpy (into[i].blends, from[i].blends, into[i].points * sizeof (real));
      into[i].designs = xmalloc (into[i].points * sizeof (real));
      memcpy (into[i].designs, from[i].designs,
              into[i].points * sizeof (real));
    }
}

static int
PositionsMatch (MMSet *old, MMSet *mm)
{
  int i, j;

  for (i = 0; i < old->instance_count; ++i)
    {
      for (j = 0; j < old->axis_count; ++j)
        if (old->positions[i * old->axis_count + j] !=
            mm->positions[i * mm->axis_count + j])
          return false;
    }
  return true;
}

static void
MMW_FuncsValid (MMW * mmw)
{
  uint32_t *ut;
  int pos, i;

  if (!SameAxes
      (mmw->axis_count, mmw->last_axis_count, mmw->mm->axismaps,
       mmw->last_axismaps))
    {
      if (mmw->old != NULL &&
          SameAxes (mmw->axis_count, mmw->old->axis_count, mmw->mm->axismaps,
                    mmw->old->axismaps))
        ut = x_u8_to_u32 (u8_force_valid (mmw->old->ndv));
      else
        {
          char *header = mmw->axis_count == 1 ? "  " :
            mmw->axis_count == 2 ? "  exch " :
            mmw->axis_count == 3 ? "  3 -1 roll " : "  4 -1 roll ";
          char *lines[4];
          for (i = 0; i < mmw->axis_count; ++i)
            lines[i] = NormalizeAxis (header, &mmw->mm->axismaps[i]);
          pos = 0;
          for (i = 0; i < mmw->axis_count; ++i)
            pos += strlen (lines[i]);
          ut = xmalloc ((pos + 20) * sizeof (uint32_t));
          u32_strcpy (ut, x_gc_u8_to_u32 ( "{\n"));
          pos = 2;
          for (i = 0; i < mmw->axis_count; ++i)
            {
              u32_strcpy (ut + pos, x_gc_u8_to_u32 ( lines[i]));
              pos += strlen (lines[i]);
            }
          u32_strcpy (ut + pos, x_gc_u8_to_u32 ( "}"));
        }
      GGadgetSetTitle (GWidgetGetControl (mmw->subwins[mmw_funcs], CID_NDV),
                       ut);
      free (ut);
      AxisDataCopyFree (mmw->last_axismaps, mmw->mm->axismaps,
                        mmw->axis_count);
      mmw->last_axis_count = mmw->axis_count;
    }
  if (mmw->last_instance_count != mmw->instance_count)
    {
      if (standard_cdvs[4] == NULL)
        {
          standard_cdvs[4] =
            xmalloc (strlen (cdv_4axis[0]) + strlen (cdv_4axis[1]) +
                      strlen (cdv_4axis[2]) + 2);
          strcpy (standard_cdvs[4], cdv_4axis[0]);
          strcat (standard_cdvs[4], cdv_4axis[1]);
          strcat (standard_cdvs[4], cdv_4axis[2]);
        }
      if (mmw->old != NULL &&
          mmw->axis_count == mmw->old->axis_count &&
          mmw->instance_count == mmw->old->instance_count &&
          PositionsMatch (mmw->old, mmw->mm))
        ut = x_u8_to_u32 (u8_force_valid (mmw->old->cdv));
      else if (mmw->instance_count == (1 << mmw->axis_count) &&
               StandardPositions (mmw->mm, mmw->instance_count,
                                  mmw->axis_count))
        ut = x_u8_to_u32 (u8_force_valid (standard_cdvs[mmw->axis_count]));
      else if (mmw->axis_count == 1 &&
               OrderedPositions (mmw->mm, mmw->instance_count))
        ut = Figure1AxisCDV (mmw);
      else
        ut = x_u8_to_u32 ("");
      GGadgetSetTitle (GWidgetGetControl (mmw->subwins[mmw_funcs], CID_CDV),
                       ut);
      free (ut);
    }
  mmw->last_instance_count = mmw->instance_count;
}

static void
MMW_WeightsValid (MMW * mmw)
{
  char *temp;
  uint32_t *ut, *utc;
  int pos, i;
  real axiscoords[4], weights[2 * MmMax];

  if (mmw->lastw_instance_count != mmw->instance_count)
    {
      temp = xmalloc (mmw->instance_count * 20 + 1);
      pos = 0;
      if (mmw->old != NULL && mmw->instance_count == mmw->old->instance_count)
        {
          for (i = 0; i < mmw->instance_count; ++i)
            {
              sprintf (temp + pos, "%g ", (double) mmw->old->defweights[i]);
              pos += strlen (temp + pos);
            }
          utc = MMDesignCoords (mmw->old);
        }
      else
        {
          for (i = 0; i < mmw->axis_count; ++i)
            {
              if (strcmp (mmw->mm->axes[i], "Weight") == 0 &&
                  400 >= mmw->mm->axismaps[i].designs[0] &&
                  400 <=
                  mmw->mm->axismaps[i].designs[mmw->mm->axismaps[i].points -
                                               1])
                axiscoords[i] = 400;
              else if (strcmp (mmw->mm->axes[i], "OpticalSize") == 0 &&
                       12 >= mmw->mm->axismaps[i].designs[0] &&
                       12 <=
                       mmw->mm->axismaps[i].designs[mmw->mm->
                                                    axismaps[i].points - 1])
                axiscoords[i] = 12;
              else
                axiscoords[i] = (mmw->mm->axismaps[i].designs[0] +
                                 mmw->mm->axismaps[i].designs[mmw->
                                                              mm->axismaps
                                                              [i].points -
                                                              1]) / 2;
            }
          i =
            ExecConvertDesignVector (axiscoords, mmw->axis_count,
                                     mmw->mm->ndv, mmw->mm->cdv, weights);
          if (i != mmw->instance_count)
            {                   /* The functions don't work */
              for (i = 0; i < mmw->instance_count; ++i)
                weights[i] = 1.0 / mmw->instance_count;
              utc = x_u8_to_u32 ("");
            }
          else
            {
              for (i = 0; i < mmw->axis_count; ++i)
                {
                  sprintf (temp + pos, "%g ", (double) axiscoords[i]);
                  pos += strlen (temp + pos);
                }
              temp[pos - 1] = '\0';
              utc = x_u8_to_u32 (u8_force_valid (temp));
              pos = 0;
            }
          for (i = 0; i < mmw->instance_count; ++i)
            {
              sprintf (temp + pos, "%g ", (double) weights[i]);
              pos += strlen (temp + pos);
            }
        }
      temp[pos - 1] = '\0';
      ut = x_u8_to_u32 (u8_force_valid (temp));
      GGadgetSetTitle (GWidgetGetControl
                       (mmw->subwins[mmw_others], CID_NewBlends), ut);
      free (temp);
      free (ut);

      GGadgetSetTitle (GWidgetGetControl
                       (mmw->subwins[mmw_others], CID_NewDesign), utc);
      free (utc);
      mmw->lastw_instance_count = mmw->instance_count;
    }
}

static GTextInfo *
TiFromFont (SplineFont *sf)
{
  GTextInfo *ti = xcalloc (1, sizeof (GTextInfo));
  ti->text = x_u8_to_u32 (u8_force_valid (sf->fontname));
  ti->fg = ti->bg = COLOR_DEFAULT;
  ti->userdata = sf;
  return ti;
}

static GTextInfo **
FontList (MMW * mmw, int instance, int *sel)
{
  FontView *fv;
  int cnt, i, pos;
  GTextInfo **ti;

  cnt = 0;
  if (mmw->old != NULL)
    {
      cnt = mmw->old->instance_count;
    }
  for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
    {
      if (fv->b.cidmaster == NULL && fv->b.sf->mm == NULL)
        ++cnt;
    }
  cnt += mmw->lcnt;

  ++cnt;                        /* New */
  ++cnt;                        /* Browse... */

  ti = xmalloc ((cnt + 1) * sizeof (GTextInfo *));
  pos = -1;
  cnt = 0;
  if (mmw->old != NULL)
    {
      for (i = 0; i < mmw->old->instance_count; ++i)
        {
          if (mmw->old->instances[i] == mmw->mm->instances[instance])
            pos = cnt;
          ti[cnt++] = TiFromFont (mmw->old->instances[i]);
        }
    }
  for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
    {
      if (fv->b.cidmaster == NULL && fv->b.sf->mm == NULL)
        {
          if (fv->b.sf == mmw->mm->instances[instance])
            pos = cnt;
          ti[cnt++] = TiFromFont (fv->b.sf);
        }
    }
  for (i = 0; i < mmw->lcnt; ++i)
    {
      if (mmw->loaded[i] == mmw->mm->instances[instance])
        pos = cnt;
      ti[cnt++] = TiFromFont (mmw->loaded[i]);
    }
  if (pos == -1)
    pos = cnt;
  ti[cnt] = xcalloc (1, sizeof (GTextInfo));
  ti[cnt]->text = utf82u_copy (C_("Font", "New"));
  ti[cnt]->bg = ti[cnt]->fg = COLOR_DEFAULT;
  ++cnt;
  ti[cnt] = xcalloc (1, sizeof (GTextInfo));
  ti[cnt]->text = utf82u_copy (_("Browse..."));
  ti[cnt]->bg = ti[cnt]->fg = COLOR_DEFAULT;
  ti[cnt]->userdata = (void *) (-1);
  ++cnt;
  ti[cnt] = xcalloc (1, sizeof (GTextInfo));

  ti[pos]->selected = true;
  *sel = pos;

  return ti;
}

static void
MMW_DesignsSetup (MMW * mmw)
{
  int i, j, sel;
  char buffer[80];
  char *pt;
  uint32_t ubuf[80];
  GTextInfo **ti;

  for (i = 0; i < mmw->instance_count; ++i)
    {
      GGadget *list = GWidgetGetControl (mmw->subwins[mmw_designs],
                                         CID_DesignFonts +
                                         i * DesignScaleFactor);
      ti = FontList (mmw, i, &sel);
      GGadgetSetList (list, ti, false);
      GGadgetSetTitle (list, ti[sel]->text);
      pt = buffer;
      for (j = 0; j < mmw->axis_count; ++j)
        {
          sprintf (pt, "%g ", (double) mmw->mm->positions[i * 4 + j]);
          pt += strlen (pt);
        }
      if (pt > buffer)
        pt[-1] = '\0';
      u32_strcpy (ubuf, x_gc_u8_to_u32 ( buffer));
      GGadgetSetTitle (GWidgetGetControl
                       (mmw->subwins[mmw_designs],
                        CID_AxisWeights + i * DesignScaleFactor), ubuf);
    }
}

static void
MMW_DoOK (MMW * mmw)
{
  real weights[MmMax + 1];
  real fbt;
  int err = false;
  char *familyname, *fn, *origname = NULL;
  int i, j;
  MMSet *setto, *dlgmm;
  FontView *fv = NULL;
  int defpos;
  struct psdict *oldprivate = NULL;
  Encoding *enc = NULL;

  if (!GetWeights
      (mmw->gw, weights, mmw->mm, mmw->instance_count, mmw->axis_count))
    return;
  fbt = GetReal8 (mmw->subwins[mmw_others], CID_ForceBoldThreshold,
                  _("Force Bold Threshold:"), &err);
  if (err)
    return;

  familyname =
    x_u32_to_u8 (u32_force_valid (_GGadgetGetTitle
				  (GWidgetGetControl (mmw->subwins[mmw_counts], CID_FamilyName))));
  /* They only need specify a family name if there are new fonts */
  if (*familyname == '\0')
    {
      free (familyname);
      for (i = 0; i < mmw->instance_count; ++i)
        if (mmw->mm->instances[i] == NULL)
          break;
        else
          fn = mmw->mm->instances[i]->familyname;
      if (i != mmw->instance_count)
        {
          ff_post_error (_("Bad Multiple Master Font"),
                         _("A Font Family name is required"));
          return;
        }
      familyname = xstrdup_or_null (fn);
    }

  /* Did we have a fontview open on this mm? */
  if (mmw->old != NULL)
    {
      for (j = 0; j < mmw->old->instance_count; ++j)
        if (mmw->old->instances[j]->fv != NULL)
          {
            fv = (FontView *) mmw->old->instances[j]->fv;
            origname = xstrdup_or_null (mmw->old->instances[j]->origname);
            enc = fv->b.map->enc;
            break;
          }
    }

  /* Make sure we free all fonts that we have lying around and aren't going */
  /* to be using. (ones we opened, ones in the old version of the mm). Also */
  /* if any font we want to use is attached to a fontview, then close the */
  /* window */
  for (i = 0; i < mmw->instance_count; ++i)
    if (mmw->mm->instances[i] != NULL)
      {
        if (mmw->old != NULL)
          {
            for (j = 0; j < mmw->old->instance_count; ++j)
              if (mmw->mm->instances[i] == mmw->old->instances[j])
                break;
            if (j != mmw->old->instance_count)
              {
                mmw->old->instances[j] = NULL;
                continue;
              }
            else if (mmw->old->normal == mmw->mm->instances[i])
              {
                mmw->old->normal = NULL;
                continue;
              }
          }
        for (j = 0; j < mmw->lcnt; ++j)
          if (mmw->mm->instances[i] == mmw->loaded[j])
            break;
        if (j != mmw->lcnt)
          {
            mmw->loaded[j] = NULL;
            continue;
          }
        if (enc == NULL && mmw->mm->instances[i]->fv != NULL)
          enc = mmw->mm->instances[i]->fv->map->enc;
        MMUsurpNew (mmw->mm->instances[i]);
      }
  if (mmw->old != NULL)
    {
      for (j = 0; j < mmw->old->instance_count; ++j)
        if (mmw->old->instances[j] != NULL)
          {
            MMDetachOld (mmw->old->instances[j]);
            mmw->old->instances[j] = NULL;
          }
      if (mmw->old->normal != NULL)
        {
          oldprivate = mmw->old->normal->private;
          mmw->old->normal->private = NULL;
          MMDetachOld (mmw->old->normal);
          mmw->old->normal = NULL;
        }
    }
  for (j = 0; j < mmw->lcnt; ++j)
    {
      if (mmw->loaded[j] != NULL)
        {
          MMDetachNew (mmw->loaded[j]);
          mmw->loaded[j] = NULL;
        }
    }

  dlgmm = mmw->mm;
  setto = mmw->old;
  if (setto != NULL)
    {
      MMSetFreeContents (setto);
      memset (setto, 0, sizeof (MMSet));
    }
  else
    setto = xzalloc (sizeof (MMSet));
  setto->axis_count = mmw->axis_count;
  setto->instance_count = mmw->instance_count;
  defpos = mmw->instance_count;
  for (i = 0; i < setto->axis_count; ++i)
    setto->axes[i] = dlgmm->axes[i];
  setto->axismaps = dlgmm->axismaps;
  setto->defweights = xcalloc (setto->instance_count, sizeof (real));
  memcpy (setto->defweights, weights,
            setto->instance_count * sizeof (real));
  free (dlgmm->defweights);
  setto->positions =
    xmalloc (setto->instance_count * setto->axis_count * sizeof (real));
  for (i = 0; i < setto->instance_count; ++i)
    {
      int k = i < defpos ? i : i + 1;
      memcpy (setto->positions + i * setto->axis_count,
              dlgmm->positions + k * dlgmm->axis_count,
              setto->axis_count * sizeof (real));
    }
  free (dlgmm->positions);
  setto->instances = xcalloc (setto->instance_count, sizeof (SplineFont *));
  for (i = 0; i < setto->instance_count; ++i)
    {
      if (dlgmm->instances[i] != NULL)
        {
          int k = i < defpos ? i : i + 1;
          setto->instances[i] = dlgmm->instances[k];
          setto->instances[i]->mm = setto;
        }
    }
  MMMatchGlyphs (setto);
  if (setto->normal == NULL)
    {
      setto->normal = MMNewFont (setto, -1, familyname);
      setto->normal->private = oldprivate;
    }
  if (fbt > 0 && fbt <= 1)
    {
      char buffer[20];
      sprintf (buffer, "%g", (double) fbt);
      if (oldprivate == NULL)
        setto->normal->private = xcalloc (1, sizeof (struct psdict));
      PSDictChangeEntry (setto->normal->private, "ForceBoldThreshold",
                         buffer);
    }
  setto->cdv = dlgmm->cdv;
  setto->ndv = dlgmm->ndv;
  for (i = 0; i < setto->instance_count; ++i)
    {
      if (setto->instances[i] == NULL)
        setto->instances[i] = MMNewFont (setto, i, familyname);
      setto->instances[i]->fv = (FontViewBase *) fv;
    }
  free (dlgmm->instances);
  free (dlgmm);
  if (origname != NULL)
    {
      for (i = 0; i < setto->instance_count; ++i)
        {
          free (setto->instances[i]->origname);
          setto->instances[i]->origname = xstrdup_or_null (origname);
        }
      free (setto->normal->origname);
      setto->normal->origname = origname;
    }
  else
    {
      for (i = 0; i < setto->instance_count; ++i)
        {
          free (setto->instances[i]->origname);
          setto->instances[i]->origname = xstrdup_or_null (setto->normal->origname);
        }
    }
  MMReblend ((FontViewBase *) fv, setto);
  if (fv != NULL)
    {
      for (i = 0; i < setto->instance_count; ++i)
        if (fv->b.sf == setto->instances[i])
          break;
      if (i == setto->instance_count)
        {
          SplineFont *sf = setto->normal;
          BDFFont *bdf;
          int same = fv->filled == fv->show;
          fv->b.sf = sf;
          bdf =
            SplineFontPieceMeal (fv->b.sf, ly_fore,
                                 sf->display_size <
                                 0 ? -sf->display_size : default_fv_font_size,
                                 72,
                                 (fv->
                                  antialias ? pf_antialias : 0) | (fv->bbsized
                                                                   ?
                                                                   pf_bbsized
                                                                   : 0),
                                 NULL);
          BDFFontFree (fv->filled);
          fv->filled = bdf;
          if (same)
            fv->show = bdf;
        }
    }
  free (familyname);

  /* Multi-Mastered bitmaps don't make much sense */
  /* Well, maybe grey-scaled could be interpolated, but yuck */
  for (i = 0; i < setto->instance_count; ++i)
    {
      BDFFont *bdf, *bnext;
      for (bdf = setto->instances[i]->bitmaps; bdf != NULL; bdf = bnext)
        {
          bnext = bdf->next;
          BDFFontFree (bdf);
        }
      setto->instances[i]->bitmaps = NULL;
    }

  if (fv == NULL)
    fv = (FontView *) FontViewCreate (setto->normal, false);
  if (enc == NULL)
    enc = default_encoding;
  FVReencode ((FontViewBase *) fv, enc);
  mmw->done = true;
}

static void
MMW_DoNext (MMW * mmw)
{
  int i, err;
  real start, end, def, *designs, *norm;
  int n, n2;
  char *yesno[3];
  yesno[0] = _("_Yes");
  yesno[1] = _("_No");
  yesno[2] = NULL;

  if (mmw->state == mmw_others)
    return;

  if (mmw->state == mmw_counts)
    {
      mmw->axis_count =
        GGadgetGetFirstListSelectedItem (GWidgetGetControl
                                         (mmw->subwins[mmw_counts],
                                          CID_AxisCount)) + 1;
      mmw->instance_count =
        GGadgetGetFirstListSelectedItem (GWidgetGetControl
                                         (mmw->subwins[mmw_counts],
                                          CID_MasterCount)) + 1;
      /* Arrays are already allocated out to maximum, and we will just leave */
      /*  them there until user hits OK, then we make them the right size */
      for (i = 0; i < 4; ++i)
        GTabSetSetEnabled (GWidgetGetControl
                           (mmw->subwins[mmw_axes], CID_WhichAxis), i,
                           i < mmw->axis_count);
      for (i = 0; i < MmMax + 1; ++i)
        GTabSetSetEnabled (GWidgetGetControl
                           (mmw->subwins[mmw_designs], CID_WhichDesign), i,
                           i < mmw->instance_count);
      /* If we've changed the axis count, and the old selected axis isn't */
      /*  available any more, choose another one */
      if (GTabSetGetSel
          (GWidgetGetControl (mmw->subwins[mmw_axes], CID_WhichAxis)) >=
          mmw->axis_count)
        GTabSetSetSel (GWidgetGetControl
                       (mmw->subwins[mmw_axes], CID_WhichAxis), 0);
      if (mmw->instance_count == (1 << mmw->axis_count))
        {
          for (i = (mmw->old == NULL) ? 0 : mmw->old->instance_count;
               i < mmw->instance_count; ++i)
            {
              mmw->mm->positions[i * 4] = (i & 1) ? 1 : 0;
              mmw->mm->positions[i * 4 + 1] = (i & 2) ? 1 : 0;
              mmw->mm->positions[i * 4 + 2] = (i & 4) ? 1 : 0;
              mmw->mm->positions[i * 4 + 3] = (i & 8) ? 1 : 0;
            }
        }
    }
  else if (mmw->state == mmw_axes)
    {
      for (i = 0; i < mmw->axis_count; ++i)
        {
          free (mmw->mm->axes[i]);
          mmw->mm->axes[i] =
            x_u32_to_u8 (u32_force_valid (_GGadgetGetTitle
					  (GWidgetGetControl
					   (mmw->subwins[mmw_axes], CID_AxisType + i * 100))));
          if (*mmw->mm->axes[i] == '\0')
            {
              GTabSetSetSel (GWidgetGetControl
                             (mmw->subwins[mmw_axes], CID_WhichAxis), i);
              ff_post_error (_("Bad Axis"),
                             _("Please set the Axis Type field"));
              return;           /* Failure */
            }
          err = false;
          start = GetReal8 (mmw->subwins[mmw_axes], CID_AxisBegin + i * 100,
                            _("Begin:"), &err);
          end = GetReal8 (mmw->subwins[mmw_axes], CID_AxisEnd + i * 100,
                          _("End:"), &err);
          def = start;
          if (start >= end || def < start || def > end)
            {
              GTabSetSetSel (GWidgetGetControl
                             (mmw->subwins[mmw_axes], CID_WhichAxis), i);
              ff_post_error (_("Bad Axis"), _("Axis range not valid"));
              return;           /* Failure */
            }
          n =
            ParseList (mmw->subwins[mmw_axes],
                       CID_IntermediateDesign + i * 100,
                       _("Design Settings:"), &err, start, def, end, &designs,
                       CID_WhichAxis, i);
          n2 =
            ParseList (mmw->subwins[mmw_axes],
                       CID_IntermediateNormalized + i * 100,
                       _("Normalized Settings:"), &err, 0, 0,
                       1, &norm, CID_WhichAxis, i);
          if (n != n2 || err)
            {
              GTabSetSetSel (GWidgetGetControl
                             (mmw->subwins[mmw_axes], CID_WhichAxis), i);
              if (!err)
                ff_post_error (_("Bad Axis"),
                               _
                               ("The number of entries in the design settings must match the number in normalized settings"));
              free (designs);
              free (norm);
              return;           /* Failure */
            }
          mmw->mm->axismaps[i].points = n;
          free (mmw->mm->axismaps[i].blends);
          free (mmw->mm->axismaps[i].designs);
          mmw->mm->axismaps[i].blends = norm;
          mmw->mm->axismaps[i].designs = designs;
          mmw->mm->axismaps[i].min = start;
          mmw->mm->axismaps[i].def = def;
          mmw->mm->axismaps[i].max = end;
        }
    }
  else if (mmw->state == mmw_designs)
    {
      real positions[MmMax + 1][4];
      int used[MmMax + 1];
      int j, k, mask;
      SplineFont *sfs[MmMax + 1];
      GTextInfo *ti;

      memset (used, 0, sizeof (used));
      memset (positions, 0, sizeof (positions));
      for (i = 0; i < mmw->instance_count; ++i)
        {
          if (!ParseWeights
              (mmw->subwins[mmw_designs],
               CID_AxisWeights + i * DesignScaleFactor,
               _("Normalized position of this design along each axis"),
               positions[i], mmw->axis_count, CID_WhichDesign, i))
            return;
          mask = 0;
          for (j = 0; j < mmw->axis_count; ++j)
            {
              if (positions[i][j] == 0)
                /* Do Nothing */ ;
              else if (positions[i][j] == 1.0)
                mask |= (1 << j);
              else
                break;
            }
          if (j == mmw->axis_count)
            used[mask] = true;
          for (j = 0; j < i - 1; ++j)
            {
              for (k = 0; k < mmw->axis_count; ++k)
                if (positions[j][k] != positions[i][k])
                  break;
              if (k == mmw->axis_count)
                {
                  char *temp;
                  GTabSetSetSel (GWidgetGetControl
                                 (mmw->subwins[mmw_designs], CID_WhichDesign),
                                 i);
                  ff_post_error (_("Bad Multiple Master Font"),
                                 _
                                 ("The set of positions, %.30s, is used more than once"),
                                 temp =
                                 GGadgetGetTitle8 (GWidgetGetControl
                                                   (mmw->subwins[mmw_designs],
                                                    CID_AxisWeights +
                                                    i * DesignScaleFactor)));
                  free (temp);
                  return;
                }
            }
          ti =
            GGadgetGetListItemSelected (GWidgetGetControl
                                        (mmw->subwins[mmw_designs],
                                         CID_DesignFonts +
                                         i * DesignScaleFactor));
          sfs[i] = ti->userdata;
          if (sfs[i] != NULL)
            {
              for (j = 0; j < i; ++j)
                if (sfs[i] == sfs[j])
                  {
                    GTabSetSetSel (GWidgetGetControl
                                   (mmw->subwins[mmw_designs],
                                    CID_WhichDesign), i);
                    ff_post_error (_("Bad Multiple Master Font"),
                                   _
                                   ("The font %.30s is assigned to two master designs"),
                                   sfs[i]->fontname);
                    return;
                  }
            }
        }
      for (i = 0; i < (1 << mmw->axis_count); ++i)
        if (!used[i])
          {
            char buffer[20], *pt = buffer;
            for (j = 0; j < mmw->axis_count; ++j)
              {
                sprintf (pt, "%d ", (i & (1 << j)) ? 1 : 0);
                pt += 2;
              }
            ff_post_error (_("Bad Multiple Master Font"),
                           _
                           ("The set of positions, %.30s, is not specified in any design (and should be)"),
                           buffer);
            return;
          }
      memcpy (mmw->mm->positions, positions, sizeof (positions));
      for (i = 0; i < mmw->instance_count; ++i)
        mmw->mm->instances[i] = sfs[i];
      if (mmw->old != NULL &&
          mmw->axis_count == mmw->old->axis_count &&
          mmw->instance_count == mmw->old->instance_count &&
          PositionsMatch (mmw->old, mmw->mm))
        /* It's what the font started with, don't complain, already has a cdv */
        ;
      else if (mmw->instance_count == (1 << mmw->axis_count)
               && StandardPositions (mmw->mm, mmw->instance_count,
                                     mmw->axis_count))
        /* It's arranged as we expect it to be */ ;
      else if (mmw->axis_count == 1 &&
               OrderedPositions (mmw->mm, mmw->instance_count))
        /* It's arranged according to our secondary expectations */ ;
      else if (mmw->instance_count == (1 << mmw->axis_count) ||
                            mmw->axis_count == 1)
        {
          if (gwwv_ask
              (_("Disordered designs"), (const char **) yesno, 0, 1,
               _
               ("The master designs are not positioned in the expected order. FontForge will be unable to suggest a ConvertDesignVector for you. Is this what you want?"))
              == 1)
            return;
        }
    }
  else if (mmw->state == mmw_funcs)
    {
      if (*_GGadgetGetTitle
          (GWidgetGetControl (mmw->subwins[mmw_funcs], CID_NDV)) == '\0'
          ||
          *_GGadgetGetTitle (GWidgetGetControl
                             (mmw->subwins[mmw_funcs], CID_CDV)) == '\0')
        {
          ff_post_error (_("Bad PostScript function"),
                         _("Bad PostScript function"));
          return;
        }
      free (mmw->mm->ndv);
      free (mmw->mm->cdv);
      mmw->mm->ndv =
        x_u32_to_u8 (u32_force_valid (_GGadgetGetTitle
				      (GWidgetGetControl (mmw->subwins[mmw_funcs], CID_NDV))));
      mmw->mm->cdv =
        x_u32_to_u8 (u32_force_valid (_GGadgetGetTitle
				      (GWidgetGetControl (mmw->subwins[mmw_funcs], CID_CDV))));
    }

  if (mmw->state == mmw_designs)
    mmw->state += 2;
  else
    ++mmw->state;
  if (mmw->state == mmw_others)
    MMW_WeightsValid (mmw);
  else if (mmw->state == mmw_funcs)
    MMW_FuncsValid (mmw);
  else if (mmw->state == mmw_designs)
    MMW_DesignsSetup (mmw);
  MMW_SetState (mmw);
}

static void
MMW_SimulateDefaultButton (MMW * mmw)
{
  if (mmw->state == mmw_others || mmw->state == mmw_named)
    MMW_DoOK (mmw);
  else
    MMW_DoNext (mmw);
}

static int
MMW_OK (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      MMW_DoOK (mmw);
    }
  return true;
}

static int
MMW_Next (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      MMW_DoNext (mmw);
    }
  return true;
}

static int
MMW_Prev (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      if (mmw->state != mmw_counts)
        {
          if (mmw->state == mmw_funcs)
            mmw->state = mmw_designs;
          else
            --mmw->state;
          MMW_SetState (mmw);
        }
    }
  return true;
}

static int
MMW_CheckOptical (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_textchanged)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      char *top, *bottom, *def;
      uint32_t *ut;
      const uint32_t *ret = _GGadgetGetTitle (g);
      int di = (GGadgetGetCid (g) - CID_AxisType) / 100;
      char buf1[20], buf2[20], buf3[20];

      uint8_t *utf8_ret = x_gc_u32_to_u8 (u32_force_valid (ret));

      if (mmw->old != NULL && di < mmw->old->axis_count &&
          u8_strcmp (utf8_ret, u8_force_valid (mmw->old->axes[di])) == 0)
        {
          sprintf (buf1, "%g", (double) mmw->old->axismaps[di].designs[0]);
          sprintf (buf2, "%g",
                   (double) mmw->old->axismaps[di].designs[mmw->old->axismaps
                                                           [di].points - 1]);
          sprintf (buf3, "%g", (double) mmw->old->axismaps[di].def);
          def = buf3;
          top = buf2;
          bottom = buf1;
        }
      else if (u8_strcmp (utf8_ret, "OpticalSize") == 0)
        {
          top = "72";
          def = "12";
          bottom = "6";
        }
      else if (u8_strcmp (utf8_ret, "Slant") == 0)
        {
          top = "22";
          def = "0";
          bottom = "-22";
        }
      else
        {
          top = "999";
          bottom = "50";
          def = "400";
        }
      ut = x_u8_to_u32 (top);
      GGadgetSetTitle (GWidgetGetControl (GGadgetGetWindow (g),
                                          GGadgetGetCid (g) - CID_AxisType +
                                          CID_AxisEnd), ut);
      free (ut);
      ut = x_u8_to_u32 (bottom);
      GGadgetSetTitle (GWidgetGetControl (GGadgetGetWindow (g),
                                          GGadgetGetCid (g) - CID_AxisType +
                                          CID_AxisBegin), ut);
      free (ut);
      ut = x_u8_to_u32 (def);
      GGadgetSetTitle (GWidgetGetControl (GGadgetGetWindow (g),
                                          GGadgetGetCid (g) - CID_AxisType +
                                          CID_AxisDefault), ut);
      free (ut);
    }
  return true;
}

static int
MMW_CheckBrowse (GGadget * g, GEvent * e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_listselected)
    {
      MMW *mmw = GDrawGetUserData (GGadgetGetWindow (g));
      /*int di = (GGadgetGetCid(g)-CID_DesignFonts)/DesignScaleFactor; */
      GTextInfo *ti = GGadgetGetListItemSelected (g);
      char *temp;
      SplineFont *sf;
      GTextInfo **tis;
      int i, sel, oldsel;
      uint32_t *ut;

      if (ti != NULL && ti->userdata == (void *) -1)
        {
          temp = GetFontNameDialog (NULL, false);
          if (temp == NULL)
            return true;
          sf = LoadSplineFont (temp, 0);
          if (sf == NULL)
            return true;
          if (sf->cidmaster != NULL || sf->subfonts != 0)
            {
              ff_post_error (_("Bad Multiple Master Font"),
                             _
                             ("CID keyed fonts may not be a master design of a multiple master font"));
              return true;
            }
          else if (sf->mm != NULL)
            {
              ff_post_error (_("Bad Multiple Master Font"),
                             _
                             ("CID keyed fonts may not be a master design of a multiple master font"));
              return true;
            }
          if (sf->fv == NULL)
            {
              if (mmw->lcnt >= mmw->lmax)
                {
                  if (mmw->lmax == 0)
                    mmw->loaded =
                      xmalloc ((mmw->lmax = 10) * sizeof (SplineFont *));
                  else
                    mmw->loaded =
                      xrealloc (mmw->loaded,
                                (mmw->lmax += 10) * sizeof (SplineFont *));
                }
              mmw->loaded[mmw->lcnt++] = sf;
              for (i = 0; i < mmw->instance_count; ++i)
                {
                  GGadget *list =
                    GWidgetGetControl (mmw->subwins[mmw_designs],
                                       CID_DesignFonts +
                                       i * DesignScaleFactor);
                  oldsel = GGadgetGetFirstListSelectedItem (list);
                  tis = FontList (mmw, i, &sel);
                  tis[sel]->selected = false;
                  tis[oldsel]->selected = true;
                  GGadgetSetList (list, tis, false);
                }
            }
          ut = x_u8_to_u32 (u8_force_valid (sf->fontname));
          GGadgetSetTitle (g, ut);
          free (ut);
        }
    }
  return true;
}

static int
mmwsub_e_h (GWindow gw, GEvent * event)
{
  if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("multiplemaster.html");
          return true;
        }
      else if (event->u.chr.keysym == 'q'
               && (event->u.chr.state & ksm_control))
        {
          if (event->u.chr.state & ksm_shift)
            MMW_Close (GDrawGetUserData (gw));
          return true;
        }
      else if (event->u.chr.chars[0] == '\r')
        {
          MMW_SimulateDefaultButton ((MMW *) GDrawGetUserData (gw));
          return true;
        }
      return false;
    }
  return true;
}

static int
mmw_e_h (GWindow gw, GEvent * event)
{
  if (event->type == et_close)
    {
      MMW *mmw = GDrawGetUserData (gw);
      MMW_Close (mmw);
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("multiplemaster.html");
          return true;
        }
      else if (event->u.chr.keysym == 'q'
               && (event->u.chr.state & ksm_control))
        {
          if (event->u.chr.state & ksm_shift)
            MMW_Close (GDrawGetUserData (gw));
          else
            MenuExit (NULL, NULL, NULL);
          return true;
        }
      else if (event->u.chr.chars[0] == '\r')
        {
          MMW_SimulateDefaultButton ((MMW *) GDrawGetUserData (gw));
          return true;
        }
      return false;
    }
  return true;
}

static MMSet *
MMCopy (MMSet *orig)
{
  MMSet *mm;
  int i;
  /* Allocate the arrays out to maximum, we'll fix them up later, and we */
  /*  retain the proper counts in the mmw structure. This means we don't */
  /*  lose data when they shrink and then restore a value */

  mm = xzalloc (sizeof (MMSet));
  mm->instance_count = MmMax + 1;
  mm->axis_count = 4;
  for (i = 0; i < orig->axis_count; ++i)
    mm->axes[i] = xstrdup_or_null (orig->axes[i]);
  mm->instances = xcalloc (MmMax + 1, sizeof (SplineFont *));
  memcpy (mm->instances, orig->instances,
          orig->instance_count * sizeof (SplineFont *));
  mm->positions = xcalloc ((MmMax + 1) * 4, sizeof (real));
  for (i = 0; i < orig->instance_count; ++i)
    memcpy (mm->positions + i * 4, orig->positions + i * orig->axis_count,
            orig->axis_count * sizeof (real));
  mm->defweights = xcalloc (MmMax + 1, sizeof (real));
  memcpy (mm->defweights, orig->defweights,
          orig->instance_count * sizeof (real));
  mm->axismaps = xcalloc (4, sizeof (struct axismap));
  for (i = 0; i < orig->axis_count; ++i)
    {
      mm->axismaps[i].points = orig->axismaps[i].points;
      mm->axismaps[i].blends =
        xmalloc (mm->axismaps[i].points * sizeof (real));
      memcpy (mm->axismaps[i].blends, orig->axismaps[i].blends,
              mm->axismaps[i].points * sizeof (real));
      mm->axismaps[i].designs =
        xmalloc (mm->axismaps[i].points * sizeof (real));
      memcpy (mm->axismaps[i].designs, orig->axismaps[i].designs,
              mm->axismaps[i].points * sizeof (real));
      mm->axismaps[i].min = orig->axismaps[i].min;
      mm->axismaps[i].max = orig->axismaps[i].max;
      mm->axismaps[i].def = orig->axismaps[i].def;
    }
  mm->cdv = xstrdup_or_null (orig->cdv);
  mm->ndv = xstrdup_or_null (orig->ndv);
  return mm;
}

void
MMWizard (MMSet *mm)
{
  MMW mmw;
  GRect pos, subpos;
  GWindow gw;
  GWindowAttrs wattrs;
  GTabInfo axisaspects[5], designaspects[MmMax + 1 + 1];
  GGadgetCreateData bgcd[8], cntgcd[9], axisgcd[4][20],
    designgcd[MmMax + 1][5], agcd[2], dgcd[3], ogcd[7];
  GTextInfo blabel[8], cntlabel[9], axislabel[4][20],
    designlabel[MmMax + 1][5], dlabel, olabels[7];
  char axisbegins[4][20], axisends[4][20], axisdefs[4][20];
  char *normalized[4], *designs[4];
  char *freeme;
  int i, k;
  int space, blen =
    GIntGetResource (_NUM_Buttonsize) * 100 / GGadgetScale (100);
  static char *designtablab[] = { "1", "2", "3", "4", "5", "6", "7", "8",
    "9", "10", "11", "12", "13", "14", "15", "16", "17", NULL
  };
#if MmMax!=16
#error "The designtablab array needs to be expanded to match MmMax"
  /* Actually it should be one bigger than MmMax */
#endif

  memset (&mmw, 0, sizeof (mmw));
  mmw.old = mm;
  if (mm != NULL)
    {
      mmw.mm = MMCopy (mm);
      mmw.axis_count = mm->axis_count;
      mmw.instance_count = mm->instance_count;
    }
  else
    {
      mmw.mm = xzalloc (sizeof (MMSet));
      mmw.axis_count = 1;
      mmw.instance_count = 2;
      mmw.mm->axis_count = 4;
      mmw.mm->instance_count = MmMax + 1;
      mmw.mm->instances = xcalloc (MmMax + 1, sizeof (SplineFont *));
      mmw.mm->positions = xcalloc ((MmMax + 1) * 4, sizeof (real));
      mmw.mm->defweights = xcalloc (MmMax + 1, sizeof (real));
      mmw.mm->axismaps = xcalloc (4, sizeof (struct axismap));
      mmw.isnew = true;
    }

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.is_dlg = true;
  wattrs.restrict_input_to_me = true;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = mmw.isnew ? _("Create MM") : _("MM _Info");
  pos.x = pos.y = 0;
  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (MMW_Width));
  pos.height = GDrawPointsToPixels (NULL, MMW_Height);
  mmw.gw = gw = GDrawCreateTopWindow (NULL, &pos, mmw_e_h, &mmw, &wattrs);

  memset (&blabel, 0, sizeof (blabel));
  memset (&bgcd, 0, sizeof (bgcd));

  mmw.canceldrop = GDrawPointsToPixels (NULL, 30);
  bgcd[0].gd.pos.x = 20;
  bgcd[0].gd.pos.y = GDrawPixelsToPoints (NULL, pos.height) - 33;
  bgcd[0].gd.pos.width = -1;
  bgcd[0].gd.pos.height = 0;
  bgcd[0].gd.flags = gg_visible | gg_enabled;
  blabel[0].text = (uint32_t *) _("_OK");
  blabel[0].text_is_1byte = true;
  blabel[0].text_has_mnemonic = true;
  bgcd[0].gd.label = &blabel[0];
  bgcd[0].gd.cid = CID_OK;
  bgcd[0].gd.handle_controlevent = MMW_OK;
  bgcd[0].creator = GButtonCreate;

  space = (MMW_Width - 4 * blen - 40) / 3;
  bgcd[1].gd.pos.x = bgcd[0].gd.pos.x + blen + space;
  bgcd[1].gd.pos.y = bgcd[0].gd.pos.y;
  bgcd[1].gd.pos.width = -1;
  bgcd[1].gd.pos.height = 0;
  bgcd[1].gd.flags = gg_visible;
  blabel[1].text = (uint32_t *) _("< _Prev");
  blabel[1].text_is_1byte = true;
  blabel[1].text_has_mnemonic = true;
  bgcd[1].gd.label = &blabel[1];
  bgcd[1].gd.handle_controlevent = MMW_Prev;
  bgcd[1].gd.cid = CID_Prev;
  bgcd[1].creator = GButtonCreate;

  bgcd[2].gd.pos.x = bgcd[1].gd.pos.x + blen + space;
  bgcd[2].gd.pos.y = bgcd[1].gd.pos.y;
  bgcd[2].gd.pos.width = -1;
  bgcd[2].gd.pos.height = 0;
  bgcd[2].gd.flags = gg_visible;
  blabel[2].text = (uint32_t *) _("_Next >");
  blabel[2].text_has_mnemonic = true;
  blabel[2].text_is_1byte = true;
  bgcd[2].gd.label = &blabel[2];
  bgcd[2].gd.handle_controlevent = MMW_Next;
  bgcd[2].gd.cid = CID_Next;
  bgcd[2].creator = GButtonCreate;

  bgcd[3].gd.pos.x = -20;
  bgcd[3].gd.pos.y = bgcd[1].gd.pos.y;
  bgcd[3].gd.pos.width = -1;
  bgcd[3].gd.pos.height = 0;
  bgcd[3].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  blabel[3].text = (uint32_t *) _("_Cancel");
  blabel[3].text_has_mnemonic = true;
  blabel[3].text_is_1byte = true;
  bgcd[3].gd.label = &blabel[3];
  bgcd[3].gd.handle_controlevent = MMW_Cancel;
  bgcd[3].gd.cid = CID_Cancel;
  bgcd[3].creator = GButtonCreate;

  bgcd[4].gd.pos.x = 2;
  bgcd[4].gd.pos.y = 2;
  bgcd[4].gd.pos.width = pos.width - 4;
  bgcd[4].gd.pos.height = pos.height - 4;
  bgcd[4].gd.flags = gg_enabled | gg_visible | gg_pos_in_pixels;
  bgcd[4].gd.cid = CID_Group;
  bgcd[4].creator = GGroupCreate;

  GGadgetsCreate (gw, bgcd);

  subpos = pos;
  subpos.y = subpos.x = 4;
  subpos.width -= 8;
  mmw.subheightdiff = GDrawPointsToPixels (NULL, 44) - 8;
  subpos.height -= mmw.subheightdiff;
  wattrs.mask = wam_events;
  for (i = mmw_counts; i <= mmw_others; ++i)
    mmw.subwins[i] =
      GWidgetCreateSubWindow (mmw.gw, &subpos, mmwsub_e_h, &mmw, &wattrs);

  memset (&cntlabel, 0, sizeof (cntlabel));
  memset (&cntgcd, 0, sizeof (cntgcd));

  k = 0;
  cntlabel[k].text = (uint32_t *) _("Number of Axes:");
  cntlabel[k].text_is_1byte = true;
  cntgcd[k].gd.label = &cntlabel[k];
  cntgcd[k].gd.pos.x = 5;
  cntgcd[k].gd.pos.y = 11;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k++].creator = GLabelCreate;

  cntgcd[k].gd.pos.x = 10;
  cntgcd[k].gd.pos.y = cntgcd[k - 1].gd.pos.y + 12;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k].gd.u.list = axiscounts;
  cntgcd[k].gd.label = &axiscounts[mmw.axis_count - 1];
  cntgcd[k].gd.cid = CID_AxisCount;
  cntgcd[k].gd.handle_controlevent = MMW_AxisCntChanged;
  cntgcd[k++].creator = GListButtonCreate;
  for (i = 0; i < 4; ++i)
    axiscounts[i].selected = false;
  axiscounts[mmw.axis_count - 1].selected = true;

  cntlabel[k].text = (uint32_t *) _("Number of Master Designs:");
  cntlabel[k].text_is_1byte = true;
  cntgcd[k].gd.label = &cntlabel[k];
  cntgcd[k].gd.pos.x = 5;
  cntgcd[k].gd.pos.y = cntgcd[k - 1].gd.pos.y + 30;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k++].creator = GLabelCreate;

  cntgcd[k].gd.pos.x = 10;
  cntgcd[k].gd.pos.y = cntgcd[k - 1].gd.pos.y + 12;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k].gd.u.list = mastercounts;
  cntgcd[k].gd.label = &mastercounts[mmw.instance_count - 1];
  cntgcd[k].gd.cid = CID_MasterCount;
  cntgcd[k++].creator = GListButtonCreate;
  for (i = 0; i < MmMax + 1; ++i)
    mastercounts[i].selected = false;
  mastercounts[mmw.instance_count - 1].selected = true;

  cntlabel[k].text = (uint32_t *) _("_Family Name:");
  cntlabel[k].text_is_1byte = true;
  cntlabel[k].text_has_mnemonic = true;
  cntgcd[k].gd.label = &cntlabel[k];
  cntgcd[k].gd.pos.x = 10;
  cntgcd[k].gd.pos.y = cntgcd[k - 1].gd.pos.y + 30;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k++].creator = GLabelCreate;

  freeme = NULL;
  if (mmw.old != NULL && mmw.old->normal->familyname != NULL)
    cntlabel[k].text = (uint32_t *) (mmw.old->normal->familyname);
  else
    cntlabel[k].text = (uint32_t *) (freeme = GetNextUntitledName ());
  cntlabel[k].text_is_1byte = true;
  cntgcd[k].gd.label = &cntlabel[k];
  cntgcd[k].gd.pos.x = 15;
  cntgcd[k].gd.pos.y = cntgcd[k - 1].gd.pos.y + 14;
  cntgcd[k].gd.pos.width = 150;
  cntgcd[k].gd.flags = gg_visible | gg_enabled;
  cntgcd[k].gd.cid = CID_FamilyName;
  cntgcd[k++].creator = GTextFieldCreate;

  GGadgetsCreate (mmw.subwins[mmw_counts], cntgcd);
  SetMasterToAxis (&mmw, true);
  free (freeme);

  memset (&axisgcd, 0, sizeof (axisgcd));
  memset (&axislabel, 0, sizeof (axislabel));
  memset (&agcd, 0, sizeof (agcd));
  memset (&axisaspects, 0, sizeof (axisaspects));

  for (i = 0; i < 4; ++i)
    {
      k = 0;
      axislabel[i][k].text = (uint32_t *) _("Axis Type:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 5;
      axisgcd[i][k].gd.pos.y = 11;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      axisgcd[i][k].gd.pos.x = 120;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y - 4;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k].gd.u.list = axistypes;
      axisgcd[i][k].gd.cid = CID_AxisType + i * 100;
      axisgcd[i][k].gd.handle_controlevent = MMW_CheckOptical;
      if (i < mmw.axis_count && mmw.mm->axes[i] != NULL)
        {
          axislabel[i][k].text =
            x_u8_to_u32 (u8_force_valid (mmw.mm->axes[i]));
          axisgcd[i][k].gd.label = &axislabel[i][k];
        }
      axisgcd[i][k++].creator = GListFieldCreate;

      axislabel[i][k].text = (uint32_t *) _("Axis Range:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 5;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y + 20;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      if (mmw.mm->axismaps[i].points < 2)
        {
          strcpy (axisbegins[i], "50");
          strcpy (axisdefs[i], "400");
          strcpy (axisends[i], "999");
        }
      else
        {
          sprintf (axisbegins[i], "%.4g",
                   (double) mmw.mm->axismaps[i].designs[0]);
          sprintf (axisends[i], "%.4g",
                   (double) mmw.mm->axismaps[i].designs[mmw.mm->
                                                        axismaps[i].points -
                                                        1]);
          sprintf (axisdefs[i], "%g",
                   (double) (mmw.mm->axismaps[i].designs[0] +
                               mmw.mm->axismaps[i].designs[mmw.mm->axismaps
                                                           [i].points -
                                                           1]) / 2);
        }

      axislabel[i][k].text = (uint32_t *) _("Begin:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 10;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y + 16;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      axislabel[i][k].text = (uint32_t *) axisbegins[i];
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 50;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y - 4;
      axisgcd[i][k].gd.pos.width = 50;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k].gd.cid = CID_AxisBegin + i * 100;
      axisgcd[i][k++].creator = GTextFieldCreate;

      axislabel[i][k].text = (uint32_t *) _("Default:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 110;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 2].gd.pos.y;
      axisgcd[i][k].gd.flags = gg_visible; // apple
      axisgcd[i][k].gd.cid = CID_AxisDefaultLabel + i * 100;
      axisgcd[i][k++].creator = GLabelCreate;

      axislabel[i][k].text = (uint32_t *) axisdefs[i];
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 148;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 2].gd.pos.y;
      axisgcd[i][k].gd.pos.width = 50;
      axisgcd[i][k].gd.flags = gg_visible; // apple
      axisgcd[i][k].gd.cid = CID_AxisDefault + i * 100;
      axisgcd[i][k++].creator = GTextFieldCreate;

      axislabel[i][k].text = (uint32_t *) _("End:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 210;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 2].gd.pos.y;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      axislabel[i][k].text = (uint32_t *) axisends[i];
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 240;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 2].gd.pos.y;
      axisgcd[i][k].gd.pos.width = 50;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k].gd.cid = CID_AxisEnd + i * 100;
      axisgcd[i][k++].creator = GTextFieldCreate;

      axislabel[i][k].text = (uint32_t *) _("Intermediate Points:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 5;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y + 26;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      normalized[i] = NULL;
      designs[i] = NULL;
      if (mmw.mm->axismaps[i].points > 2)
        {
          int l, j, len1, len2;
          char buffer[30];
          len1 = len2 = 0;
          for (l = 0; l < 2; ++l)
            {
              for (j = 1; j < mmw.mm->axismaps[i].points - 1; ++j)
                {
                  /* I wanted to separate things with commas, but that isn't */
                  /*  a good idea in Europe (comma==decimal point) */
                  sprintf (buffer, "%g ",
                           (double) mmw.mm->axismaps[i].designs[j]);
                  if (designs[i] != NULL)
                    strcpy (designs[i] + len1, buffer);
                  len1 += strlen (buffer);
                  sprintf (buffer, "%g ",
                           (double) mmw.mm->axismaps[i].blends[j]);
                  if (normalized[i] != NULL)
                    strcpy (normalized[i] + len2, buffer);
                  len2 += strlen (buffer);
                }
              if (l == 0)
                {
                  normalized[i] = xmalloc (len2 + 2);
                  designs[i] = xmalloc (len1 + 2);
                }
              else
                {
                  normalized[i][len2 - 1] = '\0';
                  designs[i][len1 - 1] = '\0';
                }
            }
        }

      axislabel[i][k].text = (uint32_t *) _("Design Settings:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 10;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y + 12;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      if (designs[i] != NULL)
        {
          axislabel[i][k].text = (uint32_t *) designs[i];
          axislabel[i][k].text_is_1byte = true;
          axisgcd[i][k].gd.label = &axislabel[i][k];
        }
      axisgcd[i][k].gd.pos.x = 120;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y - 4;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k].gd.cid = CID_IntermediateDesign + i * 100;
      axisgcd[i][k++].creator = GTextFieldCreate;

      axislabel[i][k].text = (uint32_t *) _("Normalized Settings:");
      axislabel[i][k].text_is_1byte = true;
      axisgcd[i][k].gd.label = &axislabel[i][k];
      axisgcd[i][k].gd.pos.x = 10;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y + 28;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k++].creator = GLabelCreate;

      if (normalized[i] != NULL)
        {
          axislabel[i][k].text = (uint32_t *) normalized[i];
          axislabel[i][k].text_is_1byte = true;
          axisgcd[i][k].gd.label = &axislabel[i][k];
        }
      axisgcd[i][k].gd.pos.x = 120;
      axisgcd[i][k].gd.pos.y = axisgcd[i][k - 1].gd.pos.y - 4;
      axisgcd[i][k].gd.flags = gg_visible | gg_enabled;
      axisgcd[i][k].gd.cid = CID_IntermediateNormalized + i * 100;
      axisgcd[i][k++].creator = GTextFieldCreate;

      axisaspects[i].text = (uint32_t *) _(axistablab[i]);
      axisaspects[i].text_is_1byte = true;
      axisaspects[i].gcd = axisgcd[i];
    }
  axisaspects[0].selected = true;

  agcd[0].gd.pos.x = 3;
  agcd[0].gd.pos.y = 3;
  agcd[0].gd.pos.width = MMW_Width - 10;
  agcd[0].gd.pos.height = MMW_Height - 45;
  agcd[0].gd.u.tabs = axisaspects;
  agcd[0].gd.flags = gg_visible | gg_enabled;
  agcd[0].gd.cid = CID_WhichAxis;
  agcd[0].creator = GTabSetCreate;

  GGadgetsCreate (mmw.subwins[mmw_axes], agcd);
  for (i = 0; i < 4; ++i)
    {
      free (axislabel[i][1].text);
      free (normalized[i]);
      free (designs[i]);
    }

  memset (&designgcd, 0, sizeof (designgcd));
  memset (&designlabel, 0, sizeof (designlabel));
  memset (&dgcd, 0, sizeof (dgcd));
  memset (&dlabel, 0, sizeof (dlabel));
  memset (&designaspects, 0, sizeof (designaspects));

  for (i = 0; i < MmMax + 1; ++i)
    {
      designlabel[i][0].text =
        (uint32_t *) _("Source from which this design is to be taken");
      designlabel[i][0].text_is_1byte = true;
      designgcd[i][0].gd.label = &designlabel[i][0];
      designgcd[i][0].gd.pos.x = 3;
      designgcd[i][0].gd.pos.y = 4;
      designgcd[i][0].gd.flags = gg_visible | gg_enabled;
      designgcd[i][0].creator = GLabelCreate;

      designgcd[i][1].gd.pos.x = 15;
      designgcd[i][1].gd.pos.y = 18;
      designgcd[i][1].gd.pos.width = 200;
      designgcd[i][1].gd.flags = gg_visible | gg_enabled;
      designgcd[i][1].gd.cid = CID_DesignFonts + i * DesignScaleFactor;
      designgcd[i][1].gd.handle_controlevent = MMW_CheckBrowse;
      designgcd[i][1].creator = GListButtonCreate;

      designlabel[i][2].text =
        (uint32_t *) _("Normalized position of this design along each axis");
      designlabel[i][2].text_is_1byte = true;
      designgcd[i][2].gd.label = &designlabel[i][2];
      designgcd[i][2].gd.pos.x = 3;
      designgcd[i][2].gd.pos.y = 50;
      designgcd[i][2].gd.flags = gg_visible | gg_enabled;
      designgcd[i][2].creator = GLabelCreate;

      designgcd[i][3].gd.pos.x = 15;
      designgcd[i][3].gd.pos.y = 64;
      designgcd[i][3].gd.pos.width = 200;
      designgcd[i][3].gd.flags = gg_visible | gg_enabled;
      designgcd[i][3].gd.cid = CID_AxisWeights + i * DesignScaleFactor;
      designgcd[i][3].creator = GTextFieldCreate;

      designaspects[i].text = (uint32_t *) designtablab[i];
      designaspects[i].text_is_1byte = true;
      designaspects[i].gcd = designgcd[i];
    }
  designaspects[0].selected = true;

  dlabel.text = (uint32_t *) _("Master Designs");
  dlabel.text_is_1byte = true;
  dgcd[0].gd.label = &dlabel;
  dgcd[0].gd.pos.x = 3;
  dgcd[0].gd.pos.y = 4;
  dgcd[0].gd.flags = gg_visible | gg_enabled;
  dgcd[0].creator = GLabelCreate;

  dgcd[1].gd.pos.x = 3;
  dgcd[1].gd.pos.y = 18;
  dgcd[1].gd.pos.width = MMW_Width - 10;
  dgcd[1].gd.pos.height = MMW_Height - 60;
  dgcd[1].gd.u.tabs = designaspects;
  dgcd[1].gd.flags = gg_visible | gg_enabled;
  dgcd[1].gd.cid = CID_WhichDesign;
  dgcd[1].creator = GTabSetCreate;

  GGadgetsCreate (mmw.subwins[mmw_designs], dgcd);

  memset (&ogcd, 0, sizeof (ogcd));
  memset (&olabels, 0, sizeof (olabels));

  k = 0;
  olabels[k].text = (uint32_t *) _("Normalize Design Vector Function:");
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 3;
  ogcd[k].gd.pos.y = 4;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k++].creator = GLabelCreate;

  ogcd[k].gd.pos.x = 3;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + 15;
  ogcd[k].gd.pos.width = MMW_Width - 10;
  ogcd[k].gd.pos.height = 8 * 12 + 10;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k].gd.cid = CID_NDV;
  ogcd[k++].creator = GTextAreaCreate;

  olabels[k].text = (uint32_t *) _("Convert Design Vector Function:");
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 3;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + ogcd[k - 1].gd.pos.height + 5;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k++].creator = GLabelCreate;

  ogcd[k].gd.pos.x = 3;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + 15;
  ogcd[k].gd.pos.width = MMW_Width - 10;
  ogcd[k].gd.pos.height = 8 * 12 + 10;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k].gd.cid = CID_CDV;
  ogcd[k++].creator = GTextAreaCreate;

  GGadgetsCreate (mmw.subwins[mmw_funcs], ogcd);

  memset (&ogcd, 0, sizeof (ogcd));
  memset (&olabels, 0, sizeof (olabels));

  k = 0;
  olabels[k].text = (uint32_t *) _("Contribution of each master design");
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 10;
  ogcd[k].gd.pos.y = 4;
  ogcd[k].gd.flags = gg_visible | gg_enabled | gg_cb_on;
  ogcd[k].gd.cid = CID_Explicit;
  ogcd[k].gd.handle_controlevent = MMCB_Changed;
  ogcd[k++].creator = GRadioCreate;

  olabels[k].text = (uint32_t *) _("Design Axis Values");
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 10;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + 45;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k].gd.cid = CID_ByDesign;
  ogcd[k].gd.handle_controlevent = MMCB_Changed;
  ogcd[k++].creator = GRadioCreate;

  ogcd[k].gd.pos.x = 15;
  ogcd[k].gd.pos.y = ogcd[k - 2].gd.pos.y + 18;
  ogcd[k].gd.pos.width = 240;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k].gd.cid = CID_NewBlends;
  ogcd[k++].creator = GTextFieldCreate;

  ogcd[k].gd.pos.x = 15;
  ogcd[k].gd.pos.y = ogcd[k - 2].gd.pos.y + 18;
  ogcd[k].gd.pos.width = 240;
  ogcd[k].gd.flags = gg_visible;
  ogcd[k].gd.cid = CID_NewDesign;
  ogcd[k++].creator = GTextFieldCreate;

  olabels[k].text = (uint32_t *) _("Force Bold Threshold:");
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 10;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + 45;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k++].creator = GLabelCreate;

  const char *const_pt;
  if (mmw.old != NULL &&
      (const_pt =
       PSDictHasEntry (mmw.old->normal->private,
                       "ForceBoldThreshold")) != NULL)
    olabels[k].text = (uint32_t *) const_pt;
  else
    olabels[k].text = (uint32_t *) ".3";
  olabels[k].text_is_1byte = true;
  ogcd[k].gd.label = &olabels[k];
  ogcd[k].gd.pos.x = 15;
  ogcd[k].gd.pos.y = ogcd[k - 1].gd.pos.y + 13;
  ogcd[k].gd.flags = gg_visible | gg_enabled;
  ogcd[k].gd.cid = CID_ForceBoldThreshold;
  ogcd[k++].creator = GTextFieldCreate;

  GGadgetsCreate (mmw.subwins[mmw_others], ogcd);

  mmw.state = mmw_counts;
  MMW_SetState (&mmw);
  GDrawSetVisible (mmw.subwins[mmw.state], true);
  GDrawSetVisible (mmw.gw, true);

  while (!mmw.done)
    GDrawProcessOneEvent (NULL);

  GDrawDestroyWindow (gw);
}
