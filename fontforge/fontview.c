#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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
/* Copyright (C) 2013 by Barry Schwartz */
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

#ifndef _NO_PYTHON
#include <Python.h>
#endif

#include <stdbool.h>
#include "fontforgeui.h"
#include "groups.h"
#include "psfont.h"
#include "annotations.h"
#include <gfile.h>
#include <gio.h>
#include <ustring.h>
#include <gkeysym.h>
#include <utype.h>
#include <chardata.h>
#include <gresource.h>
#include <math.h>
#include <unistd.h>
#include <xunistring.h>
#include <stdlib.h>
#include <xalloc.h>
#include <sortsmill/guile.h>
#include <sortsmill/core.h>
#include <canonicalize.h>
#include <xunistring.h>
#include <moveto_funcs.h>
#include <invoke_funcs.h>

int OpenCharsInNewWindow = 1;
char *RecentFiles[RECENT_MAX] = { NULL };

int save_to_dir = 0;            /* use sfdir rather than sfd */
uint32_t *script_menu_names[SCRIPT_MENU_MAX];
char *script_filenames[SCRIPT_MENU_MAX];
extern int onlycopydisplayed;
extern int copymetadata;
extern int copyttfinstr;
int home_char = 'A';
int compact_font_on_open = 0;
int navigation_mask = 0;        /* Initialized in startui.c */

static char *fv_fontnames = "fontview," "monospace";

#define	FV_LAB_HEIGHT	15

/* fontview icon */
#define fontview_width 16
#define fontview_height 16
static unsigned char fontview_bits[] = {
  0x00, 0x07, 0x80, 0x08, 0x40, 0x17, 0x40, 0x15, 0x60, 0x09, 0x10, 0x02,
  0xa0, 0x01, 0xa0, 0x00, 0xa0, 0x00, 0xa0, 0x00, 0x50, 0x00, 0x52, 0x00,
  0x55, 0x00, 0x5d, 0x00, 0x22, 0x00, 0x1c, 0x00
};

extern int _GScrollBar_Width;

static int fv_fontsize = 11;
static int fv_fs_init = 0;
static Color fvselcol = 0x88b2de;
static Color fvselfgcol = 0xffffff;
Color view_bgcol;
static Color fvglyphinfocol = 0x598ec6;
static Color fvemtpyslotfgcol = 0xcccbca;
static Color fvchangedcol = 0x204a87;
static Color fvhintingneededcol = 0x598ec6;

enum glyphlable
{
  gl_glyph,
  gl_name,
  gl_unicode,
  gl_encoding
};

int default_fv_showhmetrics = false;
int default_fv_showvmetrics = false;
int default_fv_glyphlabel = gl_glyph;
#define METRICS_BASELINE 0x0000c0
#define METRICS_ORIGIN	 0xc00000
#define METRICS_ADVANCE	 0x008000
FontView *fv_list = NULL;

static void
FV_ToggleCharChanged (SplineChar *sc)
{
  int i;
  int j;
  int pos;
  FontView *fv;

  for (fv = (FontView *) (sc->parent->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    {
      if (fv->b.sf != sc->parent)       /* Can happen in CID fonts if
                                           char's parent is not
                                           currently active. */
        continue;
      if (fv->v == NULL || fv->colcnt == 0)     /* Can happen in
                                                   scripts. */
        continue;
      for (pos = 0; pos < fv->b.map->enc_limit; ++pos)
        if (enc_to_gid (fv->b.map, pos) == sc->orig_pos)
          {
            i = pos / fv->colcnt;
            j = pos - i * fv->colcnt;
            i -= fv->rowoff;
            /* Normally we should be checking against fv->rowcnt
               (rather than <=rowcnt), but every now and then the WM
               forces us to use a window size which doesn't fit our
               expectations (maximized view), and we must be prepared
               for half lines. */
            if (i >= 0 && i <= fv->rowcnt)
              {
                GRect r;
                r.x = j * fv->cbw + 1;
                r.width = fv->cbw - 1;
                r.y = i * fv->cbh + 1;
                r.height = fv->lab_height - 1;
                GDrawRequestExpose (fv->v, &r, false);
              }
          }
    }
}

void
FVMarkHintsOutOfDate (SplineChar *sc)
{
  int i, j;
  int pos;
  FontView *fv;

  if (sc->parent->onlybitmaps || sc->parent->multilayer
      || sc->parent->strokedfont)
    return;
  for (fv = (FontView *) (sc->parent->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    {
      if (fv->b.sf != sc->parent)       /* Can happen in CID fonts if char's
                                           parent is not currently
                                           active. */
        continue;
      if (sc->layers[fv->b.active_layer].order2)
        continue;
      if (fv->v == NULL || fv->colcnt == 0)     /* Can happen in
                                                   scripts. */
        continue;
      for (pos = 0; pos < fv->b.map->enc_limit; ++pos)
        if (enc_to_gid (fv->b.map, pos) == sc->orig_pos)
          {
            i = pos / fv->colcnt;
            j = pos - i * fv->colcnt;
            i -= fv->rowoff;
            /* Normally we should be checking against fv->rowcnt
               (rather than <=rowcnt), but every now and then the WM
               forces us to use a window size which doesn't fit our
               expectations (maximized view), and we must be prepared
               for half lines. */
            if (i >= 0 && i <= fv->rowcnt)
              {
                GRect r;
                r.x = j * fv->cbw + 1;
                r.width = fv->cbw - 1;
                r.y = i * fv->cbh + 1;
                r.height = fv->lab_height - 1;
                GDrawDrawLine (fv->v, r.x, r.y, r.x, r.y + r.height - 1,
                               fvhintingneededcol);
                GDrawDrawLine (fv->v, r.x + 1, r.y, r.x + 1,
                               r.y + r.height - 1, fvhintingneededcol);
                GDrawDrawLine (fv->v, r.x + r.width - 1, r.y,
                               r.x + r.width - 1, r.y + r.height - 1,
                               fvhintingneededcol);
                GDrawDrawLine (fv->v, r.x + r.width - 2, r.y,
                               r.x + r.width - 2, r.y + r.height - 1,
                               fvhintingneededcol);
              }
          }
    }
}

static int
FeatureTrans (FontView *fv, int enc)
{
  SplineChar *sc;
  PST *pst;
  char *pt;
  int gid;

  if (enc < 0 || enc >= fv->b.map->enc_limit
      || (gid = enc_to_gid (fv->b.map, enc)) == -1)
    return -1;
  if (fv->cur_subtable == NULL)
    return gid;

  sc = fv->b.sf->glyphs[gid];
  if (sc == NULL)
    return -1;
  for (pst = sc->possub; pst != NULL; pst = pst->next)
    {
      if ((pst->type == pst_substitution || pst->type == pst_alternate)
          && pst->subtable == fv->cur_subtable)
        break;
    }
  if (pst == NULL)
    return -1;
  pt = strchr (pst->u.subs.variant, ' ');
  if (pt != NULL)
    *pt = '\0';
  gid = SFFindExistingSlot (fv->b.sf, -1, pst->u.subs.variant);
  if (pt != NULL)
    *pt = ' ';
  return gid;
}

static void
FVDrawGlyph (GWindow pixmap, FontView *fv, int index, int forcebg)
{
  GRect box, old2;
  int feat_gid;
  SplineChar *sc;
  struct _GImage base;
  GImage gi;
  GClut clut;
  int i, j;
  int em = fv->b.sf->ascent + fv->b.sf->descent;
  int yorg = fv->magnify * (fv->show->ascent);

  i = index / fv->colcnt;
  j = index - i * fv->colcnt;
  i -= fv->rowoff;

  if (index < fv->b.map->enc_limit && (fv->b.selected[index] || forcebg))
    {
      box.x = j * fv->cbw + 1;
      box.width = fv->cbw - 1;
      box.y = i * fv->cbh + fv->lab_height + 1;
      box.height = fv->cbw;
      GDrawFillRect (pixmap, &box,
                     fv->b.selected[index] ? fvselcol : view_bgcol);
    }
  feat_gid = FeatureTrans (fv, index);
  sc = feat_gid != -1 ? fv->b.sf->glyphs[feat_gid] : NULL;
  if (!SCWorthOutputting (sc))
    {
      int x = j * fv->cbw + 1, xend = x + fv->cbw - 2;
      int y = i * fv->cbh + fv->lab_height + 1, yend = y + fv->cbw - 1;
      GDrawDrawLine (pixmap, x, y, xend, yend, fvemtpyslotfgcol);
      GDrawDrawLine (pixmap, x, yend, xend, y, fvemtpyslotfgcol);
    }
  if (sc != NULL)
    {
      BDFChar *bdfc;

      if (fv->show != NULL && fv->show->piecemeal && feat_gid != -1
          && (feat_gid >= fv->show->glyphcnt
              || fv->show->glyphs[feat_gid] == NULL)
          && fv->b.sf->glyphs[feat_gid] != NULL)
        BDFPieceMeal (fv->show, feat_gid);

      if (fv->show != NULL && feat_gid != -1 && feat_gid < fv->show->glyphcnt
          && fv->show->glyphs[feat_gid] == NULL
          && SCWorthOutputting (fv->b.sf->glyphs[feat_gid]))
        {
          /* If we have an outline but no bitmap for this slot. */
          box.x = j * fv->cbw + 1;
          box.width = fv->cbw - 2;
          box.y = i * fv->cbh + fv->lab_height + 2;
          box.height = box.width + 1;
          GDrawDrawRect (pixmap, &box, 0xff0000);
          ++box.x;
          ++box.y;
          box.width -= 2;
          box.height -= 2;
          GDrawDrawRect (pixmap, &box, 0xff0000);
          /* When reencoding a font we can find times where
             index>=show->charcnt */
        }
      else if (fv->show != NULL && feat_gid < fv->show->glyphcnt
               && feat_gid != -1 && fv->show->glyphs[feat_gid] != NULL)
        {
          /* If fontview is set to display an embedded bitmap font
             (not a temporary font, rasterized specially for this
             purpose), then we can't use it directly, as bitmap glyphs
             may contain selections and references. So create a
             temporary copy of the glyph merging all such elements
             into a single bitmap. */
          bdfc =
            fv->show->piecemeal ? fv->
            show->glyphs[feat_gid] : BDFGetMergedChar (fv->show->
                                                       glyphs[feat_gid]);

          memset (&gi, '\0', sizeof (gi));
          memset (&base, '\0', sizeof (base));
          if (bdfc->byte_data)
            {
              gi.u.image = &base;
              base.image_type = it_index;
              if (!fv->b.selected[index])
                base.clut = fv->show->clut;
              else
                {
                  int bgr = ((fvselcol >> 16) & 0xff), bgg =
                    ((fvselcol >> 8) & 0xff), bgb = (fvselcol & 0xff);
                  int fgr = ((fvselfgcol >> 16) & 0xff), fgg =
                    ((fvselfgcol >> 8) & 0xff), fgb = (fvselfgcol & 0xff);
                  int i;
                  memset (&clut, '\0', sizeof (clut));
                  base.clut = &clut;
                  clut.clut_len = fv->show->clut->clut_len;
                  for (i = 0; i < clut.clut_len; ++i)
                    {
                      clut.clut[i] =
                        COLOR_CREATE
                        (bgr + (i * (fgr - bgr)) / (clut.clut_len - 1),
                         bgg + (i * (fgg - bgg)) / (clut.clut_len - 1),
                         bgb + (i * (fgb - bgb)) / (clut.clut_len - 1));
                    }
                }
              GDrawSetDither (NULL, false);     /* on 8 bit displays we don't want any dithering */
            }
          else
            {
              memset (&clut, '\0', sizeof (clut));
              gi.u.image = &base;
              base.image_type = it_mono;
              base.clut = &clut;
              clut.clut_len = 2;
              clut.clut[0] = fv->b.selected[index] ? fvselcol : view_bgcol;
              clut.clut[1] = fv->b.selected[index] ? fvselfgcol : 0;
            }
          base.trans = 0;
          base.clut->trans_index = 0;

          base.data = bdfc->bitmap;
          base.bytes_per_line = bdfc->bytes_per_line;
          base.width = bdfc->xmax - bdfc->xmin + 1;
          base.height = bdfc->ymax - bdfc->ymin + 1;
          box.x = j * fv->cbw;
          box.width = fv->cbw;
          box.y = i * fv->cbh + fv->lab_height + 1;
          box.height = box.width + 1;
          GDrawPushClip (pixmap, &box, &old2);
          if (!fv->b.sf->onlybitmaps && fv->show != fv->filled
              && sc->layers[fv->b.active_layer].splines == NULL
              && sc->layers[fv->b.active_layer].refs == NULL && !sc->widthset
              && !(bdfc->xmax <= 0 && bdfc->xmin == 0 && bdfc->ymax <= 0
                   && bdfc->ymax == 0))
            {
              /* If we have a bitmap but no outline character... */
              GRect b;
              b.x = box.x + 1;
              b.y = box.y + 1;
              b.width = box.width - 2;
              b.height = box.height - 2;
              GDrawDrawRect (pixmap, &b, 0x008000);
              ++b.x;
              ++b.y;
              b.width -= 2;
              b.height -= 2;
              GDrawDrawRect (pixmap, &b, 0x008000);
            }
          /* I assume that the bitmap image matches the bounding
             box. In some bitmap fonts the bitmap has white space on
             the right. This can throw off the centering algorithm. */
          if (fv->magnify > 1)
            {
              GDrawDrawImageMagnified (pixmap, &gi, NULL,
                                       j * fv->cbw + (fv->cbw - 1 -
                                                      fv->magnify *
                                                      base.width) / 2,
                                       i * fv->cbh + fv->lab_height + 1 +
                                       fv->magnify * (fv->show->ascent -
                                                      bdfc->ymax),
                                       fv->magnify * base.width,
                                       fv->magnify * base.height);
            }
          else if (base.image_type == it_index)
            {
              GDrawDrawGlyph (pixmap, &gi, NULL,
                              j * fv->cbw + (fv->cbw - 1 - base.width) / 2,
                              i * fv->cbh + fv->lab_height + 1 +
                              fv->show->ascent - bdfc->ymax);
            }
          else
            GDrawDrawImage (pixmap, &gi, NULL,
                            j * fv->cbw + (fv->cbw - 1 - base.width) / 2,
                            i * fv->cbh + fv->lab_height + 1 +
                            fv->show->ascent - bdfc->ymax);
          if (fv->showhmetrics)
            {
              int x1, x0 =
                j * fv->cbw + (fv->cbw - 1 - fv->magnify * base.width) / 2 -
                bdfc->xmin * fv->magnify;
              /* Draw advance width & horizontal origin. */
              if (fv->showhmetrics & fvm_origin)
                GDrawDrawLine (pixmap, x0,
                               i * fv->cbh + fv->lab_height + yorg - 3, x0,
                               i * fv->cbh + fv->lab_height + yorg + 2,
                               METRICS_ORIGIN);
              x1 = x0 + fv->magnify * bdfc->width;
              if (fv->showhmetrics & fvm_advanceat)
                GDrawDrawLine (pixmap, x1, i * fv->cbh + fv->lab_height + 1,
                               x1, (i + 1) * fv->cbh - 1, METRICS_ADVANCE);
              if (fv->showhmetrics & fvm_advanceto)
                GDrawDrawLine (pixmap, x0, (i + 1) * fv->cbh - 2, x1,
                               (i + 1) * fv->cbh - 2, METRICS_ADVANCE);
            }
          if (fv->showvmetrics)
            {
              int x0 =
                j * fv->cbw + (fv->cbw - 1 - fv->magnify * base.width) / 2 -
                bdfc->xmin * fv->magnify +
                fv->magnify * fv->show->pixelsize / 2;
              int y0 = i * fv->cbh + fv->lab_height + yorg;
              int yvw =
                y0 + fv->magnify * sc->vwidth * fv->show->pixelsize / em;
              if (fv->showvmetrics & fvm_baseline)
                GDrawDrawLine (pixmap, x0, i * fv->cbh + fv->lab_height + 1,
                               x0, (i + 1) * fv->cbh - 1, METRICS_BASELINE);
              if (fv->showvmetrics & fvm_advanceat)
                GDrawDrawLine (pixmap, j * fv->cbw, yvw, (j + 1) * fv->cbw,
                               yvw, METRICS_ADVANCE);
              if (fv->showvmetrics & fvm_advanceto)
                GDrawDrawLine (pixmap, j * fv->cbw + 2, y0, j * fv->cbw + 2,
                               yvw, METRICS_ADVANCE);
              if (fv->showvmetrics & fvm_origin)
                GDrawDrawLine (pixmap, x0 - 3,
                               i * fv->cbh + fv->lab_height + yorg, x0 + 2,
                               i * fv->cbh + fv->lab_height + yorg,
                               METRICS_ORIGIN);
            }
          GDrawPopClip (pixmap, &old2);
          if (!fv->show->piecemeal)
            BDFCharFree (bdfc);
        }
    }
}

static void
FVToggleCharSelected (FontView *fv, int enc)
{
  int i, j;

  if (fv->v == NULL || fv->colcnt == 0) /* Can happen in scripts. */
    return;

  i = enc / fv->colcnt;
  j = enc - i * fv->colcnt;
  i -= fv->rowoff;
  /* Normally we should be checking against fv->rowcnt (rather than
     <=rowcnt), but every now and then the WM forces us to use a
     window size which doesn't fit our expectations (maximized view),
     and we must be prepared for half lines. */
  if (i >= 0 && i <= fv->rowcnt)
    FVDrawGlyph (fv->v, fv, enc, true);
}

static void
FontViewRefreshAll (SplineFont *sf)
{
  FontView *fv;
  for (fv = (FontView *) (sf->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    if (fv->v != NULL)
      GDrawRequestExpose (fv->v, NULL, false);
}

void
FVDeselectAll (FontView *fv)
{
  int i;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    {
      if (fv->b.selected[i])
        {
          fv->b.selected[i] = false;
          FVToggleCharSelected (fv, i);
        }
    }
  fv->sel_index = 0;
}

static void
FVInvertSelection (FontView *fv)
{
  int i;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    {
      fv->b.selected[i] = !fv->b.selected[i];
      FVToggleCharSelected (fv, i);
    }
  fv->sel_index = 1;
}

static void
FVSelectAll (FontView *fv)
{
  int i;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    {
      if (!fv->b.selected[i])
        {
          fv->b.selected[i] = true;
          FVToggleCharSelected (fv, i);
        }
    }
  fv->sel_index = 1;
}

static void
FVReselect (FontView *fv, int newpos)
{
  int i;

  if (newpos < 0)
    newpos = 0;
  else if (newpos >= fv->b.map->enc_limit)
    newpos = fv->b.map->enc_limit - 1;

  if (fv->pressed_pos < fv->end_pos)
    {
      if (newpos > fv->end_pos)
        {
          for (i = fv->end_pos + 1; i <= newpos; ++i)
            if (!fv->b.selected[i])
              {
                fv->b.selected[i] = fv->sel_index;
                FVToggleCharSelected (fv, i);
              }
        }
      else if (newpos < fv->pressed_pos)
        {
          for (i = fv->end_pos; i > fv->pressed_pos; --i)
            if (fv->b.selected[i])
              {
                fv->b.selected[i] = false;
                FVToggleCharSelected (fv, i);
              }
          for (i = fv->pressed_pos - 1; i >= newpos; --i)
            if (!fv->b.selected[i])
              {
                fv->b.selected[i] = fv->sel_index;
                FVToggleCharSelected (fv, i);
              }
        }
      else
        {
          for (i = fv->end_pos; i > newpos; --i)
            if (fv->b.selected[i])
              {
                fv->b.selected[i] = false;
                FVToggleCharSelected (fv, i);
              }
        }
    }
  else
    {
      if (newpos < fv->end_pos)
        {
          for (i = fv->end_pos - 1; i >= newpos; --i)
            if (!fv->b.selected[i])
              {
                fv->b.selected[i] = fv->sel_index;
                FVToggleCharSelected (fv, i);
              }
        }
      else if (newpos > fv->pressed_pos)
        {
          for (i = fv->end_pos; i < fv->pressed_pos; ++i)
            if (fv->b.selected[i])
              {
                fv->b.selected[i] = false;
                FVToggleCharSelected (fv, i);
              }
          for (i = fv->pressed_pos + 1; i <= newpos; ++i)
            if (!fv->b.selected[i])
              {
                fv->b.selected[i] = fv->sel_index;
                FVToggleCharSelected (fv, i);
              }
        }
      else
        {
          for (i = fv->end_pos; i < newpos; ++i)
            if (fv->b.selected[i])
              {
                fv->b.selected[i] = false;
                FVToggleCharSelected (fv, i);
              }
        }
    }
  fv->end_pos = newpos;
}

static void
FVFlattenAllBitmapSelections (FontView *fv)
{
  BDFFont *bdf;
  int i;

  for (bdf = fv->b.sf->bitmaps; bdf != NULL; bdf = bdf->next)
    {
      for (i = 0; i < bdf->glyphcnt; ++i)
        if (bdf->glyphs[i] != NULL && bdf->glyphs[i]->selection != NULL)
          BCFlattenFloat (bdf->glyphs[i]);
    }
}

static int
AskChanged (SplineFont *sf)
{
  int ret;
  char *buts[4];
  char *filename, *fontname;

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;

  filename = sf->filename;
  fontname = sf->fontname;

  if (filename == NULL && sf->origname != NULL && sf->onlybitmaps
      && sf->bitmaps != NULL && sf->bitmaps->next == NULL)
    filename = sf->origname;
  if (filename == NULL)
    filename = "untitled.sfd";
  filename = GFileBaseName (filename);
  buts[0] = _("_Save");
  buts[1] = _("_Don't Save");
  buts[2] = _("_Cancel");
  buts[3] = NULL;
  ret =
    gwwv_ask (_("Font changed"), (const char **) buts, 0, 2,
              _
              ("Font %1$.40s in file %2$.40s has been changed.\nDo you want to save it?"),
              fontname, filename);
  return ret;
}

int
_FVMenuGenerate (FontView *fv, int family)
{
  FVFlattenAllBitmapSelections (fv);
  return (SFGenerateFont
          (fv->b.sf, fv->b.active_layer, family,
           fv->b.normal == NULL ? fv->b.map : fv->b.normal));
}

VISIBLE void
FVMenuGenerate (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  _FVMenuGenerate (fv, gf_none);
}

VISIBLE void
FVMenuGenerateFamily (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  _FVMenuGenerate (fv, gf_macfamily);
}

VISIBLE void
FVMenuGenerateTTC (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  _FVMenuGenerate (fv, gf_ttc);
}

extern int save_to_dir;

static int
SaveAs_FormatChange (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_radiochanged)
    {
      GGadget *fc = GWidgetGetControl (GGadgetGetWindow (g), 1000);
      char *oldname = GGadgetGetTitle8 (fc);
      int *_s2d = GGadgetGetUserData (g);
      int s2d = GGadgetIsChecked (g);
      char *pt, *newname = xmalloc (strlen (oldname) + 8);
      strcpy (newname, oldname);
      pt = strrchr (newname, '.');
      if (pt == NULL)
        pt = newname + strlen (newname);
      strcpy (pt, s2d ? ".sfdir" : ".sfd");
      GGadgetSetTitle8 (fc, newname);
      save_to_dir = *_s2d = s2d;
      SavePrefs (true);
    }
  return true;
}

int
_FVMenuSaveAs (FontView *fv)
{
  char *temp;
  char *ret;
  char *filename;
  int ok;
  int s2d =
    fv->b.cidmaster != NULL ? fv->b.cidmaster->save_to_dir : fv->b.sf->mm !=
    NULL ? fv->b.sf->mm->normal->save_to_dir : fv->b.sf->save_to_dir;
  GGadgetCreateData gcd;
  GTextInfo label;

  if (fv->b.cidmaster != NULL && fv->b.cidmaster->filename != NULL)
    temp = x_u8_strconv_from_locale (fv->b.cidmaster->filename);
  else if (fv->b.sf->mm != NULL && fv->b.sf->mm->normal->filename != NULL)
    temp = x_u8_strconv_from_locale (fv->b.sf->mm->normal->filename);
  else if (fv->b.sf->filename != NULL)
    temp = x_u8_strconv_from_locale (fv->b.sf->filename);
  else
    {
      SplineFont *sf =
        fv->b.cidmaster ? fv->b.cidmaster : fv->b.sf->mm !=
        NULL ? fv->b.sf->mm->normal : fv->b.sf;
      char *fn = sf->defbasefilename ? sf->defbasefilename : sf->fontname;
      temp = xmalloc ((strlen (fn) + 10));
      strcpy (temp, fn);
      if (sf->defbasefilename != NULL)
        /* Don't add a default suffix; they've already told us what
           name to use. */
        ;
      else if (fv->b.cidmaster != NULL)
        strcat (temp, "CID");
      else if (sf->mm == NULL)
        ;
      else
        strcat (temp, "MM");
      strcat (temp, save_to_dir ? ".sfdir" : ".sfd");
      s2d = save_to_dir;
    }

  memset (&gcd, 0, sizeof (gcd));
  memset (&label, 0, sizeof (label));
  gcd.gd.flags =
    s2d ? (gg_visible | gg_enabled | gg_cb_on) : (gg_visible | gg_enabled);
  label.text = (uint32_t *) _("Save as _Directory");
  label.text_is_1byte = true;
  label.text_has_mnemonic = true;
  gcd.gd.label = &label;
  gcd.gd.handle_controlevent = SaveAs_FormatChange;
  gcd.data = &s2d;
  gcd.creator = GCheckBoxCreate;

  ret = gwwv_save_filename_with_gadget (_("Save as..."), temp, NULL, &gcd);
  free (temp);
  if (ret == NULL)
    return 0;
  filename = utf82def_copy (ret);
  free (ret);
  FVFlattenAllBitmapSelections (fv);
  fv->b.sf->compression = 0;
  ok = SFDWrite (filename, fv->b.sf, fv->b.map, fv->b.normal, s2d);
  if (ok)
    {
      // FIXME: What does this mean?! :)
      SplineFont *sf =
        (fv->b.cidmaster) ? fv->b.cidmaster :
        ((fv->b.sf->mm != NULL) ? fv->b.sf->mm->normal : fv->b.sf);

      free (sf->filename);
      sf->filename = filename;
      sf->save_to_dir = s2d;
      free (sf->origname);
      sf->origname = xstrdup_or_null (filename);
      sf->new = false;
      if (sf->mm != NULL)
        {
          int i;
          for (i = 0; i < sf->mm->instance_count; ++i)
            {
              free (sf->mm->instances[i]->filename);
              sf->mm->instances[i]->filename = filename;
              free (sf->mm->instances[i]->origname);
              sf->mm->instances[i]->origname = xstrdup_or_null (filename);
              sf->mm->instances[i]->new = false;
            }
        }
      SplineFontSetUnChanged (sf);
      FVSetTitles (fv->b.sf);
    }
  else
    free (filename);
  return ok;
}

VISIBLE void
FVMenuSaveAs (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  _FVMenuSaveAs (fv);
}

static int
IsBackupName (char *filename)
{

  if (filename == NULL)
    return false;
  return filename[strlen (filename) - 1] == '~';
}

int
_FVMenuSave (FontView *fv)
{
  int ret = 0;
  SplineFont *sf =
    fv->b.cidmaster ? fv->b.cidmaster : fv->b.sf->mm !=
    NULL ? fv->b.sf->mm->normal : fv->b.sf;

  if (sf->filename == NULL || IsBackupName (sf->filename))
    ret = _FVMenuSaveAs (fv);
  else
    {
      FVFlattenAllBitmapSelections (fv);
      if (!SFDWriteBak (sf, fv->b.map, fv->b.normal))
        ff_post_error (_("Save Failed"), _("Save Failed"));
      else
        {
          SplineFontSetUnChanged (sf);
          ret = true;
        }
    }
  return ret;
}

VISIBLE void
FVMenuSave (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  _FVMenuSave (fv);
}

void
_FVCloseWindows (FontView *fv)
{
  int i, j;
  BDFFont *bdf;
  MetricsView *mv, *mnext;
  SplineFont *sf =
    fv->b.cidmaster ? fv->b.cidmaster : fv->b.sf->mm !=
    NULL ? fv->b.sf->mm->normal : fv->b.sf;

  if (fv->b.nextsame == NULL && fv->b.sf->fv == &fv->b
      && fv->b.sf->kcld != NULL)
    KCLD_End (fv->b.sf->kcld);
  if (fv->b.nextsame == NULL && fv->b.sf->fv == &fv->b
      && fv->b.sf->vkcld != NULL)
    KCLD_End (fv->b.sf->vkcld);

  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      {
        CharView *cv, *next;
        for (cv = (CharView *) (sf->glyphs[i]->views); cv != NULL; cv = next)
          {
            next = (CharView *) (cv->b.next);
            GDrawDestroyWindow (cv->gw);
          }
        if (sf->glyphs[i]->charinfo)
          CharInfoDestroy (sf->glyphs[i]->charinfo);
      }
  if (sf->mm != NULL)
    {
      MMSet *mm = sf->mm;
      for (j = 0; j < mm->instance_count; ++j)
        {
          SplineFont *sf = mm->instances[j];
          for (i = 0; i < sf->glyphcnt; ++i)
            if (sf->glyphs[i] != NULL)
              {
                CharView *cv, *next;
                for (cv = (CharView *) (sf->glyphs[i]->views); cv != NULL;
                     cv = next)
                  {
                    next = (CharView *) (cv->b.next);
                    GDrawDestroyWindow (cv->gw);
                  }
                if (sf->glyphs[i]->charinfo)
                  CharInfoDestroy (sf->glyphs[i]->charinfo);
              }
          for (mv = sf->metrics; mv != NULL; mv = mnext)
            {
              mnext = mv->next;
              GDrawDestroyWindow (mv->gw);
            }
        }
    }
  else if (sf->subfontcnt != 0)
    {
      for (j = 0; j < sf->subfontcnt; ++j)
        {
          for (i = 0; i < sf->subfonts[j]->glyphcnt; ++i)
            if (sf->subfonts[j]->glyphs[i] != NULL)
              {
                CharView *cv, *next;
                for (cv = (CharView *) (sf->subfonts[j]->glyphs[i]->views);
                     cv != NULL; cv = next)
                  {
                    next = (CharView *) (cv->b.next);
                    GDrawDestroyWindow (cv->gw);
                    if (sf->subfonts[j]->glyphs[i]->charinfo)
                      CharInfoDestroy (sf->subfonts[j]->glyphs[i]->charinfo);
                  }
              }
          for (mv = sf->subfonts[j]->metrics; mv != NULL; mv = mnext)
            {
              mnext = mv->next;
              GDrawDestroyWindow (mv->gw);
            }
        }
    }
  else
    {
      for (mv = sf->metrics; mv != NULL; mv = mnext)
        {
          mnext = mv->next;
          GDrawDestroyWindow (mv->gw);
        }
    }
  for (bdf = sf->bitmaps; bdf != NULL; bdf = bdf->next)
    {
      for (i = 0; i < bdf->glyphcnt; ++i)
        if (bdf->glyphs[i] != NULL)
          {
            BitmapView *bv, *next;
            for (bv = bdf->glyphs[i]->views; bv != NULL; bv = next)
              {
                next = bv->next;
                GDrawDestroyWindow (bv->gw);
              }
          }
    }
  if (fv->b.sf->fontinfo != NULL)
    FontInfoDestroy (fv->b.sf);
  if (fv->b.sf->valwin != NULL)
    ValidationDestroy (fv->b.sf);
  SVDetachFV (fv);
}

static int
SFAnyChanged (SplineFont *sf)
{
  if (sf->mm != NULL)
    {
      MMSet *mm = sf->mm;
      int i;
      if (mm->changed)
        return true;
      for (i = 0; i < mm->instance_count; ++i)
        if (sf->mm->instances[i]->changed)
          return true;

      return false;
    }
  else
    return sf->changed;
}

static int
_FVMenuClose (FontView *fv)
{
  int i;
  SplineFont *sf = fv->b.cidmaster ? fv->b.cidmaster : fv->b.sf;

  if (!SFCloseAllInstrs (fv->b.sf))
    return false;

  if (fv->b.nextsame != NULL || fv->b.sf->fv != &fv->b)
    {
      /* There's another view, can close this one with no problems. */
    }
  else if (SFAnyChanged (sf))
    {
      i = AskChanged (fv->b.sf);
      if (i == 2)               /* Cancel */
        return false;
      if (i == 0 && !_FVMenuSave (fv))  /* Save */
        return false;
      else
        SFClearAutoSave (sf);   /* if they didn't save it, remove change
                                   record. */
    }
  _FVCloseWindows (fv);
  if (sf->filename != NULL)
    RecentFilesRemember (sf->filename);
  else if (sf->origname != NULL)
    RecentFilesRemember (sf->origname);
  GDrawDestroyWindow (fv->gw);
  return true;
}

void
MenuNew (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontNew ();
}

VISIBLE void
FVMenuClose (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (fv->b.container)
    (fv->b.container->funcs->doClose) (fv->b.container);
  else
    _FVMenuClose (fv);
}

static void
FV_ReattachCVs (SplineFont *old, SplineFont *new)
{
  int i, j, pos;
  CharView *cv, *cvnext;
  SplineFont *sub;

  for (i = 0; i < old->glyphcnt; ++i)
    {
      if (old->glyphs[i] != NULL && old->glyphs[i]->views != NULL)
        {
          if (new->subfontcnt == 0)
            {
              pos =
                SFFindExistingSlot (new, old->glyphs[i]->unicodeenc,
                                    old->glyphs[i]->name);
              sub = new;
            }
          else
            {
              pos = -1;
              for (j = 0; j < new->subfontcnt && pos == -1; ++j)
                {
                  sub = new->subfonts[j];
                  pos =
                    SFFindExistingSlot (sub, old->glyphs[i]->unicodeenc,
                                        old->glyphs[i]->name);
                }
            }
          if (pos == -1)
            {
              for (cv = (CharView *) (old->glyphs[i]->views); cv != NULL;
                   cv = cvnext)
                {
                  cvnext = (CharView *) (cv->b.next);
                  GDrawDestroyWindow (cv->gw);
                }
            }
          else
            {
              for (cv = (CharView *) (old->glyphs[i]->views); cv != NULL;
                   cv = cvnext)
                {
                  cvnext = (CharView *) (cv->b.next);
                  CVChangeSC (cv, sub->glyphs[pos]);
                  cv->b.layerheads[dm_grid] = &new->grid;
                }
            }
          GDrawProcessPendingEvents (NULL);     /* Don't want to many
                                                   destroy_notify events
                                                   clogging up the
                                                   queue. */
        }
    }
}

VISIBLE void
FVMenuRevert (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontViewBase *fv = (FontViewBase *) GDrawGetUserData (gw);
  FVRevert (fv);
}

VISIBLE void
FVMenuRevertBackup (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontViewBase *fv = (FontViewBase *) GDrawGetUserData (gw);
  FVRevertBackup (fv);
}

VISIBLE void
FVMenuRevertGlyph (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVRevertGlyph ((FontViewBase *) fv);
}

void
MenuPrefs (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
           GEvent *UNUSED (e))
{
  DoPrefs ();
}

void
MenuSaveAll (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
             GEvent *UNUSED (e))
{
  FontView *fv;

  for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
    {
      if (SFAnyChanged (fv->b.sf) && !_FVMenuSave (fv))
        return;
    }
}

static void
_MenuExit (void *UNUSED (junk))
{
  FontView *fv, *next;

  for (fv = fv_list; fv != NULL; fv = next)
    {
      next = (FontView *) (fv->b.next);
      if (!_FVMenuClose (fv))
        {
          return;
        }
      if (fv->b.nextsame != NULL || fv->b.sf->fv != &fv->b)
        {
          GDrawSync (NULL);
          GDrawProcessPendingEvents (NULL);
        }
    }

  scm_c_exit_editor_main_loop (0);
}

VISIBLE void
FVMenuExit (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
            GEvent *UNUSED (e))
{
  _MenuExit (NULL);
}

void
MenuExit (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi), GEvent *e)
{
  if (e == NULL)                /* Not from the menu directly, but a
                                   shortcut. */
    _MenuExit (NULL);
  else
    DelayEvent (_MenuExit, NULL);
}

char *
GetFontNameDialog (char *dir, int mult)
{
  uint32_t *ret;
  char *u_dir;
  char *temp;

  u_dir = NULL_PASSTHRU (dir, x_u8_strconv_from_locale (dir));
  ret = FVOpenFont (_("Open Font"), u_dir, mult);
  temp = u2def_copy (ret);
  free (ret);
  free (u_dir);
  return temp;
}

void
MergeKernInfo (SplineFont *sf, EncMap *map)
{
  static char wild[] = "*.{afm,tfm,ofm,pfm,bin,hqx,dfont,feature,feat,fea}";
  static char wild2[] =
    "*.{afm,amfm,tfm,ofm,pfm,bin,hqx,dfont,feature,feat,fea}";
  char *ret = gwwv_open_filename (_("Merge Feature Info"), NULL,
                                  sf->mm != NULL ? wild2 : wild, NULL);
  char *temp;

  if (ret == NULL)
    return;                     /* Cancelled */
  temp = utf82def_copy (ret);

  if (!LoadKerningDataFromMetricsFile (sf, temp, map))
    ff_post_error (_("Load of Kerning Metrics Failed"),
                   _("Failed to load kern data from %s"), temp);
  free (ret);
  free (temp);
}

VISIBLE void
FVMenuMergeKern (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  MergeKernInfo (fv->b.sf, fv->b.map);
}

void
MenuOpen (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
          GEvent *UNUSED (e))
{
  char *temp;
  char *eod, *fpt, *file, *full;
  FontView *test;
  int fvcnt, fvtest;

  for (fvcnt = 0, test = fv_list; test != NULL;
       ++fvcnt, test = (FontView *) (test->b.next));
  do
    {
      temp = GetFontNameDialog (NULL, true);
      if (temp == NULL)
        return;
      eod = strrchr (temp, '/');
      *eod = '\0';
      file = eod + 1;
      do
        {
          fpt = strstr (file, "; ");
          if (fpt != NULL)
            *fpt = '\0';
          full = xmalloc (strlen (temp) + 1 + strlen (file) + 1);
          strcpy (full, temp);
          strcat (full, "/");
          strcat (full, file);
          ViewFont (full, 0);
          file = fpt + 2;
          free (full);
        }
      while (fpt != NULL);
      free (temp);
      for (fvtest = 0, test = fv_list; test != NULL;
           ++fvtest, test = (FontView *) (test->b.next));
    }
  while (fvtest == fvcnt);      /* did the load fail for some reason? try again */
}

VISIBLE void
FVMenuContextualHelp (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  help ("fontview.html");
}

void
MenuHelp (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
          GEvent *UNUSED (e))
{
  help ("overview.html");
}

void
MenuIndex (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
           GEvent *UNUSED (e))
{
  help ("IndexFS.html");
}

void
MenuLicense (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
             GEvent *UNUSED (e))
{
  help ("license.html");
}

static void
AboutScreenExpose (GWindow gw, GRect rect)
{
  char *start, *pt, *lastspace;
  int y, x;
  int ds, ld;
  GRect old;
  int width = 379, height = 0;
  GFont *font, *italic;
  int as, fh, linecnt;
  char *lines[20], *is, *ie;
  GImage *image = NULL;

  char copyright[40] = "Copyright © 2000-2012 George Williams";

  char msg[470] =
    "When my father finished his book on Renaissance printing"
    " (The Craft of Printing and the Publication of Shakespeare's Works)"
    " he told me that I would have to write the chapter on"
    " computer typography. This is my attempt to do so.";

  font = GDrawNewFont (NULL, "serif", 12, 400, fs_none);
  italic = GDrawNewFont (NULL, "serif", 12, 400, fs_italic);

  image = GImageRead (SHAREDIR "/pixmaps/about.png");
  if (image != NULL)
    {
      width = image->u.image->width;
      height = image->u.image->height;
    }

  GDrawSetFont (gw, font);
  GDrawGetFontMetrics (gw, font, &as, &ds, &ld);
  fh = as + ds + ld;

  linecnt = 0;
  lines[linecnt++] = msg - 1;
  for (start = msg; *start != '\0'; start = pt)
    {
      lastspace = NULL;
      for (pt = start;; ++pt)
        {
          if (*pt == ' ' || *pt == '\0')
            {
              if (GDrawGetText8Width (gw, start, pt - start) < width - 10)
                lastspace = pt;
              else
                break;
              if (*pt == '\0')
                break;
            }
        }
      if (lastspace != NULL)
        pt = lastspace;
      lines[linecnt++] = pt;
      if (*pt)
        ++pt;
    }

  strcpy (pt, (" FontForge used to be named PfaEdit."));
  pt += strlen (pt);
  lines[linecnt++] = pt;
  strcat (pt, " ");
  strcat (pt, PACKAGE_STRING);
  pt += strlen (pt);
  lines[linecnt] = pt;
  linecnt++;
  lines[linecnt] = NULL;
  is = strchr (msg, '(');
  ie = strchr (msg, ')');

  GDrawResize (gw, width, height + linecnt * fh);
  GDrawPushClip (gw, &rect, &old);

  if (image != NULL)
    GDrawDrawImage (gw, image, NULL, 0, 0);

  x = (width - GDrawGetText8Width (gw, copyright, -1)) / 2;
  GDrawDrawText8 (gw, x, height - ds, copyright, -1, 0x000000);

  y = height + as + fh / 2;
  for (int i = 1; i < linecnt; ++i)
    {
      if (is >= lines[i - 1] + 1 && is < lines[i])
        {
          x =
            8 + GDrawDrawText8 (gw, 8, y, lines[i - 1] + 1,
                                is - lines[i - 1] - 1, 0x000000);
          GDrawSetFont (gw, italic);
          GDrawDrawText8 (gw, x, y, is, lines[i] - is, 0x000000);
        }
      else if (ie >= lines[i - 1] + 1 && ie < lines[i])
        {
          x =
            8 + GDrawDrawText8 (gw, 8, y, lines[i - 1] + 1,
                                ie - lines[i - 1] - 1, 0x000000);
          GDrawSetFont (gw, font);
          GDrawDrawText8 (gw, x, y, ie, lines[i] - ie, 0x000000);
        }
      else
        GDrawDrawText8 (gw, 8, y, lines[i - 1] + 1,
                        lines[i] - lines[i - 1] - 1, 0x000000);
      y += fh;
    }
  GDrawPopClip (gw, &old);
}

static int
about_e_h (GWindow gw, GEvent *event)
{
  static char *foolishness[] = {
    /* TRANSLATORS: These strings are for fun. If they are offensive or incomprehensible */
    /* simply translate them as something dull like: "FontForge" */
    /* This is a spoof of political slogans, designed to point out how foolish they are */
    N_("A free press discriminates\nagainst the illiterate."),
    N_("A free press discriminates\nagainst the illiterate."),
    /* TRANSLATORS: This is a pun on the old latin drinking song "Gaudeamus igature!" */
    N_("Gaudeamus Ligature!"),
    N_("Gaudeamus Ligature!"),
    /* TRANSLATORS: Spoof on the bible */
    N_("In the beginning was the letter..."),
    /* TRANSLATORS: Some wit at MIT came up with this ("ontology recapitulates phylogony" is the original) */
    N_("fontology recapitulates file-ogeny")
  };

  switch (event->type)
    {
    case et_expose:
      AboutScreenExpose (gw, event->u.expose.rect);
      break;
    case et_char:
    case et_mousedown:
    case et_close:
      GGadgetEndPopup ();
      GDrawDestroyWindow (gw);
      GDrawSync (NULL);
      GDrawProcessPendingEvents (NULL);
      break;
    case et_mousemove:
      GGadgetPreparePopup8 (gw,
                            _(foolishness
                              [rand () %
                               (sizeof (foolishness) /
                                sizeof (foolishness[0]))]));
      break;
    }
  return true;
}

static void
ShowAboutScreen (void)
{
  GWindow gw;
  GRect pos;
  GWindowAttrs wattrs;

  wattrs.mask =
    wam_events | wam_cursor | wam_bordwidth | wam_backcol | wam_positioned |
    wam_utf8_wtitle | wam_isdlg;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.positioned = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = "Sorts Mill Editor";
  wattrs.border_width = 2;
  wattrs.background_color = 0xffffff;
  wattrs.is_dlg = true;
  pos.x = 200;
  pos.y = 0;
  pos.width = pos.height = 1;

  gw = GDrawCreateTopWindow (NULL, &pos, about_e_h, NULL, &wattrs);
  GDrawSetVisible (gw, true);
}

void
MenuAbout (GWindow UNUSED (base), struct gmenuitem *UNUSED (mi),
           GEvent *UNUSED (e))
{
  ShowAboutScreen ();
}

VISIBLE void
FVMenuImport (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int empty = fv->b.sf->onlybitmaps && fv->b.sf->bitmaps == NULL;
  BDFFont *bdf;
  FVImport (fv);
  if (empty && fv->b.sf->bitmaps != NULL)
    {
      for (bdf = fv->b.sf->bitmaps; bdf->next != NULL; bdf = bdf->next);
      FVChangeDisplayBitmap ((FontViewBase *) fv, bdf);
    }
}

static int
FVSelCount (FontView *fv)
{
  int i, cnt = 0;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    if (fv->b.selected[i])
      ++cnt;
  if (cnt > 10)
    {
      char *buts[3];
      buts[0] = _("_OK");
      buts[1] = _("_Cancel");
      buts[2] = NULL;
      if (gwwv_ask
          (_("Many Windows"), (const char **) buts, 0, 1,
           _
           ("This involves opening more than 10 windows.\nIs that really what you want?"))
          == 1)
        return false;
    }
  return true;
}

VISIBLE void
FVMenuOpenOutline (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i;
  SplineChar *sc;

  if (!FVSelCount (fv))
    return;
  if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
    return;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    if (fv->b.selected[i])
      {
        sc = FVMakeChar (fv, i);
        CharViewCreate (sc, fv, i);
      }
}

VISIBLE void
FVMenuOpenBitmap (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i;
  SplineChar *sc;

  if (fv->b.cidmaster ==
      NULL ? (fv->b.sf->bitmaps == NULL) : (fv->b.cidmaster->bitmaps == NULL))
    return;
  if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
    return;
  if (!FVSelCount (fv))
    return;
  for (i = 0; i < fv->b.map->enc_limit; ++i)
    if (fv->b.selected[i])
      {
        sc = FVMakeChar (fv, i);
        if (sc != NULL)
          BitmapViewCreatePick (i, fv);
      }
}

void
_MenuWarnings (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
               GEvent *UNUSED (e))
{
  ShowErrorWindow ();
}

VISIBLE void
FVMenuOpenMetrics (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
    return;
  MetricsViewCreate (fv, NULL, fv->filled == fv->show ? NULL : fv->show);
}

#if !defined(_NO_PYTHON)
VISIBLE void
FVMenuExecute (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  ScriptDlg (fv, NULL);
}
#endif

VISIBLE void
FVMenuFontInfo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
    return;
  FontMenuFontInfo (fv);
}

VISIBLE void
FVMenuMATHInfo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFMathDlg (fv->b.sf, fv->b.active_layer);
}

VISIBLE void
FVMenuFindProblems (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FindProblems (fv, NULL, NULL);
}

VISIBLE void
FVMenuValidate (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFValidationWindow (fv->b.sf, fv->b.active_layer, ff_none);
}

VISIBLE void
FVMenuSetExtremumBound (GWindow gw, struct gmenuitem *UNUSED (mi),
                        GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char buffer[40], *end, *ret;
  int val;

  sprintf (buffer, "%d",
           fv->b.sf->extrema_bound <=
           0 ? (int) rint ((fv->b.sf->ascent + fv->b.sf->descent) /
                           100.0) : fv->b.sf->extrema_bound);
  ret =
    gwwv_ask_string (_("Extremum bound..."), buffer,
                     _
                     ("Adobe says that \"big\" splines should not have extrema.\nBut they don't define what big means.\nIf the distance between the spline's end-points is bigger than this value, then the spline is \"big\" to fontforge."));
  if (ret == NULL)
    return;
  val = (int) rint (strtod (ret, &end));
  if (*end != '\0')
    ff_post_error (_("Bad Number"), _("Bad Number"));
  else
    {
      fv->b.sf->extrema_bound = val;
      if (!fv->b.sf->changed)
        {
          fv->b.sf->changed = true;
          FVSetTitles (fv->b.sf);
        }
    }
  free (ret);
}

VISIBLE void
FVMenuEmbolden (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  EmboldenDlg (fv, NULL);
}

VISIBLE void
FVMenuItalic (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  ItalicDlg (fv, NULL);
}

VISIBLE void
FVMenuSmallCaps (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  GlyphChangeDlg (fv, NULL, gc_smallcaps);
}

VISIBLE void
FVMenuChangeXHeight (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  ChangeXHeightDlg (fv, NULL);
}

VISIBLE void
FVMenuChangeGlyph (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  GlyphChangeDlg (fv, NULL, gc_generic);
}

VISIBLE void
FVMenuSubSup (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  GlyphChangeDlg (fv, NULL, gc_subsuper);
  /*AddSubSupDlg(fv); */
}

VISIBLE void
FVMenuOblique (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  ObliqueDlg (fv, NULL);
}

VISIBLE void
FVMenuCondense (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  CondenseExtendDlg (fv, NULL);
}

#define MID_24	2001
#define MID_36	2002
#define MID_48	2004
#define MID_72	2014
#define MID_96	2015
#define MID_AntiAlias	2005
#define MID_Next	2006
#define MID_Prev	2007
#define MID_NextDef	2012
#define MID_PrevDef	2013
#define MID_ShowHMetrics 2016
#define MID_ShowVMetrics 2017
#define MID_Ligatures	2020
#define MID_KernPairs	2021
#define MID_AnchorPairs	2022
#define MID_FitToBbox	2023
#define MID_DisplaySubs	2024
#define MID_32x8	2025
#define MID_16x4	2026
#define MID_8x2		2027
#define MID_BitmapMag	2028
#define MID_Layers	2029
#define MID_FontInfo	2200
#define MID_CharInfo	2201
#define MID_Transform	2202
#define MID_Stroke	2203
#define MID_RmOverlap	2204
#define MID_Simplify	2205
#define MID_Correct	2206
#define MID_BuildAccent	2208
#define MID_AvailBitmaps	2210
#define MID_RegenBitmaps	2211
#define MID_Autotrace	2212
#define MID_Round	2213
#define MID_MergeFonts	2214
#define MID_InterpolateFonts	2215
#define MID_FindProblems 2216
#define MID_Embolden	2217
#define MID_Condense	2218
#define MID_ShowDependentRefs	2222
#define MID_AddExtrema	2224
#define MID_CleanupGlyph	2225
#define MID_TilePath	2226
#define MID_BuildComposite	2227
#define MID_NLTransform	2228
#define MID_Intersection	2229
#define MID_FindInter	2230
#define MID_Styles	2231
#define MID_SimplifyMore	2233
#define MID_ShowDependentSubs	2234
#define MID_DefaultATT	2235
#define MID_POV		2236
#define MID_BuildDuplicates	2237
#define MID_StrikeInfo	2238
#define MID_FontCompare	2239
#define MID_CanonicalStart	2242
#define MID_CanonicalContours	2243
#define MID_RemoveBitmaps	2244
#define MID_Validate		2245
#define MID_MassRename		2246
#define MID_Italic		2247
#define MID_SmallCaps		2248
#define MID_SubSup		2249
#define MID_ChangeXHeight	2250
#define MID_ChangeGlyph	2251
#define MID_SetColor	2252
#define MID_SetExtremumBound	2253
#define MID_Center	2600
#define MID_Thirds	2601
#define MID_SetWidth	2602
#define MID_SetLBearing	2603
#define MID_SetRBearing	2604
#define MID_SetVWidth	2605
#define MID_RmHKern	2606
#define MID_RmVKern	2607
#define MID_VKernByClass	2608
#define MID_VKernFromH	2609
#define MID_SetBearings	2610
#define MID_AutoHint	2501
#define MID_ClearHints	2502
#define MID_ClearWidthMD	2503
#define MID_AutoInstr	2504
#define MID_EditInstructions	2505
#define MID_Editfpgm	2506
#define MID_Editprep	2507
#define MID_ClearInstrs	2508
#define MID_HStemHist	2509
#define MID_VStemHist	2510
#define MID_BlueValuesHist	2511
#define MID_Editcvt	2512
#define MID_HintSubsPt	2513
#define MID_AutoCounter	2514
#define MID_DontAutoHint	2515
#define MID_RmInstrTables	2516
#define MID_Editmaxp	2517
#define MID_Deltas	2518
#define MID_OpenBitmap	2700
#define MID_OpenOutline	2701
#define MID_Revert	2702
#define MID_Recent	2703
#define MID_ScriptMenu	2705
#define MID_RevertGlyph	2707
#define MID_RevertToBackup 2708
#define MID_GenerateTTC 2709
#define MID_OpenMetrics	2710
#define MID_ExecuteScript 2714
#define MID_Cut		2101
#define MID_Copy	2102
#define MID_Paste	2103
#define MID_Clear	2104
#define MID_SelAll	2106
#define MID_CopyRef	2107
#define MID_UnlinkRef	2108
#define MID_Undo	2109
#define MID_Redo	2110
#define MID_CopyWidth	2111
#define MID_AllFonts		2122
#define MID_DisplayedFont	2123
#define	MID_CharName		2124
#define MID_RemoveUndoes	2114
#define MID_CopyFgToBg	2115
#define MID_ClearBackground	2116
#define MID_CopyLBearing	2125
#define MID_CopyRBearing	2126
#define MID_CopyVWidth	2127
#define MID_Join	2128
#define MID_PasteInto	2129
#define MID_SameGlyphAs	2130
#define MID_RplRef	2131
#define MID_PasteAfter	2132
#define	MID_TTFInstr	2134
#define	MID_CopyLookupData	2135
#define MID_CopyL2L	2136
#define MID_CorrectRefs	2137
#define MID_Convert2CID	2800
#define MID_Flatten	2801
#define MID_InsertFont	2802
#define MID_InsertBlank	2803
#define MID_CIDFontInfo	2804
#define MID_RemoveFromCID 2805
#define MID_ConvertByCMap	2806
#define MID_FlattenByCMap	2807
#define MID_ChangeSupplement	2808
#define MID_Reencode		2830
#define MID_ForceReencode	2831
#define MID_AddUnencoded	2832
#define MID_RemoveUnused	2833
#define MID_DetachGlyphs	2834
#define MID_DetachAndRemoveGlyphs	2835
#define MID_LoadEncoding	2836
#define MID_MakeFromFont	2837
#define MID_RemoveEncoding	2838
#define MID_DisplayByGroups	2839
#define MID_Compact	2840
#define MID_SaveNamelist	2841
#define MID_RenameGlyphs	2842
#define MID_NameGlyphs		2843
#define MID_CreateMM	2900
#define MID_MMInfo	2901
#define MID_MMValid	2902
#define MID_ChangeMMBlend	2903
#define MID_BlendToNew	2904
#define MID_ModifyComposition	20902
#define MID_BuildSyllables	20903

#define MID_Warnings	3000

/*
 * Returns:
 *   -1     if nothing selected
 *   index  if exactly one char
 *   -2     if more than one char
 */
static int
fv_any_char_selected (FontViewBase *fvb)
{
  int val = -1;

  // Find a selected char.
  int i = 0;
  while (val == -1 && i < fvb->map->enc_limit)
    {
      if (fvb->selected[i])
        val = i;
      i++;
    }

  // See whether any other char also is selected.
  while (0 <= val && i < fvb->map->enc_limit)
    {
      if (fvb->selected[i])
        val = -2;
      i++;
    }

  return val;
}

static bool
fv_all_selected (FontViewBase *fvb)
{
  /* Is everything 'worth outputting' selected? */

  bool something_is_worth_outputting = false;
  bool one_is_not_selected = false;
  int i = 0;
  while (!one_is_not_selected && i < fvb->sf->glyphcnt)
    {
      if (SCWorthOutputting (fvb->sf->glyphs[i]))
        {
          something_is_worth_outputting = true;
          if (!fvb->selected[gid_to_enc (fvb->map, i)])
            one_is_not_selected = true;
        }
      i++;
    }
  return something_is_worth_outputting && !one_is_not_selected;
}

/* Returns -1 if nothing selected, if exactly one char return it, -2
   if more than one. */
static int
FVAnyCharSelected (FontView *fv)
{
  return fv_any_char_selected (&fv->b);
}

static bool
FVAllSelected (FontView *fv)
{
  return fv_all_selected (&fv->b);
}

VISIBLE void
FVMenuCopyFrom (GWindow UNUSED (gw), struct gmenuitem *mi, GEvent *UNUSED (e))
{
  /*FontView *fv = (FontView *) GDrawGetUserData(gw); */

  if (mi->mid == MID_CharName)
    copymetadata = !copymetadata;
  else if (mi->mid == MID_TTFInstr)
    copyttfinstr = !copyttfinstr;
  else
    onlycopydisplayed = (mi->mid == MID_DisplayedFont);
  SavePrefs (true);
}

VISIBLE void
FVMenuCopy (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  FVCopy ((FontViewBase *) fv, ct_fullcopy);
}

VISIBLE void
FVMenuCopyLookupData (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  FVCopy ((FontViewBase *) fv, ct_lookups);
}

VISIBLE void
FVMenuCopyRef (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  FVCopy ((FontViewBase *) fv, ct_reference);
}

VISIBLE void
FVMenuCopyWidth (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (FVAnyCharSelected (fv) == -1)
    return;
  if (mi->mid == MID_CopyVWidth && !fv->b.sf->hasvmetrics)
    return;
  FVCopyWidth ((FontViewBase *) fv,
               mi->mid == MID_CopyWidth ? ut_width : mi->mid ==
               MID_CopyVWidth ? ut_vwidth : mi->mid ==
               MID_CopyLBearing ? ut_lbearing : ut_rbearing);
}

VISIBLE void
FVMenuPaste (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  PasteIntoFV ((FontViewBase *) fv, false, NULL);
}

VISIBLE void
FVMenuPasteInto (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  PasteIntoFV ((FontViewBase *) fv, true, NULL);
}

VISIBLE void
FVMenuPasteAfter (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int pos = FVAnyCharSelected (fv);
  if (pos < 0)
    return;
  PasteIntoFV (&fv->b, 2, NULL);
}

VISIBLE void
FVMenuSameGlyphAs (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVSameGlyphAs ((FontViewBase *) fv);
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuCopyFgBg (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVCopyFgtoBg ((FontViewBase *) fv);
}

VISIBLE void
FVMenuCopyL2L (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVCopyLayerToLayer (fv);
}

VISIBLE void
FVMenuCompareL2L (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVCompareLayerToLayer (fv);
}

VISIBLE void
FVMenuClear (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVClear ((FontViewBase *) fv);
}

VISIBLE void
FVMenuClearBackground (GWindow gw, struct gmenuitem *UNUSED (mi),
                       GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVClearBackground ((FontViewBase *) fv);
}

VISIBLE void
FVMenuJoin (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVJoin ((FontViewBase *) fv);
}

VISIBLE void
FVMenuUnlinkRef (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVUnlinkRef ((FontViewBase *) fv);
}

VISIBLE void
FVMenuRemoveUndoes (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFRemoveUndoes (fv->b.sf, fv->b.selected, fv->b.map);
}

VISIBLE void
FVMenuUndo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVUndo ((FontViewBase *) fv);
}

VISIBLE void
FVMenuRedo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVRedo ((FontViewBase *) fv);
}

VISIBLE void
FVMenuCut (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVCopy (&fv->b, ct_fullcopy);
  FVClear (&fv->b);
}

VISIBLE void
FVMenuSelectAll (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVSelectAll (fv);
}

VISIBLE void
FVMenuInvertSelection (GWindow gw, struct gmenuitem *UNUSED (mi),
                       GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVInvertSelection (fv);
}

VISIBLE void
FVMenuDeselectAll (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVDeselectAll (fv);
}

enum merge_type
{ mt_set = 0, mt_merge = 4, mt_or = mt_merge, mt_restrict = 8, mt_and = 12 };
    /* Index array by merge_type(*4) + selection*2 + doit */
const uint8_t mergefunc[] = {
/* mt_set */
  0, 1,
  0, 1,
/* mt_merge */
  0, 1,
  1, 1,
/* mt_restrict */
  0, 0,
  1, 0,
/* mt_and */
  0, 0,
  0, 1,
#if 0
/* mt_xor */
  0, 1,
  1, 0
#endif
};

static enum merge_type
SelMergeType (GEvent *e)
{
  if (e->type != et_mouseup)
    return mt_set;

  return (((e->u.mouse.state & ksm_shift) ? mt_merge : 0) | ((e->u.mouse.state &
                                                              ksm_control) ?
                                                             mt_restrict : 0));
}

static char *
SubMatch (char *pattern, char *eop, char *name, int ignorecase)
{
  char ch, *ppt, *npt, *ept, *eon;

  while (pattern < eop && (ch = *pattern) != '\0')
    {
      if (ch == '*')
        {
          if (pattern[1] == '\0')
            return name + strlen (name);
          for (npt = name;; ++npt)
            {
              if ((eon = SubMatch (pattern + 1, eop, npt, ignorecase)) != NULL)
                return eon;
              if (*npt == '\0')
                return NULL;
            }
        }
      else if (ch == '?')
        {
          if (*name == '\0')
            return NULL;
          ++name;
        }
      else if (ch == '[')
        {
          /* [<char>...] matches the chars
           * [<char>-<char>...] matches any char within the range (inclusive)
           * the above may be concattenated and the resultant pattern matches
           *          anything thing which matches any of them.
           * [^<char>...] matches any char which does not match the rest of
           *          the pattern
           * []...] as a special case a ']' immediately after the '[' matches
           *          itself and does not end the pattern
           */
          int found = 0, not = 0;
          ++pattern;
          if (pattern[0] == '^')
            {
              not = 1;
              ++pattern;
            }
          for (ppt = pattern; (ppt != pattern || *ppt != ']') && *ppt != '\0';
               ++ppt)
            {
              ch = *ppt;
              if (ppt[1] == '-' && ppt[2] != ']' && ppt[2] != '\0')
                {
                  char ch2 = ppt[2];
                  if ((*name >= ch && *name <= ch2)
                      || (ignorecase && islower (ch) && islower (ch2)
                          && *name >= toupper (ch) && *name <= toupper (ch2))
                      || (ignorecase && isupper (ch) && isupper (ch2)
                          && *name >= tolower (ch) && *name <= tolower (ch2)))
                    {
                      if (!not)
                        {
                          found = 1;
                          break;
                        }
                    }
                  else
                    {
                      if (not)
                        {
                          found = 1;
                          break;
                        }
                    }
                  ppt += 2;
                }
              else if (ch == *name
                       || (ignorecase && tolower (ch) == tolower (*name)))
                {
                  if (!not)
                    {
                      found = 1;
                      break;
                    }
                }
              else
                {
                  if (not)
                    {
                      found = 1;
                      break;
                    }
                }
            }
          if (!found)
            return NULL;
          while (*ppt != ']' && *ppt != '\0')
            ++ppt;
          pattern = ppt;
          ++name;
        }
      else if (ch == '{')
        {
          /* Matches any of a comma separated list of substrings. */
          for (ppt = pattern + 1; *ppt != '\0'; ppt = ept)
            {
              for (ept = ppt; *ept != '}' && *ept != ',' && *ept != '\0';
                   ++ept);
              npt = SubMatch (ppt, ept, name, ignorecase);
              if (npt != NULL)
                {
                  char *ecurly = ept;
                  while (*ecurly != '}' && ecurly < eop && *ecurly != '\0')
                    ++ecurly;
                  if ((eon =
                       SubMatch (ecurly + 1, eop, npt, ignorecase)) != NULL)
                    return eon;
                }
              if (*ept == '}')
                return NULL;
              if (*ept == ',')
                ++ept;
            }
        }
      else if (ch == *name)
        {
          ++name;
        }
      else if (ignorecase && tolower (ch) == tolower (*name))
        {
          ++name;
        }
      else
        return NULL;
      ++pattern;
    }
  return name;
}

/* Handles *?{}[] wildcards */
static int
WildMatch (char *pattern, char *name, int ignorecase)
{
  char *eop = pattern + strlen (pattern);

  if (pattern == NULL)
    return true;

  name = SubMatch (pattern, eop, name, ignorecase);
  if (name == NULL)
    return false;
  if (*name == '\0')
    return true;

  return false;
}

static int
SS_ScriptChanged (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype != et_textfocuschanged)
    {
      char *txt = GGadgetGetTitle8 (g);
      char buf[8];
      int i;
      extern GTextInfo scripts[];

      for (i = 0; scripts[i].text != NULL; ++i)
        {
          if (strcmp ((char *) scripts[i].text, txt) == 0)
            break;
        }
      free (txt);
      if (scripts[i].text == NULL)
        return true;
      buf[0] = ((intptr_t) scripts[i].userdata) >> 24;
      buf[1] = ((intptr_t) scripts[i].userdata) >> 16;
      buf[2] = ((intptr_t) scripts[i].userdata) >> 8;
      buf[3] = ((intptr_t) scripts[i].userdata);
      buf[4] = 0;
      GGadgetSetTitle8 (g, buf);
    }
  return true;
}

static int
SS_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      int *done = GDrawGetUserData (GGadgetGetWindow (g));
      *done = 2;
    }
  return true;
}

static int
SS_Cancel (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      int *done = GDrawGetUserData (GGadgetGetWindow (g));
      *done = true;
    }
  return true;
}

static int
ss_e_h (GWindow gw, GEvent *event)
{
  int *done = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_char:
      return false;
    case et_close:
      *done = true;
      break;
    }
  return true;
}

static void
FVSelectByScript (FontView *fv, int merge)
{
  int j, gid;
  SplineChar *sc;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  extern GTextInfo scripts[];
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[10], *hvarray[21][2], *barray[8], boxes[3];
  GTextInfo label[10];
  int i, k;
  int done = 0, doit;
  char tagbuf[4];
  uint32_t tag;
  const uint32_t *ret;
  int lc_k, uc_k, select_k;
  int only_uc = 0, only_lc = 0;

  LookupUIInit ();

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&label, 0, sizeof (label));
  memset (&boxes, 0, sizeof (boxes));

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict | wam_isdlg;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = false;
  wattrs.is_dlg = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Select by Script");
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  gw = GDrawCreateTopWindow (NULL, &pos, ss_e_h, &done, &wattrs);

  k = i = 0;

  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k].gd.u.list = scripts;
  gcd[k].gd.handle_controlevent = SS_ScriptChanged;
  gcd[k++].creator = GListFieldCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("All glyphs");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _("Set the selection of the font view to all glyphs in the script.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;
  hvarray[i][0] = GCD_HPad10;
  hvarray[i++][1] = NULL;

  uc_k = k;
  label[k].text = (uint32_t *) _("Only upper case");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("Set the selection of the font view to any upper case glyphs in the script.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  lc_k = k;
  label[k].text = (uint32_t *) _("Only lower case");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("Set the selection of the font view to any lower case glyphs in the script.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;
  hvarray[i][0] = GCD_HPad10;
  hvarray[i++][1] = NULL;

  select_k = k;
  label[k].text = (uint32_t *) _("Select Results");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_rad_startnew;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _("Set the selection of the font view to the glyphs\nwhich match");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Merge Results");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("Expand the selection of the font view to include\nall the glyphs which match");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Restrict Selection");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Remove matching glyphs from the selection.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Logical And with Selection");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Remove glyphs which do not match from the selection.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;
  gcd[k - 4 + merge / 4].gd.flags |= gg_cb_on;

  hvarray[i][0] = GCD_Glue;
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  gcd[k].gd.handle_controlevent = SS_OK;
  gcd[k++].creator = GButtonCreate;

  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  gcd[k].gd.handle_controlevent = SS_Cancel;
  gcd[k++].creator = GButtonCreate;

  barray[0] = barray[2] = barray[3] = barray[4] = barray[6] = GCD_Glue;
  barray[7] = NULL;
  barray[1] = &gcd[k - 2];
  barray[5] = &gcd[k - 1];
  hvarray[i][0] = &boxes[2];
  hvarray[i++][1] = NULL;
  hvarray[i][0] = NULL;

  memset (boxes, 0, sizeof (boxes));
  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = hvarray[0];
  boxes[0].creator = GHVGroupCreate;

  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = barray;
  boxes[2].creator = GHBoxCreate;

  GGadgetsCreate (gw, boxes);
  GHVBoxSetExpandableCol (boxes[2].ret, gb_expandgluesame);
  GHVBoxSetExpandableRow (boxes[0].ret, gb_expandglue);


  GHVBoxFitWindow (boxes[0].ret);

  GDrawSetVisible (gw, true);
  ret = NULL;
  while (!done)
    {
      GDrawProcessOneEvent (NULL);
      if (done == 2)
        {
          ret = _GGadgetGetTitle (gcd[0].ret);
          if (*ret == '\0')
            {
              ff_post_error (_("No Script"), _("Please specify a script"));
              done = 0;
            }
          else if (u32_strlen (ret) > 4)
            {
              ff_post_error (_("Bad Script"), _("Scripts are 4 letter tags"));
              done = 0;
            }
        }
    }
  memset (tagbuf, ' ', 4);
  if (done == 2 && ret != NULL)
    {
      tagbuf[0] = *ret;
      if (ret[1] != '\0')
        {
          tagbuf[1] = ret[1];
          if (ret[2] != '\0')
            {
              tagbuf[2] = ret[2];
              if (ret[3] != '\0')
                tagbuf[3] = ret[3];
            }
        }
    }
  merge =
    GGadgetIsChecked (gcd[select_k + 0].ret) ? mt_set :
    GGadgetIsChecked (gcd[select_k + 1].ret) ? mt_merge :
    GGadgetIsChecked (gcd[select_k + 2].ret) ? mt_restrict : mt_and;
  only_uc = GGadgetIsChecked (gcd[uc_k + 0].ret);
  only_lc = GGadgetIsChecked (gcd[lc_k + 0].ret);

  GDrawDestroyWindow (gw);
  if (done == 1)
    return;
  tag = (tagbuf[0] << 24) | (tagbuf[1] << 16) | (tagbuf[2] << 8) | tagbuf[3];

  for (j = 0; j < map->enc_limit; ++j)
    if ((gid = enc_to_gid (map, j)) != -1 && (sc = sf->glyphs[gid]) != NULL)
      {
        doit = (SCScriptFromUnicode (sc) == tag);
        if (doit)
          {
            if (only_uc
                && (sc->unicodeenc == -1 || sc->unicodeenc > 0xffff
                    || !isupper (sc->unicodeenc)))
              doit = false;
            else if (only_lc
                     && (sc->unicodeenc == -1 || sc->unicodeenc > 0xffff
                         || !islower (sc->unicodeenc)))
              doit = false;
          }
        fv->b.selected[j] =
          mergefunc[merge + (fv->b.selected[j] ? 2 : 0) + doit];
      }
    else if (merge == mt_set)
      fv->b.selected[j] = false;

  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectByScript (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVSelectByScript (fv, SelMergeType (e));
}

static void
FVSelectColor (FontView *fv, uint32_t col, int merge)
{
  int i, doit;
  uint32_t sccol;
  SplineChar **glyphs = fv->b.sf->glyphs;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    {
      int gid = enc_to_gid (fv->b.map, i);
      sccol = (gid == -1
               || glyphs[gid] == NULL) ? COLOR_DEFAULT : glyphs[gid]->color;
      doit = sccol == col;
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectColor (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  Color col = (Color) (intptr_t) (mi->ti.userdata);
  if ((intptr_t) mi->ti.userdata == (intptr_t) -10)
    {
      struct hslrgb retcol, font_cols[6];
      retcol =
        GWidgetColor (_("Pick a color"), NULL,
                      SFFontCols (fv->b.sf, font_cols));
      if (!retcol.rgb)
        return;
      col =
        (((int) rint (255. * retcol.r)) << 16) |
        (((int) rint (255. * retcol.g)) << 8) |
        (((int) rint (255. * retcol.b)));
    }
  FVSelectColor (fv, col, SelMergeType (e));
}

static int
FVSelectByName (FontView *fv, char *ret, int merge)
{
  int j, gid, doit;
  char *end;
  SplineChar *sc;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  struct altuni *alt;

  if (!merge)
    FVDeselectAll (fv);
  if ((*ret == '0' && (ret[1] == 'x' || ret[1] == 'X'))
      || ((*ret == 'u' || *ret == 'U') && ret[1] == '+'))
    {
      int uni = (int) strtol (ret + 2, &end, 16);
      int vs = -2;
      if (*end == '.')
        {
          ++end;
          if ((*end == '0' && (end[1] == 'x' || end[1] == 'X'))
              || ((*end == 'u' || *end == 'U') && end[1] == '+'))
            end += 2;
          vs = (int) strtoul (end, &end, 16);
        }
      if (*end != '\0' || uni < 0 || uni >= 0x110000)
        {
          ff_post_error (_("Bad Number"), _("Bad Number"));
          return false;
        }
      for (j = 0; j < map->enc_limit; ++j)
        if ((gid = enc_to_gid (map, j)) != -1 && (sc = sf->glyphs[gid]) != NULL)
          {
            if (vs == -2)
              {
                for (alt = sc->altuni;
                     alt != NULL && (alt->unienc != uni || alt->fid != 0);
                     alt = alt->next);
              }
            else
              {
                for (alt = sc->altuni;
                     alt != NULL && (alt->unienc != uni || alt->vs != vs
                                     || alt->fid != 0); alt = alt->next);
              }
            doit = (sc->unicodeenc == uni && vs < 0) || alt != NULL;
            fv->b.selected[j] =
              mergefunc[merge + (fv->b.selected[j] ? 2 : 0) + doit];
          }
        else if (merge == mt_set)
          fv->b.selected[j] = false;
    }
  else
    {
      for (j = 0; j < map->enc_limit; ++j)
        if ((gid = enc_to_gid (map, j)) != -1 && (sc = sf->glyphs[gid]) != NULL)
          {
            doit = WildMatch (ret, sc->name, false);
            fv->b.selected[j] =
              mergefunc[merge + (fv->b.selected[j] ? 2 : 0) + doit];
          }
        else if (merge == mt_set)
          fv->b.selected[j] = false;
    }
  GDrawRequestExpose (fv->v, NULL, false);
  fv->sel_index = 1;
  return true;
}

VISIBLE void
FVMenuSelectByName (GWindow _gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (_gw);
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[8], *hvarray[12][2], *barray[8], boxes[3];
  GTextInfo label[8];
  int merge = SelMergeType (e);
  int done = 0, k, i;

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&label, 0, sizeof (label));
  memset (&boxes, 0, sizeof (boxes));

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = false;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Select by Name");
  wattrs.is_dlg = false;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  gw = GDrawCreateTopWindow (NULL, &pos, ss_e_h, &done, &wattrs);

  k = i = 0;

  label[k].text =
    (uint32_t *)
    _
    ("Enter either a wildcard pattern (to match glyph names)\n or a unicode encoding like \"U+0065\".");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Unix style wildcarding is accepted:\n"
                   "Most characters match themselves\n"
                   "A \"?\" will match any single character\n"
                   "A \"*\" will match an arbitrary number of characters (including none)\n"
                   "An \"[abd]\" set of characters within square brackets will match any (single) character\n"
                   "A \"{scmp,c2sc}\" set of strings within curly brackets will match any string\n"
                   "So \"a.*\" would match \"a.\" or \"a.sc\" or \"a.swash\"\n"
                   "While \"a.{scmp,c2sc}\" would match \"a.scmp\" or \"a.c2sc\"\n"
                   "And \"a.[abd]\" would match \"a.a\" or \"a.b\" or \"a.d\"");
  gcd[k++].creator = GLabelCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  gcd[k].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  gcd[k].gd.popup_msg = gcd[k - 1].gd.popup_msg;
  gcd[k++].creator = GTextFieldCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Select Results");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _("Set the selection of the font view to the glyphs\nwhich match");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Merge Results");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("Expand the selection of the font view to include\nall the glyphs which match");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Restrict Selection");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Remove matching glyphs from the selection.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("Logical And with Selection");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Remove glyphs which do not match from the selection.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i++][1] = NULL;
  gcd[k - 4 + merge / 4].gd.flags |= gg_cb_on;

  hvarray[i][0] = GCD_Glue;
  hvarray[i++][1] = NULL;

  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  gcd[k].gd.handle_controlevent = SS_OK;
  gcd[k++].creator = GButtonCreate;

  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  gcd[k].gd.handle_controlevent = SS_Cancel;
  gcd[k++].creator = GButtonCreate;

  barray[0] = barray[2] = barray[3] = barray[4] = barray[6] = GCD_Glue;
  barray[7] = NULL;
  barray[1] = &gcd[k - 2];
  barray[5] = &gcd[k - 1];
  hvarray[i][0] = &boxes[2];
  hvarray[i++][1] = NULL;
  hvarray[i][0] = NULL;

  memset (boxes, 0, sizeof (boxes));
  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = hvarray[0];
  boxes[0].creator = GHVGroupCreate;

  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = barray;
  boxes[2].creator = GHBoxCreate;

  GGadgetsCreate (gw, boxes);
  GHVBoxSetExpandableCol (boxes[2].ret, gb_expandgluesame);
  GHVBoxSetExpandableRow (boxes[0].ret, gb_expandglue);


  GHVBoxFitWindow (boxes[0].ret);

  GDrawSetVisible (gw, true);
  while (!done)
    {
      GDrawProcessOneEvent (NULL);
      if (done == 2)
        {
          char *str = GGadgetGetTitle8 (gcd[1].ret);
          int merge =
            GGadgetIsChecked (gcd[2].
                              ret) ? mt_set : GGadgetIsChecked (gcd[3].ret) ?
            mt_merge : GGadgetIsChecked (gcd[4].ret) ? mt_restrict : mt_and;
          int ret = FVSelectByName (fv, str, merge);
          free (str);
          if (!ret)
            done = 0;
        }
    }
  GDrawDestroyWindow (gw);
}

VISIBLE void
FVMenuSelectWorthOutputting (GWindow gw, struct gmenuitem *UNUSED (mi),
                             GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && SCWorthOutputting (sf->glyphs[gid]));
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuGlyphsRefs (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);
  int layer = fv->b.active_layer;

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && sf->glyphs[gid]->layers[layer].refs != NULL
              && sf->glyphs[gid]->layers[layer].splines == NULL);
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuGlyphsSplines (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);
  int layer = fv->b.active_layer;

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && sf->glyphs[gid]->layers[layer].refs == NULL
              && sf->glyphs[gid]->layers[layer].splines != NULL);
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuGlyphsBoth (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);
  int layer = fv->b.active_layer;

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && sf->glyphs[gid]->layers[layer].refs != NULL
              && sf->glyphs[gid]->layers[layer].splines != NULL);
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuGlyphsWhite (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);
  int layer = fv->b.active_layer;

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && sf->glyphs[gid]->layers[layer].refs == NULL
              && sf->glyphs[gid]->layers[layer].splines == NULL);
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectChanged (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = ((gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
              && sf->glyphs[gid]->changed);
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }

  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectHintingNeeded (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);

  for (int i = 0; i < map->enc_limit; ++i)
    {
      int doit = 0;
      int gid = enc_to_gid (map, i);
      if (gid != -1 && sf->glyphs[gid] != NULL)
        {
          if (sf->layers[fv->b.active_layer].order2)
            {
              if (sf->glyphs[gid]->layers[fv->b.active_layer].splines != NULL
                  && sf->glyphs[gid]->ttf_instrs_len <= 0)
                doit = 1;
              else if (sf->glyphs[gid]->instructions_out_of_date)
                doit = 1;
            }
          else if (sf->glyphs[gid]->changedsincelasthinted)
            doit = 1;
        }
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectAutohintable (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, gid, doit;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  int merge = SelMergeType (e);

  for (i = 0; i < map->enc_limit; ++i)
    {
      doit = (gid = enc_to_gid (map, i)) != -1 && sf->glyphs[gid] != NULL
        && !sf->glyphs[gid]->manualhints;
      fv->b.selected[i] = mergefunc[merge + (fv->b.selected[i] ? 2 : 0) + doit];
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSelectByPST (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVSelectByPST (fv);
}

VISIBLE void
FVMenuFindRpl (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  SVCreate (fv);
}

VISIBLE void
FVMenuReplaceWithRef (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVReplaceOutlineWithReference (fv, .001);
}

VISIBLE void
FVMenuCorrectRefs (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontViewBase *fv = (FontViewBase *) GDrawGetUserData (gw);

  FVCorrectReferences (fv);
}

VISIBLE void
FVMenuCharInfo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int pos = FVAnyCharSelected (fv);
  if (pos < 0)
    return;
  if (fv->b.cidmaster != NULL
      && (enc_to_gid (fv->b.map, pos) == -1
          || fv->b.sf->glyphs[enc_to_gid (fv->b.map, pos)] == NULL))
    return;
  SCCharInfo (SFMakeChar (fv->b.sf, fv->b.map, pos), fv->b.active_layer,
              fv->b.map, pos);
}

VISIBLE void
FVMenuBDFInfo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (fv->b.sf->bitmaps == NULL)
    return;
  if (fv->show != fv->filled)
    SFBdfProperties (fv->b.sf, fv->b.map, fv->show);
  else
    SFBdfProperties (fv->b.sf, fv->b.map, NULL);
}

VISIBLE void
FVMenuBaseHoriz (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *sf = fv->b.cidmaster == NULL ? fv->b.sf : fv->b.cidmaster;
  sf->horiz_base = SFBaselines (sf, sf->horiz_base, false);
  SFBaseSort (sf);
}

VISIBLE void
FVMenuBaseVert (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *sf = fv->b.cidmaster == NULL ? fv->b.sf : fv->b.cidmaster;
  sf->vert_base = SFBaselines (sf, sf->vert_base, true);
  SFBaseSort (sf);
}

VISIBLE void
FVMenuJustify (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *sf = fv->b.cidmaster == NULL ? fv->b.sf : fv->b.cidmaster;
  JustifyDlg (sf);
}

VISIBLE void
FVMenuMassRename (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVMassGlyphRename (fv);
}

static void
FVSetColor (FontView *fv, uint32_t col)
{
  int i;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    if (fv->b.selected[i])
      {
        SplineChar *sc = SFMakeChar (fv->b.sf, fv->b.map, i);
        sc->color = col;
      }
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuSetColor (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  Color col = (Color) (intptr_t) (mi->ti.userdata);
  if ((intptr_t) mi->ti.userdata == (intptr_t) -10)
    {
      struct hslrgb retcol, font_cols[6];
      retcol =
        GWidgetColor (_("Pick a color"), NULL,
                      SFFontCols (fv->b.sf, font_cols));
      if (!retcol.rgb)
        return;
      col =
        (((int) rint (255. * retcol.r)) << 16) |
        (((int) rint (255. * retcol.g)) << 8) |
        (((int) rint (255. * retcol.b)));
    }
  FVSetColor (fv, col);
}

VISIBLE void
FVMenuShowDependentRefs (GWindow gw, struct gmenuitem *UNUSED (mi),
                         GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int pos = FVAnyCharSelected (fv);
  SplineChar *sc;

  if (pos < 0 || enc_to_gid (fv->b.map, pos) == -1)
    return;
  sc = fv->b.sf->glyphs[enc_to_gid (fv->b.map, pos)];
  if (sc == NULL || sc->dependents == NULL)
    return;
  SCRefBy (sc);
}

VISIBLE void
FVMenuShowDependentSubs (GWindow gw, struct gmenuitem *UNUSED (mi),
                         GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int pos = FVAnyCharSelected (fv);
  SplineChar *sc;

  if (pos < 0 || enc_to_gid (fv->b.map, pos) == -1)
    return;
  sc = fv->b.sf->glyphs[enc_to_gid (fv->b.map, pos)];
  if (sc == NULL)
    return;
  SCSubBy (sc);
}

static int
getorigin (void *UNUSED (d), BasePoint *base, int index)
{
  /*FontView *fv = (FontView *) d; */

  base->x = base->y = 0;
  switch (index)
    {
    case 0:                    /* Character origin */
      /* all done */
      break;
    case 1:                    /* Center of selection */
      /*CVFindCenter(cv,base,!CVAnySel(cv,NULL,NULL,NULL,NULL)); */
      break;
    default:
      return false;
    }
  return true;
}

static void
FVDoTransform (FontView *fv)
{
  enum transdlg_flags flags = tdf_enableback | tdf_enablekerns;
  if (FVAnyCharSelected (fv) == -1)
    return;
  if (FVAllSelected (fv))
    flags = tdf_enableback | tdf_enablekerns | tdf_defaultkerns;
  TransformDlgCreate (fv, FVTransFunc, getorigin, flags, cvt_none);
}

VISIBLE void
FVMenuTransform (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVDoTransform (fv);
}

VISIBLE void
FVMenuPOV (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  struct pov_data pov_data;
  if (FVAnyCharSelected (fv) == -1 || fv->b.sf->onlybitmaps)
    return;
  if (PointOfViewDlg (&pov_data, fv->b.sf, false) == -1)
    return;
  FVPointOfView ((FontViewBase *) fv, &pov_data);
}

VISIBLE void
FVMenuNLTransform (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  if (FVAnyCharSelected (fv) == -1)
    return;
  NonLinearDlg (fv, NULL);
}

VISIBLE void
FVMenuBitmaps (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  BitmapDlg (fv, NULL,
             mi->mid == MID_RemoveBitmaps ? -1 : (mi->mid == MID_AvailBitmaps));
}

VISIBLE void
FVMenuStroke (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVStroke (fv);
}

#ifdef FONTFORGE_CONFIG_TILEPATH
VISIBLE void
FVMenuTilePath (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVTile (fv);
}

VISIBLE void
FVMenuPatternTile (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVPatternTile (fv);
}
#endif

VISIBLE void
FVMenuOverlap (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (fv->b.sf->onlybitmaps)
    return;

  /* We know it's more likely that we'll find a problem in the overlap
     code than anywhere else, so let's save the current state against
     a crash. */
  DoAutoSaves ();

  FVOverlap (&fv->b,
             mi->mid == MID_RmOverlap ? over_remove : mi->mid ==
             MID_Intersection ? over_intersect : over_findinter);
}

VISIBLE void
FVMenuInline (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  OutlineDlg (fv, NULL, NULL, true);
}

VISIBLE void
FVMenuOutline (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  OutlineDlg (fv, NULL, NULL, false);
}

VISIBLE void
FVMenuShadow (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  ShadowDlg (fv, NULL, NULL, false);
}

VISIBLE void
FVMenuWireframe (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  ShadowDlg (fv, NULL, NULL, true);
}

static void
FVSimplify (FontView *fv, int type)
{
  static struct simplifyinfo smpls[] = {
    {sf_normal, 0, 0, 0, 0, 0, 0},
    {sf_normal, .75, .05, 0, -1, 0, 0},
    {sf_normal, .75, .05, 0, -1, 0, 0}
  };
  struct simplifyinfo *smpl = &smpls[type + 1];

  if (smpl->linelenmax == -1 || (type == 0 && !smpl->set_as_default))
    {
      smpl->err = (fv->b.sf->ascent + fv->b.sf->descent) / 1000.;
      smpl->linelenmax = (fv->b.sf->ascent + fv->b.sf->descent) / 100.;
    }

  if (type == 1)
    {
      if (!SimplifyDlg (fv->b.sf, smpl))
        return;
      if (smpl->set_as_default)
        smpls[1] = *smpl;
    }
  _FVSimplify ((FontViewBase *) fv, smpl);
}

VISIBLE void
FVMenuSimplify (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FVSimplify ((FontView *) GDrawGetUserData (gw), false);
}

VISIBLE void
FVMenuSimplifyMore (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FVSimplify ((FontView *) GDrawGetUserData (gw), true);
}

VISIBLE void
FVMenuCleanup (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FVSimplify ((FontView *) GDrawGetUserData (gw), -1);
}

VISIBLE void
FVMenuCanonicalStart (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FVCanonicalStart ((FontViewBase *) GDrawGetUserData (gw));
}

VISIBLE void
FVMenuCanonicalContours (GWindow gw, struct gmenuitem *UNUSED (mi),
                         GEvent *UNUSED (e))
{
  FVCanonicalContours ((FontViewBase *) GDrawGetUserData (gw));
}

VISIBLE void
FVMenuAddExtrema (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FVAddExtrema ((FontViewBase *) GDrawGetUserData (gw), false);
}

VISIBLE void
FVMenuCorrectDir (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVCorrectDir ((FontViewBase *) fv);
}

VISIBLE void
FVMenuRound2Int (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FVRound2Int ((FontViewBase *) GDrawGetUserData (gw), 1.0);
}

VISIBLE void
FVMenuRound2Hundredths (GWindow gw, struct gmenuitem *UNUSED (mi),
                        GEvent *UNUSED (e))
{
  FVRound2Int ((FontViewBase *) GDrawGetUserData (gw), 100.0);
}

VISIBLE void
FVMenuCluster (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FVCluster ((FontViewBase *) GDrawGetUserData (gw));
}

VISIBLE void
FVMenuAutotrace (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  GCursor ct = 0;

  if (fv->v != NULL)
    {
      ct = GDrawGetCursor (fv->v);
      GDrawSetCursor (fv->v, ct_watch);
      GDrawSync (NULL);
      GDrawProcessPendingEvents (NULL);
    }
  FVAutoTrace (&fv->b, e != NULL && (e->u.mouse.state & ksm_shift));
  if (fv->v != NULL)
    GDrawSetCursor (fv->v, ct);
}

VISIBLE void
FVMenuBuildAccent (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FVBuildAccent ((FontViewBase *) GDrawGetUserData (gw), true);
}

VISIBLE void
FVMenuBuildComposite (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FVBuildAccent ((FontViewBase *) GDrawGetUserData (gw), false);
}

VISIBLE void
FVMenuBuildDuplicate (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FVBuildDuplicate ((FontViewBase *) GDrawGetUserData (gw));
}

#ifdef KOREAN
VISIBLE void
FVMenuShowGroup (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  ShowGroup (((FontView *) GDrawGetUserData (gw))->sf);
}
#endif

VISIBLE void
FVMenuCompareFonts (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FontCompareDlg (fv);
}

VISIBLE void
FVMenuMergeFonts (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVMergeFonts (fv);
}

VISIBLE void
FVMenuInterpFonts (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVInterpolateFonts (fv);
}

static void FVShowInfo (FontView *fv);

void
FVChangeChar (FontView *fv, int i)
{

  if (i != -1)
    {
      FVDeselectAll (fv);
      fv->b.selected[i] = true;
      fv->sel_index = 1;
      fv->end_pos = fv->pressed_pos = i;
      FVToggleCharSelected (fv, i);
      FVScrollToChar (fv, i);
      FVShowInfo (fv);
    }
}

void
FVScrollToChar (FontView *fv, int i)
{

  if (fv->v == NULL || fv->colcnt == 0) /* Can happen in scripts. */
    return;

  if (i != -1)
    {
      if (i / fv->colcnt < fv->rowoff
          || i / fv->colcnt >= fv->rowoff + fv->rowcnt)
        {
          fv->rowoff = i / fv->colcnt;
          if (fv->rowcnt >= 3)
            --fv->rowoff;
          if (fv->rowoff + fv->rowcnt >= fv->rowltot)
            fv->rowoff = fv->rowltot - fv->rowcnt;
          if (fv->rowoff < 0)
            fv->rowoff = 0;
          GScrollBarSetPos (fv->vsb, fv->rowoff);
          GDrawRequestExpose (fv->v, NULL, false);
        }
    }
}

static void
FVScrollToGID (FontView *fv, int gid)
{
  FVScrollToChar (fv, gid_to_enc (fv->b.map, gid));
}

static void
FV_ChangeGID (FontView *fv, int gid)
{
  FVChangeChar (fv, gid_to_enc (fv->b.map, gid));
}

static void
_FVMenuChangeChar (FontView *fv, int mid)
{
  SplineFont *sf = fv->b.sf;
  EncMap *map = fv->b.map;
  int pos = FVAnyCharSelected (fv);

  if (pos >= 0)
    {
      if (mid == MID_Next)
        ++pos;
      else if (mid == MID_Prev)
        --pos;
      else if (mid == MID_NextDef)
        {
          for (++pos;
               pos < map->enc_limit
               && (enc_to_gid (map, pos) == -1
                   || !SCWorthOutputting (sf->glyphs[enc_to_gid (map, pos)])
                   || (fv->show != fv->filled
                       && fv->show->glyphs[enc_to_gid (map, pos)] == NULL));
               ++pos);
          if (pos >= map->enc_limit)
            {
              int selpos = FVAnyCharSelected (fv);
              char *iconv_name = ((map->enc->iconv_name) ?
                                  map->enc->iconv_name : map->enc->enc_name);
              if (strstr (iconv_name, "2022") != NULL && selpos < 0x2121)
                pos = 0x2121;
              else if (strstr (iconv_name, "EUC") != NULL && selpos < 0xa1a1)
                pos = 0xa1a1;
              else if (map->enc->is_tradchinese)
                {
                  if (strcasestr (map->enc->enc_name, "HK") != NULL
                      && selpos < 0x8140)
                    pos = 0x8140;
                  else
                    pos = 0xa140;
                }
              else if (map->enc->is_japanese)
                {
                  if (strcasestr (iconv_name, "SJIS") != NULL
                      || (strcasestr (iconv_name, "JIS") != NULL
                          && strcasestr (iconv_name, "SHIFT") != NULL))
                    {
                      if (selpos < 0x8100)
                        pos = 0x8100;
                      else if (selpos < 0xb000)
                        pos = 0xb000;
                    }
                }
              else if (map->enc->is_korean)
                {
                  if (strcasestr (iconv_name, "JOHAB") != NULL)
                    {
                      if (selpos < 0x8431)
                        pos = 0x8431;
                    }
                  else
                    {
                      /* Wansung, EUC-KR */
                      if (selpos < 0xa1a1)
                        pos = 0xa1a1;
                    }
                }
              else if (map->enc->is_simplechinese)
                {
                  if (strcasecmp (iconv_name, "EUC-CN") == 0 && selpos < 0xa1a1)
                    pos = 0xa1a1;
                }
              if (pos >= map->enc_limit)
                return;
            }
        }
      else if (mid == MID_PrevDef)
        {
          for (--pos;
               pos >= 0 && (enc_to_gid (map, pos) == -1
                            || !SCWorthOutputting (sf->glyphs[enc_to_gid
                                                              (map, pos)])
                            || (fv->show != fv->filled
                                && fv->show->glyphs[enc_to_gid (map, pos)] ==
                                NULL)); --pos);
          if (pos < 0)
            return;
        }
    }
  if (pos < 0)
    pos = map->enc_limit - 1;
  else if (pos >= map->enc_limit)
    pos = 0;
  if (pos >= 0 && pos < map->enc_limit)
    FVChangeChar (fv, pos);
}

VISIBLE void
FVMenuChangeChar (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  _FVMenuChangeChar (fv, mi->mid);
}

static void
FVShowSubFont (FontView *fv, SplineFont *new)
{
  MetricsView *mv, *mvnext;
  BDFFont *newbdf;
  int wascompact = fv->b.normal != NULL;
  int flags = 0;
  extern int use_freetype_to_rasterize_fv;

  for (mv = fv->b.sf->metrics; mv != NULL; mv = mvnext)
    {
      /* Don't bother trying to fix up metrics views, just not worth
         it. */
      mvnext = mv->next;
      GDrawDestroyWindow (mv->gw);
    }
  if (wascompact)
    {
      EncMapFree (fv->b.map);
      fv->b.map = fv->b.normal;
      fv->b.normal = NULL;
      fv->b.selected = xrealloc (fv->b.selected, fv->b.map->enc_limit);
      memset (fv->b.selected, 0, fv->b.map->enc_limit);
    }
  CIDSetEncMap ((FontViewBase *) fv, new);
  if (wascompact)
    {
      fv->b.normal = EncMapCopy (fv->b.map);
      CompactEncMap (fv->b.map, fv->b.sf);
      FontViewReformatOne (&fv->b);
      FVSetTitle (&fv->b);
    }
  flags |= fv->antialias ? pf_antialias : 0;
  flags |= fv->bbsized ? pf_bbsized : 0;
  flags |= use_freetype_to_rasterize_fv && !fv->b.sf->strokedfont
    && !fv->b.sf->multilayer ? pf_ft_nohints : 0;
  newbdf =
    SplineFontPieceMeal (fv->b.sf, fv->b.active_layer, fv->filled->pixelsize,
                         72, flags, NULL);
  BDFFontFree (fv->filled);
  if (fv->filled == fv->show)
    fv->show = newbdf;
  fv->filled = newbdf;
  GDrawRequestExpose (fv->v, NULL, true);
}

VISIBLE void
FVMenuGotoChar (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int merge_with_selection = false;
  int pos = GotoChar (fv->b.sf, fv->b.map, &merge_with_selection);
  if (fv->b.cidmaster != NULL && pos != -1 && !fv->b.map->enc->is_compact)
    {
      SplineFont *cidmaster = fv->b.cidmaster;
      int k, hadk = cidmaster->subfontcnt;
      for (k = 0; k < cidmaster->subfontcnt; ++k)
        {
          SplineFont *sf = cidmaster->subfonts[k];
          if (pos < sf->glyphcnt && sf->glyphs[pos] != NULL)
            break;
          if (pos < sf->glyphcnt)
            hadk = k;
        }
      if (k == cidmaster->subfontcnt && pos >= fv->b.sf->glyphcnt)
        k = hadk;
      if (k != cidmaster->subfontcnt && cidmaster->subfonts[k] != fv->b.sf)
        FVShowSubFont (fv, cidmaster->subfonts[k]);
      if (pos >= fv->b.sf->glyphcnt)
        pos = -1;
    }
  if (!merge_with_selection)
    FVChangeChar (fv, pos);
  else
    {
      if (!fv->b.selected[pos])
        {
          fv->b.selected[pos] = ++fv->sel_index;
          FVToggleCharSelected (fv, pos);
        }
      fv->end_pos = fv->pressed_pos = pos;
      FVScrollToChar (fv, pos);
      FVShowInfo (fv);
    }
}

VISIBLE void
FVMenuLigatures (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFShowLigatures (fv->b.sf, NULL);
}

VISIBLE void
FVMenuKernPairs (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFKernClassTempDecompose (fv->b.sf, false);
  SFShowKernPairs (fv->b.sf, NULL, NULL, fv->b.active_layer);
  SFKernCleanup (fv->b.sf, false);
}

VISIBLE void
FVMenuAnchorPairs (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFShowKernPairs (fv->b.sf, NULL, mi->ti.userdata, fv->b.active_layer);
}

VISIBLE void
FVMenuShowAtt (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  ShowAtt (fv->b.sf, fv->b.active_layer);
}

VISIBLE void
FVMenuDisplaySubs (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (fv->cur_subtable != 0)
    {
      fv->cur_subtable = NULL;
    }
  else
    {
      SplineFont *sf = fv->b.sf;
      OTLookup *otf;
      struct lookup_subtable *sub;
      int cnt, k;
      char **names = NULL;
      if (sf->cidmaster)
        sf = sf->cidmaster;
      for (k = 0; k < 2; ++k)
        {
          cnt = 0;
          for (otf = sf->gsub_lookups; otf != NULL; otf = otf->next)
            {
              if (otf->lookup_type == gsub_single)
                {
                  for (sub = otf->subtables; sub != NULL; sub = sub->next)
                    {
                      if (names)
                        names[cnt] = sub->subtable_name;
                      ++cnt;
                    }
                }
            }
          if (cnt == 0)
            break;
          if (names == NULL)
            names = xmalloc ((cnt + 3) * sizeof (char *));
          else
            {
              names[cnt++] = "-";
              names[cnt++] = _("New Lookup Subtable...");
              names[cnt] = NULL;
            }
        }
      sub = NULL;
      if (names != NULL)
        {
          int ret =
            gwwv_choose (_("Display Substitution..."), (const char **) names,
                         cnt, 0,
                         _("Pick a substitution to display in the window."));
          if (ret != -1)
            sub = SFFindLookupSubtable (sf, names[ret]);
          free (names);
          if (ret == -1)
            return;
        }
      if (sub == NULL)
        sub =
          SFNewLookupSubtableOfType (sf, gsub_single, NULL, fv->b.active_layer);
      if (sub != NULL)
        fv->cur_subtable = sub;
    }
  GDrawRequestExpose (fv->v, NULL, false);
}

static void
FVChangeDisplayFont (FontView *fv, BDFFont *bdf)
{
  int samesize = 0;
  int rcnt, ccnt;
  int oldr, oldc;
  int first_time = fv->show == NULL;

  if (fv->v == NULL)            /* Can happen in scripts */
    return;

  if (fv->show != bdf)
    {
      oldc = fv->cbw * fv->colcnt;
      oldr = fv->cbh * fv->rowcnt;

      fv->show = bdf;
      fv->b.active_bitmap = bdf == fv->filled ? NULL : bdf;
      if (fv->user_requested_magnify != -1)
        fv->magnify = fv->user_requested_magnify;
      else if (bdf->pixelsize < 20)
        {
          if (bdf->pixelsize <= 9)
            fv->magnify = 3;
          else
            fv->magnify = 2;
          samesize = (fv->show
                      && fv->cbw == (bdf->pixelsize * fv->magnify) + 1);
        }
      else
        fv->magnify = 1;
      if (!first_time && fv->cbw == fv->magnify * bdf->pixelsize + 1)
        samesize = true;
      fv->cbw = (bdf->pixelsize * fv->magnify) + 1;
      fv->cbh = (bdf->pixelsize * fv->magnify) + 1 + fv->lab_height + 1;
      fv->resize_expected = !samesize;
      ccnt = fv->b.sf->desired_col_cnt;
      rcnt = fv->b.sf->desired_row_cnt;
      if (((bdf->pixelsize <= fv->b.sf->display_size
            || bdf->pixelsize <= -fv->b.sf->display_size)
           && fv->b.sf->top_enc != -1 /* Not defaulting. */ )
          || bdf->pixelsize <= 48)
        {
          /* Use the desired sizes. */
        }
      else
        {
          if (bdf->pixelsize > 48)
            {
              ccnt = 8;
              rcnt = 2;
            }
          else if (bdf->pixelsize >= 96)
            {
              ccnt = 4;
              rcnt = 1;
            }
          if (!first_time)
            {
              if (ccnt < oldc / fv->cbw)
                ccnt = oldc / fv->cbw;
              if (rcnt < oldr / fv->cbh)
                rcnt = oldr / fv->cbh;
            }
        }
      if (samesize)
        {
          GDrawRequestExpose (fv->v, NULL, false);
        }
      else if (fv->b.container != NULL
               && fv->b.container->funcs->doResize != NULL)
        {
          (fv->b.container->funcs->doResize) (fv->b.container, &fv->b,
                                              ccnt * fv->cbw + 1 +
                                              GDrawPointsToPixels (fv->gw,
                                                                   _GScrollBar_Width),
                                              rcnt * fv->cbh + 1 + fv->mbh +
                                              fv->infoh);
        }
      else
        {
          GDrawResize (fv->gw,
                       ccnt * fv->cbw + 1 + GDrawPointsToPixels (fv->gw,
                                                                 _GScrollBar_Width),
                       rcnt * fv->cbh + 1 + fv->mbh + fv->infoh);
        }
    }
}

struct md_data
{
  int done;
  int ish;
  FontView *fv;
};

static int
md_e_h (GWindow gw, GEvent *e)
{
  if (e->type == et_close)
    {
      struct md_data *d = GDrawGetUserData (gw);
      d->done = true;
    }
  else if (e->type == et_controlevent
           && e->u.control.subtype == et_buttonactivate)
    {
      struct md_data *d = GDrawGetUserData (gw);
      static int masks[] =
        { fvm_baseline, fvm_origin, fvm_advanceat, fvm_advanceto, -1 };
      int i, metrics;
      if (GGadgetGetCid (e->u.control.g) == 10)
        {
          metrics = 0;
          for (i = 0; masks[i] != -1; ++i)
            if (GGadgetIsChecked (GWidgetGetControl (gw, masks[i])))
              metrics |= masks[i];
          if (d->ish)
            default_fv_showhmetrics = d->fv->showhmetrics = metrics;
          else
            default_fv_showvmetrics = d->fv->showvmetrics = metrics;
        }
      d->done = true;
    }
  else if (e->type == et_char)
    {
#if 0
      if (e->u.chr.keysym == GK_F1 || e->u.chr.keysym == GK_Help)
        {
          help ("fontinfo.html");
          return true;
        }
#endif
      return false;
    }
  return true;
}

VISIBLE void
FVMenuShowMetrics (GWindow fvgw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (fvgw);
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  struct md_data d;
  GGadgetCreateData gcd[7];
  GTextInfo label[6];
  int metrics =
    mi->mid == MID_ShowHMetrics ? fv->showhmetrics : fv->showvmetrics;

  d.fv = fv;
  d.done = 0;
  d.ish = mi->mid == MID_ShowHMetrics;

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title =
    d.ish ? _("Show H. Metrics") : _("Show V. Metrics");
  pos.x = pos.y = 0;
  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (170));
  pos.height = GDrawPointsToPixels (NULL, 130);
  gw = GDrawCreateTopWindow (NULL, &pos, md_e_h, &d, &wattrs);

  memset (&label, 0, sizeof (label));
  memset (&gcd, 0, sizeof (gcd));

  label[0].text = (uint32_t *) _("Baseline");
  label[0].text_is_1byte = true;
  gcd[0].gd.label = &label[0];
  gcd[0].gd.pos.x = 8;
  gcd[0].gd.pos.y = 8;
  gcd[0].gd.flags =
    gg_enabled | gg_visible | (metrics & fvm_baseline ? gg_cb_on : 0);
  gcd[0].gd.cid = fvm_baseline;
  gcd[0].creator = GCheckBoxCreate;

  label[1].text = (uint32_t *) _("Origin");
  label[1].text_is_1byte = true;
  gcd[1].gd.label = &label[1];
  gcd[1].gd.pos.x = 8;
  gcd[1].gd.pos.y = gcd[0].gd.pos.y + 16;
  gcd[1].gd.flags =
    gg_enabled | gg_visible | (metrics & fvm_origin ? gg_cb_on : 0);
  gcd[1].gd.cid = fvm_origin;
  gcd[1].creator = GCheckBoxCreate;

  label[2].text = (uint32_t *) _("Advance Width as a Line");
  label[2].text_is_1byte = true;
  gcd[2].gd.label = &label[2];
  gcd[2].gd.pos.x = 8;
  gcd[2].gd.pos.y = gcd[1].gd.pos.y + 16;
  gcd[2].gd.flags =
    gg_enabled | gg_visible | gg_utf8_popup | (metrics & fvm_advanceat ?
                                               gg_cb_on : 0);
  gcd[2].gd.cid = fvm_advanceat;
  gcd[2].gd.popup_msg =
    (uint32_t *)
    _
    ("Display the advance width as a line\nperpendicular to the advance direction");
  gcd[2].creator = GCheckBoxCreate;

  label[3].text = (uint32_t *) _("Advance Width as a Bar");
  label[3].text_is_1byte = true;
  gcd[3].gd.label = &label[3];
  gcd[3].gd.pos.x = 8;
  gcd[3].gd.pos.y = gcd[2].gd.pos.y + 16;
  gcd[3].gd.flags =
    gg_enabled | gg_visible | gg_utf8_popup | (metrics & fvm_advanceto ?
                                               gg_cb_on : 0);
  gcd[3].gd.cid = fvm_advanceto;
  gcd[3].gd.popup_msg =
    (uint32_t *)
    _
    ("Display the advance width as a bar under the glyph\nshowing the extent of the advance");
  gcd[3].creator = GCheckBoxCreate;

  label[4].text = (uint32_t *) _("_OK");
  label[4].text_is_1byte = true;
  label[4].text_has_mnemonic = true;
  gcd[4].gd.label = &label[4];
  gcd[4].gd.pos.x = 20 - 3;
  gcd[4].gd.pos.y = GDrawPixelsToPoints (NULL, pos.height) - 35 - 3;
  gcd[4].gd.pos.width = -1;
  gcd[4].gd.pos.height = 0;
  gcd[4].gd.flags = gg_visible | gg_enabled | gg_but_default;
  gcd[4].gd.cid = 10;
  gcd[4].creator = GButtonCreate;

  label[5].text = (uint32_t *) _("_Cancel");
  label[5].text_is_1byte = true;
  label[5].text_has_mnemonic = true;
  gcd[5].gd.label = &label[5];
  gcd[5].gd.pos.x = -20;
  gcd[5].gd.pos.y = gcd[4].gd.pos.y + 3;
  gcd[5].gd.pos.width = -1;
  gcd[5].gd.pos.height = 0;
  gcd[5].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  gcd[5].creator = GButtonCreate;

  GGadgetsCreate (gw, gcd);

  GDrawSetVisible (gw, true);
  while (!d.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);

  SavePrefs (true);
  GDrawRequestExpose (fv->v, NULL, false);
}

static void
FV_ChangeDisplayBitmap (FontView *fv, BDFFont *bdf)
{
  FVChangeDisplayFont (fv, bdf);
  fv->b.sf->display_size = fv->show->pixelsize;
}

VISIBLE void
FVMenuSize (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int dspsize = fv->filled->pixelsize;
  int changedmodifier = false;
  extern int use_freetype_to_rasterize_fv;

  fv->magnify = 1;
  fv->user_requested_magnify = -1;
  if (mi->mid == MID_24)
    default_fv_font_size = dspsize = 24;
  else if (mi->mid == MID_36)
    default_fv_font_size = dspsize = 36;
  else if (mi->mid == MID_48)
    default_fv_font_size = dspsize = 48;
  else if (mi->mid == MID_72)
    default_fv_font_size = dspsize = 72;
  else if (mi->mid == MID_96)
    default_fv_font_size = dspsize = 96;
  else if (mi->mid == MID_FitToBbox)
    {
      default_fv_bbsized = fv->bbsized = !fv->bbsized;
      fv->b.sf->display_bbsized = fv->bbsized;
      changedmodifier = true;
    }
  else
    {
      default_fv_antialias = fv->antialias = !fv->antialias;
      fv->b.sf->display_antialias = fv->antialias;
      changedmodifier = true;
    }

  SavePrefs (true);
  if (fv->filled != fv->show || fv->filled->pixelsize != dspsize
      || changedmodifier)
    {
      BDFFont *new, *old;
      int flags = 0;
      old = fv->filled;
      flags |= fv->antialias ? pf_antialias : 0;
      flags |= fv->bbsized ? pf_bbsized : 0;
      flags |= use_freetype_to_rasterize_fv && !fv->b.sf->strokedfont
        && !fv->b.sf->multilayer ? pf_ft_nohints : 0;
      new =
        SplineFontPieceMeal (fv->b.sf, fv->b.active_layer, dspsize, 72, flags,
                             NULL);
      fv->filled = new;
      FVChangeDisplayFont (fv, new);
      BDFFontFree (old);
      fv->b.sf->display_size = -dspsize;
      if (fv->b.cidmaster != NULL)
        {
          int i;
          for (i = 0; i < fv->b.cidmaster->subfontcnt; ++i)
            fv->b.cidmaster->subfonts[i]->display_size = -dspsize;
        }
    }
}

void
FVSetUIToMatch (FontView *destfv, FontView *srcfv)
{
  extern int use_freetype_to_rasterize_fv;

  if (destfv->filled == NULL || srcfv->filled == NULL)
    return;
  if (destfv->magnify != srcfv->magnify
      || destfv->user_requested_magnify != srcfv->user_requested_magnify
      || destfv->bbsized != srcfv->bbsized
      || destfv->antialias != srcfv->antialias
      || destfv->filled->pixelsize != srcfv->filled->pixelsize)
    {
      BDFFont *new, *old;
      int flags = 0;
      destfv->magnify = srcfv->magnify;
      destfv->user_requested_magnify = srcfv->user_requested_magnify;
      destfv->bbsized = srcfv->bbsized;
      destfv->antialias = srcfv->antialias;
      old = destfv->filled;
      flags |= destfv->antialias ? pf_antialias : 0;
      flags |= destfv->bbsized ? pf_bbsized : 0;
      flags |= use_freetype_to_rasterize_fv && !destfv->b.sf->strokedfont
        && !destfv->b.sf->multilayer ? pf_ft_nohints : 0;
      new =
        SplineFontPieceMeal (destfv->b.sf, destfv->b.active_layer,
                             srcfv->filled->pixelsize, 72, flags, NULL);
      destfv->filled = new;
      FVChangeDisplayFont (destfv, new);
      BDFFontFree (old);
    }
}

static void
FV_LayerChanged (FontView *fv)
{
  extern int use_freetype_to_rasterize_fv;
  BDFFont *new, *old;
  int flags = 0;

  fv->magnify = 1;
  fv->user_requested_magnify = -1;

  old = fv->filled;
  flags |= fv->antialias ? pf_antialias : 0;
  flags |= fv->bbsized ? pf_bbsized : 0;
  flags |= use_freetype_to_rasterize_fv && !fv->b.sf->strokedfont
    && !fv->b.sf->multilayer ? pf_ft_nohints : 0;
  new =
    SplineFontPieceMeal (fv->b.sf, fv->b.active_layer, fv->filled->pixelsize,
                         72, flags, NULL);
  fv->filled = new;
  FVChangeDisplayFont (fv, new);
  fv->b.sf->display_size = -fv->filled->pixelsize;
  BDFFontFree (old);
}

VISIBLE void
FVMenuChangeLayer (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  fv->b.active_layer = mi->mid;
  fv->b.sf->display_layer = mi->mid;
  FV_LayerChanged (fv);
}

VISIBLE void
FVMenuMagnify (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int magnify =
    fv->user_requested_magnify != -1 ? fv->user_requested_magnify : fv->magnify;
  char def[20], *end, *ret;
  int val;
  BDFFont *show = fv->show;

  sprintf (def, "%d", magnify);
  ret =
    gwwv_ask_string (_("Bitmap Magnification..."), def,
                     _("Please specify a bitmap magnification factor."));
  if (ret == NULL)
    return;
  val = strtol (ret, &end, 10);
  if (val < 1 || val > 5 || *end != '\0')
    ff_post_error (_("Bad Number"), _("Bad Number"));
  else
    {
      fv->user_requested_magnify = val;
      fv->show = fv->filled;
      fv->b.active_bitmap = NULL;
      FVChangeDisplayFont (fv, show);
    }
  free (ret);
}

VISIBLE void
FVMenuWSize (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int h, v;
  extern int default_fv_col_count, default_fv_row_count;

  if (mi->mid == MID_32x8)
    {
      h = 32;
      v = 8;
    }
  else if (mi->mid == MID_16x4)
    {
      h = 16;
      v = 4;
    }
  else
    {
      h = 8;
      v = 2;
    }
  GDrawResize (fv->gw,
               h * fv->cbw + 1 + GDrawPointsToPixels (fv->gw,
                                                      _GScrollBar_Width),
               v * fv->cbh + 1 + fv->mbh + fv->infoh);
  fv->b.sf->desired_col_cnt = default_fv_col_count = h;
  fv->b.sf->desired_row_cnt = default_fv_row_count = v;

  SavePrefs (true);
}

VISIBLE void
FVMenuGlyphLabel (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  default_fv_glyphlabel = fv->glyphlabel = mi->mid;

  GDrawRequestExpose (fv->v, NULL, false);

  SavePrefs (true);
}

VISIBLE void
FVMenuShowBitmap (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  BDFFont *bdf = mi->ti.userdata;

  FV_ChangeDisplayBitmap (fv, bdf);     /* Let's not change any of the others */
}

static void
FV_ShowFilled (FontView *fv)
{

  fv->magnify = 1;
  fv->user_requested_magnify = 1;
  if (fv->show != fv->filled)
    FVChangeDisplayFont (fv, fv->filled);
  fv->b.sf->display_size = -fv->filled->pixelsize;
  fv->b.active_bitmap = NULL;
}

VISIBLE void
FVMenuCenter (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontViewBase *fv = (FontViewBase *) GDrawGetUserData (gw);
  FVMetricsCenter (fv, mi->mid == MID_Center);
}

VISIBLE void
FVMenuSetWidth (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (FVAnyCharSelected (fv) == -1)
    return;
  if (mi->mid == MID_SetVWidth && !fv->b.sf->hasvmetrics)
    return;
  FVSetWidth (fv,
              mi->mid == MID_SetWidth ? wt_width : mi->mid ==
              MID_SetLBearing ? wt_lbearing : mi->mid ==
              MID_SetRBearing ? wt_rbearing : mi->mid ==
              MID_SetBearings ? wt_bearings : wt_vwidth);
}

VISIBLE void
FVMenuAutoWidth (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVAutoWidth2 (fv);
}

VISIBLE void
FVMenuKernByClasses (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  ShowKernClasses (fv->b.sf, NULL, fv->b.active_layer, false);
}

VISIBLE void
FVMenuVKernByClasses (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  ShowKernClasses (fv->b.sf, NULL, fv->b.active_layer, true);
}

VISIBLE void
FVMenuRemoveKern (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVRemoveKerns (&fv->b);
}

VISIBLE void
FVMenuRemoveVKern (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVRemoveVKerns (&fv->b);
}

VISIBLE void
FVMenuKPCloseup (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i;

  for (i = 0; i < fv->b.map->enc_limit; ++i)
    if (fv->b.selected[i])
      break;
  KernPairD (fv->b.sf,
             i == fv->b.map->enc_limit ? NULL : enc_to_gid (fv->b.map, i) ==
             -1 ? NULL : fv->b.sf->glyphs[enc_to_gid (fv->b.map, i)], NULL,
             fv->b.active_layer, false);
}

VISIBLE void
FVMenuVKernFromHKern (GWindow gw, struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  FVVKernFromHKern (&fv->b);
}

VISIBLE void
FVMenuAutoHint (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVAutoHint (&fv->b);
}

VISIBLE void
FVMenuAutoHintSubs (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVAutoHintSubs (&fv->b);
}

VISIBLE void
FVMenuAutoCounter (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVAutoCounter (&fv->b);
}

VISIBLE void
FVMenuDontAutoHint (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVDontAutoHint (&fv->b);
}

VISIBLE void
FVMenuDeltas (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (!hasFreeTypeDebugger ())
    return;
  DeltaSuggestionDlg (fv, NULL);
}

VISIBLE void
FVMenuAutoInstr (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVAutoInstr (&fv->b);
}

VISIBLE void
FVMenuEditInstrs (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int index = FVAnyCharSelected (fv);
  SplineChar *sc;
  if (index < 0)
    return;
  sc = SFMakeChar (fv->b.sf, fv->b.map, index);
  SCEditInstructions (sc);
}

VISIBLE void
FVMenuEditTable (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFEditTable (fv->b.sf,
               mi->mid == MID_Editprep ? CHR ('p', 'r', 'e', 'p')
               : mi->mid == MID_Editfpgm ? CHR ('f', 'p', 'g', 'm')
               : mi->mid == MID_Editmaxp ? CHR ('m', 'a', 'x', 'p')
               : CHR ('c', 'v', 't', ' '));
}

VISIBLE void
FVMenuRmInstrTables (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  TtfTablesFree (fv->b.sf->ttf_tables);
  fv->b.sf->ttf_tables = NULL;
  if (!fv->b.sf->changed)
    {
      fv->b.sf->changed = true;
      FVSetTitles (fv->b.sf);
    }
}

VISIBLE void
FVMenuClearInstrs (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVClearInstrs (&fv->b);
}

VISIBLE void
FVMenuClearHints (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVClearHints (&fv->b);
}

VISIBLE void
FVMenuHistograms (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SFHistogram (fv->b.sf, fv->b.active_layer, NULL,
               FVAnyCharSelected (fv) != -1 ? fv->b.selected : NULL,
               fv->b.map,
               ((mi->mid == MID_HStemHist) ?
                hist_hstem :
                (mi->mid == MID_VStemHist) ? hist_vstem : hist_blues));
}

static void
FontViewSetTitle (FontView *fv)
{
  uint32_t *title, *ititle;
  char *file = NULL;
  char *enc;
  int len;

  if (fv->gw == NULL)           /* In scripting. */
    return;

  enc = SFEncodingName (fv->b.sf, fv->b.normal ? fv->b.normal : fv->b.map);
  len = strlen (fv->b.sf->fontname) + 1 + strlen (enc) + 6;
  if (fv->b.normal)
    len += strlen (_("Compact")) + 1;
  if (fv->b.cidmaster != NULL)
    {
      if ((file = fv->b.cidmaster->filename) == NULL)
        file = fv->b.cidmaster->origname;
    }
  else
    {
      if ((file = fv->b.sf->filename) == NULL)
        file = fv->b.sf->origname;
    }
  if (file != NULL)
    len += 2 + strlen (file);
  title = xmalloc ((len + 1) * sizeof (uint32_t));
  u32_strcpy (title, x_gc_u8_to_u32 (fv->b.sf->fontname));
  if (fv->b.sf->changed)
    u32_strcat (title, x_gc_u8_to_u32 ("*"));
  if (file != NULL)
    {
      u32_strcat (title, x_gc_u8_to_u32 ("  "));
      u32_strcat (title, x_gc_u32_strconv_from_locale (GFileBaseName (file)));
    }
  u32_strcat (title, x_gc_u8_to_u32 (" ("));
  if (fv->b.normal)
    {
      utf82u_strcat (title, _("Compact"));
      u32_strcat (title, x_gc_u8_to_u32 (" "));
    }
  u32_strcat (title, x_gc_u8_to_u32 (enc));
  u32_strcat (title, x_gc_u8_to_u32 (")"));
  free (enc);

  ititle = x_u8_to_u32 (u8_force_valid (fv->b.sf->fontname));
  GDrawSetWindowTitles (fv->gw, title, ititle);
  free (title);
  free (ititle);
}

static void
FontViewSetTitles (SplineFont *sf)
{
  FontView *fv;

  for (fv = (FontView *) (sf->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    FontViewSetTitle (fv);
}

VISIBLE void
FVMenuShowSubFont (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *new = mi->ti.userdata;
  FVShowSubFont (fv, new);
}

VISIBLE void
FVMenuConvert2CID (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;
  struct cidmap *cidmap;

  if (cidmaster != NULL)
    return;
  SFFindNearTop (fv->b.sf);
  cidmap = AskUserForCIDMap ();
  if (cidmap == NULL)
    return;
  MakeCIDMaster (fv->b.sf, fv->b.map, false, NULL, cidmap);
  SFRestoreNearTop (fv->b.sf);
}

static enum fchooserret
CMapFilter (GGadget *g, GDirEntry * ent, const uint32_t *dir)
{
  enum fchooserret ret = GFileChooserDefFilter (g, ent, dir);
  char buf2[256];
  FILE *file;
  static char *cmapflag = "%!PS-Adobe-3.0 Resource-CMap";

  if (ret == fc_show && !ent->isdir)
    {
      char *locale_dir = x_u32_strconv_to_locale (dir);
      char *locale_base = x_u32_strconv_to_locale (ent->name);
      char *filename = xcalloc (strlen (locale_dir) + strlen (locale_base) + 2,
                                sizeof (char));
      strcat (filename, locale_dir);
      strcat (filename, "/");
      strcat (filename, locale_base);
      free (locale_base);
      free (locale_dir);
      file = fopen (filename, "r");
      if (file == NULL)
        ret = fc_hide;
      else
        {
          if (fgets (buf2, sizeof (buf2), file) == NULL
              || strncmp (buf2, cmapflag, strlen (cmapflag)) != 0)
            ret = fc_hide;
          fclose (file);
        }
      free (filename);
    }
  return ret;
}

VISIBLE void
FVMenuConvertByCMap (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;
  char *cmapfilename;

  if (cidmaster != NULL)
    return;
  cmapfilename =
    gwwv_open_filename (_("Find an adobe CMap file..."), NULL, NULL,
                        CMapFilter);
  if (cmapfilename == NULL)
    return;
  MakeCIDMaster (fv->b.sf, fv->b.map, true, cmapfilename, NULL);
  free (cmapfilename);
}

VISIBLE void
FVMenuFlatten (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;

  if (cidmaster == NULL)
    return;
  SFFlatten (cidmaster);
}

VISIBLE void
FVMenuFlattenByCMap (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;
  char *cmapname;

  if (cidmaster == NULL)
    return;
  cmapname =
    gwwv_open_filename (_("Find an adobe CMap file..."), NULL, NULL,
                        CMapFilter);
  if (cmapname == NULL)
    return;
  SFFindNearTop (fv->b.sf);
  SFFlattenByCMap (cidmaster, cmapname);
  SFRestoreNearTop (fv->b.sf);
  free (cmapname);
}

VISIBLE void
FVMenuInsertFont (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;
  SplineFont *new;
  struct cidmap *map;
  char *filename;
  extern NameList *force_names_when_opening;

  if (cidmaster == NULL || cidmaster->subfontcnt >= 255)
    /* Open type allows 1 byte to specify the fdselect. */
    return;

  filename = GetFontNameDialog (NULL, false);
  if (filename == NULL)
    return;
  new = LoadSplineFont (filename, 0);
  free (filename);
  if (new == NULL)
    return;
  if (new->fv == &fv->b)        /* Already part of us. */
    return;
  if (new->fv != NULL)
    {
      if (((FontView *) (new->fv))->gw != NULL)
        GDrawRaise (((FontView *) (new->fv))->gw);
      ff_post_error (_("Please close font"),
                     _("Please close %s before inserting it into a CID font"),
                     new->origname);
      return;
    }
  EncMapFree (new->map);
  if (force_names_when_opening != NULL)
    SFRenameGlyphsToNamelist (new, force_names_when_opening);

  map =
    FindCidMap (cidmaster->cidregistry, cidmaster->ordering,
                cidmaster->supplement, cidmaster);
  SFEncodeToMap (new, map);
  if (!PSDictHasEntry (new->private, "lenIV"))
    PSDictChangeEntry (new->private, "lenIV", "1");     /* It's 4 by
                                                           default; in CIDs
                                                           the convention
                                                           seems to be 1. */
  new->display_antialias = fv->b.sf->display_antialias;
  new->display_bbsized = fv->b.sf->display_bbsized;
  new->display_size = fv->b.sf->display_size;
  FVInsertInCID ((FontViewBase *) fv, new);
  CIDMasterAsDes (new);
}

VISIBLE void
FVMenuInsertBlank (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster, *sf;
  struct cidmap *map;

  if (cidmaster == NULL || cidmaster->subfontcnt >= 255)
    /* OpenType allows 1 byte to specify the fdselect. */
    return;
  map =
    FindCidMap (cidmaster->cidregistry, cidmaster->ordering,
                cidmaster->supplement, cidmaster);
  sf = SplineFontBlank (MaxCID (map));
  sf->glyphcnt = sf->glyphmax;
  sf->cidmaster = cidmaster;
  sf->display_antialias = fv->b.sf->display_antialias;
  sf->display_bbsized = fv->b.sf->display_bbsized;
  sf->display_size = fv->b.sf->display_size;
  sf->private = xcalloc (1, sizeof (struct psdict));
  PSDictChangeEntry (sf->private, "lenIV", "1");        /* It's 4 by default;
                                                           in CIDs the
                                                           convention seems
                                                           to be 1. */
  FVInsertInCID ((FontViewBase *) fv, sf);
}

VISIBLE void
FVMenuRemoveFontFromCID (GWindow gw, struct gmenuitem *UNUSED (mi),
                         GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char *buts[3];
  SplineFont *cidmaster = fv->b.cidmaster, *sf = fv->b.sf, *replace;
  int i;
  MetricsView *mv, *mnext;
  FontView *fvs;

  if (cidmaster == NULL || cidmaster->subfontcnt <= 1)
    /* Can't remove the last font. */
    return;

  buts[0] = _("_Remove");
  buts[1] = _("_Cancel");
  buts[2] = NULL;
  if (gwwv_ask
      (_("_Remove Font"), (const char **) buts, 0, 1,
       _
       ("Are you sure you wish to remove sub-font %1$.40s from the CID font %2$.40s"),
       sf->fontname, cidmaster->fontname) == 1)
    return;

  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      {
        CharView *cv, *next;
        for (cv = (CharView *) (sf->glyphs[i]->views); cv != NULL; cv = next)
          {
            next = (CharView *) (cv->b.next);
            GDrawDestroyWindow (cv->gw);
          }
      }
  GDrawProcessPendingEvents (NULL);
  for (mv = fv->b.sf->metrics; mv != NULL; mv = mnext)
    {
      mnext = mv->next;
      GDrawDestroyWindow (mv->gw);
    }
  GDrawSync (NULL);
  GDrawProcessPendingEvents (NULL);
  /* Just in case... */
  GDrawSync (NULL);
  GDrawProcessPendingEvents (NULL);

  for (i = 0; i < cidmaster->subfontcnt; ++i)
    if (cidmaster->subfonts[i] == sf)
      break;
  replace = i == 0 ? cidmaster->subfonts[1] : cidmaster->subfonts[i - 1];
  while (i < cidmaster->subfontcnt - 1)
    {
      cidmaster->subfonts[i] = cidmaster->subfonts[i + 1];
      ++i;
    }
  --cidmaster->subfontcnt;

  for (fvs = (FontView *) (fv->b.sf->fv); fvs != NULL;
       fvs = (FontView *) (fvs->b.nextsame))
    {
      if (fvs->b.sf == sf)
        CIDSetEncMap ((FontViewBase *) fvs, replace);
    }
  FontViewReformatAll (fv->b.sf);
  SplineFontFree (sf);
}

VISIBLE void
FVMenuCIDFontInfo (GWindow gw, struct gmenuitem *UNUSED (mi),
                   GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;

  if (cidmaster == NULL)
    return;
  FontInfo (cidmaster, fv->b.active_layer, -1, false);
}

VISIBLE void
FVMenuChangeSupplement (GWindow gw, struct gmenuitem *UNUSED (mi),
                        GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *cidmaster = fv->b.cidmaster;
  struct cidmap *cidmap;
  char buffer[20];
  char *ret, *end;
  int supple;

  if (cidmaster == NULL)
    return;
  sprintf (buffer, "%d", cidmaster->supplement);
  ret =
    gwwv_ask_string (_("Change Supplement..."), buffer,
                     _("Please specify a new supplement for %.20s-%.20s"),
                     cidmaster->cidregistry, cidmaster->ordering);
  if (ret == NULL)
    return;
  supple = strtol (ret, &end, 10);
  if (*end != '\0' || supple <= 0)
    {
      free (ret);
      ff_post_error (_("Bad Number"), _("Bad Number"));
      return;
    }
  free (ret);
  if (supple != cidmaster->supplement)
    {
      /* This will make noises if it can't find an appropriate
         cidmap. */
      cidmap =
        FindCidMap (cidmaster->cidregistry, cidmaster->ordering, supple,
                    cidmaster);
      cidmaster->supplement = supple;
      FontViewSetTitle (fv);
    }
}

static SplineChar *
FVFindACharInDisplay (FontView *fv)
{
  int start, end, enc, gid;
  EncMap *map = fv->b.map;
  SplineFont *sf = fv->b.sf;
  SplineChar *sc;

  start = fv->rowoff * fv->colcnt;
  end = start + fv->rowcnt * fv->colcnt;
  for (enc = start; enc < end && enc < map->enc_limit; ++enc)
    {
      if ((gid = enc_to_gid (map, enc)) != -1 && (sc = sf->glyphs[gid]) != NULL)
        return sc;
    }
  return NULL;
}

VISIBLE void
FVMenuReencode (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  Encoding *enc = NULL;
  SplineChar *sc;

  sc = FVFindACharInDisplay (fv);
  enc = FindOrMakeEncoding (mi->ti.userdata);
  if (enc == NULL)
    {
      IError ("Known encoding could not be found");
      return;
    }
  FVReencode ((FontViewBase *) fv, enc);
  if (sc != NULL)
    {
      int enc = gid_to_enc (fv->b.map, sc->orig_pos);
      if (enc != -1)
        FVScrollToChar (fv, enc);
    }
}

VISIBLE void
FVMenuForceEncode (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  Encoding *enc = NULL;
  int oldcnt = fv->b.map->enc_limit;

  enc = FindOrMakeEncoding (mi->ti.userdata);
  if (enc == NULL)
    {
      IError ("Known encoding could not be found");
      return;
    }
  SFForceEncoding (fv->b.sf, fv->b.map, enc);
  if (oldcnt < fv->b.map->enc_limit)
    {
      fv->b.selected = xrealloc (fv->b.selected, fv->b.map->enc_limit);
      memset (fv->b.selected + oldcnt, 0, fv->b.map->enc_limit - oldcnt);
    }
  if (fv->b.normal != NULL)
    {
      EncMapFree (fv->b.normal);
      fv->b.normal = NULL;
    }
  SFReplaceEncodingBDFProps (fv->b.sf, fv->b.map);
  FontViewSetTitle (fv);
  FontViewReformatOne (&fv->b);
}

VISIBLE void
FVMenuDisplayByGroups (GWindow gw, struct gmenuitem *UNUSED (mi),
                       GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  DisplayGroups (fv);
}

VISIBLE void
FVMenuDefineGroups (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  DefineGroups (fv);
}

VISIBLE void
FVMenuMMValid (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  MMSet *mm = fv->b.sf->mm;

  if (mm == NULL)
    return;
  MMValid (mm, true);
}

VISIBLE void
FVMenuCreateMM (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
                GEvent *UNUSED (e))
{
  MMWizard (NULL);
}

VISIBLE void
FVMenuMMInfo (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  MMSet *mm = fv->b.sf->mm;

  if (mm == NULL)
    return;
  MMWizard (mm);
}

VISIBLE void
FVMenuChangeMMBlend (GWindow gw, struct gmenuitem *UNUSED (mi),
                     GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  MMSet *mm = fv->b.sf->mm;

  MMChangeBlend (mm, fv, false);
}

VISIBLE void
FVMenuBlendToNew (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  MMSet *mm = fv->b.sf->mm;

  if (mm == NULL)
    return;
  MMChangeBlend (mm, fv, true);
}

VISIBLE void
cflistcheck_fv (GWindow UNUSED (gw), struct gmenuitem *mi, GEvent *UNUSED (e))
{
  /*FontView *fv = (FontView *) GDrawGetUserData(gw); */

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_AllFonts:
          mi->ti.checked = !onlycopydisplayed;
          break;
        case MID_DisplayedFont:
          mi->ti.checked = onlycopydisplayed;
          break;
        case MID_CharName:
          mi->ti.checked = copymetadata;
          break;
        case MID_TTFInstr:
          mi->ti.checked = copyttfinstr;
          break;
        }
    }
}

VISIBLE void
sllistcheck_fv (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  fv = fv;
}

VISIBLE void
htlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);
  int multilayer = fv->b.sf->multilayer;

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_AutoHint:
          mi->ti.disabled = anychars == -1 || multilayer;
          break;
        case MID_HintSubsPt:
          mi->ti.disabled = fv->b.sf->layers[fv->b.active_layer].order2
            || anychars == -1 || multilayer;
          break;
        case MID_AutoCounter:
        case MID_DontAutoHint:
          mi->ti.disabled = fv->b.sf->layers[fv->b.active_layer].order2
            || anychars == -1 || multilayer;
          break;
        case MID_AutoInstr:
        case MID_EditInstructions:
        case MID_Deltas:
          mi->ti.disabled = !fv->b.sf->layers[fv->b.active_layer].order2
            || anychars == -1 || multilayer;
          break;
        case MID_RmInstrTables:
          mi->ti.disabled = fv->b.sf->ttf_tables == NULL;
          break;
        case MID_Editfpgm:
        case MID_Editprep:
        case MID_Editcvt:
        case MID_Editmaxp:
          mi->ti.disabled = !fv->b.sf->layers[fv->b.active_layer].order2
            || multilayer;
          break;
        case MID_ClearHints:
        case MID_ClearWidthMD:
        case MID_ClearInstrs:
          mi->ti.disabled = anychars == -1;
          break;
        }
    }
}

VISIBLE void
fllistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);
  FontView *fvs;

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
#ifndef _NO_PYTHON
        case MID_ExecuteScript:
          mi->ti.disabled = (!Py_IsInitialized ());
          break;
#endif

        case MID_GenerateTTC:
          for (fvs = fv_list; fvs != NULL; fvs = (FontView *) (fvs->b.next))
            {
              if (fvs != fv)
                break;
            }
          mi->ti.disabled = fvs == NULL;
          break;

        case MID_Revert:
          mi->ti.disabled = fv->b.sf->origname == NULL || fv->b.sf->new;
          break;

        case MID_RevertToBackup:
          /* We really do want to use filename here and origname
             above. */
          mi->ti.disabled = true;
          if (fv->b.sf->filename != NULL)
            {
              if (fv->b.sf->backedup == bs_dontknow)
                {
                  char *buf = xmalloc (strlen (fv->b.sf->filename) + 20);
                  strcpy (buf, fv->b.sf->filename);
                  if (fv->b.sf->compression != 0)
                    strcat (buf, compressors[fv->b.sf->compression - 1].ext);
                  strcat (buf, "~");
                  if (access (buf, F_OK) == 0)
                    fv->b.sf->backedup = bs_backedup;
                  else
                    fv->b.sf->backedup = bs_not;
                  free (buf);
                }
              if (fv->b.sf->backedup == bs_backedup)
                mi->ti.disabled = false;
            }
          break;

        case MID_RevertGlyph:
          mi->ti.disabled = fv->b.sf->origname == NULL
            || fv->b.sf->sfd_version < 2 || anychars == -1
            || fv->b.sf->compression != 0;
          break;

        case MID_Recent:
          mi->ti.disabled = !RecentFilesAny ();
          break;

        case MID_ScriptMenu:
          mi->ti.disabled = script_menu_names[0] == NULL;
          break;
        }
    }
}

VISIBLE void
edlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int pos = FVAnyCharSelected (fv), i, gid;
  int not_pasteable = (pos == -1)
    || (!CopyContainsSomething ()
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/png")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/svg+xml")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/svg-xml")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/svg")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/bmp")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/eps")
        && !GDrawSelectionHasType (fv->gw, sn_clipboard, "image/ps"));
  RefChar *base = CopyContainsRef (fv->b.sf);
  int base_enc = (base != NULL) ? gid_to_enc (fv->b.map, base->orig_pos) : -1;


  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Paste:
        case MID_PasteInto:
          mi->ti.disabled = not_pasteable;
          break;
        case MID_PasteAfter:
          mi->ti.disabled = not_pasteable || pos < 0;
          break;
        case MID_SameGlyphAs:
          /* Can't be self-referential. */
          mi->ti.disabled = not_pasteable || (base == NULL)
            || (fv->b.cidmaster != NULL) || (base_enc == -1)
            || fv->b.selected[base_enc];
          break;
        case MID_Join:
        case MID_Cut:
        case MID_Copy:
        case MID_Clear:
        case MID_CopyWidth:
        case MID_CopyLBearing:
        case MID_CopyRBearing:
        case MID_CopyRef:
        case MID_UnlinkRef:
        case MID_RemoveUndoes:
        case MID_CopyFgToBg:
        case MID_CopyL2L:
          mi->ti.disabled = (pos == -1);
          break;
        case MID_RplRef:
        case MID_CorrectRefs:
          mi->ti.disabled = (pos == -1) || (fv->b.cidmaster != NULL)
            || fv->b.sf->multilayer;
          break;
        case MID_CopyLookupData:
          mi->ti.disabled = pos == -1 || (fv->b.sf->gpos_lookups == NULL
                                          && fv->b.sf->gsub_lookups == NULL);
          break;
        case MID_CopyVWidth:
          mi->ti.disabled = pos == -1 || !fv->b.sf->hasvmetrics;
          break;
        case MID_ClearBackground:
          mi->ti.disabled = true;
          if (pos != -1 && !(onlycopydisplayed && fv->filled != fv->show))
            {
              for (i = 0; i < fv->b.map->enc_limit; ++i)
                if (fv->b.selected[i] && (gid = enc_to_gid (fv->b.map, i)) != -1
                    && fv->b.sf->glyphs[gid] != NULL)
                  if (fv->b.sf->glyphs[gid]->layers[ly_back].images != NULL
                      || fv->b.sf->glyphs[gid]->layers[ly_back].splines != NULL)
                    {
                      mi->ti.disabled = false;
                      break;
                    }
            }
          break;
        case MID_Undo:
          for (i = 0; i < fv->b.map->enc_limit; ++i)
            if (fv->b.selected[i] && (gid = enc_to_gid (fv->b.map, i)) != -1
                && fv->b.sf->glyphs[gid] != NULL)
              if (fv->b.sf->glyphs[gid]->layers[fv->b.active_layer].undoes !=
                  NULL)
                break;
          mi->ti.disabled = i == fv->b.map->enc_limit;
          break;
        case MID_Redo:
          for (i = 0; i < fv->b.map->enc_limit; ++i)
            if (fv->b.selected[i] && (gid = enc_to_gid (fv->b.map, i)) != -1
                && fv->b.sf->glyphs[gid] != NULL)
              if (fv->b.sf->glyphs[gid]->layers[fv->b.active_layer].redoes !=
                  NULL)
                break;
          mi->ti.disabled = i == fv->b.map->enc_limit;
          break;
        }
    }
}

VISIBLE void
trlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Transform:
          mi->ti.disabled = anychars == -1;
          break;
        case MID_NLTransform:
        case MID_POV:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
        }
    }
}

VISIBLE void
validlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_FindProblems:
          mi->ti.disabled = anychars == -1;
          break;
        case MID_Validate:
          mi->ti.disabled = fv->b.sf->strokedfont || fv->b.sf->multilayer;
          break;
        }
    }
}

VISIBLE void
ellistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv), gid;
  int anybuildable, anytraceable;
  int in_modal = (fv->b.container != NULL && fv->b.container->funcs->is_modal);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_FontInfo:
          mi->ti.disabled = in_modal;
          break;
        case MID_CharInfo:
          mi->ti.disabled = anychars < 0
            || (gid = enc_to_gid (fv->b.map, anychars)) == -1
            || (fv->b.cidmaster != NULL && fv->b.sf->glyphs[gid] == NULL)
            || in_modal;
          break;
        case MID_Transform:
          mi->ti.disabled = anychars == -1;
          /* Some Transformations make sense on bitmaps now. */
          break;
        case MID_AddExtrema:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
        case MID_Simplify:
        case MID_Stroke:
        case MID_RmOverlap:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
        case MID_Styles:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
        case MID_Round:
        case MID_Correct:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
#ifdef FONTFORGE_CONFIG_TILEPATH
        case MID_TilePath:
          mi->ti.disabled = anychars == -1 || fv->b.sf->onlybitmaps;
          break;
#endif
        case MID_AvailBitmaps:
          mi->ti.disabled = fv->b.sf->mm != NULL;
          break;
        case MID_RegenBitmaps:
        case MID_RemoveBitmaps:
          mi->ti.disabled = fv->b.sf->bitmaps == NULL || fv->b.sf->onlybitmaps
            || fv->b.sf->mm != NULL;
          break;
        case MID_BuildAccent:
          anybuildable = false;
          if (anychars != -1)
            {
              int i;
              for (i = 0; i < fv->b.map->enc_limit; ++i)
                if (fv->b.selected[i])
                  {
                    SplineChar *sc = NULL, dummy;
                    gid = enc_to_gid (fv->b.map, i);
                    if (gid != -1)
                      sc = fv->b.sf->glyphs[gid];
                    if (sc == NULL)
                      sc = SCBuildDummy (&dummy, fv->b.sf, fv->b.map, i);
                    if (SFIsSomethingBuildable
                        (fv->b.sf, sc, fv->b.active_layer, false)
                        || SFIsDuplicatable (fv->b.sf, sc))
                      {
                        anybuildable = true;
                        break;
                      }
                  }
            }
          mi->ti.disabled = !anybuildable;
          break;
        case MID_Autotrace:
          anytraceable = false;
          if (FindAutoTraceName () != NULL && anychars != -1)
            {
              int i;
              for (i = 0; i < fv->b.map->enc_limit; ++i)
                if (fv->b.selected[i] && (gid = enc_to_gid (fv->b.map, i)) != -1
                    && fv->b.sf->glyphs[gid] != NULL
                    && fv->b.sf->glyphs[gid]->layers[ly_back].images != NULL)
                  {
                    anytraceable = true;
                    break;
                  }
            }
          mi->ti.disabled = !anytraceable;
          break;
        case MID_MergeFonts:
          mi->ti.disabled = fv->b.sf->bitmaps != NULL && fv->b.sf->onlybitmaps;
          break;
        case MID_FontCompare:
          mi->ti.disabled = fv_list->b.next == NULL;
          break;
        case MID_InterpolateFonts:
          mi->ti.disabled = fv->b.sf->onlybitmaps;
          break;
        }
    }
}

VISIBLE void
mtlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Center:
        case MID_Thirds:
        case MID_SetWidth:
        case MID_SetLBearing:
        case MID_SetRBearing:
        case MID_SetBearings:
          mi->ti.disabled = anychars == -1;
          break;
        case MID_SetVWidth:
          mi->ti.disabled = anychars == -1 || !fv->b.sf->hasvmetrics;
          break;
        case MID_VKernByClass:
        case MID_VKernFromH:
        case MID_RmVKern:
          mi->ti.disabled = !fv->b.sf->hasvmetrics;
          break;
        }
    }
}

VISIBLE void
balistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      if (mi->mid == MID_BuildAccent || mi->mid == MID_BuildComposite)
        {
          int anybuildable = false;
          int onlyaccents = mi->mid == MID_BuildAccent;
          int i, gid;
          for (i = 0; i < fv->b.map->enc_limit; ++i)
            if (fv->b.selected[i])
              {
                SplineChar *sc = NULL, dummy;
                if ((gid = enc_to_gid (fv->b.map, i)) != -1)
                  sc = fv->b.sf->glyphs[gid];
                if (sc == NULL)
                  sc = SCBuildDummy (&dummy, fv->b.sf, fv->b.map, i);
                if (SFIsSomethingBuildable
                    (fv->b.sf, sc, fv->b.active_layer, onlyaccents))
                  {
                    anybuildable = true;
                    break;
                  }
              }
          mi->ti.disabled = !anybuildable;
        }
      else if (mi->mid == MID_BuildDuplicates)
        {
          int anybuildable = false;
          int i, gid;
          for (i = 0; i < fv->b.map->enc_limit; ++i)
            if (fv->b.selected[i])
              {
                SplineChar *sc = NULL, dummy;
                if ((gid = enc_to_gid (fv->b.map, i)) != -1)
                  sc = fv->b.sf->glyphs[gid];
                if (sc == NULL)
                  sc = SCBuildDummy (&dummy, fv->b.sf, fv->b.map, i);
                if (SFIsDuplicatable (fv->b.sf, sc))
                  {
                    anybuildable = true;
                    break;
                  }
              }
          mi->ti.disabled = !anybuildable;
        }
    }
}

VISIBLE void
delistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i = FVAnyCharSelected (fv);
  int gid = i < 0 ? -1 : enc_to_gid (fv->b.map, i);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_ShowDependentRefs:
          mi->ti.disabled = gid < 0 || fv->b.sf->glyphs[gid] == NULL
            || fv->b.sf->glyphs[gid]->dependents == NULL;
          break;
        case MID_ShowDependentSubs:
          mi->ti.disabled = gid < 0 || fv->b.sf->glyphs[gid] == NULL
            || !SCUsedBySubs (fv->b.sf->glyphs[gid]);
          break;
        }
    }
}

VISIBLE void
infolistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_StrikeInfo:
          mi->ti.disabled = fv->b.sf->bitmaps == NULL;
          break;
        case MID_MassRename:
          mi->ti.disabled = anychars == -1;
          break;
        case MID_SetColor:
          mi->ti.disabled = anychars == -1;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem dummyitem[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Font|_New"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'N'},
  },

  GMENUITEM_EMPTY
};

static GMenuItem fllist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Font|_New"),
      .image = (GImage *) "filenew.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'N'},

    .shortcut = H_ ("New|Ctl+N"),
    .invoke = MenuNew,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Open"),
      .image = (GImage *) "fileopen.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'O'},

    .shortcut = H_ ("Open|Ctl+O"),
    .invoke = MenuOpen,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Recen_t"),
      .image = (GImage *) "filerecent.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 't'},
    
    .sub = dummyitem,
    .moveto = MenuRecentBuild,
    .mid = MID_Recent},

  {
    .ti = {
      .text = (uint32_t *) N_("_Close"),
      .image = (GImage *) "fileclose.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Close|Ctl+Shft+Q"),
    .invoke = FVMenuClose,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Save"),
      .image = (GImage *) "filesave.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},

    .shortcut = H_ ("Save|Ctl+S"),
    .invoke = FVMenuSave,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("S_ave as..."),
      .image = (GImage *) "filesaveas.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'a'},

    .shortcut = H_ ("Save as...|Ctl+Shft+S"),
    .invoke = FVMenuSaveAs,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Save A_ll"),
      .image = (GImage *) "filesaveall.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},

    .shortcut = H_ ("Save All|Alt+Ctl+S"),
    .invoke = MenuSaveAll,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Generate Fonts..."),
      .image = (GImage *) "filegenerate.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'G'},

    .shortcut = H_ ("Generate Fonts...|Ctl+Shft+G"),
    .invoke = FVMenuGenerate,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Generate Mac _Family..."),
      .image = (GImage *) "filegeneratefamily.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Generate Mac Family...|Alt+Ctl+G"),
    .invoke = FVMenuGenerateFamily,

    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Generate TTC..."),
      .image = (GImage *) "filegeneratefamily.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Generate TTC...|No Shortcut"),
    .invoke = FVMenuGenerateTTC,

    .mid = MID_GenerateTTC},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Import..."),
      .image = (GImage *) "fileimport.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("Import...|Ctl+Shft+I"),
    .invoke = FVMenuImport,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Merge Feature Info..."),
      .image = (GImage *) "filemergefeature.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},

    .shortcut = H_ ("Merge Kern Info...|Alt+Ctl+Shft+K"),
    .invoke = FVMenuMergeKern,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Revert File"),
      .image = (GImage *) "filerevert.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Revert File|Ctl+Shft+R"),
    .invoke = FVMenuRevert,
    .mid = MID_Revert},

  {
    .ti = {
      .text = (uint32_t *) N_("Revert To _Backup"),
      .image = (GImage *) "filerevertbackup.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Revert To Backup|No Shortcut"),
    .invoke = FVMenuRevertBackup,
    .mid = MID_RevertToBackup},

  {
    .ti = {
      .text = (uint32_t *) N_("Revert Gl_yph"),
      .image = (GImage *) "filerevertglyph.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Revert Glyph|Alt+Ctl+R"),
    .invoke = FVMenuRevertGlyph,
    .mid = MID_RevertGlyph},

  GMENUITEM_LINE,

#if !defined(_NO_PYTHON)
  {
    .ti = {
      .text = (uint32_t *) N_("E_xecute Script..."),
      .image = (GImage *) "python.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'x'},

    .shortcut = H_ ("Execute Script...|Ctl+."),
    .invoke = FVMenuExecute,
    .mid = MID_ExecuteScript},

#endif
#if !defined(_NO_PYTHON)
  GMENUITEM_LINE,

#endif
  {
    .ti = {
      .text = (uint32_t *) N_("Pr_eferences..."),
      .image = (GImage *) "fileprefs.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'e'},

    .shortcut = H_ ("Preferences...|No Shortcut"),
    .invoke = MenuPrefs,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Quit"),
      .image = (GImage *) "filequit.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'Q'},

    .shortcut = H_ ("Quit|Ctl+Q"),
    .invoke = FVMenuExit,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem cflist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_All Fonts"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},
    .shortcut = H_ ("All Fonts|No Shortcut"),
    .invoke = FVMenuCopyFrom,
    .mid = MID_AllFonts},

  {
    .ti = {
      .text = (uint32_t *) N_("_Displayed Font"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'D'},

    .shortcut = H_ ("Displayed Font|No Shortcut"),
    .invoke = FVMenuCopyFrom,
    .mid = MID_DisplayedFont},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Glyph _Metadata"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'N'},

    .shortcut = H_ ("Glyph Metadata|No Shortcut"),
    .invoke = FVMenuCopyFrom,
    .mid = MID_CharName},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_TrueType Instructions"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'N'},

    .shortcut = H_ ("TrueType Instructions|No Shortcut"),
    .invoke = FVMenuCopyFrom,
    .mid = MID_TTFInstr},

  GMENUITEM_EMPTY
};

static GMenuItem sclist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Color|Choose..."),
      .image = (GImage *) "colorwheel.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) -10,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Color Choose|No Shortcut"),
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Color|Default"),
      .image = (GImage *) "colordef.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Default|No Shortcut"),
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorwhite.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .userdata = (void *) 0xffffff,
      .image_precedes = true,
      .mnemonic = '\0'},
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorred.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,

      .userdata = (void *) 0xff0000,
      .image_precedes = true,
      .mnemonic = '\0'},
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorgreen.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .userdata = (void *) 0x00ff00,
      .image_precedes = true,
      .mnemonic = '\0'},
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorblue.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .userdata = (void *) 0x0000ff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "coloryellow.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,

      .userdata = (void *) 0xffff00,
      .image_precedes = true
    },
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorcyan.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .userdata = (void *) 0x00ffff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colormagenta.png",
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .userdata = (void *) 0xff00ff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSelectColor,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem sllist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Select _All"),
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},
    .shortcut = H_ ("Select All|Ctl+A"),
    
    .invoke = FVMenuSelectAll,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Invert Selection"),
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("Invert Selection|Ctl+Escape"),
    .invoke = FVMenuInvertSelection,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Deselect All"),
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},
    .shortcut = H_ ("Deselect All|Escape"),

    .invoke = FVMenuDeselectAll,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Select by _Color"),
      .bg = COLOR_DEFAULT,
      .fg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true
    },

    .shortcut = H_ ("Select by Color|No Shortcut"),
    .sub = sclist,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Select by _Wildcard..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true
    },

    .shortcut = H_ ("Select by Wildcard...|No Shortcut"),
    .invoke = FVMenuSelectByName,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Select by _Script..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true
    },

    .shortcut = H_ ("Select by Script...|No Shortcut"),
    .invoke = FVMenuSelectByScript,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Glyphs Worth Outputting"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true
    },

    .shortcut = H_ ("Glyphs Worth Outputting|No Shortcut"),
        
    .invoke = FVMenuSelectWorthOutputting,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Glyphs with only _References"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Glyphs with only References|No Shortcut"),
        
    .invoke = FVMenuGlyphsRefs,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Glyphs with only S_plines"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Glyphs with only Splines|No Shortcut"),
        
    .invoke = FVMenuGlyphsSplines,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Glyphs with both"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Glyphs with both|No Shortcut"),
    .invoke = FVMenuGlyphsBoth,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("W_hitespace Glyphs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Whitespace Glyphs|No Shortcut"),
    .invoke = FVMenuGlyphsWhite,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Changed Glyphs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Changed Glyphs|No Shortcut"),
    .invoke = FVMenuSelectChanged,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Hinting Needed"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Hinting Needed|No Shortcut"),
    .invoke = FVMenuSelectHintingNeeded,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Autohinta_ble"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Autohintable|No Shortcut"),
    .invoke = FVMenuSelectAutohintable,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Hold [Shift] key to merge"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Hold [Control] key to restrict"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Selec_t By Lookup Subtable..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut = H_ ("Select By Lookup Subtable...|No Shortcut"),
        
    .invoke = FVMenuSelectByPST,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem edlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Undo"),
      .image = (GImage *) "editundo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'U'},

    .shortcut = H_ ("Undo|Ctl+Z"),
    .invoke = FVMenuUndo,
    .mid = MID_Undo},

  {
    .ti = {
      .text = (uint32_t *) N_("_Redo"),
      .image = (GImage *) "editredo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Redo|Ctl+Y"),
    .invoke = FVMenuRedo,
    .mid = MID_Redo},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Cu_t"),
      .image = (GImage *) "editcut.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 't'},

    .shortcut = H_ ("Cut|Ctl+X"),
    .invoke = FVMenuCut,
    .mid = MID_Cut},

  {
    .ti = {
      .text = (uint32_t *) N_("_Copy"),
      .image = (GImage *) "editcopy.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Copy|Ctl+C"),
    .invoke = FVMenuCopy,
    .mid = MID_Copy},

  {
    .ti = {
      .text = (uint32_t *) N_("C_opy Reference"),
      .image = (GImage *) "editcopyref.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Copy Reference|Ctl+G"),
    .invoke = FVMenuCopyRef,
    .mid = MID_CopyRef},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _Lookup Data"),
      .image = (GImage *) "editcopylookupdata.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Copy Lookup Data|Alt+Ctl+C"),
    .invoke = FVMenuCopyLookupData,
    .mid = MID_CopyLookupData},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _Width"),
      .image = (GImage *) "editcopywidth.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'W'},

    .shortcut = H_ ("Copy Width|Ctl+W"),
    .invoke = FVMenuCopyWidth,
    .mid = MID_CopyWidth},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _VWidth"),
      .image = (GImage *) "editcopyvwidth.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},

    .shortcut = H_ ("Copy VWidth|No Shortcut"),
    .invoke = FVMenuCopyWidth,
    .mid = MID_CopyVWidth},

  {
    .ti = {
      .text = (uint32_t *) N_("Co_py LBearing"),
      .image = (GImage *) "editcopylbearing.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'p'},

    .shortcut = H_ ("Copy LBearing|No Shortcut"),
    .invoke = FVMenuCopyWidth,
    .mid = MID_CopyLBearing},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy RBearin_g"),
      .image = (GImage *) "editcopyrbearing.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'g'},

    .shortcut = H_ ("Copy RBearing|No Shortcut"),
    .invoke = FVMenuCopyWidth,
    .mid = MID_CopyRBearing},

  {
    .ti = {
      .text = (uint32_t *) N_("_Paste"),
      .image = (GImage *) "editpaste.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Paste|Ctl+V"),
    .invoke = FVMenuPaste,
    .mid = MID_Paste},

  {
    .ti = {
      .text = (uint32_t *) N_("Paste Into"),
      .image = (GImage *) "editpasteinto.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Paste Into|Ctl+Shft+V"),
    .invoke = FVMenuPasteInto,
    .mid = MID_PasteInto},

  {
    .ti = {
      .text = (uint32_t *) N_("Paste After"),
      .image = (GImage *) "editpasteafter.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Paste After|Alt+Ctl+Shft+V"),
    .invoke = FVMenuPasteAfter,
    .mid = MID_PasteAfter},

  {
    .ti = {
      .text = (uint32_t *) N_("Sa_me Glyph As"),
      .image = (GImage *) "editsameas.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'm'},

    .shortcut = H_ ("Same Glyph As|No Shortcut"),
    .invoke = FVMenuSameGlyphAs,
    .mid = MID_SameGlyphAs},

  {
    .ti = {
      .text = (uint32_t *) N_("C_lear"),
      .image = (GImage *) "editclear.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},

    .shortcut = H_ ("Clear|No Shortcut"),
    .invoke = FVMenuClear,
    .mid = MID_Clear},

  {
    .ti = {
      .text = (uint32_t *) N_("Clear _Background"),
      .image = (GImage *) "editclearback.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Clear Background|No Shortcut"),
    .invoke = FVMenuClearBackground,
    .mid = MID_ClearBackground},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _Fg To Bg"),
      .image = (GImage *) "editcopyfg2bg.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Copy Fg To Bg|Ctl+Shft+C"),
    .invoke = FVMenuCopyFgBg,
    .mid = MID_CopyFgToBg},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy Layer To Layer"),
      .image = (GImage *) "editcopylayer2layer.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Copy Layer To Layer|No Shortcut"),
    .invoke = FVMenuCopyL2L,
    .mid = MID_CopyL2L},

  {
    .ti = {
      .text = (uint32_t *) N_("_Join"),
      .image = (GImage *) "editjoin.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'J'},

    .shortcut = H_ ("Join|Ctl+Shft+J"),
    .invoke = FVMenuJoin,
    .mid = MID_Join},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Select"),
      .image = (GImage *) "editselect.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},
    .sub = sllist,
    .moveto = sllistcheck_fv,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("F_ind / Replace..."),
      .image = (GImage *) "editfind.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'i'},

    .shortcut = H_ ("Find / Replace...|Alt+Ctl+F"),
    .invoke = FVMenuFindRpl,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Replace with Reference"),
      .image = (GImage *) "editrplref.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'i'},

    .shortcut = H_ ("Replace with Reference|Alt+Ctl+Shft+F"),
        
    .invoke = FVMenuReplaceWithRef,
    .mid = MID_RplRef},

  {
    .ti = {
      .text = (uint32_t *) N_("Correct References"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'i'},

    .shortcut = H_ ("Correct References|No Shortcut"),
    .invoke = FVMenuCorrectRefs,
    .mid = MID_CorrectRefs},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("U_nlink Reference"),
      .image = (GImage *) "editunlink.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'U'},

    .shortcut = H_ ("Unlink Reference|Ctl+U"),
    .invoke = FVMenuUnlinkRef,
    .mid = MID_UnlinkRef},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _From"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},
    .sub = cflist,
    .moveto = cflistcheck_fv,
    
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Remo_ve Undoes"),
      .image = (GImage *) "editrmundoes.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'e'},

    .shortcut = H_ ("Remove Undoes|No Shortcut"),
    .invoke = FVMenuRemoveUndoes,
    .mid = MID_RemoveUndoes},

  GMENUITEM_EMPTY
};

static GMenuItem smlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Simplify"),
      .image = (GImage *) "elementsimplify.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},

    .shortcut = H_ ("Simplify|Ctl+Shft+M"),
    .invoke = FVMenuSimplify,
    .mid = MID_Simplify},

  {
    .ti = {
      .text = (uint32_t *) N_("Simplify More..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},

    .shortcut = H_ ("Simplify More...|Alt+Ctl+Shft+M"),
    .invoke = FVMenuSimplifyMore,
    .mid = MID_SimplifyMore},

  {
    .ti = {
      .text = (uint32_t *) N_("Clea_nup Glyph"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'n'},

    .shortcut = H_ ("Cleanup Glyph|No Shortcut"),
    .invoke = FVMenuCleanup,
    .mid = MID_CleanupGlyph},

  {
    .ti = {
      .text = (uint32_t *) N_("Canonical Start _Point"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'n'},

    .shortcut = H_ ("Canonical Start Point|No Shortcut"),
    .invoke = FVMenuCanonicalStart,
    .mid = MID_CanonicalStart},

  {
    .ti = {
      .text = (uint32_t *) N_("Canonical _Contours"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'n'},

    .shortcut = H_ ("Canonical Contours|No Shortcut"),
    .invoke = FVMenuCanonicalContours,
    .mid = MID_CanonicalContours},

  GMENUITEM_EMPTY
};

static GMenuItem rmlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Remove Overlap"),
      .image = (GImage *) "overlaprm.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'O'},
    .shortcut = H_ ("Remove Overlap|Ctl+Shft+O"),
    .invoke = FVMenuOverlap,
    .mid = MID_RmOverlap},

  {
    .ti = {
      .text = (uint32_t *) N_("_Intersect"),
      .image = (GImage *) "overlapintersection.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Intersect|No Shortcut"),
    .invoke = FVMenuOverlap,

    .mid = MID_Intersection},

  {
    .ti = {
      .text = (uint32_t *) N_("_Find Intersections"),
      .image = (GImage *) "overlapfindinter.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'O'},
    .shortcut = H_ ("Find Intersections|No Shortcut"),
    .invoke = FVMenuOverlap,

    .mid = MID_FindInter},

  GMENUITEM_EMPTY
};

static GMenuItem eflist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Change _Weight..."),
      .image = (GImage *) "styleschangeweight.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Change Weight...|Ctl+Shft+!"),
    .invoke = FVMenuEmbolden,

    .mid = MID_Embolden},

  {
    .ti = {
      .text = (uint32_t *) N_("_Italic..."),
      .image = (GImage *) "stylesitalic.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Italic...|No Shortcut"),
    .invoke = FVMenuItalic,

    .mid = MID_Italic},

  {
    .ti = {
      .text = (uint32_t *) N_("Obli_que..."),
      .image = (GImage *) "stylesoblique.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Oblique...|No Shortcut"),
    .invoke = FVMenuOblique,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Condense/Extend..."),
      .image = (GImage *) "stylesextendcondense.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Condense...|No Shortcut"),

    .invoke = FVMenuCondense,
    .mid = MID_Condense},

  {
    .ti = {
      .text = (uint32_t *) N_("Change _X-Height..."),
      .image = (GImage *) "styleschangexheight.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Change XHeight...|No Shortcut"),
    .invoke = FVMenuChangeXHeight,

    .mid = MID_ChangeXHeight},

  {
    .ti = {
      .text = (uint32_t *) N_("Change _Glyph..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Change Glyph...|No Shortcut"),
    .invoke = FVMenuChangeGlyph,

    .mid = MID_ChangeGlyph},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Add _Small Capitals..."),
      .image = (GImage *) "stylessmallcaps.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Add Small Caps...|No Shortcut"),
    .invoke = FVMenuSmallCaps,

    .mid = MID_SmallCaps},

  {
    .ti = {
      .text = (uint32_t *) N_("Add Subscripts/Superscripts..."),
      .image = (GImage *) "stylessubsuper.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Add Subscripts/Superscripts...|No Shortcut"),
        
    .invoke = FVMenuSubSup,
    .mid = MID_SubSup},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("In_line..."),
      .image = (GImage *) "stylesinline.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Inline|No Shortcut"),
    .invoke = FVMenuInline,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Outline..."),
      .image = (GImage *) "stylesoutline.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Outline|No Shortcut"),
    .invoke = FVMenuOutline,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("S_hadow..."),
      .image = (GImage *) "stylesshadow.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Shadow|No Shortcut"),
    .invoke = FVMenuShadow,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Wireframe..."),
      .image = (GImage *) "styleswireframe.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Wireframe|No Shortcut"),
    .invoke = FVMenuWireframe,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem balist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Build Accented Glyph"),
      .image = (GImage *) "elementbuildaccent.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Build Accented Glyph|Ctl+Shft+A"),
    .invoke = FVMenuBuildAccent,

    .mid = MID_BuildAccent},

  {
    .ti = {
      .text = (uint32_t *) N_("Build _Composite Glyph"),
      .image = (GImage *) "elementbuildcomposite.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Build Composite Glyph|No Shortcut"),
    .invoke = FVMenuBuildComposite,

    .mid = MID_BuildComposite},

  {
    .ti = {
      .text = (uint32_t *) N_("Buil_d Duplicate Glyph"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Build Duplicate Glyph|No Shortcut"),
    .invoke = FVMenuBuildDuplicate,

    .mid = MID_BuildDuplicates},

#ifdef KOREAN
  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) _STR_ShowGrp,
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},
    .invoke = FVMenuShowGroup},

#endif
  GMENUITEM_EMPTY
};

static GMenuItem delist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_References..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'u'},

    .shortcut = H_ ("References...|Alt+Ctl+I"),
    .invoke = FVMenuShowDependentRefs,

    .mid = MID_ShowDependentRefs},

  {
    .ti = {
      .text = (uint32_t *) N_("_Substitutions..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Substitutions...|No Shortcut"),
    .invoke = FVMenuShowDependentSubs,

    .mid = MID_ShowDependentSubs},

  GMENUITEM_EMPTY
};

static GMenuItem trlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Transform..."),
      .image = (GImage *) "elementtransform.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut = H_ ("Transform...|Ctl+\\"),
    .invoke = FVMenuTransform,
    .mid = MID_Transform},

  {
    .ti = {
      .text = (uint32_t *) N_("_Point of View Projection..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut = H_ ("Point of View Projection...|Ctl+Shft+<"),
    .invoke = FVMenuPOV,

    .mid = MID_POV},

  {
    .ti = {
      .text = (uint32_t *) N_("_Non Linear Transform..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut = H_ ("Non Linear Transform...|Ctl+Shft+|"),
    .invoke = FVMenuNLTransform,

    .mid = MID_NLTransform},

  GMENUITEM_EMPTY
};

static GMenuItem rndlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("To _Int"),
      .image = (GImage *) "elementround.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("To Int|Ctl+Shft+_"),
    .invoke = FVMenuRound2Int,
    .mid = MID_Round},

  {
    .ti = {
      .text = (uint32_t *) N_("To _Hundredths"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("To Hundredths|No Shortcut"),
    .invoke = FVMenuRound2Hundredths,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Cluster"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Cluster|No Shortcut"),
    
    .invoke = FVMenuCluster,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem scollist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Color|Choose..."),
      .image = (GImage *) "colorwheel.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) -10,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Color Choose|No Shortcut"),
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Color|Default"),
      .image = (GImage *) "colordef.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Default|No Shortcut"),
    .invoke = FVMenuSetColor,

    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorwhite.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0xffffff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorred.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0xff0000,
      .image_precedes = true
    },
        
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorgreen.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata  =(void *) 0x00ff00,
      .image_precedes = true
    },
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorblue.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0x0000ff,
      .image_precedes = true
    },
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "coloryellow.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0xffff00,
      .image_precedes = true
    },
        
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colorcyan.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0x00ffff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSetColor,
    .mid = 0},

  {
    .ti = {
      .image = (GImage *) "colormagenta.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .userdata = (void *) 0xff00ff,
      .image_precedes = true
    },
        
    .invoke = FVMenuSetColor,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem infolist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_MATH Info..."),
      .image = (GImage *) "elementmathinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("MATH Info...|No Shortcut"),
    .invoke = FVMenuMATHInfo,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_BDF Info..."),
      .image = (GImage *) "elementbdfinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("BDF Info...|No Shortcut"),
    .invoke = FVMenuBDFInfo,

    .mid = MID_StrikeInfo},

  {
    .ti = {
      .text = (uint32_t *) N_("_Horizontal Baselines..."),
      .image = (GImage *) "elementhbaselines.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Horizontal Baselines...|No Shortcut"),
    .invoke = FVMenuBaseHoriz,

    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Vertical Baselines..."),
      .image = (GImage *) "elementvbaselines.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Vertical Baselines...|No Shortcut"),
    .invoke = FVMenuBaseVert,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Justification..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Justification...|No Shortcut"),
    .invoke = FVMenuJustify,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Show _Dependent"),
      .image = (GImage *) "elementshowdep.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .sub = delist,
    .moveto = delistcheck_fv,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Mass Glyph _Rename..."),
      .image = (GImage *) "elementrenameglyph.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Mass Glyph Rename...|No Shortcut"),
    .invoke = FVMenuMassRename,

    .mid = MID_MassRename},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _Color"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Set Color|No Shortcut"),

    .sub = scollist,
    .mid = MID_SetColor},

  GMENUITEM_EMPTY
};

static GMenuItem validlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Find Pr_oblems..."),
      .image = (GImage *) "elementfindprobs.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Find Problems...|Ctl+E"),
    .invoke = FVMenuFindProblems,

    .mid = MID_FindProblems},

  {
    .ti = {
      .text = (uint32_t *) N_("_Validate..."),
      .image = (GImage *) "elementvalidate.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Validate...|No Shortcut"),
    .invoke = FVMenuValidate,
    .mid = MID_Validate},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Set E_xtremum Bound..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Set Extremum bound...|No Shortcut"),
        
    .invoke = FVMenuSetExtremumBound,
    .mid = MID_SetExtremumBound},

  GMENUITEM_EMPTY
};

static GMenuItem ellist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Font Info..."),
      .image = (GImage *) "elementfontinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Font Info...|Ctl+Shft+F"),
    .invoke = FVMenuFontInfo,
    .mid = MID_FontInfo},

  {
    .ti = {
      .text = (uint32_t *) N_("_Glyph Info..."),
      .image = (GImage *) "elementglyphinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("Glyph Info...|Ctl+I"),
    .invoke = FVMenuCharInfo,
    .mid = MID_CharInfo},

  {
    .ti = {
      .text = (uint32_t *) N_("Other Info"),
      .image = (GImage *) "elementotherinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .sub = infolist,
    .moveto = infolistcheck_fv,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Validation"),
      .image = (GImage *) "elementvalidate.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .sub = validlist,
    .moveto = validlistcheck_fv,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Bitm_ap Strikes Available..."),
      .image = (GImage *) "elementbitmapsavail.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},

    .shortcut = H_ ("Bitmap Strikes Available...|Ctl+Shft+B"),
    .invoke = FVMenuBitmaps,

    .mid = MID_AvailBitmaps},

  {
    .ti = {
      .text = (uint32_t *) N_("Regenerate _Bitmap Glyphs..."),
      .image = (GImage *) "elementregenbitmaps.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Regenerate Bitmap Glyphs...|Ctl+B"),
    .invoke = FVMenuBitmaps,

    .mid = MID_RegenBitmaps},

  {
    .ti = {
      .text = (uint32_t *) N_("Remove Bitmap Glyphs..."),
      .image = (GImage *) "elementremovebitmaps.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Remove Bitmap Glyphs...|No Shortcut"),
    .invoke = FVMenuBitmaps,

    .mid = MID_RemoveBitmaps},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("St_yle"),
      .image = (GImage *) "elementstyles.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},
    .sub = eflist,

    .mid = MID_Styles},

  {
    .ti = {
      .text = (uint32_t *) N_("_Transformations"),
      .image = (GImage *) "elementtransform.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .sub = trlist,
    .moveto = trlistcheck_fv,
    .mid = MID_Transform},

  {
    .ti = {
      .text = (uint32_t *) N_("_Expand Stroke..."),
      .image = (GImage *) "elementexpandstroke.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'E'},
    .shortcut = H_ ("Expand Stroke...|Ctl+Shft+E"),

    .invoke = FVMenuStroke,
    .mid = MID_Stroke},

#ifdef FONTFORGE_CONFIG_TILEPATH
  {
    .ti = {
      .text = (uint32_t *) N_("Tile _Path..."),
      .image = (GImage *) "elementtilepath.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Tile Path...|No Shortcut"),
    .invoke = FVMenuTilePath,
    .mid = MID_TilePath},

  {
    .ti = {
      .text = (uint32_t *) N_("Tile Pattern..."),
      .image = (GImage *) "elementtilepattern.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .shortcut = H_ ("Tile Pattern...|No Shortcut"),
    .invoke = FVMenuPatternTile,

    .mid = 0},

#endif
  {
    .ti = {
      .text = (uint32_t *) N_("O_verlap"),
      .image = (GImage *) "overlaprm.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'O'},
    .sub = rmlist,

    .mid = MID_RmOverlap},

  {
    .ti = {
      .text = (uint32_t *) N_("_Simplify"),
      .image = (GImage *) "elementsimplify.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},

    .sub = smlist,
    .mid = MID_Simplify},

  {
    .ti = {
      .text = (uint32_t *) N_("Add E_xtrema"),
      .image = (GImage *) "elementaddextrema.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'x'},

    .shortcut = H_ ("Add Extrema|Ctl+Shft+X"),
    .invoke = FVMenuAddExtrema,

    .mid = MID_AddExtrema},

  {
    .ti = {
      .text = (uint32_t *) N_("Roun_d"),
      .image = (GImage *) "elementround.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .sub = rndlist,

    .mid = MID_Round},

  {
    .ti = {
      .text = (uint32_t *) N_("Autot_race"),
      .image = (GImage *) "elementautotrace.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'r'},

    .shortcut = H_ ("Autotrace|Ctl+Shft+T"),
    .invoke = FVMenuAutotrace,
    .mid = MID_Autotrace},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Correct Direction"),
      .image = (GImage *) "elementcorrectdir.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'D'},

    .shortcut = H_ ("Correct Direction|Ctl+Shft+D"),
    .invoke = FVMenuCorrectDir,

    .mid = MID_Correct},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("B_uild"),
      .image = (GImage *) "elementbuildaccent.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .sub = balist,
    .moveto = balistcheck_fv,
    .mid = MID_BuildAccent},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Merge Fonts..."),
      .image = (GImage *) "elementmergefonts.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},

    .shortcut = H_ ("Merge Fonts...|No Shortcut"),
    .invoke = FVMenuMergeFonts,
    .mid = MID_MergeFonts},

  {
    .ti = {
      .text = (uint32_t *) N_("Interpo_late Fonts..."),
      .image = (GImage *) "elementinterpolatefonts.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'p'},
    .shortcut = H_ ("Interpolate Fonts...|No Shortcut"),
    .invoke = FVMenuInterpFonts,
    .mid = MID_InterpolateFonts},

  {
    .ti = {
      .text = (uint32_t *) N_("Compare Fonts..."),
      .image = (GImage *) "elementcomparefonts.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'p'},

    .shortcut = H_ ("Compare Fonts...|No Shortcut"),
    .invoke = FVMenuCompareFonts,
    .mid = MID_FontCompare},

  {
    .ti = {
      .text = (uint32_t *) N_("Compare Layers..."),
      .image = (GImage *) "elementcomparelayers.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'p'},
    .shortcut = H_ ("Compare Layers...|No Shortcut"),
    .invoke = FVMenuCompareL2L,
    .mid = 0},

  GMENUITEM_EMPTY
};

static GMenuItem dummyall[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("All"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},
    .shortcut = H_ ("All|No Shortcut"),
  },

  GMENUITEM_EMPTY
};

// *INDENT-ON*

/* Builds up a menu containing all the anchor classes */
VISIBLE void
aplistbuild_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  GMenuItemArrayFree (mi->sub);
  mi->sub = NULL;

  _aplistbuild (mi, fv->b.sf, FVMenuAnchorPairs);
}

// *INDENT-OFF*

static GMenuItem cblist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Kern Pairs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},
    .shortcut = H_ ("Kern Pairs|No Shortcut"),
    .invoke = FVMenuKernPairs,
    .mid = MID_KernPairs},

  {
    .ti = {
      .text = (uint32_t *) N_("_Anchored Pairs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},
    .shortcut = H_ ("Anchored Pairs|No Shortcut"),
    .sub = dummyall,
    .moveto = aplistbuild_fv,
    .mid = MID_AnchorPairs},

  {
    .ti = {
      .text = (uint32_t *) N_("_Ligatures"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'L'},
    .shortcut = H_ ("Ligatures|No Shortcut"),
    .invoke = FVMenuLigatures,
    .mid = MID_Ligatures},

  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
cblistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *sf = fv->b.sf;
  bool anyligs = false;
  bool anykerns = false;
  PST *pst;

  if (sf->kerns)
    anykerns = true;

  for (enc_iter_t p = enc_iter (fv->b.map); !enc_done (p); p = enc_next (p))
    {
      int gid = enc_gid (p);
      if (gid != -1 && sf->glyphs[gid] != NULL)
        {
          for (pst = sf->glyphs[gid]->possub; pst != NULL; pst = pst->next)
            {
              if (pst->type == pst_ligature)
                {
                  anyligs = true;
                  if (anykerns)
                    break;
                }
            }
          if (sf->glyphs[gid]->kerns != NULL)
            {
              anykerns = true;
              if (anyligs)
                break;
            }
        }
    }

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Ligatures:
          mi->ti.disabled = !anyligs;
          break;
        case MID_KernPairs:
          mi->ti.disabled = !anykerns;
          break;
        case MID_AnchorPairs:
          mi->ti.disabled = sf->anchor == NULL;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem gllist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Glyph Image"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},
    .shortcut = H_ ("Glyph Image|No Shortcut"),
    .invoke = FVMenuGlyphLabel,
    .mid = gl_glyph},

  {
    .ti = {
      .text = (uint32_t *) N_("_Name"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},
    .shortcut = H_ ("Name|No Shortcut"),
    .invoke = FVMenuGlyphLabel,
    .mid = gl_name},

  {
    .ti = {
      .text = (uint32_t *) N_("_Unicode"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'L'},
    .shortcut = H_ ("Unicode|No Shortcut"),
    .invoke = FVMenuGlyphLabel,
    .mid = gl_unicode},

  {
    .ti = {
      .text = (uint32_t *) N_("_Encoding Hex"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'L'},
    .shortcut = H_ ("Encoding Hex|No Shortcut"),
    .invoke = FVMenuGlyphLabel,
    .mid = gl_encoding},

  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
gllistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      mi->ti.checked = fv->glyphlabel == mi->mid;
    }
}

static GMenuItem emptymenu[] = {
  GMENUITEM_LINE,
  GMENUITEM_EMPTY
};

VISIBLE void
FVEncodingMenuBuild (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (mi->sub != NULL)
    {
      GMenuItemArrayFree (mi->sub);
      mi->sub = NULL;
    }
  mi->sub = GetEncodingMenu (FVMenuReencode, fv->b.map->enc);
}

VISIBLE void
FVMenuAddUnencoded (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char *ret, *end;
  int cnt;

  ret =
    gwwv_ask_string
    (_("Add Encoding Slots..."), "1",
     fv->b.cidmaster ? _("How many CID slots do you wish to add?")
     : _("How many unencoded glyph slots do you wish to add?"));
  if (ret == NULL)
    return;
  cnt = strtol (ret, &end, 10);
  if (*end != '\0' || cnt <= 0)
    {
      free (ret);
      ff_post_error (_("Bad Number"), _("Bad Number"));
      return;
    }
  free (ret);
  FVAddUnencoded ((FontViewBase *) fv, cnt);
}

VISIBLE void
FVMenuRemoveUnused (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVRemoveUnused ((FontViewBase *) fv);
}

VISIBLE void
FVMenuCompact (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineChar *sc;

  sc = FVFindACharInDisplay (fv);
  FVCompact ((FontViewBase *) fv);
  if (sc != NULL)
    {
      int enc = gid_to_enc (fv->b.map, sc->orig_pos);
      if (enc != -1)
        FVScrollToChar (fv, enc);
    }
}

VISIBLE void
FVMenuDetachGlyphs (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  FVDetachGlyphs ((FontViewBase *) fv);
}

VISIBLE void
FVMenuDetachAndRemoveGlyphs (GWindow gw, struct gmenuitem *UNUSED (mi),
                             GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char *buts[3];

  buts[0] = _("_Remove");
  buts[1] = _("_Cancel");
  buts[2] = NULL;

  if (gwwv_ask
      (_("Detach & Remove Glyphs"), (const char **) buts, 0, 1,
       _
       ("Are you sure you wish to remove these glyphs? This operation cannot be undone."))
      == 1)
    return;

  FVDetachAndRemoveGlyphs ((FontViewBase *) fv);
}

VISIBLE void
FVForceEncodingMenuBuild (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if (mi->sub != NULL)
    {
      GMenuItemArrayFree (mi->sub);
      mi->sub = NULL;
    }
  mi->sub = GetEncodingMenu (FVMenuForceEncode, fv->b.map->enc);
}

VISIBLE void
FVMenuAddEncodingName (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
                       GEvent *UNUSED (e))
{
  char *ret;
  Encoding *enc;

  /* Search the iconv database for the named encoding */
  ret =
    gwwv_ask_string (_("Add Encoding Name..."), NULL,
                     _
                     ("Please provide the name of an encoding in the iconv database which you want in the menu."));
  if (ret == NULL)
    return;
  enc = FindOrMakeEncoding (ret);
  if (enc == NULL)
    ff_post_error (_("Invalid Encoding"), _("Invalid Encoding"));
  free (ret);
}

VISIBLE void
FVMenuLoadEncoding (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  LoadEncodingFile ();
}

VISIBLE void
FVMenuMakeFromFont (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  (void) MakeEncoding (fv->b.sf, fv->b.map);
}

VISIBLE void
FVMenuRemoveEncoding (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
                      GEvent *UNUSED (e))
{
  RemoveEncoding ();
}

VISIBLE void
FVMenuMakeNamelist (GWindow gw, struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char *filename;
  FILE *file;

  char buffer[strlen (getUserDataDir ()) + strlen (fv->b.sf->fontname) + 20];
  sprintf (buffer, "%s/%s.nam", getUserDataDir (), fv->b.sf->fontname);

  filename =
    gwwv_save_filename (_("Make Namelist"),
                        x_gc_u8_strconv_from_locale (buffer), "*.nam");
  if (filename == NULL)
    return;
  file = fopen (x_gc_u8_strconv_from_locale (filename), "w");
  if (file == NULL)
    {
      ff_post_error (_("Namelist creation failed"), _("Could not write %s"),
                     filename);
      free (filename);
      return;
    }
  FVB_MakeNamelist ((FontViewBase *) fv, file);
  fclose (file);
}

VISIBLE void
FVMenuLoadNamelist (GWindow UNUSED (gw), struct gmenuitem *UNUSED (mi),
                    GEvent *UNUSED (e))
{
  /* Read in a name list and copy it into the prefs dir so that we'll find */
  /*  it in the future */
  /* Be prepared to update what we've already got if names match */
  char buffer[1025];
  char *ret = gwwv_open_filename (_("Load Namelist"), NULL,
                                  "*.nam", NULL);
  char *temp, *pt;
  char *buts[3];
  FILE *old, *new;
  int ch, ans;
  NameList *nl;

  if (ret == NULL)
    return;                     /* Cancelled */
  temp = utf82def_copy (ret);
  pt = strrchr (temp, '/');
  if (pt == NULL)
    pt = temp;
  else
    ++pt;
  snprintf (buffer, sizeof (buffer), "%s/%s", getUserDataDir (), pt);
  if (access (buffer, F_OK) == 0)
    {
      buts[0] = _("_Replace");
      buts[1] = _("_Cancel");
      buts[2] = NULL;
      ans =
        gwwv_ask (_("Replace"), (const char **) buts, 0, 1,
                  _("A name list with this name already exists. Replace it?"));
      if (ans == 1)
        {
          free (temp);
          free (ret);
          return;
        }
    }

  old = fopen (temp, "r");
  if (old == NULL)
    {
      ff_post_error (_("No such file"), _("Could not read %s"), ret);
      free (ret);
      free (temp);
      return;
    }
  if ((nl = LoadNamelist (temp)) == NULL)
    {
      ff_post_error (_("Bad namelist file"), _("Could not parse %s"), ret);
      free (ret);
      free (temp);
      fclose (old);
      return;
    }
  free (ret);
  free (temp);
  if (nl->uses_unicode)
    {
      if (nl->a_utf8_name != NULL)
        ff_post_notice (_("Non-ASCII glyphnames"),
                        _
                        ("This namelist contains at least one non-ASCII glyph name, namely: %s"),
                        nl->a_utf8_name);
      else
        ff_post_notice (_("Non-ASCII glyphnames"),
                        _
                        ("This namelist is based on a namelist which contains non-ASCII glyph names"));
    }

  new = fopen (buffer, "w");
  if (new == NULL)
    {
      ff_post_error (_("Create failed"), _("Could not write %s"), buffer);
      fclose (old);
      return;
    }

  while ((ch = getc (old)) != EOF)
    putc (ch, new);
  fclose (old);
  fclose (new);
}

VISIBLE void
FVMenuRenameByNamelist (GWindow gw, struct gmenuitem *UNUSED (mi),
                        GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  char **namelists = AllNamelistNames ();
  int i;
  int ret;
  NameList *nl;
  extern int allow_utf8_glyphnames;

  for (i = 0; namelists[i] != NULL; ++i);
  ret =
    gwwv_choose (_("Rename by NameList"), (const char **) namelists, i, 0,
                 _
                 ("Rename the glyphs in this font to the names found in the selected namelist"));
  if (ret == -1)
    return;
  nl = NameListByName (namelists[ret]);
  if (nl == NULL)
    {
      IError ("Couldn't find namelist");
      return;
    }
  else if (nl != NULL && nl->uses_unicode && !allow_utf8_glyphnames)
    {
      ff_post_error (_("Namelist contains non-ASCII names"),
                     _
                     ("Glyph names should be limited to characters in the ASCII character set, but there are names in this namelist which use characters outside that range."));
      return;
    }
  SFRenameGlyphsToNamelist (fv->b.sf, nl);
  GDrawRequestExpose (fv->v, NULL, false);
}

VISIBLE void
FVMenuNameGlyphs (GWindow gw, struct gmenuitem *UNUSED (mi), GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  /* Read a file containing a list of names, and add an unencoded glyph for */
  /*  each name */
  char buffer[33];
  char *ret = gwwv_open_filename (_("Load glyph names"), NULL, "*", NULL);
  char *temp, *pt;
  FILE *file;
  int ch;
  SplineChar *sc;
  FontView *fvs;

  if (ret == NULL)
    return;                     /* Cancelled */
  temp = utf82def_copy (ret);

  file = fopen (temp, "r");
  if (file == NULL)
    {
      ff_post_error (_("No such file"), _("Could not read %s"), ret);
      free (ret);
      free (temp);
      return;
    }
  pt = buffer;
  while (true)
    {
      ch = getc (file);
      if (ch != EOF && !isspace (ch))
        {
          if (pt < buffer + sizeof (buffer) - 1)
            *pt++ = ch;
        }
      else
        {
          if (pt != buffer)
            {
              *pt = '\0';
              sc = NULL;
              for (fvs = (FontView *) (fv->b.sf->fv); fvs != NULL;
                   fvs = (FontView *) (fvs->b.nextsame))
                {
                  EncMap *map = fvs->b.map;

                  // FIXME: It is unlikely this actually needs to be
                  // done, because such an entry should have been
                  // removed already:
                  remove_enc_to_gid (map, map->enc_limit);

                  fvs->b.selected =
                    xrealloc (fvs->b.selected, (map->enc_limit + 1));
                  memset (fvs->b.selected + map->enc_limit, 0, 1);

                  ++map->enc_limit;

                  if (sc == NULL)
                    {
                      sc = SFMakeChar (fv->b.sf, map, map->enc_limit - 1);
                      free (sc->name);
                      sc->name = xstrdup_or_null (buffer);
                      sc->comment = xstrdup (".");      /* Mark as something for sfd file */
                      /*SCLigDefault(sc); */
                    }

                  set_enc_to_gid (map, map->enc_limit - 1, sc->orig_pos);
                  set_gid_to_enc (map, sc->orig_pos, map->enc_limit - 1);
                }
              pt = buffer;
            }
          if (ch == EOF)
            break;
        }
    }
  fclose (file);
  free (ret);
  free (temp);
  FontViewReformatAll (fv->b.sf);
}

// *INDENT-OFF*

static GMenuItem enlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Reencode"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'E'},
    .shortcut = H_ ("Reencode|No Shortcut"),
    .sub = emptymenu,
    .moveto = FVEncodingMenuBuild,
    .mid = MID_Reencode},

  {
    .ti = {
      .text = (uint32_t *) N_("_Compact"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Compact|No Shortcut"),
    .invoke = FVMenuCompact,
    .mid = MID_Compact},

  {
    .ti = {
      .text = (uint32_t *) N_("_Force Encoding"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Force Encoding|No Shortcut"),
    .sub = emptymenu,
    .moveto = FVForceEncodingMenuBuild,
    .mid = MID_ForceReencode},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Add Encoding Slots..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Add Encoding Slots...|No Shortcut"),
    .invoke = FVMenuAddUnencoded,
    .mid = MID_AddUnencoded},

  {
    .ti = {
      .text = (uint32_t *) N_("Remove _Unused Slots"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Remove Unused Slots|No Shortcut"),
    .invoke = FVMenuRemoveUnused,
    .mid = MID_RemoveUnused},

  {
    .ti = {
      .text = (uint32_t *) N_("_Detach Glyphs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Detach Glyphs|No Shortcut"),
    .invoke = FVMenuDetachGlyphs,
    .mid = MID_DetachGlyphs},

  {
    .ti = {
      .text = (uint32_t *) N_("Detach & Remo_ve Glyphs..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Detach & Remove Glyphs...|No Shortcut"),
    .invoke = FVMenuDetachAndRemoveGlyphs,
    .mid = MID_DetachAndRemoveGlyphs},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Add E_ncoding Name..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Add Encoding Name...|No Shortcut"),
    .invoke = FVMenuAddEncodingName,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_Load Encoding..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Load Encoding...|No Shortcut"),
    .invoke = FVMenuLoadEncoding,
    .mid = MID_LoadEncoding},

  {
    .ti = {
      .text = (uint32_t *) N_("Ma_ke From Font..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Make From Font...|No Shortcut"),
    .invoke = FVMenuMakeFromFont,
    .mid = MID_MakeFromFont},

  {
    .ti = {
      .text = (uint32_t *) N_("Remove En_coding..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Remove Encoding...|No Shortcut"),
    .invoke = FVMenuRemoveEncoding,
    .mid = MID_RemoveEncoding},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Display By _Groups..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Display By Groups...|No Shortcut"),
    .invoke = FVMenuDisplayByGroups,
    .mid = MID_DisplayByGroups},

  {
    .ti = {
      .text = (uint32_t *) N_("D_efine Groups..."),
      NULL,
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Define Groups...|No Shortcut"),
    .invoke = FVMenuDefineGroups,
    .mid = 0},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Save Namelist of Font..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Save Namelist of Font...|No Shortcut"),
    .invoke = FVMenuMakeNamelist,
    .mid = MID_SaveNamelist},

  {
    .ti = {
      .text = (uint32_t *) N_("L_oad Namelist..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Load Namelist...|No Shortcut"),
    .invoke = FVMenuLoadNamelist,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("Rename Gl_yphs..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Rename Glyphs...|No Shortcut"),
    .invoke = FVMenuRenameByNamelist,
    .mid = MID_RenameGlyphs},

  {
    .ti = {
      .text = (uint32_t *) N_("Cre_ate Named Glyphs..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut = H_ ("Create Named Glyphs...|No Shortcut"),
    .invoke = FVMenuNameGlyphs,
    .mid = MID_NameGlyphs},

  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
enlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int gid;
  SplineFont *sf = fv->b.sf;
  EncMap *map = fv->b.map;
  bool anyglyphs = false;

  for (enc_iter_t p = enc_iter (map); !enc_done (p); p = enc_next (p))
    if (enc_gid (p) != -1 && fv->b.selected[enc_enc (p)])
      anyglyphs = true;

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Compact:
          mi->ti.checked = fv->b.normal != NULL;
          break;
        case MID_Reencode:
        case MID_ForceReencode:
          mi->ti.disabled = fv->b.cidmaster != NULL;
          break;
#if 0
        case MID_AddUnencoded:
          mi->ti.disabled = fv->b.normal != NULL;
          break;
#endif
        case MID_DetachGlyphs:
        case MID_DetachAndRemoveGlyphs:
          mi->ti.disabled = !anyglyphs;
          break;
        case MID_RemoveUnused:
          gid = map->enc_limit > 0 ? enc_to_gid (map, map->enc_limit - 1) : -1;
          mi->ti.disabled = gid != -1 && SCWorthOutputting (sf->glyphs[gid]);
          break;
        case MID_MakeFromFont:
          mi->ti.disabled = fv->b.cidmaster != NULL || map->enc_limit > 1024
            || map->enc != &custom;
          break;
        case MID_RemoveEncoding:
          break;
        case MID_DisplayByGroups:
          mi->ti.disabled = fv->b.cidmaster != NULL || group_root == NULL;
          break;
        case MID_NameGlyphs:
          mi->ti.disabled = fv->b.normal != NULL || fv->b.cidmaster != NULL;
          break;
        case MID_RenameGlyphs:
        case MID_SaveNamelist:
          mi->ti.disabled = fv->b.cidmaster != NULL;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem lylist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Layer|Foreground"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = true,
      .checkable = true,
      .checked = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true
    },
    .invoke = FVMenuChangeLayer,
    .mid = ly_fore
  },

  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
lylistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  SplineFont *sf = fv->b.sf;
  int ly;
  GMenuItem *sub;

  sub = xcalloc (sf->layer_cnt + 1, sizeof (GMenuItem));
  for (ly = ly_fore; ly < sf->layer_cnt; ++ly)
    {
      sub[ly - 1].ti.text = utf82u_copy (sf->layers[ly].name);
      sub[ly - 1].ti.checkable = true;
      sub[ly - 1].ti.checked = ly == fv->b.active_layer;
      sub[ly - 1].invoke = FVMenuChangeLayer;
      sub[ly - 1].mid = ly;
      sub[ly - 1].ti.fg = sub[ly - 1].ti.bg = COLOR_DEFAULT;
    }
  GMenuItemArrayFree (mi->sub);
  mi->sub = sub;
}

// *INDENT-OFF*

static GMenuItem vwlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Next Glyph"),
      .image = (GImage *) "viewnext.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'N'},

    .shortcut = H_ ("Next Glyph|Ctl+]"),
    .invoke = FVMenuChangeChar,
    .mid = MID_Next},

  {
    .ti = {
      .text = (uint32_t *) N_("_Prev Glyph"),
      .image = (GImage *) "viewprev.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Prev Glyph|Ctl+["),
    .invoke = FVMenuChangeChar,
    .mid = MID_Prev},

  {
    .ti = {
      .text = (uint32_t *) N_("Next _Defined Glyph"),
      .image = (GImage *) "viewnextdef.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'D'},

    .shortcut = H_ ("Next Defined Glyph|Alt+Ctl+]"),
    .invoke = FVMenuChangeChar,
    .mid = MID_NextDef},

  {
    .ti = {
      .text = (uint32_t *) N_("Prev Defined Gl_yph"),
      .image = (GImage *) "viewprevdef.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'a'},

    .shortcut = H_ ("Prev Defined Glyph|Alt+Ctl+["),
    .invoke = FVMenuChangeChar,
    .mid = MID_PrevDef},

  {
    .ti = {
      .text = (uint32_t *) N_("_Goto"),
      .image = (GImage *) "viewgoto.png",
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'G'},

    .shortcut = H_ ("Goto|Ctl+Shft+>"),
    .invoke = FVMenuGotoChar
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Layers"),
      .image = (GImage *) "viewlayers.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .sub = lylist,
    .moveto = lylistcheck_fv
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Show ATT"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'S'},
    .shortcut = H_ ("Show ATT|No Shortcut"),
    .invoke = FVMenuShowAtt
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Display S_ubstitutions..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'u'},

    .shortcut = H_ ("Display Substitutions...|No Shortcut"),
    .invoke = FVMenuDisplaySubs,
    .mid = MID_DisplaySubs},

  {
    .ti = {
      .text = (uint32_t *) N_("Com_binations"),
            .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'b'},
    .sub = cblist,
    .moveto = cblistcheck_fv
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Label Gl_yph By"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'b'},
    .sub = gllist,
    .moveto = gllistcheck_fv
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("S_how H. Metrics..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},

    .shortcut = H_ ("Show H. Metrics...|No Shortcut"),
    .invoke = FVMenuShowMetrics,
    .mid = MID_ShowHMetrics},

  {
    .ti = {
      .text = (uint32_t *) N_("Show _V. Metrics..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},

    .shortcut = H_ ("Show V. Metrics...|No Shortcut"),
    .invoke = FVMenuShowMetrics,
    .mid = MID_ShowVMetrics},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("32x8 cell window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '2'},

    .shortcut = H_ ("32x8 cell window|Ctl+Shft+%"),
    .invoke = FVMenuWSize,
    .mid = MID_32x8},

  {
    .ti = {
      .text = (uint32_t *) N_("_16x4 cell window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '3'},

    .shortcut = H_ ("16x4 cell window|Ctl+Shft+^"),
    .invoke = FVMenuWSize,
    .mid = MID_16x4},

  {
    .ti = {
      .text = (uint32_t *) N_("_8x2  cell window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '3'},

    .shortcut = H_ ("8x2  cell window|Ctl+Shft+*"),
    .invoke = FVMenuWSize,
    .mid = MID_8x2},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_24 pixel outline"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '2'},

    .shortcut = H_ ("24 pixel outline|Ctl+2"),
    .invoke = FVMenuSize,
    .mid = MID_24},

  {
    .ti = {
      .text = (uint32_t *) N_("_36 pixel outline"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '3'},

    .shortcut = H_ ("36 pixel outline|Ctl+3"),
    .invoke = FVMenuSize,
    .mid = MID_36},

  {
    .ti = {
      .text = (uint32_t *) N_("_48 pixel outline"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '4'},

    .shortcut = H_ ("48 pixel outline|Ctl+4"),
    .invoke = FVMenuSize,
    .mid = MID_48},

  {
    .ti = {
      .text = (uint32_t *) N_("_72 pixel outline"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '4'},

    .shortcut = H_ ("72 pixel outline|Ctl+7"),
    .invoke = FVMenuSize,
    .mid = MID_72},

  {
    .ti = {
      .text = (uint32_t *) N_("_96 pixel outline"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '4'},

    .shortcut = H_ ("96 pixel outline|Ctl+9"),
    .invoke = FVMenuSize,
    .mid = MID_96},

  {
    .ti = {
      .text = (uint32_t *) N_("_Anti Alias"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},
    .shortcut = H_ ("Anti Alias|Ctl+5"),
    .invoke = FVMenuSize,
    .mid = MID_AntiAlias},

  {
    .ti = {
      .text = (uint32_t *) N_("_Fit to font bounding box"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},
    .shortcut = H_ ("Fit to font bounding box|Ctl+6"),
    .invoke = FVMenuSize,
    .mid = MID_FitToBbox},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Bitmap _Magnification..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .checkable = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("Bitmap Magnification...|No Shortcut"),
    .invoke = FVMenuMagnify,
    .mid = MID_BitmapMag},

  /* Some extra room to show bitmaps */
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
vwlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);
  int i, base;
  BDFFont *bdf;
  char buffer[50];
  int pos;
  SplineFont *sf = fv->b.sf;
  SplineFont *master = sf->cidmaster ? sf->cidmaster : sf;
  EncMap *map = fv->b.map;
  OTLookup *otl;

  for (i = 0;
       vwlist[i].ti.text == NULL
       || strcmp ((char *) vwlist[i].ti.text,
                  _("Bitmap _Magnification...")) != 0; ++i);
  base = i + 1;
  for (i = base; vwlist[i].ti.text != NULL; ++i)
    {
      free (vwlist[i].ti.text);
      vwlist[i].ti.text = NULL;
    }

  vwlist[base - 1].ti.disabled = true;
  if (master->bitmaps != NULL)
    {
      for (bdf = master->bitmaps, i = base;
           i < sizeof (vwlist) / sizeof (vwlist[0]) - 1 && bdf != NULL;
           ++i, bdf = bdf->next)
        {
          if (BDFDepth (bdf) == 1)
            sprintf (buffer, _("%d pixel bitmap"), bdf->pixelsize);
          else
            sprintf (buffer, _("%d@%d pixel bitmap"), bdf->pixelsize,
                     BDFDepth (bdf));
          vwlist[i].ti.text = (uint32_t *) utf82u_copy (buffer);
          vwlist[i].ti.checkable = true;
          vwlist[i].ti.checked = bdf == fv->show;
          vwlist[i].ti.userdata = bdf;
          vwlist[i].invoke = FVMenuShowBitmap;
          vwlist[i].ti.fg = vwlist[i].ti.bg = COLOR_DEFAULT;
          if (bdf == fv->show)
            vwlist[base - 1].ti.disabled = false;
        }
    }
  GMenuItemArrayFree (mi->sub);
  mi->sub = GMenuItemArrayCopy (vwlist, NULL);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Next:
        case MID_Prev:
          mi->ti.disabled = anychars < 0;
          break;
        case MID_NextDef:
          pos = anychars + 1;
          if (anychars < 0)
            pos = map->enc_limit;
          for (;
               pos < map->enc_limit
               && (enc_to_gid (map, pos) == -1
                   || !SCWorthOutputting (sf->glyphs[enc_to_gid (map, pos)]));
               ++pos);
          mi->ti.disabled = pos == map->enc_limit;
          break;
        case MID_PrevDef:
          for (pos = anychars - 1;
               pos >= 0
               && (enc_to_gid (map, pos) == -1
                   || !SCWorthOutputting (sf->glyphs[enc_to_gid (map, pos)]));
               --pos);
          mi->ti.disabled = pos < 0;
          break;
        case MID_DisplaySubs:
          {
            SplineFont *_sf = sf;
            mi->ti.checked = fv->cur_subtable != NULL;
            if (_sf->cidmaster)
              _sf = _sf->cidmaster;
            for (otl = _sf->gsub_lookups; otl != NULL; otl = otl->next)
              if (otl->lookup_type == gsub_single && otl->subtables != NULL)
                break;
            mi->ti.disabled = otl == NULL;
          }
          break;
        case MID_ShowHMetrics:
          /*mi->ti.checked = fv->showhmetrics; */
          break;
        case MID_ShowVMetrics:
          /*mi->ti.checked = fv->showvmetrics; */
          mi->ti.disabled = !sf->hasvmetrics;
          break;
        case MID_32x8:
          mi->ti.checked = (fv->rowcnt == 8 && fv->colcnt == 32);
          mi->ti.disabled = fv->b.container != NULL;
          break;
        case MID_16x4:
          mi->ti.checked = (fv->rowcnt == 4 && fv->colcnt == 16);
          mi->ti.disabled = fv->b.container != NULL;
          break;
        case MID_8x2:
          mi->ti.checked = (fv->rowcnt == 2 && fv->colcnt == 8);
          mi->ti.disabled = fv->b.container != NULL;
          break;
        case MID_24:
          mi->ti.checked = (fv->show != NULL && fv->show == fv->filled
                            && fv->show->pixelsize == 24);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_36:
          mi->ti.checked = (fv->show != NULL && fv->show == fv->filled
                            && fv->show->pixelsize == 36);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_48:
          mi->ti.checked = (fv->show != NULL && fv->show == fv->filled
                            && fv->show->pixelsize == 48);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_72:
          mi->ti.checked = (fv->show != NULL && fv->show == fv->filled
                            && fv->show->pixelsize == 72);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_96:
          mi->ti.checked = (fv->show != NULL && fv->show == fv->filled
                            && fv->show->pixelsize == 96);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_AntiAlias:
          mi->ti.checked = (fv->show != NULL && fv->show->clut != NULL);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_FitToBbox:
          mi->ti.checked = (fv->show != NULL && fv->show->bbsized);
          mi->ti.disabled = sf->onlybitmaps && fv->show != fv->filled;
          break;
        case MID_Layers:
          mi->ti.disabled = sf->layer_cnt <= 2 || sf->multilayer;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem histlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_HStem"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},
    .shortcut = H_ ("HStem|No Shortcut"),
    .invoke = FVMenuHistograms,
    .mid = MID_HStemHist},

  {
    .ti = {
      .text = (uint32_t *) N_("_VStem"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},
    .shortcut = H_ ("VStem|No Shortcut"),
    .invoke = FVMenuHistograms,
    .mid = MID_VStemHist},

  {
    .ti = {
      .text = (uint32_t *) N_("BlueValues"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},
    .shortcut = H_ ("BlueValues|No Shortcut"),
    .invoke = FVMenuHistograms,
    .mid = MID_BlueValuesHist},

  GMENUITEM_EMPTY
};

static GMenuItem htlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Auto_Hint"),
      .image = (GImage *) "hintsautohint.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},
    .shortcut = H_ ("AutoHint|Ctl+Shft+H"),
    .invoke = FVMenuAutoHint,
    .mid = MID_AutoHint},

  {
    .ti = {
      .text = (uint32_t *) N_("Hint _Substitution Pts"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},

    .shortcut = H_ ("Hint Substitution Pts|No Shortcut"),
    .invoke = FVMenuAutoHintSubs,
    .mid = MID_HintSubsPt},

  {
    .ti = {
      .text = (uint32_t *) N_("Auto _Counter Hint"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},

    .shortcut = H_ ("Auto Counter Hint|No Shortcut"),
    .invoke = FVMenuAutoCounter,

    .mid = MID_AutoCounter},

  {
    .ti = {
      .text = (uint32_t *) N_("_Don't AutoHint"),
      .image = (GImage *) "hintsdontautohint.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},

    .shortcut = H_ ("Don't AutoHint|No Shortcut"),
    .invoke = FVMenuDontAutoHint,

    .mid = MID_DontAutoHint},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Auto_Instr"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},
    .shortcut = H_ ("AutoInstr|Ctl+T"),
        
    .invoke = FVMenuAutoInstr,
    .mid = MID_AutoInstr},

  {
    .ti = {
      .text = (uint32_t *) N_("_Edit Instructions..."),
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},

    .shortcut = H_ ("Edit Instructions...|No Shortcut"),
    .invoke = FVMenuEditInstrs,

    .mid = MID_EditInstructions},

  {
    .ti = {
      .text = (uint32_t *) N_("Edit 'fpgm'..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Edit 'fpgm'...|No Shortcut"),
    .invoke = FVMenuEditTable,

    .mid = MID_Editfpgm},

  {
    .ti = {
      .text = (uint32_t *) N_("Edit 'prep'..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Edit 'prep'...|No Shortcut"),
    .invoke = FVMenuEditTable,

    .mid = MID_Editprep},

  {
    .ti = {
      .text = (uint32_t *) N_("Edit 'maxp'..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Edit 'maxp'...|No Shortcut"),
    .invoke = FVMenuEditTable,

    .mid = MID_Editmaxp},

  {
    .ti = {
      .text = (uint32_t *) N_("Edit 'cvt '..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Edit 'cvt '...|No Shortcut"),
    .invoke = FVMenuEditTable,

    .mid = MID_Editcvt},

  {
    .ti = {
      .text = (uint32_t *) N_("Remove Instr Tables"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},

    .shortcut = H_ ("Remove Instr Tables|No Shortcut"),
    .invoke = FVMenuRmInstrTables,

    .mid = MID_RmInstrTables},

  {
    .ti = {
      .text = (uint32_t *) N_("S_uggest Deltas..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},

    .shortcut = H_ ("Suggest Deltas|No Shortcut"),
    .invoke = FVMenuDeltas,
    .mid = MID_Deltas},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Clear Hints"),
      .image = (GImage *) "hintsclearvstems.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Clear Hints|No Shortcut"),
    .invoke = FVMenuClearHints,

    .mid = MID_ClearHints},

  {
    .ti = {
      .text = (uint32_t *) N_("Clear Instructions"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Clear Instructions|No Shortcut"),
    .invoke = FVMenuClearInstrs,

    .mid = MID_ClearInstrs},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Histograms"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .sub = histlist
  },

  GMENUITEM_EMPTY
};

static GMenuItem mtlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Center in Width"),
      .image = (GImage *) "metricscenter.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Center in Width|No Shortcut"),
    .invoke = FVMenuCenter,
    .mid = MID_Center},

  {
    .ti = {
      .text = (uint32_t *) N_("_Thirds in Width"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut = H_ ("Thirds in Width|No Shortcut"),
    .invoke = FVMenuCenter,
    .mid = MID_Thirds},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _Width..."),
      .image = (GImage *) "metricssetwidth.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'W'},

    .shortcut = H_ ("Set Width...|Ctl+Shft+L"),
    .invoke = FVMenuSetWidth,
    .mid = MID_SetWidth},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _LBearing..."),
      .image = (GImage *) "metricssetlbearing.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'L'},

    .shortcut = H_ ("Set LBearing...|Ctl+L"),
    .invoke = FVMenuSetWidth,
    .mid = MID_SetLBearing},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _RBearing..."),
      .image = (GImage *) "metricssetrbearing.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Set RBearing...|Ctl+R"),
    .invoke = FVMenuSetWidth,
    .mid = MID_SetRBearing},

  {
    .ti = {
      .text = (uint32_t *) N_("Set Both Bearings..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},

    .shortcut = H_ ("Set Both Bearings...|No Shortcut"),
    .invoke = FVMenuSetWidth,

    .mid = MID_SetBearings},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Set _Vertical Advance..."),

      .image = (GImage *) "metricssetvwidth.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},

    .shortcut = H_ ("Set Vertical Advance...|No Shortcut"),
    .invoke = FVMenuSetWidth,

    .mid = MID_SetVWidth},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Auto Width..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},

    .shortcut = H_ ("Auto Width...|Ctl+Shft+W"),
    .invoke = FVMenuAutoWidth
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Ker_n By Classes..."),
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},

    .shortcut = H_ ("Kern By Classes...|No Shortcut"),
    .invoke = FVMenuKernByClasses
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Remove All Kern _Pairs"),
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Remove All Kern Pairs|No Shortcut"),
    .invoke = FVMenuRemoveKern,

    .mid = MID_RmHKern},

  {
    .ti = {
      .text = (uint32_t *) N_("Kern Pair Closeup..."),
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Kern Pair Closeup...|No Shortcut"),
    .invoke = FVMenuKPCloseup
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("VKern By Classes..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'K'},

    .shortcut = H_ ("VKern By Classes...|No Shortcut"),
    .invoke = FVMenuVKernByClasses,

    .mid = MID_VKernByClass},

  {
    .ti = {
      .text = (uint32_t *) N_("VKern From HKern"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("VKern From HKern|No Shortcut"),
    .invoke = FVMenuVKernFromHKern,

    .mid = MID_VKernFromH},

  {
    .ti = {
      .text = (uint32_t *) N_("Remove All VKern Pairs"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},

    .shortcut = H_ ("Remove All VKern Pairs|No Shortcut"),
    .invoke = FVMenuRemoveVKern,

    .mid = MID_RmVKern},

  GMENUITEM_EMPTY
};

static GMenuItem cdlist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Convert to CID"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Convert to CID|No Shortcut"),
    .invoke = FVMenuConvert2CID,

    .mid = MID_Convert2CID},

  {
    .ti = {
      .text = (uint32_t *) N_("Convert By C_Map"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut = H_ ("Convert By CMap|No Shortcut"),
    .invoke = FVMenuConvertByCMap,

    .mid = MID_ConvertByCMap},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Flatten"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},
    .shortcut = H_ ("Flatten|No Shortcut"),
    .invoke = FVMenuFlatten,
    .mid = MID_Flatten},

  {
    .ti = {
      .text = (uint32_t *) N_("Fl_attenByCMap"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut = H_ ("FlattenByCMap|No Shortcut"),
    .invoke = FVMenuFlattenByCMap,
    .mid = MID_FlattenByCMap},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Insert F_ont..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut = H_ ("Insert Font...|No Shortcut"),
    .invoke = FVMenuInsertFont,
    .mid = MID_InsertFont},

  {
    .ti = {
      .text = (uint32_t *) N_("Insert _Blank"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},

    .shortcut = H_ ("Insert Blank|No Shortcut"),
    .invoke = FVMenuInsertBlank,
    .mid = MID_InsertBlank},

  {
    .ti = {
      .text = (uint32_t *) N_("_Remove Font"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'R'},
    .shortcut = H_ ("Remove Font|No Shortcut"),
    .invoke = FVMenuRemoveFontFromCID,
    .mid = MID_RemoveFromCID},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Change Supplement..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("Change Supplement...|No Shortcut"),
    .invoke = FVMenuChangeSupplement,
    .mid = MID_ChangeSupplement},

  {
    .ti = {
      .text = (uint32_t *) N_("C_ID Font Info..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut = H_ ("CID Font Info...|No Shortcut"),
    .invoke = FVMenuCIDFontInfo,
    .mid = MID_CIDFontInfo},

  /* Extra room to show sub-font names */
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY,
  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
cdlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, base, j;
  SplineFont *sub, *cidmaster = fv->b.cidmaster;

  for (i = 0; cdlist[i].mid != MID_CIDFontInfo; ++i);
  base = i + 2;
  for (i = base; cdlist[i].ti.text != NULL; ++i)
    {
      free (cdlist[i].ti.text);
      cdlist[i].ti.text = NULL;
    }

  cdlist[base - 1].ti.fg = cdlist[base - 1].ti.bg = COLOR_DEFAULT;
  if (cidmaster == NULL)
    {
      cdlist[base - 1].ti.line = false;
    }
  else
    {
      cdlist[base - 1].ti.line = true;
      for (j = 0, i = base;
           i < sizeof (cdlist) / sizeof (cdlist[0]) - 1
           && j < cidmaster->subfontcnt; ++i, ++j)
        {
          sub = cidmaster->subfonts[j];
          cdlist[i].ti.text = x_u8_to_u32 (u8_force_valid (sub->fontname));
          cdlist[i].ti.checkable = true;
          cdlist[i].ti.checked = sub == fv->b.sf;
          cdlist[i].ti.userdata = sub;
          cdlist[i].invoke = FVMenuShowSubFont;
          cdlist[i].ti.fg = cdlist[i].ti.bg = COLOR_DEFAULT;
        }
    }
  GMenuItemArrayFree (mi->sub);
  mi->sub = GMenuItemArrayCopy (cdlist, NULL);

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_Convert2CID:
        case MID_ConvertByCMap:
          mi->ti.disabled = cidmaster != NULL || fv->b.sf->mm != NULL;
          break;
        case MID_InsertFont:
        case MID_InsertBlank:
          /* OpenType allows at most 255 subfonts (PS allows more, but why go to the effort to make safe font check that? */
          mi->ti.disabled = cidmaster == NULL || cidmaster->subfontcnt >= 255;
          break;
        case MID_RemoveFromCID:
          mi->ti.disabled = cidmaster == NULL || cidmaster->subfontcnt <= 1;
          break;
        case MID_Flatten:
        case MID_FlattenByCMap:
        case MID_CIDFontInfo:
        case MID_ChangeSupplement:
          mi->ti.disabled = cidmaster == NULL;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem mmlist[] = {

  /* TRANSLATORS: Here (and following) MM means "MultiMaster" */

  {
    .ti = {
      .text = (uint32_t *) N_("_Create MM..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Create MM...|No Shortcut"),
    .invoke = FVMenuCreateMM,
    .mid = MID_CreateMM},

  {
    .ti = {
      .text = (uint32_t *) N_("MM _Validity Check"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("MM Validity Check|No Shortcut"),
    .invoke = FVMenuMMValid,
    .mid = MID_MMValid},

  {
    .ti = {
      .text = (uint32_t *) N_("MM _Info..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("MM Info...|No Shortcut"),
    .invoke = FVMenuMMInfo,
    .mid = MID_MMInfo},

  {
    .ti = {
      .text = (uint32_t *) N_("_Blend to New Font..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Blend to New Font...|No Shortcut"),
    .invoke = FVMenuBlendToNew,
    .mid = MID_BlendToNew},

  {
    .ti = {
      .text = (uint32_t *) N_("MM Change Default _Weights..."),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("MM Change Default Weights...|No Shortcut"),
    .invoke = FVMenuChangeMMBlend,
    .mid = MID_ChangeMMBlend},

  /* Extra room to show sub-font names */
  GMENUITEM_EMPTY,
};

// *INDENT-ON*

VISIBLE void
mmlistcheck_fv (GWindow gw, struct gmenuitem *mi, GEvent *UNUSED (e))
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int i, base, j;
  MMSet *mm = fv->b.sf->mm;
  SplineFont *sub;
  GMenuItem *mml;

  for (i = 0; mmlist[i].mid != MID_ChangeMMBlend; ++i);
  base = i + 2;
  if (mm == NULL)
    mml = mmlist;
  else
    {
      mml = xcalloc (base + mm->instance_count + 2, sizeof (GMenuItem));
      memcpy (mml, mmlist, sizeof (mmlist));
      mml[base - 1].ti.fg = mml[base - 1].ti.bg = COLOR_DEFAULT;
      mml[base - 1].ti.line = true;
      for (j = 0, i = base; j < mm->instance_count + 1; ++i, ++j)
        {
          if (j == 0)
            sub = mm->normal;
          else
            sub = mm->instances[j - 1];
          mml[i].ti.text = x_u8_to_u32 (u8_force_valid (sub->fontname));
          mml[i].ti.checkable = true;
          mml[i].ti.checked = sub == fv->b.sf;
          mml[i].ti.userdata = sub;
          mml[i].invoke = FVMenuShowSubFont;
          mml[i].ti.fg = mml[i].ti.bg = COLOR_DEFAULT;
        }
    }
  GMenuItemArrayFree (mi->sub);
  mi->sub = GMenuItemArrayCopy (mml, NULL);
  if (mml != mmlist)
    {
      for (i = base; mml[i].ti.text != NULL; ++i)
        free (mml[i].ti.text);
      free (mml);
    }

  for (mi = mi->sub; mi->ti.text != NULL || mi->ti.line; ++mi)
    {
      switch (mi->mid)
        {
        case MID_CreateMM:
          mi->ti.disabled = false;
          break;
        case MID_MMInfo:
        case MID_MMValid:
        case MID_BlendToNew:
          mi->ti.disabled = mm == NULL;
          break;
        case MID_ChangeMMBlend:
          mi->ti.disabled = mm == NULL;
          break;
        }
    }
}

// *INDENT-OFF*

static GMenuItem wnmenu[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("New O_utline Window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'u'},
    .shortcut = H_ ("New Outline Window|Ctl+H"),
    .invoke = FVMenuOpenOutline,
    .mid = MID_OpenOutline},

  {
    .ti = {
      .text = (uint32_t *) N_("New _Bitmap Window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'B'},
    .shortcut = H_ ("New Bitmap Window|Ctl+J"),
    .invoke = FVMenuOpenBitmap,
    .mid = MID_OpenBitmap},

  {
    .ti = {
      .text = (uint32_t *) N_("New _Metrics Window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},

    .shortcut = H_ ("New Metrics Window|Ctl+K"),
    .invoke = FVMenuOpenMetrics,
    .mid = MID_OpenMetrics},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Warnings"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},
    .shortcut = H_ ("Warnings|No Shortcut"),
    .invoke = _MenuWarnings,
    .mid = MID_Warnings},

  GMENUITEM_LINE,

  GMENUITEM_EMPTY
};

// *INDENT-ON*

VISIBLE void
FVWindowMenuBuild (GWindow gw, struct gmenuitem *mi, GEvent *e)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);
  int anychars = FVAnyCharSelected (fv);
  struct gmenuitem *wmi;
  int in_modal = (fv->b.container != NULL && fv->b.container->funcs->is_modal);

  WindowMenuBuild (gw, mi, e);
  for (wmi = mi->sub; wmi->ti.text != NULL || wmi->ti.line; ++wmi)
    {
      switch (wmi->mid)
        {
        case MID_OpenOutline:
          wmi->ti.disabled = anychars == -1 || in_modal;
          break;
        case MID_OpenBitmap:
          wmi->ti.disabled = anychars == -1 || fv->b.sf->bitmaps == NULL
            || in_modal;
          break;
        case MID_OpenMetrics:
          wmi->ti.disabled = in_modal;
          break;
        case MID_Warnings:
          wmi->ti.disabled = ErrorWindowExists ();
          break;
        }
    }
}

// *INDENT-OFF*

GMenuItem helplist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_Help"),
      .image = (GImage *) "helphelp.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},
    .shortcut = H_ ("Help|F1"),
    .invoke = FVMenuContextualHelp
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Overview"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Overview|Shft+F1"),
    .invoke = MenuHelp
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Index"),
      .image = (GImage *) "helpindex.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut = H_ ("Index|Ctl+F1"),
    .invoke = MenuIndex,
    .mid = 0},

  {
    .ti = {
      .text = (uint32_t *) N_("_About..."),
      .image = (GImage *) "helpabout.png",
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'A'},

    .shortcut = H_ ("About...|No Shortcut"),
    .invoke = MenuAbout,
    .mid = 0},

  GMENUITEM_EMPTY
};

GMenuItem fvpopupmenu[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Cu_t"),
      .image = (GImage *) "editcut.png",
      .fg = COLOR_DEFAULT,

      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 't'},
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCut,
    .mid = MID_Cut},

  {
    .ti = {
      .text = (uint32_t *) N_("_Copy"),
      .image = (GImage *) "editcopy.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCopy,
    .mid = MID_Copy},

  {
    .ti = {
      .text = (uint32_t *) N_("C_opy Reference"),
      .image = (GImage *) "editcopyref.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'o'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCopyRef,
    .mid = MID_CopyRef},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _Width"),
      .image = (GImage *) "editcopywidth.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'W'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCopyWidth,
    .mid = MID_CopyWidth},

  {
    .ti = {
      .text = (uint32_t *) N_("_Paste"),
      .image = (GImage *) "editpaste.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'P'},
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuPaste,
    .mid = MID_Paste},

  {
    .ti = {
      .text = (uint32_t *) N_("C_lear"),
      .image = (GImage *) "editclear.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},
    .shortcut_char = 0,
    .short_mask = 0,
    
    .invoke = FVMenuClear,
    .mid = MID_Clear},

  {
    .ti = {
      .text = (uint32_t *) N_("Copy _Fg To Bg"),
      .image = (GImage *) "editcopyfg2bg.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},

    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,
    .invoke = FVMenuCopyFgBg,
    .mid = MID_CopyFgToBg},

  {
    .ti = {
      .text = (uint32_t *) N_("U_nlink Reference"),
      .image = (GImage *) "editunlink.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'U'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuUnlinkRef,
    .mid = MID_UnlinkRef},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Glyph _Info..."),
      .image = (GImage *) "elementglyphinfo.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCharInfo,
    .mid = MID_CharInfo},

  {
    .ti = {
      .text = (uint32_t *) N_("_Transform..."),
      .image = (GImage *) "elementtransform.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'T'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuTransform,
    .mid = MID_Transform},

  {
    .ti = {
      .text = (uint32_t *) N_("_Expand Stroke..."),

      .image = (GImage *) "elementexpandstroke.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'E'},
    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,

    .invoke = FVMenuStroke,
    .mid = MID_Stroke},

  //  /*    { { (uint32_t *) N_("_Remove Overlap"),
  //	.image = (GImage *) "overlaprm.png",
  //	.fg = COLOR_DEFAULT,
  //	.bg = COLOR_DEFAULT,
  //	NULL,
  //	NULL,
  //	0,
  //	1,
  //	0,
  //	0,
  //	0,
  //	0,
  //	1,
  //	1,
  //	0,
  //	'O' },
  //	'\0',
  //	ksm_control|ksm_shift,
  //	NULL,
  //	NULL,
  //	.invoke = FVMenuOverlap,
  //	.mid = MID_RmOverlap },
  //  */
  //  /*    { { (uint32_t *) N_("_Simplify"),
  //	.image = (GImage *) "elementsimplify.png",
  //	.fg = COLOR_DEFAULT,
  //	.bg = COLOR_DEFAULT,
  //	NULL,
  //	NULL,
  //	0,
  //	1,
  //	0,
  //	0,
  //	0,
  //	0,
  //	1,
  //	1,
  //	0,
  //	'S' },
  //	'\0',
  //	ksm_control|ksm_shift,
  //	NULL,
  //	NULL,
  //	.invoke = FVMenuSimplify,
  //	.mid = MID_Simplify },
  //  */
  //  /*    { { (uint32_t *) N_("Add E_xtrema"),
  //	.image = (GImage *) "elementaddextrema.png",
  //	.fg = COLOR_DEFAULT,
  //	.bg = COLOR_DEFAULT,
  //	NULL,
  //	NULL,
  //	0,
  //	1,
  //	0,
  //	0,
  //	0,
  //	0,
  //	1,
  //	1,
  //	0,
  //	'x' },
  //	'X',
  //	ksm_control|ksm_shift,
  //	NULL,
  //	NULL,
  //	.invoke = FVMenuAddExtrema,
  //	.mid = MID_AddExtrema },
  //  */

  {
    .ti = {
      .text = (uint32_t *) N_("To _Int"),
      .image = (GImage *) "elementround.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'I'},
    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,
    .invoke = FVMenuRound2Int,
    .mid = MID_Round},

  {
    .ti = {
      .text = (uint32_t *) N_("_Correct Direction"),
      .image = (GImage *) "elementcorrectdir.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'D'},
    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,
    .invoke = FVMenuCorrectDir,
    .mid = MID_Correct},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Auto_Hint"),
      .image = (GImage *) "hintsautohint.png",

      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},
    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,
    .invoke = FVMenuAutoHint,
    .mid = MID_AutoHint},

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("_Center in Width"),
      .image = (GImage *) "metricscenter.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},

    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = FVMenuCenter,
    .mid = MID_Center},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _Width..."),
      .image = (GImage *) "metricssetwidth.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'W'},

    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,
    .invoke = FVMenuSetWidth,
    .mid = MID_SetWidth},

  {
    .ti = {
      .text = (uint32_t *) N_("Set _Vertical Advance..."),
      .image = (GImage *) "metricssetvwidth.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},
    .shortcut_char = '\0',
    .short_mask = ksm_control | ksm_shift,

    .invoke = FVMenuSetWidth,
    .mid = MID_SetVWidth},

  GMENUITEM_EMPTY
};

static GMenuItem mblist[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("_File"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'F'},
    .sub = fllist,
    .moveto = fllistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Edit"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'E'},
    .sub = edlist,
    .moveto = edlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("E_lement"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},
    .sub = ellist,
    .moveto = ellistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Tools"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .disabled = true,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'l'},
    .sub = NULL,
    .moveto = fv_tools_list_check
  },

  {
    .ti = {
      .text = (uint32_t *) N_("H_ints"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'i'},
    .sub = htlist,
    .moveto = htlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("E_ncoding"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},
    .sub = enlist,
    .moveto = enlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_View"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'V'},
    .sub = vwlist,
    .moveto = vwlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Metrics"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'M'},
    .sub = mtlist,
    .moveto = mtlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_CID"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'C'},
    .sub = cdlist,
    .moveto = cdlistcheck_fv
  },

  /* TRANSLATORS: Here (and following) MM means "MultiMaster" */
  {
    .ti = {
      .text = (uint32_t *) N_("MM"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = '\0'},
    .sub = mmlist,
    .moveto = mmlistcheck_fv
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Window"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'W'},
    .sub = wnmenu,
    .moveto = FVWindowMenuBuild
  },

  {
    .ti = {
      .text = (uint32_t *) N_("_Help"),
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true,
      .text_has_mnemonic = true,
      .mnemonic = 'H'},
    .sub = helplist
  },

  GMENUITEM_EMPTY
};

// *INDENT-ON*

void
FVRefreshChar (FontView *fv, int gid)
{
  BDFChar *bdfc;
  int i, j;
  MetricsView *mv;

  /* Can happen in scripts *//* Can happen if we do an AutoHint when generating a tiny font for freetype context */
  if (fv->v == NULL || fv->colcnt == 0 || fv->b.sf->glyphs[gid] == NULL)
    return;
#if 0
  /* What on earth was this code for? it breaks updates of things like "a.sc" */
  if (fv->cur_subtable == NULL
      && strchr (fv->b.sf->glyphs[gid]->name, '.') != NULL)
    {
      char *temp = xstrdup_or_null (fv->b.sf->glyphs[gid]->name);
      SplineChar *sc2;
      *strchr (temp, '.') = '\0';
      sc2 = SFGetChar (fv->b.sf, -1, temp);
      if (sc2 != NULL && sc2->orig_pos != gid)
        gid = sc2->orig_pos;
      free (temp);
    }
#endif

  for (fv = (FontView *) (fv->b.sf->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    {
      for (mv = fv->b.sf->metrics; mv != NULL; mv = mv->next)
        MVRefreshChar (mv, fv->b.sf->glyphs[gid]);
      if (fv->show == fv->filled)
        bdfc = BDFPieceMealCheck (fv->show, gid);
      else
        bdfc = fv->show->glyphs[gid];
      if (bdfc == NULL)
        bdfc = BDFPieceMeal (fv->show, gid);

      /* A glyph may be encoded in several places, all need updating */
      SCM enc_list = gid_to_enc_list (fv->b.map, gid);
      for (SCM p = enc_list; !scm_is_null (p); p = SCM_CDR (p))
        {
          const ssize_t enc = scm_to_ssize_t (SCM_CAR (p));
          i = enc / fv->colcnt;
          j = enc - i * fv->colcnt;
          i -= fv->rowoff;
          if (i >= 0 && i < fv->rowcnt)
            FVDrawGlyph (fv->v, fv, enc, true);
        }
    }
}

void
FVRegenChar (FontView *fv, SplineChar *sc)
{
  struct splinecharlist *dlist;
  MetricsView *mv;

  if (fv->v == NULL)            /* Can happen in scripts */
    return;

  /* sc->changedsincelasthinted = true; *//* Why was this here? */
  if (sc->orig_pos < fv->filled->glyphcnt)
    {
      BDFCharFree (fv->filled->glyphs[sc->orig_pos]);
      fv->filled->glyphs[sc->orig_pos] = NULL;
    }
  /* FVRefreshChar does NOT do this for us */
  for (mv = fv->b.sf->metrics; mv != NULL; mv = mv->next)
    MVRegenChar (mv, sc);

  FVRefreshChar (fv, sc->orig_pos);

  for (dlist = sc->dependents; dlist != NULL; dlist = dlist->next)
    FVRegenChar (fv, dlist->sc);
}

static void
AddSubPST (SplineChar *sc, struct lookup_subtable *sub, char *variant)
{
  PST *pst;

  pst = xzalloc (sizeof (PST));
  pst->type = pst_substitution;
  pst->subtable = sub;
  pst->u.alt.components = xstrdup_or_null (variant);
  pst->next = sc->possub;
  sc->possub = pst;
}

SplineChar *
FVMakeChar (FontView *fv, int enc)
{
  SplineFont *sf = fv->b.sf;
  SplineChar *base_sc = SFMakeChar (sf, fv->b.map, enc), *feat_sc = NULL;
  int feat_gid = FeatureTrans (fv, enc);

  if (fv->cur_subtable == NULL)
    return base_sc;

  if (feat_gid == -1)
    {
      int uni = -1;
      FeatureScriptLangList *fl = fv->cur_subtable->lookup->features;

      if (base_sc->unicodeenc >= 0x600 && base_sc->unicodeenc <= 0x6ff
          && fl != NULL && (fl->featuretag == CHR ('i', 'n', 'i', 't')
                            || fl->featuretag == CHR ('m', 'e', 'd', 'i')
                            || fl->featuretag == CHR ('f', 'i', 'n', 'a')
                            || fl->featuretag == CHR ('i', 's', 'o', 'l')))
        {
          uni =
            fl->featuretag == CHR ('i', 'n', 'i',
                                   't') ? ArabicForms[base_sc->unicodeenc -
                                                      0x600].
            initial : fl->featuretag == CHR ('m', 'e', 'd',
                                             'i') ? ArabicForms[base_sc->
                                                                unicodeenc -
                                                                0x600].
            medial : fl->featuretag == CHR ('f', 'i', 'n',
                                            'a') ? ArabicForms[base_sc->
                                                               unicodeenc -
                                                               0x600].
            final : fl->featuretag == CHR ('i', 's', 'o',
                                           'l') ? ArabicForms[base_sc->
                                                              unicodeenc -
                                                              0x600].
            isolated : -1;
          feat_sc = SFGetChar (sf, uni, NULL);
          if (feat_sc != NULL)
            return feat_sc;
        }
      feat_sc = SFSplineCharCreate (sf);
      feat_sc->unicodeenc = uni;
      if (uni != -1)
        {
          feat_sc->name = xmalloc (8);
          feat_sc->unicodeenc = uni;
          sprintf (feat_sc->name, "uni%04X", uni);
        }
      else if (fv->cur_subtable->suffix != NULL)
        {
          feat_sc->name =
            xmalloc (strlen (base_sc->name) +
                     strlen (fv->cur_subtable->suffix) + 2);
          sprintf (feat_sc->name, "%s.%s", base_sc->name,
                   fv->cur_subtable->suffix);
        }
      else if (fl == NULL)
        {
          feat_sc->name = xstrdup (x_gc_strjoin (base_sc->name, ".unknown", NULL));
        }
      else
        {
          /* OpenType feature tag */
          feat_sc->name = xmalloc (strlen (base_sc->name) + 6);
          sprintf (feat_sc->name, "%s.%c%c%c%c", base_sc->name,
                   (int) (fl->featuretag >> 24),
                   (int) ((fl->featuretag >> 16) & 0xff),
                   (int) ((fl->featuretag >> 8) & 0xff),
                   (int) ((fl->featuretag) & 0xff));
        }
      SFAddGlyphAndEncode (sf, feat_sc, fv->b.map, fv->b.map->enc_limit);
      AddSubPST (base_sc, fv->cur_subtable, feat_sc->name);
      return feat_sc;
    }
  else
    return base_sc;
}

/* we style some glyph names differently, see FVExpose() */
#define _uni_italic	0x2
#define _uni_rotated	(1<<2)
#define _uni_fontmax	(2<<2)

static GFont *
FVCheckFont (FontView *fv, int type)
{
  if (fv->fontset[type] == NULL)
    {
      int style = 0;
      if (type & _uni_italic)
        style |= fs_italic;
      if (type & _uni_rotated)
        style |= fs_rotated;
      fv->fontset[type] =
        GDrawNewFont (fv->v, fv_fontnames, fv_fontsize, 400, style);
    }
  return fv->fontset[type];
}

extern uint32_t adobes_pua_alts[0x200][3];

static void
do_Adobe_Pua (uint32_t *buf, int sob, int uni)
{
  int i, j;

  for (i = j = 0; j < sob - 1 && i < 3; ++i)
    {
      int ch = adobes_pua_alts[uni - 0xf600][i];
      if (ch == 0)
        break;
      if (ch >= 0xf600 && ch <= 0xf7ff && adobes_pua_alts[ch - 0xf600] != 0)
        {
          do_Adobe_Pua (buf + j, sob - j, ch);
          while (buf[j] != 0)
            ++j;
        }
      else
        buf[j++] = ch;
    }
  buf[j] = 0;
}

static void
FVExpose (FontView *fv, GWindow pixmap, GEvent *event)
{
  int i, j, width, gid;
  int changed;
  GRect old, old2, r;
  GClut clut;
  struct _GImage base;
  GImage gi;
  SplineChar dummy;
  int styles, laststyles = 0;
  Color bg, def_fg;
  int fgxor;

  def_fg = GDrawGetDefaultForeground (NULL);
  memset (&gi, '\0', sizeof (gi));
  memset (&base, '\0', sizeof (base));
  if (fv->show->clut != NULL)
    {
      gi.u.image = &base;
      base.image_type = it_index;
      base.clut = fv->show->clut;
      GDrawSetDither (NULL, false);
      base.trans = -1;
      /*base.clut->trans_index = 0; */
    }
  else
    {
      memset (&clut, '\0', sizeof (clut));
      gi.u.image = &base;
      base.image_type = it_mono;
      base.clut = &clut;
      clut.clut_len = 2;
      clut.clut[0] = view_bgcol;
    }

  GDrawSetFont (pixmap, fv->fontset[0]);
  GDrawSetLineWidth (pixmap, 0);
  GDrawPushClip (pixmap, &event->u.expose.rect, &old);
  GDrawFillRect (pixmap, NULL, view_bgcol);
  for (i = 0; i <= fv->rowcnt; ++i)
    {
      GDrawDrawLine (pixmap, 0, i * fv->cbh, fv->width, i * fv->cbh, def_fg);
      GDrawDrawLine (pixmap, 0, i * fv->cbh + fv->lab_height, fv->width,
                     i * fv->cbh + fv->lab_height, 0x808080);
    }
  for (i = 0; i <= fv->colcnt; ++i)
    GDrawDrawLine (pixmap, i * fv->cbw, 0, i * fv->cbw, fv->height, def_fg);
  for (i = event->u.expose.rect.y / fv->cbh;
       i <= fv->rowcnt
       && (event->u.expose.rect.y + event->u.expose.rect.height + fv->cbh -
           1) / fv->cbh; ++i)
    for (j = 0; j < fv->colcnt; ++j)
      {
        int index = (i + fv->rowoff) * fv->colcnt + j;
        SplineChar *sc;
        styles = 0;
        if (index < fv->b.map->enc_limit && index != -1)
          {
            uint32_t buf[60];
            char cbuf[8];
            char utf8_buf[8];
            int use_utf8 = false;
            Color fg;
            extern const int amspua[];
            int uni;
            struct cidmap *cidmap = NULL;
            sc = (gid = enc_to_gid (fv->b.map, index)) != -1 ?
              fv->b.sf->glyphs[gid] : NULL;

            if (fv->b.cidmaster != NULL)
              cidmap =
                FindCidMap (fv->b.cidmaster->cidregistry,
                            fv->b.cidmaster->ordering,
                            fv->b.cidmaster->supplement, fv->b.cidmaster);

            if ((fv->b.map->enc == &custom && index < 256)
                || (fv->b.map->enc != &custom
                    && index < fv->b.map->enc->char_cnt) || (cidmap != NULL
                                                             && index <
                                                             MaxCID (cidmap)))
              fg = def_fg;
            else
              fg = 0x505050;
            if (sc == NULL)
              sc = SCBuildDummy (&dummy, fv->b.sf, fv->b.map, index);
            uni = sc->unicodeenc;
            buf[0] = buf[1] = 0;
            if (fv->b.sf->uni_interp == ui_ams && uni >= 0xe000
                && uni <= 0xf8ff && amspua[uni - 0xe000] != 0)
              uni = amspua[uni - 0xe000];
            switch (fv->glyphlabel)
              {
              case gl_name:
                uc_strncpy (buf, sc->name, sizeof (buf) / sizeof (buf[0]));
                break;
              case gl_unicode:
                if (sc->unicodeenc != -1)
                  {
                    sprintf (cbuf, "%04x", sc->unicodeenc);
                    u32_strcpy (buf, x_gc_u8_to_u32 (cbuf));
                  }
                else
                  u32_strcpy (buf, x_gc_u8_to_u32 ("?"));
                break;
              case gl_encoding:
                if (fv->b.map->enc->only_1byte
                    || (fv->b.map->enc->has_1byte && index < 256))
                  sprintf (cbuf, "%02x", index);
                else
                  sprintf (cbuf, "%04x", index);
                u32_strcpy (buf, x_gc_u8_to_u32 (cbuf));
                break;
              case gl_glyph:
                if (uni == 0xad)
                  buf[0] = '-';
                else if (fv->b.sf->uni_interp == ui_adobe && uni >= 0xf600
                         && uni <= 0xf7ff && adobes_pua_alts[uni - 0xf600] != 0)
                  {
                    use_utf8 = false;
                    do_Adobe_Pua (buf, sizeof (buf), uni);
                  }
                else if (uni >= 0xe0020 && uni <= 0xe007e)
                  {
                    buf[0] = uni - 0xe0000;     /* A map of Ascii for language names */
                  }
                else if (uni > 0 && uni < unicode4_size)
                  {
                    char *pt = utf8_buf;
                    use_utf8 = true;
                    pt = utf8_idpb (pt, uni);
                    *pt = '\0';
                  }
                else
                  {
                    char *pt = strchr (sc->name, '.');
                    buf[0] = '?';
                    fg = 0xff0000;
                    if (pt != NULL)
                      {
                        int i, n = pt - sc->name;
                        char *end;
                        SplineFont *cm = fv->b.sf->cidmaster;
                        if (n == 7 && sc->name[0] == 'u' && sc->name[1] == 'n'
                            && sc->name[2] == 'i'
                            && (i =
                                strtol (sc->name + 3, &end, 16),
                                end - sc->name == 7))
                          buf[0] = i;
                        else if (n >= 5 && n <= 7 && sc->name[0] == 'u'
                                 && (i =
                                     strtol (sc->name + 1, &end, 16),
                                     end - sc->name == n))
                          buf[0] = i;
                        else if (cm != NULL
                                 && (i = CIDFromName (sc->name, cm)) != -1)
                          {
                            int uni;
                            uni =
                              CID2Uni (FindCidMap
                                       (cm->cidregistry, cm->ordering,
                                        cm->supplement, cm), i);
                            if (uni != -1)
                              buf[0] = uni;
                          }
                        else
                          {
                            int uni;
                            *pt = '\0';
                            uni =
                              UniFromName (sc->name, fv->b.sf->uni_interp,
                                           fv->b.map->enc);
                            if (uni != -1)
                              buf[0] = uni;
                            *pt = '.';
                          }
                        if (strstr (pt, ".vert") != NULL)
                          styles = _uni_rotated;
                        if (buf[0] != '?')
                          {
                            fg = def_fg;
                            if (strstr (pt, ".italic") != NULL)
                              styles = _uni_italic;
                          }
                      }
                    else if (strncmp (sc->name, "hwuni", 5) == 0)
                      {
                        int uni = -1;
                        sscanf (sc->name, "hwuni%x", (unsigned *) &uni);
                        if (uni != -1)
                          buf[0] = uni;
                      }
                    else if (strncmp (sc->name, "italicuni", 9) == 0)
                      {
                        int uni = -1;
                        sscanf (sc->name, "italicuni%x", (unsigned *) &uni);
                        if (uni != -1)
                          {
                            buf[0] = uni;
                            styles = _uni_italic;
                          }
                        fg = def_fg;
                      }
                    else if (strncmp (sc->name, "vertcid_", 8) == 0
                             || strncmp (sc->name, "vertuni", 7) == 0)
                      {
                        styles = _uni_rotated;
                      }
                  }
                break;
              }
            r.x = j * fv->cbw + 1;
            r.width = fv->cbw - 1;
            r.y = i * fv->cbh + 1;
            r.height = fv->lab_height - 1;
            bg = view_bgcol;
            fgxor = 0x000000;
            changed = sc->changed;
            if (fv->b.sf->onlybitmaps && gid < fv->show->glyphcnt)
              changed = gid == -1
                || fv->show->glyphs[gid] ==
                NULL ? false : fv->show->glyphs[gid]->changed;
            if (changed || sc->layers[ly_back].splines != NULL
                || sc->layers[ly_back].images != NULL
                || sc->color != COLOR_DEFAULT)
              {
                if (sc->layers[ly_back].splines != NULL
                    || sc->layers[ly_back].images != NULL
                    || sc->color != COLOR_DEFAULT)
                  bg = sc->color != COLOR_DEFAULT ? sc->color : 0x808080;
                if (sc->changed)
                  {
                    fgxor = bg ^ fvchangedcol;
                    bg = fvchangedcol;
                  }
                GDrawFillRect (pixmap, &r, bg);
              }
            if ((!fv->b.sf->layers[fv->b.active_layer].order2
                 && sc->changedsincelasthinted)
                || (fv->b.sf->layers[fv->b.active_layer].order2
                    && sc->layers[fv->b.active_layer].splines != NULL
                    && sc->ttf_instrs_len <= 0)
                || (fv->b.sf->layers[fv->b.active_layer].order2
                    && sc->instructions_out_of_date))
              {
                Color hintcol = fvhintingneededcol;
                if (fv->b.sf->layers[fv->b.active_layer].order2
                    && sc->instructions_out_of_date && sc->ttf_instrs_len > 0)
                  hintcol = 0xff0000;
                GDrawDrawLine (pixmap, r.x, r.y, r.x, r.y + r.height - 1,
                               hintcol);
                GDrawDrawLine (pixmap, r.x + 1, r.y, r.x + 1,
                               r.y + r.height - 1, hintcol);
                GDrawDrawLine (pixmap, r.x + 2, r.y, r.x + 2,
                               r.y + r.height - 1, hintcol);
                GDrawDrawLine (pixmap, r.x + r.width - 1, r.y,
                               r.x + r.width - 1, r.y + r.height - 1, hintcol);
                GDrawDrawLine (pixmap, r.x + r.width - 2, r.y,
                               r.x + r.width - 2, r.y + r.height - 1, hintcol);
                GDrawDrawLine (pixmap, r.x + r.width - 3, r.y,
                               r.x + r.width - 3, r.y + r.height - 1, hintcol);
              }
            if (use_utf8 && sc->unicodeenc != -1 &&
                /* Pango complains if we try to draw non characters */
                /* These two are guaranteed "NOT A UNICODE CHARACTER" in all planes */
                ((sc->unicodeenc & 0xffff) == 0xfffe || (sc->unicodeenc & 0xffff) == 0xffff || (sc->unicodeenc >= 0xfdd0 && sc->unicodeenc <= 0xfdef) ||        /* noncharacters */
                 (sc->unicodeenc >= 0xfe00 && sc->unicodeenc <= 0xfe0f) ||      /* variation selectors */
                 (sc->unicodeenc >= 0xe0110 && sc->unicodeenc <= 0xe01ff) ||    /* variation selectors */
                 /*  The surrogates in BMP aren't valid either */
                 (sc->unicodeenc >= 0xd800 && sc->unicodeenc <= 0xdfff)))
              {                 /* surrogates */
                GDrawDrawLine (pixmap, r.x, r.y, r.x + r.width - 1,
                               r.y + r.height - 1, 0x000000);
                GDrawDrawLine (pixmap, r.x, r.y + r.height - 1,
                               r.x + r.width - 1, r.y, 0x000000);
              }
            else if (use_utf8)
              {
                GTextBounds size;
                if (styles != laststyles)
                  GDrawSetFont (pixmap, FVCheckFont (fv, styles));
                width = GDrawGetText8Bounds (pixmap, utf8_buf, -1, &size);
                if (size.lbearing == 0 && size.rbearing == 0)
                  {
                    utf8_buf[0] = 0xe0 | (0xfffd >> 12);
                    utf8_buf[1] = 0x80 | ((0xfffd >> 6) & 0x3f);
                    utf8_buf[2] = 0x80 | (0xfffd & 0x3f);
                    utf8_buf[3] = 0;
                    width = GDrawGetText8Bounds (pixmap, utf8_buf, -1, &size);
                  }
                width = size.rbearing - size.lbearing + 1;
                if (width >= fv->cbw - 1)
                  {
                    GDrawPushClip (pixmap, &r, &old2);
                    width = fv->cbw - 1;
                  }
                if (sc->unicodeenc < 0x80 || sc->unicodeenc >= 0xa0)
                  {
                    GDrawDrawText8 (pixmap,
                                    j * fv->cbw + (fv->cbw - 1 - width) / 2 -
                                    size.lbearing,
                                    i * fv->cbh + fv->lab_as + 1, utf8_buf,
                                    -1, fg ^ fgxor);
                  }
                if (width >= fv->cbw - 1)
                  GDrawPopClip (pixmap, &old2);
                laststyles = styles;
              }
            else
              {
                if (styles != laststyles)
                  GDrawSetFont (pixmap, FVCheckFont (fv, styles));
                width = GDrawGetTextWidth (pixmap, buf, -1);
                if (width >= fv->cbw - 1)
                  {
                    GDrawPushClip (pixmap, &r, &old2);
                    width = fv->cbw - 1;
                  }
                if (sc->unicodeenc < 0x80 || sc->unicodeenc >= 0xa0)
                  {
                    GDrawDrawText (pixmap,
                                   j * fv->cbw + (fv->cbw - 1 - width) / 2,
                                   i * fv->cbh + fv->lab_as + 1, buf, -1,
                                   fg ^ fgxor);
                  }
                if (width >= fv->cbw - 1)
                  GDrawPopClip (pixmap, &old2);
                laststyles = styles;
              }
          }
        FVDrawGlyph (pixmap, fv, index, false);
      }
  if (fv->showhmetrics & fvm_baseline)
    {
      for (i = 0; i <= fv->rowcnt; ++i)
        GDrawDrawLine (pixmap, 0,
                       i * fv->cbh + fv->lab_height +
                       fv->magnify * fv->show->ascent + 1, fv->width,
                       i * fv->cbh + fv->lab_height +
                       fv->magnify * fv->show->ascent + 1, METRICS_BASELINE);
    }
  GDrawPopClip (pixmap, &old);
  GDrawSetDither (NULL, true);
}

static char *chosung[] =
  { "G", "GG", "N", "D", "DD", "L", "M", "B", "BB", "S", "SS", "", "J", "JJ",
  "C", "K", "T", "P", "H", NULL
};

static char *jungsung[] =
  { "A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O", "WA", "WAE", "OE",
  "YO", "U", "WEO", "WE", "WI", "YU", "EU", "YI", "I", NULL
};

static char *jongsung[] =
  { "", "G", "GG", "GS", "N", "NJ", "NH", "D", "L", "LG", "LM", "LB", "LS",
  "LT", "LP", "LH", "M", "B", "BS", "S", "SS", "NG", "J", "C", "K", "T", "P",
  "H", NULL
};

void
FVDrawInfo (FontView *fv, GWindow pixmap, GEvent *event)
{
  GRect old, r;
  char buffer[250], *pt;
  uint32_t ubuffer[250];
  Color bg = GDrawGetDefaultBackground (GDrawGetDisplayOfWindow (pixmap));
  SplineChar *sc, dummy;
  SplineFont *sf = fv->b.sf;
  EncMap *map = fv->b.map;
  int gid;
  int uni;
  Color fg = fvglyphinfocol;
  int ulen, tlen;
  const char *uniname;

  if (event->u.expose.rect.y + event->u.expose.rect.height <= fv->mbh)
    return;

  GDrawSetFont (pixmap, fv->fontset[0]);
  GDrawPushClip (pixmap, &event->u.expose.rect, &old);

  r.x = 0;
  r.width = fv->width;
  r.y = fv->mbh;
  r.height = fv->infoh;
  GDrawFillRect (pixmap, &r, bg);
  if (fv->end_pos >= map->enc_limit || fv->pressed_pos >= map->enc_limit
      || fv->end_pos < 0 || fv->pressed_pos < 0)
    fv->end_pos = fv->pressed_pos = -1; /* Can happen after reencoding */
  if (fv->end_pos == -1)
    {
      GDrawPopClip (pixmap, &old);
      return;
    }

  if (map->remap != NULL)
    {
      int localenc = fv->end_pos;
      struct remap *remap = map->remap;
      while (remap->infont != -1)
        {
          if (localenc >= remap->infont
              && localenc <= remap->infont + (remap->lastenc - remap->firstenc))
            {
              localenc += remap->firstenc - remap->infont;
              break;
            }
          ++remap;
        }
      sprintf (buffer, "%-5d (0x%04x) ", localenc, localenc);
    }
  else if (map->enc->only_1byte || (map->enc->has_1byte && fv->end_pos < 256))
    sprintf (buffer, "%-3d (0x%02x) ", fv->end_pos, fv->end_pos);
  else
    sprintf (buffer, "%-5d (0x%04x) ", fv->end_pos, fv->end_pos);
  sc = (gid = enc_to_gid (fv->b.map, fv->end_pos)) != -1 ?
    sf->glyphs[gid] : NULL;
  if (fv->b.cidmaster == NULL || fv->b.normal == NULL || sc == NULL)
    SCBuildDummy (&dummy, sf, fv->b.map, fv->end_pos);
  else
    dummy = *sc;
  if (sc == NULL)
    sc = &dummy;
  uni = dummy.unicodeenc != -1 ? dummy.unicodeenc : sc->unicodeenc;
  if (uni != -1)
    sprintf (buffer + strlen (buffer), "U+%04X", uni);
  else
    sprintf (buffer + strlen (buffer), "U+????");
  sprintf (buffer + strlen (buffer), "  %.*s",
           (int) (sizeof (buffer) - strlen (buffer) - 1), sc->name);

  strcat (buffer, "  ");
  utf82u_strcpy (ubuffer, buffer);
  ulen = u32_strlen (ubuffer);

  if (uni == -1 && (pt = strchr (sc->name, '.')) != NULL && pt - sc->name < 30)
    {
      strncpy (buffer, sc->name, pt - sc->name);
      buffer[(pt - sc->name)] = '\0';
      uni = UniFromName (buffer, fv->b.sf->uni_interp, map->enc);
      if (uni != -1)
        {
          sprintf (buffer, "U+%04X ", uni);
          u32_strcat (ubuffer, x_gc_u8_to_u32 (buffer));
        }
      fg = 0x707070;
    }
  if (uni != -1)
    {
      uniname = uninm_name (names_db, (unsigned int) uni);
      if (uniname != NULL)
        {
          utf82u_strncpy (ubuffer + u32_strlen (ubuffer), uniname, 80);
        }
      else if (uni >= 0xAC00 && uni <= 0xD7A3)
        {
          sprintf (buffer, "Hangul Syllable %s%s%s",
                   chosung[(uni - 0xAC00) / (21 * 28)],
                   jungsung[(uni - 0xAC00) / 28 % 21],
                   jongsung[(uni - 0xAC00) % 28]);
          uc_strncat (ubuffer, buffer, 80);
        }
      else
        {
          uc_strncat (ubuffer, UnicodeRange (uni), 80);
        }
    }

  tlen =
    GDrawDrawText (pixmap, 10, fv->mbh + fv->lab_as, ubuffer, ulen,
                   fvglyphinfocol);
  GDrawDrawText (pixmap, 10 + tlen, fv->mbh + fv->lab_as, ubuffer + ulen, -1,
                 fg);
  GDrawPopClip (pixmap, &old);
}

static void
FVShowInfo (FontView *fv)
{
  GRect r;

  if (fv->v == NULL)            /* Can happen in scripts */
    return;

  r.x = 0;
  r.width = fv->width;
  r.y = fv->mbh;
  r.height = fv->infoh;
  GDrawRequestExpose (fv->gw, &r, false);
}

void
FVChar (FontView *fv, GEvent *event)
{
  int i, pos, cnt, gid;
  extern int navigation_mask;

  if (event->u.chr.keysym == 's' && (event->u.chr.state & ksm_control)
      && (event->u.chr.state & ksm_meta))
    MenuSaveAll (NULL, NULL, NULL);
  else if (event->u.chr.keysym == 'q' && (event->u.chr.state & ksm_control)
           && (event->u.chr.state & ksm_meta))
    MenuExit (NULL, NULL, NULL);
  else if (event->u.chr.keysym == 'I' && (event->u.chr.state & ksm_shift)
           && (event->u.chr.state & ksm_meta))
    FVMenuCharInfo (fv->gw, NULL, NULL);
  else if ((event->u.chr.keysym == '[' || event->u.chr.keysym == ']')
           && (event->u.chr.state & ksm_control))
    {
      _FVMenuChangeChar (fv, event->u.chr.keysym == '[' ? MID_Prev : MID_Next);
    }
  else if ((event->u.chr.keysym == '{' || event->u.chr.keysym == '}')
           && (event->u.chr.state & ksm_control))
    {
      _FVMenuChangeChar (fv,
                         event->u.chr.keysym ==
                         '{' ? MID_PrevDef : MID_NextDef);
    }
  else if (event->u.chr.keysym == '\\' && (event->u.chr.state & ksm_control))
    {
      /* European keyboards need a funky modifier to get \ */
      FVDoTransform (fv);
    }
  else if (event->u.chr.keysym == GK_Left || event->u.chr.keysym == GK_Tab
           || event->u.chr.keysym == GK_BackTab
           || event->u.chr.keysym == GK_Up || event->u.chr.keysym == GK_Right
           || event->u.chr.keysym == GK_Down
           || event->u.chr.keysym == GK_KP_Left
           || event->u.chr.keysym == GK_KP_Up
           || event->u.chr.keysym == GK_KP_Right
           || event->u.chr.keysym == GK_KP_Down
           || event->u.chr.keysym == GK_Home
           || event->u.chr.keysym == GK_KP_Home
           || event->u.chr.keysym == GK_End
           || event->u.chr.keysym == GK_KP_End
           || event->u.chr.keysym == GK_Page_Up
           || event->u.chr.keysym == GK_KP_Page_Up
           || event->u.chr.keysym == GK_Prior
           || event->u.chr.keysym == GK_Page_Down
           || event->u.chr.keysym == GK_KP_Page_Down
           || event->u.chr.keysym == GK_Next)
    {
      int end_pos = fv->end_pos;
      /* We move the currently selected char. If there is none, then pick */
      /*  something on the screen */
      if (end_pos == -1)
        end_pos = (fv->rowoff + fv->rowcnt / 2) * fv->colcnt;
      switch (event->u.chr.keysym)
        {
        case GK_Tab:
          pos = end_pos;
          do
            {
              if (event->u.chr.state & ksm_shift)
                --pos;
              else
                ++pos;
              if (pos >= fv->b.map->enc_limit)
                pos = 0;
              else if (pos < 0)
                pos = fv->b.map->enc_limit - 1;
            }
          while (pos != end_pos
                 && ((gid = enc_to_gid (fv->b.map, pos)) == -1
                     || !SCWorthOutputting (fv->b.sf->glyphs[gid])));
          if (pos == end_pos)
            ++pos;
          if (pos >= fv->b.map->enc_limit)
            pos = 0;
          break;
#if GK_Tab!=GK_BackTab
        case GK_BackTab:
          pos = end_pos;
          do
            {
              --pos;
              if (pos < 0)
                pos = fv->b.map->enc_limit - 1;
            }
          while (pos != end_pos
                 && ((gid = enc_to_gid (fv->b.map, pos)) == -1
                     || !SCWorthOutputting (fv->b.sf->glyphs[gid])));
          if (pos == end_pos)
            --pos;
          if (pos < 0)
            pos = 0;
          break;
#endif
        case GK_Left:
        case GK_KP_Left:
          pos = end_pos - 1;
          break;
        case GK_Right:
        case GK_KP_Right:
          pos = end_pos + 1;
          break;
        case GK_Up:
        case GK_KP_Up:
          pos = end_pos - fv->colcnt;
          break;
        case GK_Down:
        case GK_KP_Down:
          pos = end_pos + fv->colcnt;
          break;
        case GK_End:
        case GK_KP_End:
          pos = fv->b.map->enc_limit;
          break;
        case GK_Home:
        case GK_KP_Home:
          pos = 0;
          if (fv->b.sf->top_enc != -1
              && fv->b.sf->top_enc < fv->b.map->enc_limit)
            pos = fv->b.sf->top_enc;
          else
            {
              pos = SFFindSlot (fv->b.sf, fv->b.map, home_char, NULL);
              if (pos == -1)
                pos = 0;
            }
          break;
        case GK_Page_Up:
        case GK_KP_Page_Up:
#if GK_Prior!=GK_Page_Up
        case GK_Prior:
#endif
          pos = (fv->rowoff - fv->rowcnt + 1) * fv->colcnt;
          break;
        case GK_Page_Down:
        case GK_KP_Page_Down:
#if GK_Next!=GK_Page_Down
        case GK_Next:
#endif
          pos = (fv->rowoff + fv->rowcnt + 1) * fv->colcnt;
          break;
        }
      if (pos < 0)
        pos = 0;
      if (pos >= fv->b.map->enc_limit)
        pos = fv->b.map->enc_limit - 1;
      if (event->u.chr.state & ksm_shift && event->u.chr.keysym != GK_Tab
          && event->u.chr.keysym != GK_BackTab)
        {
          FVReselect (fv, pos);
        }
      else
        {
          FVDeselectAll (fv);
          fv->b.selected[pos] = true;
          FVToggleCharSelected (fv, pos);
          fv->pressed_pos = pos;
          fv->sel_index = 1;
        }
      fv->end_pos = pos;
      FVShowInfo (fv);
      FVScrollToChar (fv, pos);
    }
  else if (event->u.chr.keysym == GK_Help)
    {
      MenuHelp (NULL, NULL, NULL);      /* Menu does F1 */
    }
  else if (event->u.chr.keysym == GK_Escape)
    {
      FVDeselectAll (fv);
    }
  else if (event->u.chr.chars[0] == '\r' || event->u.chr.chars[0] == '\n')
    {
      if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
        return;
      for (i = cnt = 0; i < fv->b.map->enc_limit && cnt < 10; ++i)
        if (fv->b.selected[i])
          {
            SplineChar *sc = SFMakeChar (fv->b.sf, fv->b.map, i);
            if (fv->show == fv->filled)
              {
                CharViewCreate (sc, fv, i);
              }
            else
              {
                BDFFont *bdf = fv->show;
                BitmapViewCreate (BDFMakeGID (bdf, sc->orig_pos), bdf, fv, i);
              }
            ++cnt;
          }
    }
  else
    if ((event->u.chr.state & ((GMenuMask () | navigation_mask) &
                               ~(ksm_shift | ksm_capslock))) ==
        navigation_mask && event->type == et_char && event->u.chr.keysym != 0
        && (event->u.chr.keysym <
            GK_Special /* || event->u.chr.keysym>=0x10000 */ ))
    {
      SplineFont *sf = fv->b.sf;
      int enc = EncFromUni (event->u.chr.keysym, fv->b.map->enc);
      if (enc == -1)
        {
          for (i = 0; i < sf->glyphcnt; ++i)
            {
              if (sf->glyphs[i] != NULL)
                if (sf->glyphs[i]->unicodeenc == event->u.chr.keysym)
                  break;
            }
          if (i != -1)
            enc = gid_to_enc (fv->b.map, i);
        }
      if (enc < fv->b.map->enc_limit && enc != -1)
        FVChangeChar (fv, enc);
    }
}

static void
utf82u_annot_strncat (uint32_t *to, const char *from, int len)
{
  register uint32_t ch;

  to += u32_strlen (to);
  while ((ch = u8_get_next ((const uint8_t **) &from)) != '\0' && --len >= 0)
    {
      if (ch == '\t')
        {
          *(to++) = ' ';
          ch = ' ';
        }
      *(to++) = ch;
    }
  *to = 0;
}

void
SCPreparePopup (GWindow gw, SplineChar *sc, struct remap *remap, int localenc,
                int actualuni)
{
  static uint32_t space[810];
  char cspace[162];
  int upos = -1;
  int done = false;
  const char *uniname;
  const char *uniannot;

  /* If a glyph is multiply mapped then the inbuild unicode enc may not be */
  /*  the actual one used to access the glyph */
  if (remap != NULL)
    {
      while (remap->infont != -1)
        {
          if (localenc >= remap->infont
              && localenc <= remap->infont + (remap->lastenc - remap->firstenc))
            {
              localenc += remap->firstenc - remap->infont;
              break;
            }
          ++remap;
        }
    }

  if (actualuni != -1)
    upos = actualuni;
  else if (sc->unicodeenc != -1)
    upos = sc->unicodeenc;
  else
    {
      snprintf (cspace, sizeof (cspace), "%u 0x%x U+???? \"%.25s\" ",
                localenc, localenc, sc->name == NULL ? "" : sc->name);
      u32_strcpy (space, x_gc_u8_to_u32 (cspace));
      done = true;
    }

  if (!done)
    {
      uniname = uninm_name (names_db, upos);
      if (uniname != NULL)
        {
          snprintf (cspace, sizeof (cspace),
                    "%u 0x%x U+%04x \"%.25s\" %.100s", localenc, localenc,
                    upos, sc->name == NULL ? "" : sc->name, uniname);
          utf82u_strcpy (space, cspace);
        }
      else if (upos >= 0xAC00 && upos <= 0xD7A3)
        {
          snprintf (cspace, sizeof (cspace),
                    "%u 0x%x U+%04x \"%.25s\" Hangul Syllable %s%s%s",
                    localenc, localenc, upos,
                    sc->name == NULL ? "" : sc->name,
                    chosung[(upos - 0xAC00) / (21 * 28)],
                    jungsung[(upos - 0xAC00) / 28 % 21],
                    jongsung[(upos - 0xAC00) % 28]);
          utf82u_strcpy (space, cspace);
        }
      else
        {
          snprintf (cspace, sizeof (cspace), "%u 0x%x U+%04x \"%.25s\" %.50s",
                    localenc, localenc, upos,
                    sc->name == NULL ? "" : sc->name, UnicodeRange (upos));
          utf82u_strcpy (space, cspace);
        }
    }
  uniannot = uninm_annotation (names_db, upos);
  if (uniannot != NULL)
    {
      int left = sizeof (space) / sizeof (space[0]) - u32_strlen (space) - 1;
      if (left > 4)
        {
          u32_strcat (space, x_gc_u8_to_u32 ("\n"));
          utf82u_annot_strncat (space, uniannot, left - 2);
        }
    }
  if (sc->comment != NULL)
    {
      int left = sizeof (space) / sizeof (space[0]) - u32_strlen (space) - 1;
      if (left > 4)
        {
          u32_strcat (space, x_gc_u8_to_u32 ("\n\n"));
          utf82u_strncpy (space + u32_strlen (space), sc->comment, left - 2);
        }
    }
  GGadgetPreparePopup (gw, space);
}

static void
noop (void *UNUSED (_fv))
{
}

static void *
ddgencharlist (void *_fv, int32_t *len)
{
  int i, j, cnt, gid;
  FontView *fv = (FontView *) _fv;
  SplineFont *sf = fv->b.sf;
  EncMap *map = fv->b.map;
  char *data;

  for (i = cnt = 0; i < map->enc_limit; ++i)
    if (fv->b.selected[i] && (gid = enc_to_gid (map, i)) != -1
        && sf->glyphs[gid] != NULL)
      cnt += strlen (sf->glyphs[gid]->name) + 1;
  data = xmalloc (cnt + 1);
  data[0] = '\0';
  for (cnt = 0, j = 1; j <= fv->sel_index; ++j)
    {
      for (i = cnt = 0; i < map->enc_limit; ++i)
        if (fv->b.selected[i] && (gid = enc_to_gid (map, i)) != -1
            && sf->glyphs[gid] != NULL)
          {
            strcpy (data + cnt, sf->glyphs[gid]->name);
            cnt += strlen (sf->glyphs[gid]->name);
            strcpy (data + cnt++, " ");
          }
    }
  if (cnt > 0)
    data[--cnt] = '\0';
  *len = cnt;
  return data;
}

static void
FVMouse (FontView *fv, GEvent *event)
{
  int pos =
    (event->u.mouse.y / fv->cbh + fv->rowoff) * fv->colcnt +
    event->u.mouse.x / fv->cbw;
  int gid;
  int realpos = pos;
  SplineChar *sc, dummy;
  int dopopup = true;

  if (event->type == et_mousedown)
    CVPaletteDeactivate ();
  if (pos < 0)
    {
      pos = 0;
      dopopup = false;
    }
  else if (pos >= fv->b.map->enc_limit)
    {
      pos = fv->b.map->enc_limit - 1;
      if (pos < 0)              /* No glyph slots in font */
        return;
      dopopup = false;
    }

  sc = (gid = enc_to_gid (fv->b.map, pos)) != -1 ? fv->b.sf->glyphs[gid] : NULL;
  if (sc == NULL)
    sc = SCBuildDummy (&dummy, fv->b.sf, fv->b.map, pos);
  if (event->type == et_mouseup && event->u.mouse.clicks == 2)
    {
      if (fv->pressed)
        {
          GDrawCancelTimer (fv->pressed);
          fv->pressed = NULL;
        }
      if (fv->b.container != NULL && fv->b.container->funcs->is_modal)
        return;
      if (fv->cur_subtable != NULL)
        {
          sc = FVMakeChar (fv, pos);
          pos = gid_to_enc (fv->b.map, sc->orig_pos);
        }
      if (sc == &dummy)
        {
          sc = SFMakeChar (fv->b.sf, fv->b.map, pos);
          gid = enc_to_gid (fv->b.map, pos);
        }
      if (fv->show == fv->filled)
        {
          SplineFont *sf = fv->b.sf;
          gid = -1;
          if (!OpenCharsInNewWindow)
            for (gid = sf->glyphcnt - 1; gid >= 0; --gid)
              if (sf->glyphs[gid] != NULL && sf->glyphs[gid]->views != NULL)
                break;
          if (gid != -1)
            {
              CharView *cv = (CharView *) (sf->glyphs[gid]->views);
              CVChangeSC (cv, sc);
              GDrawSetVisible (cv->gw, true);
              GDrawRaise (cv->gw);
            }
          else
            CharViewCreate (sc, fv, pos);
        }
      else
        {
          BDFFont *bdf = fv->show;
          BDFChar *bc = BDFMakeGID (bdf, gid);
          gid = -1;
          if (!OpenCharsInNewWindow)
            for (gid = bdf->glyphcnt - 1; gid >= 0; --gid)
              if (bdf->glyphs[gid] != NULL && bdf->glyphs[gid]->views != NULL)
                break;
          if (gid != -1)
            {
              BitmapView *bv = bdf->glyphs[gid]->views;
              BVChangeBC (bv, bc, true);
              GDrawSetVisible (bv->gw, true);
              GDrawRaise (bv->gw);
            }
          else
            BitmapViewCreate (bc, bdf, fv, pos);
        }
    }
  else if (event->type == et_mousemove)
    {
      if (dopopup)
        SCPreparePopup (fv->v, sc, fv->b.map->remap, pos,
                        sc == &dummy ? dummy.unicodeenc : UniFromEnc (pos,
                                                                      fv->
                                                                      b.map->enc));
    }
  if (event->type == et_mousedown)
    {
      if (fv->drag_and_drop)
        {
          GDrawSetCursor (fv->v, ct_pointer);
          fv->any_dd_events_sent = fv->drag_and_drop = false;
        }
      if (!(event->u.mouse.state & ksm_shift) && event->u.mouse.clicks <= 1)
        {
          if (!fv->b.selected[pos])
            FVDeselectAll (fv);
          else if (event->u.mouse.button != 3)
            {
              fv->drag_and_drop = fv->has_dd_no_cursor = true;
              fv->any_dd_events_sent = false;
              GDrawSetCursor (fv->v, ct_prohibition);
              GDrawGrabSelection (fv->v, sn_drag_and_drop);
              GDrawAddSelectionType (fv->v, sn_drag_and_drop, "STRING", fv, 0,
                                     sizeof (char), ddgencharlist, noop);
            }
        }
      fv->pressed_pos = fv->end_pos = pos;
      FVShowInfo (fv);
      if (!fv->drag_and_drop)
        {
          if (!(event->u.mouse.state & ksm_shift))
            fv->sel_index = 1;
          else if (fv->sel_index < 255)
            ++fv->sel_index;
          if (fv->pressed != NULL)
            {
              GDrawCancelTimer (fv->pressed);
              fv->pressed = NULL;
            }
          else if (event->u.mouse.state & ksm_shift)
            {
              fv->b.selected[pos] = fv->b.selected[pos] ? 0 : fv->sel_index;
              FVToggleCharSelected (fv, pos);
            }
          else if (!fv->b.selected[pos])
            {
              fv->b.selected[pos] = fv->sel_index;
              FVToggleCharSelected (fv, pos);
            }
          if (event->u.mouse.button == 3)
            GMenuCreatePopupMenu (fv->v, event, fvpopupmenu);
          else
            fv->pressed = GDrawRequestTimer (fv->v, 200, 100, NULL);
        }
    }
  else if (fv->drag_and_drop)
    {
      GWindow othergw = GDrawGetPointerWindow (fv->v);

      if (othergw == fv->v || othergw == fv->gw || othergw == NULL)
        {
          if (!fv->has_dd_no_cursor)
            {
              fv->has_dd_no_cursor = true;
              GDrawSetCursor (fv->v, ct_prohibition);
            }
        }
      else
        {
          if (fv->has_dd_no_cursor)
            {
              fv->has_dd_no_cursor = false;
              GDrawSetCursor (fv->v, ct_ddcursor);
            }
        }
      if (event->type == et_mouseup)
        {
          if (pos != fv->pressed_pos)
            {
              GDrawPostDragEvent (fv->v, event,
                                  event->type ==
                                  et_mouseup ? et_drop : et_drag);
              fv->any_dd_events_sent = true;
            }
          fv->drag_and_drop = fv->has_dd_no_cursor = false;
          GDrawSetCursor (fv->v, ct_pointer);
          if (!fv->any_dd_events_sent)
            FVDeselectAll (fv);
          fv->any_dd_events_sent = false;
        }
    }
  else if (fv->pressed != NULL)
    {
      int showit = realpos != fv->end_pos;
      FVReselect (fv, realpos);
      if (showit)
        FVShowInfo (fv);
      if (event->type == et_mouseup)
        {
          GDrawCancelTimer (fv->pressed);
          fv->pressed = NULL;
        }
    }
  if (event->type == et_mouseup && dopopup)
    SCPreparePopup (fv->v, sc, fv->b.map->remap, pos,
                    sc == &dummy ? dummy.unicodeenc : UniFromEnc (pos,
                                                                  fv->b.
                                                                  map->enc));
  if (event->type == et_mouseup)
    SVAttachFV (fv, 2);
}

static void
FVResize (FontView *fv, GEvent *event)
{
  extern int default_fv_row_count, default_fv_col_count;
  GRect pos, screensize;
  int topchar;

  if (fv->colcnt != 0)
    topchar = fv->rowoff * fv->colcnt;
  else if (fv->b.sf->top_enc != -1 && fv->b.sf->top_enc < fv->b.map->enc_limit)
    topchar = fv->b.sf->top_enc;
  else
    {
      /* Position on 'A' (or whatever they ask for) if it exists */
      topchar = SFFindSlot (fv->b.sf, fv->b.map, home_char, NULL);
      if (topchar == -1)
        {
          for (topchar = 0; topchar < fv->b.map->enc_limit; ++topchar)
            if (enc_to_gid (fv->b.map, topchar) != -1
                && fv->b.sf->glyphs[enc_to_gid (fv->b.map, topchar)] != NULL)
              break;
          if (topchar == fv->b.map->enc_limit)
            topchar = 0;
        }
    }
  if (!event->u.resize.sized)
    /* WM isn't responding to my resize requests, so no point in trying */ ;
  else
    if ((event->u.resize.size.width -
         GDrawPointsToPixels (fv->gw, _GScrollBar_Width) - 1) % fv->cbw != 0
        || (event->u.resize.size.height - fv->mbh - fv->infoh -
            1) % fv->cbh != 0)
    {
      int cc =
        (event->u.resize.size.width + fv->cbw / 2 -
         GDrawPointsToPixels (fv->gw, _GScrollBar_Width) - 1) / fv->cbw;
      int rc =
        (event->u.resize.size.height - fv->mbh - fv->infoh - 1) / fv->cbh;
      if (cc <= 0)
        cc = 1;
      if (rc <= 0)
        rc = 1;
      GDrawGetSize (GDrawGetRoot (NULL), &screensize);
      if (cc * fv->cbw + GDrawPointsToPixels (fv->gw, _GScrollBar_Width) >
          screensize.width)
        --cc;
      if (rc * fv->cbh + fv->mbh + fv->infoh + 10 > screensize.height)
        --rc;
      GDrawResize (fv->gw,
                   cc * fv->cbw + 1 + GDrawPointsToPixels (fv->gw,
                                                           _GScrollBar_Width),
                   rc * fv->cbh + 1 + fv->mbh + fv->infoh);
      /* somehow KDE loses this event of mine so to get even the vague effect */
      /*  we can't just return */
/*return;*/
    }

  pos.width = GDrawPointsToPixels (fv->gw, _GScrollBar_Width);
  pos.height = event->u.resize.size.height - fv->mbh - fv->infoh;
  pos.x = event->u.resize.size.width - pos.width;
  pos.y = fv->mbh + fv->infoh;
  GGadgetResize (fv->vsb, pos.width, pos.height);
  GGadgetMove (fv->vsb, pos.x, pos.y);
  pos.width = pos.x;
  pos.x = 0;
  GDrawResize (fv->v, pos.width, pos.height);

  fv->width = pos.width;
  fv->height = pos.height;
  fv->colcnt = (fv->width - 1) / fv->cbw;
  if (fv->colcnt < 1)
    fv->colcnt = 1;
  fv->rowcnt = (fv->height - 1) / fv->cbh;
  if (fv->rowcnt < 1)
    fv->rowcnt = 1;
  fv->rowltot = (fv->b.map->enc_limit + fv->colcnt - 1) / fv->colcnt;

  GScrollBarSetBounds (fv->vsb, 0, fv->rowltot, fv->rowcnt);
  fv->rowoff = topchar / fv->colcnt;
  if (fv->rowoff >= fv->rowltot - fv->rowcnt)
    fv->rowoff = fv->rowltot - fv->rowcnt;
  if (fv->rowoff < 0)
    fv->rowoff = 0;
  GScrollBarSetPos (fv->vsb, fv->rowoff);
  GDrawRequestExpose (fv->gw, NULL, true);
  GDrawRequestExpose (fv->v, NULL, true);

  if (fv->rowcnt != fv->b.sf->desired_row_cnt
      || fv->colcnt != fv->b.sf->desired_col_cnt)
    {
      default_fv_row_count = fv->rowcnt;
      default_fv_col_count = fv->colcnt;
      fv->b.sf->desired_row_cnt = fv->rowcnt;
      fv->b.sf->desired_col_cnt = fv->colcnt;
      SavePrefs (true);
    }
}

static void
FVTimer (FontView *fv, GEvent *event)
{

  if (event->u.timer.timer == fv->pressed)
    {
      GEvent e;
      GDrawGetPointerPosition (fv->v, &e);
      if (e.u.mouse.y < 0 || e.u.mouse.y >= fv->height)
        {
          real dy = 0;
          if (e.u.mouse.y < 0)
            dy = -1;
          else if (e.u.mouse.y >= fv->height)
            dy = 1;
          if (fv->rowoff + dy < 0)
            dy = 0;
          else if (fv->rowoff + dy + fv->rowcnt > fv->rowltot)
            dy = 0;
          fv->rowoff += dy;
          if (dy != 0)
            {
              GScrollBarSetPos (fv->vsb, fv->rowoff);
              GDrawScroll (fv->v, NULL, 0, dy * fv->cbh);
            }
        }
    }
  else if (event->u.timer.timer == fv->resize)
    {
      /* It's a delayed resize event (for kde which sends continuous resizes) */
      fv->resize = NULL;
      FVResize (fv, (GEvent *) (event->u.timer.userdata));
    }
  else if (event->u.timer.userdata != NULL)
    {
      /* It's a delayed function call */
      void (*func) (FontView *) =
        (void (*)(FontView *)) (event->u.timer.userdata);
      func (fv);
    }
}

void
FVDelay (FontView *fv, void (*func) (FontView *))
{
  GDrawRequestTimer (fv->v, 100, 0, (void *) func);
}

static int
FVScroll (GGadget *g, GEvent *e)
{
  FontView *fv = GGadgetGetUserData (g);
  int newpos = fv->rowoff;
  struct sbevent *sb = &e->u.control.u.sb;

  switch (sb->type)
    {
    case et_sb_top:
      newpos = 0;
      break;
    case et_sb_uppage:
      newpos -= fv->rowcnt;
      break;
    case et_sb_up:
      --newpos;
      break;
    case et_sb_down:
      ++newpos;
      break;
    case et_sb_downpage:
      newpos += fv->rowcnt;
      break;
    case et_sb_bottom:
      newpos = fv->rowltot - fv->rowcnt;
      break;
    case et_sb_thumb:
    case et_sb_thumbrelease:
      newpos = sb->pos;
      break;
    }
  if (newpos > fv->rowltot - fv->rowcnt)
    newpos = fv->rowltot - fv->rowcnt;
  if (newpos < 0)
    newpos = 0;
  if (newpos != fv->rowoff)
    {
      int diff = newpos - fv->rowoff;
      fv->rowoff = newpos;
      GScrollBarSetPos (fv->vsb, fv->rowoff);
      GDrawScroll (fv->v, NULL, 0, diff * fv->cbh);
    }
  return true;
}

static int
v_e_h (GWindow gw, GEvent *event)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if ((event->type == et_mouseup || event->type == et_mousedown)
      && (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return GGadgetDispatchEvent (fv->vsb, event);
    }

  GGadgetPopupExternalEvent (event);
  switch (event->type)
    {
    case et_expose:
      GDrawSetLineWidth (gw, 0);
      FVExpose (fv, gw, event);
      break;
    case et_char:
      if (fv->b.container != NULL)
        (fv->b.container->funcs->charEvent) (fv->b.container, event);
      else
        FVChar (fv, event);
      break;
    case et_mousemove:
    case et_mousedown:
    case et_mouseup:
      if (event->type == et_mousedown)
        GDrawSetGIC (gw, fv->gic, 0, 20);
      if (fv->notactive && event->type == et_mousedown)
        (fv->b.container->funcs->activateMe) (fv->b.container, &fv->b);
      FVMouse (fv, event);
      break;
    case et_timer:
      FVTimer (fv, event);
      break;
    case et_focus:
      if (event->u.focus.gained_focus)
        {
          GDrawSetGIC (gw, fv->gic, 0, 20);
#if 0
          CVPaletteDeactivate ();
#endif
        }
      break;
    }
  return true;
}

static void
FontView_ReformatOne (FontView *fv)
{
  FontView *fvs;

  if (fv->v == NULL || fv->colcnt == 0) /* Can happen in scripts */
    return;

  GDrawSetCursor (fv->v, ct_watch);
  fv->rowltot = (fv->b.map->enc_limit + fv->colcnt - 1) / fv->colcnt;
  GScrollBarSetBounds (fv->vsb, 0, fv->rowltot, fv->rowcnt);
  if (fv->rowoff > fv->rowltot - fv->rowcnt)
    {
      fv->rowoff = fv->rowltot - fv->rowcnt;
      if (fv->rowoff < 0)
        fv->rowoff = 0;
      GScrollBarSetPos (fv->vsb, fv->rowoff);
    }
  for (fvs = (FontView *) (fv->b.sf->fv); fvs != NULL;
       fvs = (FontView *) (fvs->b.nextsame))
    if (fvs != fv && fvs->b.sf == fv->b.sf)
      break;
  GDrawRequestExpose (fv->v, NULL, false);
  GDrawSetCursor (fv->v, ct_pointer);
}

static void
FontView_ReformatAll (SplineFont *sf)
{
  BDFFont *new, *old, *bdf;
  FontView *fv;
  MetricsView *mvs;
  extern int use_freetype_to_rasterize_fv;

  if (((FontView *) (sf->fv))->v == NULL || ((FontView *) (sf->fv))->colcnt == 0)       /* Can happen in scripts */
    return;

  for (fv = (FontView *) (sf->fv); fv != NULL;
       fv = (FontView *) (fv->b.nextsame))
    {
      int flags = 0;
      GDrawSetCursor (fv->v, ct_watch);
      old = fv->filled;
      flags |= fv->antialias ? pf_antialias : 0;
      flags |= fv->bbsized ? pf_bbsized : 0;
      flags |= use_freetype_to_rasterize_fv && !sf->strokedfont
        && !sf->multilayer ? pf_ft_nohints : 0;
      /* In CID fonts fv->b.sf may not be same as sf */
      new =
        SplineFontPieceMeal (fv->b.sf, fv->b.active_layer,
                             fv->filled->pixelsize, 72, flags, NULL);
      fv->filled = new;
      if (fv->show == old)
        fv->show = new;
      else
        {
          for (bdf = sf->bitmaps;
               bdf != NULL && (bdf->pixelsize != fv->show->pixelsize
                               || BDFDepth (bdf) != BDFDepth (fv->show));
               bdf = bdf->next);
          if (bdf != NULL)
            fv->show = bdf;
          else
            fv->show = new;
        }
      BDFFontFree (old);
      fv->rowltot = (fv->b.map->enc_limit + fv->colcnt - 1) / fv->colcnt;
      GScrollBarSetBounds (fv->vsb, 0, fv->rowltot, fv->rowcnt);
      if (fv->rowoff > fv->rowltot - fv->rowcnt)
        {
          fv->rowoff = fv->rowltot - fv->rowcnt;
          if (fv->rowoff < 0)
            fv->rowoff = 0;
          GScrollBarSetPos (fv->vsb, fv->rowoff);
        }
      GDrawRequestExpose (fv->v, NULL, false);
      GDrawSetCursor (fv->v, ct_pointer);
    }
  for (mvs = sf->metrics; mvs != NULL; mvs = mvs->next)
    if (mvs->bdf == NULL)
      {
        BDFFontFree (mvs->show);
        int flags = 0;
        flags |=
          mvs->antialias ? (pf_antialias | pf_ft_recontext) : pf_ft_recontext;
        mvs->show =
          SplineFontPieceMeal (sf, mvs->layer, mvs->ptsize, mvs->dpi, flags,
                               NULL);
        GDrawRequestExpose (mvs->gw, NULL, false);
      }
}

static void
FontViewRemove (FontView *fv)
{
  if (fv_list == fv)
    fv_list = (FontView *) (fv->b.next);
  else
    {
      FontView *n;
      for (n = fv_list; n->b.next != &fv->b; n = (FontView *) (n->b.next));
      n->b.next = fv->b.next;
    }
  if (fv_list != NULL)          /* Freeing a large font can take forever, and if we're just going to exit there's no real reason to do so... */
    FontViewFree (&fv->b);
}

static int
fv_e_h (GWindow gw, GEvent *event)
{
  FontView *fv = (FontView *) GDrawGetUserData (gw);

  if ((event->type == et_mouseup || event->type == et_mousedown)
      && (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return GGadgetDispatchEvent (fv->vsb, event);
    }

  switch (event->type)
    {
    case et_selclear:
      ClipboardClear ();
      break;
    case et_expose:
      GDrawSetLineWidth (gw, 0);
      FVDrawInfo (fv, gw, event);
      break;
    case et_resize:
      /* KDE sends a continuous stream of resize events, and gets very */
      /*  confused if I start resizing the window myself, try to wait for */
      /*  the user to finish before responding to resizes */
      if (event->u.resize.sized || fv->resize_expected)
        {
          if (fv->resize)
            GDrawCancelTimer (fv->resize);
          fv->resize_event = *event;
          fv->resize =
            GDrawRequestTimer (fv->v, 300, 0, (void *) &fv->resize_event);
          fv->resize_expected = false;
        }
      break;
    case et_char:
      if (fv->b.container != NULL)
        (fv->b.container->funcs->charEvent) (fv->b.container, event);
      else
        FVChar (fv, event);
      break;
    case et_mousedown:
      GDrawSetGIC (gw, fv->gwgic, 0, 20);
      if (fv->notactive)
        (fv->b.container->funcs->activateMe) (fv->b.container, &fv->b);
      break;
    case et_close:
      FVMenuClose (gw, NULL, NULL);
      break;
    case et_create:
      fv->b.next = (FontViewBase *) fv_list;
      fv_list = fv;
      break;
    case et_destroy:
      if (fv->qg != NULL)
        QGRmFontView (fv->qg, fv);
      FontViewRemove (fv);
      break;
    }
  return true;
}

static void
FontViewOpenKids (FontView *fv)
{
  SplineFont *sf = fv->b.sf;

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;

  int k = 0;
  do
    {
      SplineFont *_sf = sf->subfontcnt == 0 ? sf : sf->subfonts[k];
      for (int i = 0; i < _sf->glyphcnt; i++)
        if (_sf->glyphs[i] != NULL && _sf->glyphs[i]->wasopen)
          {
            _sf->glyphs[i]->wasopen = false;
            CharViewCreate (_sf->glyphs[i], fv, -1);
          }
      k++;
    }
  while (k < sf->subfontcnt);
}

static FontView *
__FontViewCreate (SplineFont *sf)
{
  int i;

  FontView *fv = xzalloc (sizeof (FontView));
  fv->b.tag = FF_FONT_WINDOW;

  int ps =
    sf->display_size < 0 ? -sf->display_size : sf->display_size ==
    0 ? default_fv_font_size : sf->display_size;

  if (ps > 200)
    ps = 128;

  /* Filename != NULL if we opened an sfd file. Sfd files know whether */
  /*  the font is compact or not and should not depend on a global flag */
  /* If a font is new, then compaction will make it vanish completely */
  if (sf->fv == NULL && compact_font_on_open && sf->filename == NULL
      && !sf->new)
    {
      sf->compacted = true;
      for (i = 0; i < sf->subfontcnt; ++i)
        sf->subfonts[i]->compacted = true;
    }
  fv->b.nextsame = sf->fv;
  fv->b.active_layer = sf->display_layer;
  sf->fv = (FontViewBase *) fv;
  if (sf->mm != NULL)
    {
      sf->mm->normal->fv = (FontViewBase *) fv;
      for (i = 0; i < sf->mm->instance_count; ++i)
        sf->mm->instances[i]->fv = (FontViewBase *) fv;
    }
  if (sf->subfontcnt == 0)
    {
      fv->b.sf = sf;
      if (fv->b.nextsame != NULL)
        {
          fv->b.map = EncMapCopy (fv->b.nextsame->map);
          fv->b.normal =
            fv->b.nextsame->normal ==
            NULL ? NULL : EncMapCopy (fv->b.nextsame->normal);
        }
      else if (sf->compacted)
        {
          fv->b.normal = sf->map;
          fv->b.map = CompactEncMap (EncMapCopy (sf->map), sf);
        }
      else
        {
          fv->b.map = sf->map;
          fv->b.normal = NULL;
        }
    }
  else
    {
      fv->b.cidmaster = sf;
      for (i = 0; i < sf->subfontcnt; ++i)
        sf->subfonts[i]->fv = (FontViewBase *) fv;
      for (i = 0; i < sf->subfontcnt; ++i)      /* Search for a subfont that contains more than ".notdef" (most significant in .gai fonts) */
        if (sf->subfonts[i]->glyphcnt > 1)
          {
            fv->b.sf = sf->subfonts[i];
            break;
          }
      if (fv->b.sf == NULL)
        fv->b.sf = sf->subfonts[0];
      sf = fv->b.sf;
      if (fv->b.nextsame == NULL)
        {
          EncMapFree (sf->map);
          sf->map = NULL;
        }
      fv->b.map = EncMap1to1 (sf->glyphcnt);
      if (sf->compacted)
        {
          fv->b.normal = fv->b.map;
          fv->b.map = CompactEncMap (EncMapCopy (fv->b.map), sf);
        }
    }
  fv->b.selected = xcalloc (fv->b.map->enc_limit, sizeof (char));
  fv->user_requested_magnify = -1;
  fv->magnify = (ps <= 9) ? 3 : (ps < 20) ? 2 : 1;
  fv->cbw = (ps * fv->magnify) + 1;
  fv->cbh = (ps * fv->magnify) + 1 + fv->lab_height + 1;
  fv->antialias = sf->display_antialias;
  fv->bbsized = sf->display_bbsized;
  fv->glyphlabel = default_fv_glyphlabel;

  fv->end_pos = -1;
#ifndef _NO_PYTHON
  PyFF_InitFontHook ((FontViewBase *) fv);
#endif
  return fv;
}

static void
FontViewInit (void)
{
  static int done = false;

  if (done)
    return;

  done = true;
  mb2DoGetText (mblist);
  mbDoGetText (fvpopupmenu);
}

static GResStruct fontview_re[] = {
  {"GlyphInfoColor", rt_color, &fvglyphinfocol, NULL, 0},
  {"EmptySlotFgColor", rt_color, &fvemtpyslotfgcol, NULL, 0},
  {"SelectedColor", rt_color, &fvselcol, NULL, 0},
  {"SelectedFgColor", rt_color, &fvselfgcol, NULL, 0},
  {"ChangedColor", rt_color, &fvchangedcol, NULL, 0},
  {"HintingNeededColor", rt_color, &fvhintingneededcol, NULL, 0},
  {"FontSize", rt_int, &fv_fontsize, NULL, 0},
  {"FontFamily", rt_string, &fv_fontnames, NULL, 0},
  GRESSTRUCT_EMPTY
};

static void
FVCreateInnards (FontView *fv, GRect *pos)
{
  GWindow gw = fv->gw;
  GWindowAttrs wattrs;
  GGadgetData gd;
  BDFFont *bdf;
  int as, ds, ld;
  extern int use_freetype_to_rasterize_fv;
  SplineFont *sf = fv->b.sf;
  int flags = 0;
  int ptsize;

  fv->lab_height = FV_LAB_HEIGHT - 13 + GDrawPointsToPixels (NULL, fv_fontsize);

  memset (&gd, 0, sizeof (gd));
  gd.pos.y = pos->y;
  gd.pos.height = pos->height;
  gd.pos.width = GDrawPointsToPixels (gw, _GScrollBar_Width);
  gd.pos.x = pos->width;
  gd.u.sbinit = NULL;
  gd.flags = gg_visible | gg_enabled | gg_pos_in_pixels | gg_sb_vert;
  gd.handle_controlevent = FVScroll;
  fv->vsb = GScrollBarCreate (gw, &gd, fv);


  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask = wam_events | wam_cursor | wam_backcol;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.cursor = ct_pointer;
  wattrs.background_color = view_bgcol;
  fv->v = GWidgetCreateSubWindow (gw, pos, v_e_h, fv, &wattrs);
  GDrawSetVisible (fv->v, true);

  fv->gic = GDrawCreateInputContext (fv->v, gic_root | gic_orlesser);
  fv->gwgic = GDrawCreateInputContext (fv->gw, gic_root | gic_orlesser);
  GDrawSetGIC (fv->v, fv->gic, 0, 20);
  GDrawSetGIC (fv->gw, fv->gic, 0, 20);

  fv->fontset = xcalloc (_uni_fontmax, sizeof (GFont *));
  fv->fontset[0] = GDrawNewFont (gw, fv_fontnames, fv_fontsize, 400, fs_none);
  GDrawSetFont (fv->v, fv->fontset[0]);
  GDrawGetFontMetrics (fv->v, fv->fontset[0], &as, &ds, &ld);
  fv->lab_as = as;
  fv->showhmetrics = default_fv_showhmetrics;
  fv->showvmetrics = default_fv_showvmetrics && sf->hasvmetrics;
  flags |= fv->antialias ? pf_antialias : 0;
  flags |= fv->bbsized ? pf_bbsized : 0;
  flags |= use_freetype_to_rasterize_fv && !sf->strokedfont
    && !sf->multilayer ? pf_ft_nohints : 0;
  if (sf->display_size < 0)
    ptsize = -sf->display_size;
  else
    ptsize = default_fv_font_size;
  bdf =
    SplineFontPieceMeal (fv->b.sf, fv->b.active_layer, ptsize, 72, flags, NULL);
  fv->filled = bdf;
  if (sf->display_size > 0)
    {
      for (bdf = sf->bitmaps;
           bdf != NULL && bdf->pixelsize != sf->display_size; bdf = bdf->next);
      if (bdf == NULL)
        bdf = fv->filled;
    }
  if (sf->onlybitmaps && bdf == fv->filled && sf->bitmaps != NULL)
    bdf = sf->bitmaps;
  fv->cbw = -1;
  FVChangeDisplayFont (fv, bdf);
}

static FontView *
FontView_Create (SplineFont *sf, int hide)
{
  FontView *fv = (FontView *) __FontViewCreate (sf);
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetData gd;
  GRect gsize;
  static GWindow icon = NULL;
  static int nexty = 0;
  GRect size;

  FontViewInit ();
  if (icon == NULL)
    icon =
      GDrawCreateBitmap (NULL, fontview_width, fontview_height, fontview_bits);

  GDrawGetSize (GDrawGetRoot (NULL), &size);

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask = wam_events | wam_cursor | wam_icon;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.cursor = ct_pointer;
  wattrs.icon = icon;
  pos.width = sf->desired_col_cnt * fv->cbw + 1;
  pos.height = sf->desired_row_cnt * fv->cbh + 1;
  pos.x = size.width - pos.width - 30;
  pos.y = nexty;
  nexty += 2 * fv->cbh + 50;
  if (nexty + pos.height > size.height)
    nexty = 0;
  fv->gw = gw = GDrawCreateTopWindow (NULL, &pos, fv_e_h, fv, &wattrs);
  FontViewSetTitle (fv);

  if (!fv_fs_init)
    {
      GResourceFind (fontview_re, "FontView.");
      view_bgcol =
        GResourceFindColor ("View.Background", COLOR_CREATE (0xff, 0xff, 0xff));
      fv_fs_init = true;
    }

  memset (&gd, 0, sizeof (gd));
  gd.flags = gg_visible | gg_enabled;
  helplist[0].invoke = FVMenuContextualHelp;
#define CALLBACKS_INDEX 3       /* FIXME: There has to be a better way than this. */
  if (fv_menu != NULL)
    mblist[CALLBACKS_INDEX].ti.disabled = false;
  mblist[CALLBACKS_INDEX].sub = fv_menu;
  gd.u.menu2 = mblist;
  fv->mb = GMenu2BarCreate (gw, &gd, NULL);
  GGadgetGetSize (fv->mb, &gsize);
  fv->mbh = gsize.height;
  fv->infoh = 1 + GDrawPointsToPixels (NULL, fv_fontsize);

  pos.x = 0;
  pos.y = fv->mbh + fv->infoh;
  FVCreateInnards (fv, &pos);

  if (!hide)
    {
      GDrawSetVisible (gw, true);
      FontViewOpenKids (fv);
    }
  return fv;
}

static FontView *
FontView_Append (FontView *fv)
{
  /* Normally fontviews get added to the fv list when their windows are */
  /*  created. but we don't create any windows here, so... */
  FontView *test;

  if (fv_list == NULL)
    fv_list = fv;
  else
    {
      for (test = fv_list; test->b.next != NULL;
           test = (FontView *) test->b.next);
      test->b.next = (FontViewBase *) fv;
    }
  return fv;
}

FontView *
FontNew (void)
{
  return FontView_Create (SplineFontNew (), false);
}

static void
FontView_Free (FontView *fv)
{
  int i;
  FontView *prev;
  FontView *fvs;

  if (fv->b.sf == NULL)         /* Happens when usurping a font to put it into an MM */
    BDFFontFree (fv->filled);
  else if (fv->b.nextsame == NULL && fv->b.sf->fv == &fv->b)
    {
      EncMapFree (fv->b.map);
      SplineFontFree (fv->b.cidmaster ? fv->b.cidmaster : fv->b.sf);
      BDFFontFree (fv->filled);
    }
  else
    {
      EncMapFree (fv->b.map);
      for (fvs = (FontView *) (fv->b.sf->fv), i = 0; fvs != NULL;
           fvs = (FontView *) (fvs->b.nextsame))
        if (fvs->filled == fv->filled)
          ++i;
      if (i == 1)
        BDFFontFree (fv->filled);
      if (fv->b.sf->fv == &fv->b)
        {
          if (fv->b.cidmaster == NULL)
            fv->b.sf->fv = fv->b.nextsame;
          else
            {
              fv->b.cidmaster->fv = fv->b.nextsame;
              for (i = 0; i < fv->b.cidmaster->subfontcnt; ++i)
                fv->b.cidmaster->subfonts[i]->fv = fv->b.nextsame;
            }
        }
      else
        {
          for (prev = (FontView *) (fv->b.sf->fv); prev->b.nextsame != &fv->b;
               prev = (FontView *) (prev->b.nextsame));
          prev->b.nextsame = fv->b.nextsame;
        }
    }
  free (fv->b.selected);
  free (fv->fontset);
#ifndef _NO_PYTHON
  PyFF_FreeFV (&fv->b);
#endif
  free (fv);
}

static int
FontViewWinInfo (FontView *fv, int *cc, int *rc)
{
  if (fv == NULL || fv->colcnt == 0 || fv->rowcnt == 0)
    {
      *cc = 16;
      *rc = 4;
      return -1;
    }

  *cc = fv->colcnt;
  *rc = fv->rowcnt;

  return fv->rowoff * fv->colcnt;
}

static FontViewBase *
FVAny (void)
{
  return (FontViewBase *) fv_list;
}

static int
FontIsActive (SplineFont *sf)
{
  FontView *fv;

  for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
    if (fv->b.sf == sf)
      return true;

  return false;
}

static SplineFont *
FontOfFilename (const char *filename)
{
  FontView *fv;

  char *abs_file =
    XDIE_ON_NULL (canonicalize_filename_mode (filename, CAN_MISSING));
  for (fv = fv_list; fv != NULL; fv = (FontView *) (fv->b.next))
    {
      if (fv->b.sf->filename != NULL
          && strcmp (fv->b.sf->filename, abs_file) == 0)
        return fv->b.sf;
      else if (fv->b.sf->origname != NULL
               && strcmp (fv->b.sf->origname, abs_file) == 0)
        return fv->b.sf;
    }
  free (abs_file);
  return NULL;
}

static void
FVExtraEncSlots (FontView *fv, int encmax)
{
  if (fv->colcnt != 0)
    {                           /* Ie. scripting vs. UI */
      fv->rowltot = (encmax + 1 + fv->colcnt - 1) / fv->colcnt;
      GScrollBarSetBounds (fv->vsb, 0, fv->rowltot, fv->rowcnt);
    }
}

static void
FV_BiggerGlyphCache (FontView *fv, int gidcnt)
{
  if (fv->filled != NULL)
    BDFOrigFixup (fv->filled, gidcnt, fv->b.sf);
}

static void
FontView_Close (FontView *fv)
{
  if (fv->gw != NULL)
    GDrawDestroyWindow (fv->gw);
  else
    FontViewRemove (fv);
}


struct fv_interface gdraw_fv_interface = {
  (FontViewBase *(*)(SplineFont *, int)) FontView_Create,
  (FontViewBase *(*)(SplineFont *)) __FontViewCreate,
  (void (*)(FontViewBase *)) FontView_Close,
  (void (*)(FontViewBase *)) FontView_Free,
  (void (*)(FontViewBase *)) FontViewSetTitle,
  FontViewSetTitles,
  FontViewRefreshAll,
  (void (*)(FontViewBase *)) FontView_ReformatOne,
  FontView_ReformatAll,
  (void (*)(FontViewBase *)) FV_LayerChanged,
  FV_ToggleCharChanged,
  (int (*)(FontViewBase *, int *, int *)) FontViewWinInfo,
  FontIsActive,
  FVAny,
  (FontViewBase *(*)(FontViewBase *)) FontView_Append,
  FontOfFilename,
  (void (*)(FontViewBase *, int)) FVExtraEncSlots,
  (void (*)(FontViewBase *, int)) FV_BiggerGlyphCache,
  (void (*)(FontViewBase *, BDFFont *)) FV_ChangeDisplayBitmap,
  (void (*)(FontViewBase *)) FV_ShowFilled,
  FV_ReattachCVs,
  (void (*)(FontViewBase *)) FVDeselectAll,
  (void (*)(FontViewBase *, int)) FVScrollToGID,
  (void (*)(FontViewBase *, int)) FVScrollToChar,
  (void (*)(FontViewBase *, int)) FV_ChangeGID,
  SF_CloseAllInstrs
};

/* ************************************************************************** */
/* ***************************** Embedded FontViews ************************* */
/* ************************************************************************** */

static void
FVCopyInnards (FontView *fv, GRect *pos, int infoh, FontView *fvorig,
               GWindow dw, int def_layer, struct fvcontainer *kf)
{

  fv->notactive = true;
  fv->gw = dw;
  fv->infoh = infoh;
  fv->b.container = kf;
  fv->rowcnt = 4;
  fv->colcnt = 16;
  fv->b.active_layer = def_layer;
  FVCreateInnards (fv, pos);
  memcpy (fv->b.selected, fvorig->b.selected, fv->b.map->enc_limit);
  fv->rowoff = (fvorig->rowoff * fvorig->colcnt) / fv->colcnt;
}

void
KFFontViewInits (struct kf_dlg *kf, GGadget *drawable)
{
  GGadgetData gd;
  GRect pos, gsize, sbsize;
  GWindow dw = GDrawableGetWindow (drawable);
  int infoh;
  int ps;
  FontView *fvorig = (FontView *) kf->sf->fv;

  FontViewInit ();

  kf->dw = dw;

  memset (&gd, 0, sizeof (gd));
  gd.flags = gg_visible | gg_enabled;
  helplist[0].invoke = FVMenuContextualHelp;
  gd.u.menu2 = mblist;
  kf->mb = GMenu2BarCreate (dw, &gd, NULL);
  GGadgetGetSize (kf->mb, &gsize);
  kf->mbh = gsize.height;
  kf->guts = drawable;

  ps = kf->sf->display_size;
  kf->sf->display_size = -24;
  kf->first_fv = __FontViewCreate (kf->sf);
  kf->first_fv->b.container = (struct fvcontainer *) kf;
  kf->second_fv = __FontViewCreate (kf->sf);
  kf->second_fv->b.container = (struct fvcontainer *) kf;

  kf->infoh = infoh = 1 + GDrawPointsToPixels (NULL, fv_fontsize);
  kf->first_fv->mbh = kf->mbh;
  pos.x = 0;
  pos.y = kf->mbh + infoh + kf->fh + 4;
  pos.width = 16 * kf->first_fv->cbw + 1;
  pos.height = 4 * kf->first_fv->cbh + 1;

  GDrawSetUserData (dw, kf->first_fv);
  FVCopyInnards (kf->first_fv, &pos, infoh, fvorig, dw, kf->def_layer,
                 (struct fvcontainer *) kf);
  pos.height = 4 * kf->first_fv->cbh + 1;       /* We don't know the real fv->cbh until after creating the innards. The size of the last window is probably wrong, we'll fix later */
  kf->second_fv->mbh = kf->mbh;
  kf->label2_y = pos.y + pos.height + 2;
  pos.y = kf->label2_y + kf->fh + 2;
  GDrawSetUserData (dw, kf->second_fv);
  FVCopyInnards (kf->second_fv, &pos, infoh, fvorig, dw, kf->def_layer,
                 (struct fvcontainer *) kf);

  kf->sf->display_size = ps;

  GGadgetGetSize (kf->second_fv->vsb, &sbsize);
  gsize.x = gsize.y = 0;
  gsize.width = pos.width + sbsize.width;
  gsize.height = pos.y + pos.height;
  GGadgetSetDesiredSize (drawable, NULL, &gsize);
}

/* ************************************************************************** */
/* ************************** Glyph Set from Selection ********************** */
/* ************************************************************************** */

struct gsd
{
  struct fvcontainer base;
  FontView *fv;
  int done;
  int good;
  GWindow gw;
};

static void
gs_activateMe (struct fvcontainer *UNUSED (fvc), FontViewBase *UNUSED (fvb))
{
  /*struct gsd *gs = (struct gsd *) fvc; */
}

static void
gs_charEvent (struct fvcontainer *fvc, void *event)
{
  struct gsd *gs = (struct gsd *) fvc;
  FVChar (gs->fv, event);
}

static void
gs_doClose (struct fvcontainer *fvc)
{
  struct gsd *gs = (struct gsd *) fvc;
  gs->done = true;
}

#define CID_Guts	1000
#define CID_TopBox	1001

static void
gs_doResize (struct fvcontainer *fvc, FontViewBase *UNUSED (fvb), int width,
             int height)
{
  struct gsd *gs = (struct gsd *) fvc;
  /*FontView *fv = (FontView *) fvb; */
  GRect size;

  memset (&size, 0, sizeof (size));
  size.width = width;
  size.height = height;
  GGadgetSetDesiredSize (GWidgetGetControl (gs->gw, CID_Guts), NULL, &size);
  GHVBoxFitWindow (GWidgetGetControl (gs->gw, CID_TopBox));
}

static struct fvcontainer_funcs glyphset_funcs = {
  fvc_glyphset,
  true,                         /* Modal dialog. No charviews, etc. */
  gs_activateMe,
  gs_charEvent,
  gs_doClose,
  gs_doResize
};

static int
GS_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct gsd *gs = GDrawGetUserData (GGadgetGetWindow (g));
      gs->done = true;
      gs->good = true;
    }
  return true;
}

static int
GS_Cancel (GGadget *g, GEvent *e)
{
  struct gsd *gs;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      gs = GDrawGetUserData (GGadgetGetWindow (g));
      gs->done = true;
    }
  return true;
}

static void
gs_sizeSet (struct gsd *gs, GWindow dw)
{
  GRect size, gsize;
  int width, height, y;
  int cc, rc, topchar;
  GRect subsize;
  FontView *fv = gs->fv;

  if (gs->fv->vsb == NULL)
    return;

  GDrawGetSize (dw, &size);
  GGadgetGetSize (gs->fv->vsb, &gsize);
  width = size.width - gsize.width;
  height = size.height - gs->fv->mbh - gs->fv->infoh;

  y = gs->fv->mbh + gs->fv->infoh;

  topchar = fv->rowoff * fv->colcnt;
  cc = (width - 1) / fv->cbw;
  if (cc < 1)
    cc = 1;
  rc = (height - 1) / fv->cbh;
  if (rc < 1)
    rc = 1;
  subsize.x = 0;
  subsize.y = 0;
  subsize.width = cc * fv->cbw + 1;
  subsize.height = rc * fv->cbh + 1;
  GDrawResize (fv->v, subsize.width, subsize.height);
  GDrawMove (fv->v, 0, y);
  GGadgetMove (fv->vsb, subsize.width, y);
  GGadgetResize (fv->vsb, gsize.width, subsize.height);

  fv->colcnt = cc;
  fv->rowcnt = rc;
  fv->width = subsize.width;
  fv->height = subsize.height;
  fv->rowltot = (fv->b.map->enc_limit + fv->colcnt - 1) / fv->colcnt;
  GScrollBarSetBounds (fv->vsb, 0, fv->rowltot, fv->rowcnt);
  fv->rowoff = topchar / fv->colcnt;
  if (fv->rowoff >= fv->rowltot - fv->rowcnt)
    fv->rowoff = fv->rowltot - fv->rowcnt;
  if (fv->rowoff < 0)
    fv->rowoff = 0;
  GScrollBarSetPos (fv->vsb, fv->rowoff);

  GDrawRequestExpose (fv->v, NULL, true);
}

static int
gs_sub_e_h (GWindow pixmap, GEvent *event)
{
  FontView *active_fv;
  struct gsd *gs;

  if (event->type == et_destroy)
    return true;

  active_fv = (FontView *) GDrawGetUserData (pixmap);
  gs = (struct gsd *) (active_fv->b.container);

  if ((event->type == et_mouseup || event->type == et_mousedown)
      && (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return GGadgetDispatchEvent (active_fv->vsb, event);
    }


  switch (event->type)
    {
    case et_expose:
      FVDrawInfo (active_fv, pixmap, event);
      break;
    case et_char:
      gs_charEvent (&gs->base, event);
      break;
    case et_mousedown:
      return false;
      break;
    case et_mouseup:
    case et_mousemove:
      return false;
    case et_resize:
      gs_sizeSet (gs, pixmap);
      break;
    }
  return true;
}

static int
gs_e_h (GWindow gw, GEvent *event)
{
  struct gsd *gs = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_close:
      gs->done = true;
      break;
    case et_char:
      FVChar (gs->fv, event);
      break;
    }
  return true;
}

char *
GlyphSetFromSelection (SplineFont *sf, int def_layer, char *current)
{
  struct gsd gs;
  GRect pos;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[5], boxes[3];
  GGadgetCreateData *varray[21], *buttonarray[8];
  GTextInfo label[5];
  int i, j, k, guts_row, gid, len;
  char *ret, *rpt;
  SplineChar *sc;
  GGadget *drawable;
  GWindow dw;
  GGadgetData gd;
  GRect gsize, sbsize;
  int infoh, mbh;
  int ps;
  FontView *fvorig = (FontView *) sf->fv;
  GGadget *mb;
  char *start, *pt;
  int ch;

  FontViewInit ();

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));
  memset (&gs, 0, sizeof (gs));

  gs.base.funcs = &glyphset_funcs;

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = true;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Glyph Set by Selection");
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  gs.gw = GDrawCreateTopWindow (NULL, &pos, gs_e_h, &gs, &wattrs);

  i = j = 0;

  guts_row = j / 2;
  gcd[i].gd.flags = gg_enabled | gg_visible;
  gcd[i].gd.cid = CID_Guts;
  gcd[i].gd.u.drawable_e_h = gs_sub_e_h;
  gcd[i].creator = GDrawableCreate;
  varray[j++] = &gcd[i++];
  varray[j++] = NULL;

  label[i].text =
    (uint32_t *)
    _
    ("Select glyphs in the font view above.\nThe selected glyphs become your glyph class.");
  label[i].text_is_1byte = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags = gg_enabled | gg_visible;
  gcd[i].creator = GLabelCreate;
  varray[j++] = &gcd[i++];
  varray[j++] = NULL;

  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[i].text = (uint32_t *) _("_OK");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = GS_OK;
  gcd[i++].creator = GButtonCreate;

  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[i].text = (uint32_t *) _("_Cancel");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = GS_Cancel;
  gcd[i++].creator = GButtonCreate;

  buttonarray[0] = GCD_Glue;
  buttonarray[1] = &gcd[i - 2];
  buttonarray[2] = GCD_Glue;
  buttonarray[3] = GCD_Glue;
  buttonarray[4] = &gcd[i - 1];
  buttonarray[5] = GCD_Glue;
  buttonarray[6] = NULL;
  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = buttonarray;
  boxes[2].creator = GHBoxCreate;
  varray[j++] = &boxes[2];
  varray[j++] = NULL;
  varray[j++] = NULL;

  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = varray;
  boxes[0].gd.cid = CID_TopBox;
  boxes[0].creator = GHVGroupCreate;

  GGadgetsCreate (gs.gw, boxes);

  GHVBoxSetExpandableRow (boxes[0].ret, guts_row);
  GHVBoxSetExpandableCol (boxes[2].ret, gb_expandgluesame);

  drawable = GWidgetGetControl (gs.gw, CID_Guts);
  dw = GDrawableGetWindow (drawable);

  memset (&gd, 0, sizeof (gd));
  gd.flags = gg_visible | gg_enabled;
  helplist[0].invoke = FVMenuContextualHelp;
  gd.u.menu2 = mblist;
  mb = GMenu2BarCreate (dw, &gd, NULL);
  GGadgetGetSize (mb, &gsize);
  mbh = gsize.height;

  ps = sf->display_size;
  sf->display_size = -24;
  gs.fv = __FontViewCreate (sf);

  infoh = 1 + GDrawPointsToPixels (NULL, fv_fontsize);
  gs.fv->mbh = mbh;
  pos.x = 0;
  pos.y = mbh + infoh;
  pos.width = 16 * gs.fv->cbw + 1;
  pos.height = 4 * gs.fv->cbh + 1;

  GDrawSetUserData (dw, gs.fv);
  FVCopyInnards (gs.fv, &pos, infoh, fvorig, dw, def_layer,
                 (struct fvcontainer *) &gs);
  pos.height = 4 * gs.fv->cbh + 1;      /* We don't know the real fv->cbh
                                           until after creating the
                                           innards. The size of the last
                                           window is probably wrong, we'll
                                           fix later */
  memset (gs.fv->b.selected, 0, gs.fv->b.map->enc_limit);
  if (current != NULL && strcmp (current, _("{Everything Else}")) != 0)
    {
      int first = true;
      for (start = current; *start == ' '; ++start);
      while (*start)
        {
          for (pt = start; *pt != '\0' && *pt != ' '; ++pt);
          ch = *pt;
          *pt = '\0';
          sc = SFGetChar (sf, -1, start);
          *pt = ch;
          if (sc != NULL && gid_to_enc_is_set (gs.fv->b.map, sc->orig_pos))
            {
              ssize_t enc = gid_to_enc (gs.fv->b.map, sc->orig_pos);
              gs.fv->b.selected[enc] = true;
              if (first)
                {
                  first = false;
                  gs.fv->rowoff = enc / gs.fv->colcnt;
                }
            }
          start = pt;
          while (*start == ' ')
            ++start;
        }
    }
  sf->display_size = ps;

  GGadgetGetSize (gs.fv->vsb, &sbsize);
  gsize.x = gsize.y = 0;
  gsize.width = pos.width + sbsize.width;
  gsize.height = pos.y + pos.height;
  GGadgetSetDesiredSize (drawable, NULL, &gsize);

  GHVBoxFitWindow (boxes[0].ret);
  GDrawSetVisible (gs.gw, true);
  while (!gs.done)
    GDrawProcessOneEvent (NULL);

  ret = rpt = NULL;
  if (gs.good)
    {
      for (k = 0; k < 2; ++k)   /* FIXME: Get rid of this goofball
                                   k-loop. */
        {
          len = 0;
          for (ssize_t enc = 0; enc < gs.fv->b.map->enc_limit; ++enc)
            {
              if (gs.fv->b.selected[enc])
                {
                  if (enc_to_gid_is_set (gs.fv->b.map, enc))
                    {
                      gid = enc_to_gid (gs.fv->b.map, enc);
                      if (sf->glyphs[gid] != NULL)
                        {
                          sc = sf->glyphs[gid];
                          char *repr = xstrdup_or_null (sc->name);
                          if (ret == NULL)
                            len += strlen (repr) + 2;
                          else
                            {
                              strcpy (rpt, repr);
                              rpt += strlen (repr);
                              free (repr);
                              *rpt++ = ' ';
                            }
                        }
                    }
                }
            }
          if (k == 0)
            ret = rpt = xmalloc (len + 1);
          else if (rpt != ret && rpt[-1] == ' ')
            rpt[-1] = '\0';
          else
            *rpt = '\0';
        }
    }
  FontViewFree (&gs.fv->b);
  GDrawSetUserData (gs.gw, NULL);
  GDrawSetUserData (dw, NULL);
  GDrawDestroyWindow (gs.gw);
  return ret;
}
