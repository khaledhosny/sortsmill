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
#include "fontforgevw.h"
#include "fffreetype.h"
#include <math.h>

FT_Library ff_ft_context;

bool
hasFreeType (void)
{
  static bool done = false;
  static bool ok = false;

  if (done)
    return ok;
  done = true;

  if (FT_Init_FreeType (&ff_ft_context))
    return false;

  ok = true;
  return true;
}

void
doneFreeType (void)
{
  if (ff_ft_context != NULL)
    FT_Done_FreeType (ff_ft_context);
  ff_ft_context = NULL;
}

bool
hasFreeTypeByteCode (void)
{
  if (!hasFreeType ())
    return false;

#ifdef TT_CONFIG_OPTION_BYTECODE_INTERPRETER
  return true;
#else
  return false;
#endif
}

bool
hasFreeTypeDebugger (void)
{
  if (!hasFreeTypeByteCode ())
    return false;
#if FREETYPE_HAS_DEBUGGER
  if (FT_Set_Debug_Hook != NULL && TT_RunIns != NULL)
    return true;
#endif

  return false;
}

char *
FreeTypeStringVersion (void)
{
  int ma, mi, pa;
  static char buffer[60];

  if (!hasFreeType ())
    return "";
  FT_Library_Version (ff_ft_context, &ma, &mi, &pa);
  sprintf (buffer, "FreeType %d.%d.%d", ma, mi, pa);
  return buffer;
}

static void
TransitiveClosureAdd (SplineChar **new, SplineChar **old, SplineChar *sc,
                      int layer)
{
  RefChar *ref;

  if (new[sc->orig_pos] != NULL)        /* already done */
    return;
  new[sc->orig_pos] = sc;
  for (ref = sc->layers[layer].refs; ref != NULL; ref = ref->next)
    TransitiveClosureAdd (new, old, ref->sc, layer);
}

static void
AddIf (SplineFont *sf, SplineChar **new, SplineChar **old, int unienc,
       int layer)
{
  SplineChar *sc;

  sc = SFGetChar (sf, unienc, NULL);
  if (sc != NULL && SCWorthOutputting (sc))
    TransitiveClosureAdd (new, old, sc, layer);
}

void
FreeTypeFreeContext (void *freetypecontext)
{
  FTC *ftc = freetypecontext;

  if (ftc == NULL)
    return;

  if (ftc->face != NULL)
    FT_Done_Face (ftc->face);
  if (ftc->shared_ftc)
    return;
#ifdef HAVE_MMAP
  if (ftc->memoryfile)
    munmap (ftc->memoryfile, ftc->len);
#else
  free (ftc->memoryfile);
#endif
  if (ftc->file != NULL)
    fclose (ftc->file);
  free (ftc->glyph_indeces);
  free (ftc);
}

void *
__FreeTypeFontContext (FT_Library context,
                       SplineFont *sf, SplineChar *sc, FontViewBase *fv,
                       int layer,
                       enum fontformat ff, int flags, void *shared_ftc)
{
  /* build up a temporary font consisting of:
   *  sc!=NULL   => Just that character (and its references)
   *  fv!=NULL   => selected characters
   *  else       => the entire font
   */
  FTC *ftc;
  SplineChar **old = sf->glyphs, **new;
  uint8_t *selected = fv != NULL ? fv->selected : NULL;
  EncMap *map = fv != NULL ? fv->map : sf->fv != NULL ? sf->fv->map : sf->map;
  int i, cnt, notdefpos;

  if (context == NULL)
    return NULL;
  if (sf->multilayer)
    return NULL;

  ftc = xcalloc (1, sizeof (FTC));
  if (shared_ftc != NULL)
    {
      *ftc = *(FTC *) shared_ftc;
      ftc->face = NULL;
      ftc->shared_ftc = shared_ftc;
      ftc->em = ((FTC *) shared_ftc)->em;
      ftc->layer = layer;
    }
  else
    {
      ftc->sf = sf;
      ftc->em = sf->ascent + sf->descent;
      ftc->file = NULL;
      ftc->layer = layer;

      ftc->file = tmpfile ();
      if (ftc->file == NULL)
        {
          free (ftc);
          return NULL;
        }

      old = sf->glyphs;
      notdefpos = SFFindNotdef (sf, -2);        /* Do this early */
      if (sc != NULL || selected != NULL)
        {
          /* Build up a font consisting of those characters we actually use */
          new = xcalloc (sf->glyphcnt, sizeof (SplineChar *));
          if (sc != NULL)
            TransitiveClosureAdd (new, old, sc, layer);
          else
            for (i = 0; i < map->enc_limit; ++i)
              if (selected[i] && enc_to_gid (map, i) != -1 &&
                  SCWorthOutputting (old[enc_to_gid (map, i)]))
                TransitiveClosureAdd (new, old, old[enc_to_gid (map, i)],
                                      layer);
          /* Add these guys so we'll get reasonable blue values */
          /* we won't rasterize them */
          if (PSDictHasEntry (sf->private, "BlueValues") == NULL)
            {
              AddIf (sf, new, old, 'I', layer);
              AddIf (sf, new, old, 'O', layer);
              AddIf (sf, new, old, 'x', layer);
              AddIf (sf, new, old, 'o', layer);
            }
          if (notdefpos != -1)
            TransitiveClosureAdd (new, old, sf->glyphs[notdefpos], layer);
          /* If there's a .notdef use it so that we don't generate our own .notdef (which can add cvt entries) */
          sf->glyphs = new;
        }
      sf->internal_temp = true;
      switch (ff)
        {
        case ff_ttf:
        case ff_ttfsym:
          ftc->isttf = true;
          /* Fall through.... */
        case ff_pfb:
        case ff_pfa:
        case ff_otf:
        case ff_otfcid:
          if (!_WriteTTFFont
              (ftc->file, sf, ff, NULL, bf_none, flags, map, layer))
            goto fail;
          break;
        default:
          goto fail;
        }
      sf->internal_temp = false;

      if (sf->subfontcnt != 0)
        {
          /* can only be an otfcid */
          int k, max = 0;
          for (k = 0; k < sf->subfontcnt; ++k)
            if (sf->subfonts[k]->glyphcnt > max)
              max = sf->subfonts[k]->glyphcnt;
          ftc->glyph_indeces = xmalloc (max * sizeof (int));
          memset (ftc->glyph_indeces, -1, max * sizeof (int));
          for (i = 0; i < max; ++i)
            {
              for (k = 0; k < sf->subfontcnt; ++k)
                {
                  if (i < sf->subfonts[k]->glyphcnt &&
                      SCWorthOutputting (sf->subfonts[k]->glyphs[i]))
                    {
                      ftc->glyph_indeces[i] =
                        sf->subfonts[k]->glyphs[i]->ttf_glyph;
                      break;
                    }
                }
            }
        }
      else
        {
          ftc->glyph_indeces = xmalloc (sf->glyphcnt * sizeof (int));
          memset (ftc->glyph_indeces, -1, sf->glyphcnt * sizeof (int));
          cnt = 1;
          if (notdefpos != -1)
            ftc->glyph_indeces[notdefpos] = 0;
          if (ff == ff_pfa || ff == ff_pfb)
            {
              for (i = 0; i < sf->glyphcnt; ++i)
                {
                  if (i != notdefpos && SCWorthOutputting (sf->glyphs[i]))
                    ftc->glyph_indeces[i] = cnt++;
                }
            }
          else
            {
              for (i = 0; i < sf->glyphcnt; ++i)
                {
                  if (SCWorthOutputting (sf->glyphs[i]))
                    {
                      ftc->glyph_indeces[i] = sf->glyphs[i]->ttf_glyph;
                    }
                }
            }
        }

      fseek (ftc->file, 0, SEEK_END);
      ftc->len = ftell (ftc->file);
      fseek (ftc->file, 0, SEEK_SET);
#ifdef HAVE_MMAP
      ftc->memoryfile =
        mmap (NULL, ftc->len, PROT_READ, MAP_PRIVATE, fileno (ftc->file), 0);
      if (ftc->memoryfile == MAP_FAILED)
        goto fail;
#else
      ftc->memoryfile = malloc (ftc->len);
      if (ftc->memoryfile == NULL)
        goto fail;
      if (fread (ftc->memoryfile, 1, ftc->len, ftc->file) != ftc->len)
        goto fail;
#endif
      if (sf->glyphs != old)
        {
          free (sf->glyphs);
          sf->glyphs = old;
        }
    }

  if (FT_New_Memory_Face (context, ftc->memoryfile, ftc->len, 0, &ftc->face))
    goto fail;
  GlyphHashFree (sf);           /* If we created a tiny font, our hash table may reflect that */

  return ftc;

fail:
  sf->internal_temp = false;
  GlyphHashFree (sf);
  FreeTypeFreeContext (ftc);
  if (sf->glyphs != old)
    {
      free (sf->glyphs);
      sf->glyphs = old;
    }
  return NULL;
}

void *
_FreeTypeFontContext (SplineFont *sf, SplineChar *sc, FontViewBase *fv,
                      int layer, enum fontformat ff, int flags,
                      void *shared_ftc)
{

  if (!hasFreeType ())
    return NULL;

  return (__FreeTypeFontContext (ff_ft_context, sf, sc, fv,
                                 layer, ff, flags, shared_ftc));
}

static void
BCTruncateToDepth (BDFChar *bdfc, int depth)
{
  int div = 255 / ((1 << depth) - 1);
  int i, j;

  for (i = 0; i <= bdfc->ymax - bdfc->ymin; ++i)
    {
      for (j = 0; j < bdfc->bytes_per_line; ++j)
        bdfc->bitmap[i * bdfc->bytes_per_line + j] =
          (bdfc->bitmap[i * bdfc->bytes_per_line + j] + div / 2) / div;
    }
}

static BDFChar *
BdfCFromBitmap (FT_Bitmap * bitmap, int bitmap_left,
                int bitmap_top, int pixelsize, int depth, SplineChar *sc,
                FT_Glyph_Metrics * metrics)
{
  BDFChar *bdfc;

  bdfc = xzalloc (sizeof (BDFChar));
  bdfc->sc = sc;
  bdfc->ymax = bitmap_top - 1;
  bdfc->ymin = bitmap_top - bitmap->rows;
  if (bitmap->rows == 0)
    bdfc->ymax = bdfc->ymin;
  bdfc->xmin = bitmap_left;
  bdfc->xmax = bitmap_left + bitmap->width - 1;
  if (bitmap->width == 0)
    bdfc->xmax = bdfc->xmin;
  bdfc->byte_data = (depth != 1);
  bdfc->depth = depth;
  if (sc != NULL)
    {
      bdfc->width =
        rint (sc->width * pixelsize /
              (real) (sc->parent->ascent + sc->parent->descent));
      bdfc->vwidth =
        rint (sc->vwidth * pixelsize /
              (real) (sc->parent->ascent + sc->parent->descent));
      bdfc->orig_pos = sc->orig_pos;
    }
  if (metrics != NULL)
    {
      bdfc->width = rint (metrics->horiAdvance / 64.0);
      bdfc->vwidth = rint (metrics->vertAdvance / 64.0);
    }
  bdfc->bytes_per_line = bitmap->pitch;
  bdfc->refs = NULL;
  bdfc->dependents = NULL;
  if (bdfc->bytes_per_line == 0)
    bdfc->bytes_per_line = 1;
  bdfc->bitmap = xmalloc ((bdfc->ymax - bdfc->ymin + 1) * bdfc->bytes_per_line);
  if (bitmap->rows == 0 || bitmap->width == 0)
    memset (bdfc->bitmap, 0,
            (bdfc->ymax - bdfc->ymin + 1) * bdfc->bytes_per_line);
  else
    memcpy (bdfc->bitmap, bitmap->buffer, bitmap->rows * bdfc->bytes_per_line);
  BCCompressBitmap (bdfc);
  if (depth != 1 && depth != 8)
    BCTruncateToDepth (bdfc, depth);
  return bdfc;
}

static BDFChar *
BDFCReClut (BDFChar *bdfc)
{
  /* Made for something with a bit depth of 4, but we use 8 here */
  uint8_t *pt, *end;

  if (bdfc == NULL)
    return NULL;
  pt = bdfc->bitmap;
  end = pt + bdfc->bytes_per_line * (bdfc->ymax - bdfc->ymin + 1);
  while (pt < end)
    *pt++ *= 17;
  return bdfc;
}

BDFChar *
SplineCharFreeTypeRasterize (void *freetypecontext, int gid,
                             int ptsize, int dpi, int depth)
{
  FTC *ftc = freetypecontext;
  BDFChar *bdfc;
  SplineChar *sc;
  FT_GlyphSlot slot;
  int pixelsize = (int) rint ((ptsize * dpi) / 72.0);

  if (ftc->glyph_indeces[gid] == -1)
    goto fail;

  if (FT_Set_Char_Size (ftc->face, ptsize * 64, 0, dpi, 0))
    goto fail;

  FT_Int32 load_flags = FT_LOAD_NO_AUTOHINT | FT_LOAD_RENDER;
  if (depth == 1)
    load_flags |= FT_LOAD_TARGET_MONO;

  if (FT_Load_Glyph (ftc->face, ftc->glyph_indeces[gid], load_flags))
    goto fail;

  slot = ftc->face->glyph;
  sc = ftc->sf->glyphs[gid];
  bdfc = BdfCFromBitmap (&slot->bitmap, slot->bitmap_left, slot->bitmap_top,
                         pixelsize, depth, sc, &slot->metrics);
  return bdfc;

fail:
  if (depth == 1)
    return SplineCharRasterize (ftc->sf->glyphs[gid], ftc->layer, pixelsize);
  else
    return (BDFCReClut
            (SplineCharAntiAlias
             (ftc->sf->glyphs[gid], ftc->layer, pixelsize, 4)));
}

BDFFont *
SplineFontFreeTypeRasterize (void *freetypecontext, int pixelsize, int depth)
{
  FTC *ftc = freetypecontext, *subftc = NULL;
  SplineFont *sf = ftc->sf, *subsf;
  int i, k;
  BDFFont *bdf = SplineFontToBDFHeader (sf, pixelsize, true);

  if (depth != 1)
    BDFClut (bdf, 1 << (depth / 2));

  k = 0;
  do
    {
      if (sf->subfontcnt == 0)
        {
          subsf = sf;
          subftc = ftc;
        }
      else
        {
          subsf = sf->subfonts[k];
          subftc = FreeTypeFontContext (subsf, NULL, NULL, ftc->layer);
        }
      for (i = 0; i < subsf->glyphcnt; ++i)
        if (SCWorthOutputting (subsf->glyphs[i]))
          {
            /* If we could not allocate an ftc for this subfont, the revert to */
            /*  our own rasterizer */
            if (subftc != NULL)
              bdf->glyphs[i] =
                SplineCharFreeTypeRasterize (subftc, i, pixelsize, 72, depth);
            else if (depth == 1)
              bdf->glyphs[i] =
                SplineCharRasterize (subsf->glyphs[i], ftc->layer, pixelsize);
            else
              bdf->glyphs[i] =
                SplineCharAntiAlias (subsf->glyphs[i], ftc->layer, pixelsize,
                                     (1 << (depth / 2)));
            ff_progress_next ();
          }
        else
          bdf->glyphs[i] = NULL;
      if (subftc != NULL && subftc != ftc)
        FreeTypeFreeContext (subftc);
      subftc = NULL;
      ++k;
    }
  while (k < sf->subfontcnt);
  ff_progress_end_indicator ();
  return bdf;
}

/* ************************************************************************** */
struct ft_context
{
  SplinePointList *hcpl, *lcpl, *cpl;
  SplinePoint *last;
  double scalex, scaley;
  SplinePointList *orig_cpl;
  SplinePoint *orig_sp;
  RefChar *orig_ref;
  int order2;
};

static void
FT_ClosePath (struct ft_context *context)
{
  if (context->cpl != NULL)
    {
      if (context->cpl->first->me.x != context->last->me.x ||
          context->cpl->first->me.y != context->last->me.y)
        SplineMake (context->last, context->cpl->first, context->order2);
      else
        {
          context->cpl->first->prevcp = context->last->prevcp;
          context->last->prev->to = context->cpl->first;
          context->cpl->first->prev = context->last->prev;
          SplinePointFree (context->last);
        }
      context->cpl->last = context->cpl->first;
      context->last = NULL;
      if (context->orig_cpl != NULL)
        context->orig_cpl = context->orig_cpl->next;
      while (context->orig_cpl == NULL)
        {
          if (context->orig_ref == NULL)
            break;
          context->orig_cpl = context->orig_ref->layers[0].splines;
          context->orig_ref = context->orig_ref->next;
        }
      while (!context->order2 && context->orig_cpl != NULL &&
             context->orig_cpl->first->next == NULL)
        context->orig_cpl = context->orig_cpl->next;
      /* free type skips open contours with a single point in pfbs */
      context->orig_sp = NULL;
    }
}

static int
FT_MoveTo (const FT_Vector * to, void *user)
{
  struct ft_context *context = user;

  FT_ClosePath (context);

  context->cpl = xzalloc (sizeof (SplinePointList));
  if (context->lcpl == NULL)
    context->hcpl = context->cpl;
  else
    context->lcpl->next = context->cpl;
  context->lcpl = context->cpl;

  if (context->orig_cpl != NULL)
    context->orig_sp = context->orig_cpl->first;

  context->last = context->cpl->first =
    xzalloc (sizeof (SplinePoint));
  context->last->me.x = to->x * context->scalex;
  context->last->me.y = to->y * context->scaley;
  if (context->orig_sp == NULL)
    context->last->ttfindex = -2;
  else
    {
      context->last->ttfindex = context->orig_sp->ttfindex;
      context->last->nextcpindex = context->orig_sp->nextcpindex;
    }
  return 0;
}

static int
FT_LineTo (const FT_Vector * to, void *user)
{
  struct ft_context *context = user;
  SplinePoint *sp;

  sp = SplinePointCreate (to->x * context->scalex, to->y * context->scaley);
  sp->ttfindex = -1;
  SplineMake (context->last, sp, context->order2);
  context->last = sp;

  if (context->orig_sp != NULL && context->orig_sp->next != NULL)
    {
      context->orig_sp = context->orig_sp->next->to;
      if (context->orig_sp != NULL)
        {
          sp->ttfindex = context->orig_sp->ttfindex;
          sp->nextcpindex = context->orig_sp->nextcpindex;
        }
    }
  return 0;
}

static int
FT_ConicTo (const FT_Vector * _cp, const FT_Vector * to, void *user)
{
  struct ft_context *context = user;
  SplinePoint *sp;

  sp = SplinePointCreate (to->x * context->scalex, to->y * context->scaley);
  sp->noprevcp = false;
  sp->prevcp.x = _cp->x * context->scalex;
  sp->prevcp.y = _cp->y * context->scaley;
  context->last->nextcp = sp->prevcp;
  context->last->nonextcp = false;
  SplineMake2 (context->last, sp);
  context->last = sp;

  if (context->orig_sp != NULL)
    {
      context->orig_sp = context->orig_sp->next->to;
      if (context->orig_sp != NULL)
        {
          sp->ttfindex = context->orig_sp->ttfindex;
          sp->nextcpindex = context->orig_sp->nextcpindex;
        }
    }
  if (sp->ttfindex == 0xfffe)
    sp->ttfindex = 0xffff;
  return 0;
}

static int
FT_CubicTo (const FT_Vector * cp1, const FT_Vector * cp2,
            const FT_Vector * to, void *user)
{
  struct ft_context *context = user;
  SplinePoint *sp;

  sp = SplinePointCreate (to->x * context->scalex, to->y * context->scaley);
  sp->noprevcp = false;
  sp->prevcp.x = cp2->x * context->scalex;
  sp->prevcp.y = cp2->y * context->scaley;
  context->last->nextcp.x = cp1->x * context->scalex;
  context->last->nextcp.y = cp1->y * context->scaley;
  SplineMake3 (context->last, sp);
  context->last = sp;

  if (context->orig_sp != NULL)
    {
      context->orig_sp = context->orig_sp->next->to;
      if (context->orig_sp != NULL)
        sp->ttfindex = context->orig_sp->ttfindex;
    }
  return 0;
}

static FT_Outline_Funcs outlinefuncs = {
  FT_MoveTo,
  FT_LineTo,
  FT_ConicTo,
  FT_CubicTo,
  0, 0                          /* I don't understand shift and delta */
};

SplineSet *
FreeType_GridFitChar (void *single_glyph_context, int enc,
                      real ptsizey, real ptsizex, int dpi, uint16_t *width,
                      SplineChar *sc, int depth, int scaled)
{
  FT_GlyphSlot slot;
  FTC *ftc = (FTC *) single_glyph_context;
  struct ft_context outline_context;
  static int bc_checked = false;

  if (ftc->face == (void *) -1)
    return NULL;

  if (!bc_checked && ftc->isttf)
    {
      bc_checked = true;
      if (!hasFreeTypeByteCode ())
        ff_post_notice (_("No ByteCode Interpreter"),
                        _
                        ("These results are those of the freetype autohinter. They do not reflect the truetype instructions."));
    }

  if (FT_Set_Char_Size
      (ftc->face, (int) (ptsizex * 64), (int) (ptsizey * 64), dpi, dpi))
    return NULL;              /* Error Return */

  FT_Int32 load_flags = FT_LOAD_NO_AUTOHINT | FT_LOAD_NO_BITMAP;
  if (depth == 1)
    load_flags |= FT_LOAD_TARGET_MONO;

  if (FT_Load_Glyph (ftc->face, ftc->glyph_indeces[enc], load_flags));
    return NULL;

  slot = ftc->face->glyph;
  memset (&outline_context, '\0', sizeof (outline_context));
  if (scaled)
    {
      /* The outline's position is expressed in 24.6 fixed numbers representing */
      /*  pixels. I want to scale it back to the original coordinate system */
      outline_context.scalex = ftc->em / (64.0 * rint (ptsizex * dpi / 72.0));
      outline_context.scaley = ftc->em / (64.0 * rint (ptsizey * dpi / 72.0));
    }
  else
    {
      /* leave as pixels */
      outline_context.scalex = 1.0 / 64.0;
      outline_context.scaley = 1.0 / 64.0;
    }
  outline_context.orig_ref = sc->layers[ftc->layer].refs;
  outline_context.orig_cpl = sc->layers[ftc->layer].splines;
  while (outline_context.orig_cpl == NULL && outline_context.orig_ref != NULL)
    {
      outline_context.orig_cpl = outline_context.orig_ref->layers[0].splines;
      outline_context.orig_ref = outline_context.orig_ref->next;
    }
  while (!ftc->isttf && outline_context.orig_cpl != NULL &&
         outline_context.orig_cpl->first->next == NULL)
    outline_context.orig_cpl = outline_context.orig_cpl->next;
  /* free type skips open contours with a single point in pfbs */
  outline_context.orig_sp = NULL;
  outline_context.order2 = ftc->isttf;
  if (!FT_Outline_Decompose (&slot->outline, &outlinefuncs, &outline_context))
    {
      FT_ClosePath (&outline_context);
      *width = outline_context.scalex * slot->advance.x;
      return outline_context.hcpl;
    }
  return NULL;
}

struct freetype_raster *
FreeType_GetRaster (void *single_glyph_context,
                    int enc, real ptsizey, real ptsizex, int dpi, int depth)
{
  FT_GlyphSlot slot;
  struct freetype_raster *ret;
  FTC *ftc = (FTC *) single_glyph_context;

  if (ftc->face == (void *) -1)
    return NULL;

  if (FT_Set_Char_Size
      (ftc->face, (int) (ptsizex * 64), (int) (ptsizey * 64), dpi, dpi))
    return NULL;              /* Error Return */

  FT_Int32 load_flags = FT_LOAD_NO_AUTOHINT | FT_LOAD_NO_BITMAP;
  if (depth == 1)
    load_flags |= FT_LOAD_TARGET_MONO;

  if (FT_Load_Glyph (ftc->face, ftc->glyph_indeces[enc], load_flags))
    return NULL;

  slot = ((FT_Face) (ftc->face))->glyph;
  if (FT_Render_Glyph
      (slot, depth == 1 ? ft_render_mode_mono : ft_render_mode_normal))
    return NULL;

  if (slot->bitmap.pixel_mode != ft_pixel_mode_mono &&
      slot->bitmap.pixel_mode != ft_pixel_mode_grays)
    return NULL;
  ret = xmalloc (sizeof (struct freetype_raster));

  ret->rows = slot->bitmap.rows;
  ret->cols = slot->bitmap.width;
  ret->bytes_per_row = slot->bitmap.pitch;
  ret->as = slot->bitmap_top;
  ret->lb = slot->bitmap_left;
  ret->num_greys = slot->bitmap.num_grays;
  /* Can't find any description of freetype's bitendianness */
  /* But the obvious seems to work */
  ret->bitmap = xmalloc (ret->rows * ret->bytes_per_row);
  memcpy (ret->bitmap, slot->bitmap.buffer, ret->rows * ret->bytes_per_row);
  return ret;
}

static void
FillOutline (SplineSet *spl, FT_Outline * outline, int *pmax, int *cmax,
             real scale, DBounds *bb, int order2, int ignore_clip)
{
  int k;
  int pcnt, ccnt;
  SplinePoint *sp;
  SplineSet *ss;

  if (order2)
    {
      for (k = 0; k < 2; ++k)
        {
          pcnt = ccnt = 0;
          for (ss = spl; ss != NULL; ss = ss->next)
            if (ss->first->prev != NULL)
              {
                if ((ignore_clip == 1 && ss->is_clip_path)
                    || (ignore_clip == 2 && !ss->is_clip_path))
                  continue;
                for (sp = ss->first;;)
                  {
                    if (k)
                      {
                        outline->points[pcnt].x =
                          rint (sp->me.x * scale) - bb->minx;
                        outline->points[pcnt].y =
                          rint (sp->me.y * scale) - bb->miny;
                        outline->tags[pcnt] = 1;        /* On curve */
                      }
                    ++pcnt;
                    if (sp->next == NULL)
                      break;
                    if (!sp->nonextcp)
                      {
                        if (k)
                          {
                            outline->points[pcnt].x =
                              rint (sp->nextcp.x * scale) - bb->minx;
                            outline->points[pcnt].y =
                              rint (sp->nextcp.y * scale) - bb->miny;
                          }
                        ++pcnt;
                      }
                    sp = sp->next->to;
                    if (sp == ss->first)
                      break;
                  }
                if (k)
                  outline->contours[ccnt] = pcnt - 1;
                ++ccnt;
              }
          if (!k)
            {
              outline->n_contours = ccnt;
              outline->n_points = pcnt;
              if (pcnt > *pmax || *pmax == 0)
                {
                  *pmax = pcnt == 0 ? 1 : pcnt;
                  outline->points =
                    xrealloc (outline->points, *pmax * sizeof (FT_Vector));
                  outline->tags =
                    xrealloc (outline->tags, *pmax * sizeof (char));
                }
              memset (outline->tags, 0, pcnt);
              if (ccnt > *cmax || *cmax == 0)
                {
                  *cmax = ccnt == 0 ? 1 : ccnt;
                  outline->contours =
                    xrealloc (outline->contours, *cmax * sizeof (short));
                }
              outline->flags = ft_outline_none;
            }
        }
    }
  else
    {
      for (k = 0; k < 2; ++k)
        {
          pcnt = ccnt = 0;
          for (ss = spl; ss != NULL; ss = ss->next)
            if (ss->first->prev != NULL)
              {
                for (sp = ss->first;;)
                  {
                    if (k)
                      {
                        outline->points[pcnt].x =
                          rint (sp->me.x * scale) - bb->minx;
                        outline->points[pcnt].y =
                          rint (sp->me.y * scale) - bb->miny;
                        outline->tags[pcnt] = 1;        /* On curve */
                      }
                    ++pcnt;
                    if (sp->next == NULL)
                      break;
                    if (!sp->nonextcp || !sp->next->to->noprevcp)
                      {
                        if (k)
                          {
                            outline->points[pcnt].x =
                              rint (sp->nextcp.x * scale) - bb->minx;
                            outline->points[pcnt].y =
                              rint (sp->nextcp.y * scale) - bb->miny;
                            outline->tags[pcnt] = 2;    /* cubic control */
                            outline->points[pcnt + 1].x =
                              rint (sp->next->to->prevcp.x * scale) - bb->minx;
                            outline->points[pcnt + 1].y =
                              rint (sp->next->to->prevcp.y * scale) - bb->miny;
                            outline->tags[pcnt + 1] = 2;        /* cubic control */
                          }
                        pcnt += 2;
                      }
                    sp = sp->next->to;
                    if (sp == ss->first)
                      break;
                  }
                if (k)
                  outline->contours[ccnt] = pcnt - 1;
                ++ccnt;
              }
          if (!k)
            {
              outline->n_contours = ccnt;
              outline->n_points = pcnt;
              if (pcnt > *pmax || *pmax == 0)
                {
                  *pmax = pcnt == 0 ? 1 : pcnt;
                  outline->points =
                    xrealloc (outline->points, *pmax * sizeof (FT_Vector));
                  outline->tags =
                    xrealloc (outline->tags, *pmax * sizeof (char));
                }
              memset (outline->tags, 0, pcnt);
              if (ccnt > *cmax || *cmax == 0)
                {
                  *cmax = ccnt == 0 ? 1 : ccnt;
                  outline->contours =
                    xrealloc (outline->contours, *cmax * sizeof (short));
                }
              /* You might think we should set ft_outline_reverse_fill here */
              /*  because this is a postscript font. But ff stores fonts */
              /*  internally in truetype order, and we didn't reverse it */
              /*  above (the way we would if we were really generating PS) */
              outline->flags = ft_outline_none;
            }
        }
    }
}

static SplineSet *
LayerAllOutlines (Layer *layer)
{
  SplineSet *head, *last, *cur;
  RefChar *r;

  if (layer->refs == NULL)
    return layer->splines;
  head = SplinePointListCopy (layer->splines);
  if (head != NULL)
    for (last = head; last->next != NULL; last = last->next);
  for (r = layer->refs; r != NULL; r = r->next)
    {
      cur = SplinePointListCopy (r->layers[0].splines);
      if (cur == NULL)
        /* Do Nothing */ ;
      else if (head == NULL)
        {
          head = cur;
          for (last = head; last->next != NULL; last = last->next);
        }
      else
        {
          last->next = cur;
          for (; last->next != NULL; last = last->next);
        }
    }
  return head;
}

static SplineSet *
StrokeOutline (Layer *layer, SplineChar *sc)
{
  StrokeInfo si;
  RefChar *r;
  SplineSet *head = NULL, *tail = NULL, *c;

  memset (&si, 0, sizeof (si));
  if (sc->parent->strokedfont)
    {
      si.radius = sc->parent->strokewidth / 2;
      si.join = lj_bevel;
      si.cap = lc_butt;
      si.stroke_type = si_std;
      head = SplineSetStroke (layer->splines, &si, layer->order2);
      if (head != NULL)
        for (tail = head; tail->next != NULL; tail = tail->next);
      for (r = layer->refs; r != NULL; r = r->next)
        {
          c = SplineSetStroke (r->layers[0].splines, &si, r->layers[0].order2);
          if (c == NULL)
            /* Do Nothing */ ;
          else if (head == NULL)
            {
              head = c;
              for (tail = head; tail->next != NULL; tail = tail->next);
            }
          else
            {
              tail->next = c;
              for (; tail->next != NULL; tail = tail->next);
            }
        }
      return head;
    }
  else
    {
      si.radius = layer->stroke_pen.width / 2;
      si.join = layer->stroke_pen.linejoin;
      si.cap = layer->stroke_pen.linecap;
      si.stroke_type = si_std;
      return SplineSetStroke (layer->splines, &si, layer->order2);
    }
}

static SplineSet *
RStrokeOutline (struct reflayer *layer, SplineChar *sc)
{
  StrokeInfo si;

  memset (&si, 0, sizeof (si));
  si.radius = layer->stroke_pen.width / 2;
  si.join = layer->stroke_pen.linejoin;
  si.cap = layer->stroke_pen.linecap;
  si.stroke_type = si_std;
  return SplineSetStroke (layer->splines, &si, layer->order2);
}

static void
MergeBitmaps (FT_Bitmap * bitmap, FT_Bitmap * newstuff, struct brush *brush,
              uint8_t *clipmask, double scale, DBounds *bbox, SplineChar *sc)
{
  int i, j;
  uint32_t col = brush->col;

  if (col == COLOR_INHERITED)
    col = 0x000000;
  col = 3 * ((col >> 16) & 0xff) + 6 * ((col >> 8) & 0xff) + 1 * (col & 0xff);
  col = 0xff - col;

  if (bitmap->num_grays == 256)
    {
      if (clipmask != NULL)
        {
          for (i = 0; i < bitmap->rows; ++i)
            for (j = 0; j < bitmap->pitch; ++j)
              newstuff->buffer[i * bitmap->pitch + j] *=
                clipmask[i * bitmap->pitch + j];
        }
      PatternPrep (sc, brush, scale);
      for (i = 0; i < bitmap->rows; ++i)
        for (j = 0; j < bitmap->pitch; ++j)
          {
            bitmap->buffer[i * bitmap->pitch + j] =
              (newstuff->buffer[i * bitmap->pitch + j] *
               GradientHere (scale, bbox, i, j, brush->gradient, brush->pattern,
                             col) + (255 - newstuff->buffer[i * bitmap->pitch +
                                                            j]) *
               bitmap->buffer[i * bitmap->pitch + j] + 127) / 255;
          }
      if (brush->pattern != NULL)
        {
          BDFCharFree (brush->pattern->pat);
          brush->pattern->pat = NULL;
        }
    }
  else
    {
      if (clipmask != NULL)
        {
          for (i = 0; i < bitmap->rows; ++i)
            for (j = 0; j < bitmap->pitch; ++j)
              newstuff->buffer[i * bitmap->pitch + j] &=
                clipmask[i * bitmap->pitch + j];
        }
      /* A gradient makes no sense on a bitmap, so don't even check */
      /*  (unless we were doing dithering, which we aren't) */
      if (col >= 0x80)
        {
          /* Bitmap set */
          for (i = 0; i < bitmap->rows; ++i)
            for (j = 0; j < bitmap->pitch; ++j)
              {
                bitmap->buffer[i * bitmap->pitch + j] |=
                  newstuff->buffer[i * bitmap->pitch + j];
              }
        }
      else
        {
          /* Bitmap clear */
          for (i = 0; i < bitmap->rows; ++i)
            for (j = 0; j < bitmap->pitch; ++j)
              {
                bitmap->buffer[i * bitmap->pitch + j] &=
                  ~newstuff->buffer[i * bitmap->pitch + j];
              }
        }
    }
}

BDFChar *
SplineCharFreeTypeRasterizeNoHints (SplineChar *sc, int layer,
                                    int ptsize, int dpi, int depth)
{
  FT_Outline outline;
  FT_Bitmap bitmap, temp;
  int i;
  int cmax, pmax;
  real rscale =
    (ptsize * dpi) / 72.0 / (double) (sc->parent->ascent + sc->parent->descent);
  real scale = rscale * (1 << 6);
  BDFChar *bdfc;
  int err = 0;
  DBounds b;
  SplineSet *all;

  if (!hasFreeType ())
    return NULL;
  if (sc->layers[layer].order2 && sc->parent->strokedfont)
    return NULL;
  if (sc->layers[layer].order2 && sc->parent->multilayer)
    {
      /* I don't support stroking of order2 splines */
      for (i = ly_fore; i < sc->layer_cnt; ++i)
        {
          if (sc->layers[i].dostroke)
            return NULL;
        }
    }
  if (sc->parent->multilayer)
    {
      /* I don't support images here */
      for (i = ly_fore; i < sc->layer_cnt; ++i)
        {
          if (sc->layers[i].images != NULL)
            return NULL;
        }
    }

  SplineCharLayerFindBounds (sc, layer, &b);
  if (b.maxx - b.minx > 32767)
    b.maxx = b.minx + 32767;
  if (b.maxy - b.miny > 32767)
    b.maxy = b.miny + 32767;
  b.minx *= scale;
  b.maxx *= scale;
  b.miny *= scale;
  b.maxy *= scale;

  b.minx = (1 << 6) * floor (b.minx * (1.0 / (1 << 6)));
  b.miny = (1 << 6) * floor (b.miny * (1.0 / (1 << 6)));
  b.maxx = (1 << 6) * ceil (b.maxx * (1.0 / (1 << 6)));
  b.maxy = (1 << 6) * ceil (b.maxy * (1.0 / (1 << 6)));

  memset (&bitmap, 0, sizeof (bitmap));
  bitmap.rows = (((int) (b.maxy - b.miny)) >> 6);
  bitmap.width = (((int) (b.maxx - b.minx)) >> 6);
  if (depth == 1)
    {
      bitmap.pitch = (bitmap.width + 7) >> 3;
      bitmap.num_grays = 2;
      bitmap.pixel_mode = ft_pixel_mode_mono;
    }
  else
    {
      bitmap.pitch = bitmap.width;
      bitmap.num_grays = 256;
      bitmap.pixel_mode = ft_pixel_mode_grays;
    }
  bitmap.buffer = xcalloc (bitmap.pitch * bitmap.rows, sizeof (uint8_t));
  memset (&temp, 0, sizeof (temp));
  if (sc->parent->multilayer && !(sc->layer_cnt == 2 &&
                                  !sc->layers[ly_fore].dostroke &&
                                  sc->layers[ly_fore].dofill &&
                                  sc->layers[ly_fore].refs == NULL &&
                                  (sc->layers[ly_fore].fill_brush.col ==
                                   COLOR_INHERITED
                                   || sc->layers[ly_fore].fill_brush.col ==
                                   0x000000)))
    {
      temp = bitmap;
      temp.buffer = xmalloc (bitmap.pitch * bitmap.rows);
    }

  memset (&outline, 0, sizeof (outline));
  pmax = cmax = 0;

  if (sc->parent->strokedfont)
    {
      SplineSet *stroked = StrokeOutline (&sc->layers[layer], sc);
      memset (temp.buffer, 0, temp.pitch * temp.rows);
      FillOutline (stroked, &outline, &pmax, &cmax,
                   scale, &b, sc->layers[layer].order2, false);
      err |= (FT_Outline_Get_Bitmap) (ff_ft_context, &outline, &bitmap);
      SplinePointListsFree (stroked);
    }
  else if (temp.buffer == NULL)
    {
      all = LayerAllOutlines (&sc->layers[layer]);
      FillOutline (all, &outline, &pmax, &cmax,
                   scale, &b, sc->layers[layer].order2, false);
      err = (FT_Outline_Get_Bitmap) (ff_ft_context, &outline, &bitmap);
      if (sc->layers[layer].splines != all)
        SplinePointListsFree (all);
    }
  else
    {
      int j;
      RefChar *r;
      /* Can only get here if multilayer */
      err = 0;
      for (i = ly_fore; i < sc->layer_cnt; ++i)
        {
          uint8_t *clipmask = NULL;
          if (SSHasClip (sc->layers[i].splines))
            {
              memset (temp.buffer, 0, temp.pitch * temp.rows);
              FillOutline (sc->layers[i].splines, &outline, &pmax, &cmax,
                           scale, &b, sc->layers[i].order2, 2);
              err |= (FT_Outline_Get_Bitmap) (ff_ft_context, &outline, &temp);
              clipmask = xmalloc (bitmap.pitch * bitmap.rows);
              memcpy (clipmask, temp.buffer, bitmap.pitch * bitmap.rows);
            }
          if (sc->layers[i].dofill)
            {
              memset (temp.buffer, 0, temp.pitch * temp.rows);
              FillOutline (sc->layers[i].splines, &outline, &pmax, &cmax,
                           scale, &b, sc->layers[i].order2, true);
              err |= (FT_Outline_Get_Bitmap) (ff_ft_context, &outline, &temp);
              MergeBitmaps (&bitmap, &temp, &sc->layers[i].fill_brush, clipmask,
                            rscale, &b, sc);
            }
          if (sc->layers[i].dostroke)
            {
              SplineSet *stroked = StrokeOutline (&sc->layers[i], sc);
              memset (temp.buffer, 0, temp.pitch * temp.rows);
              FillOutline (stroked, &outline, &pmax, &cmax,
                           scale, &b, sc->layers[i].order2, true);
              err |= (FT_Outline_Get_Bitmap) (ff_ft_context, &outline, &temp);
              MergeBitmaps (&bitmap, &temp, &sc->layers[i].stroke_pen.brush,
                            clipmask, rscale, &b, sc);
              SplinePointListsFree (stroked);
            }
          for (r = sc->layers[i].refs; r != NULL; r = r->next)
            {
              for (j = 0; j < r->layer_cnt; ++j)
                {
                  if (r->layers[j].dofill)
                    {
                      memset (temp.buffer, 0, temp.pitch * temp.rows);
                      FillOutline (r->layers[j].splines, &outline, &pmax, &cmax,
                                   scale, &b, sc->layers[i].order2, true);
                      err |=
                        (FT_Outline_Get_Bitmap) (ff_ft_context, &outline,
                                                 &temp);
                      MergeBitmaps (&bitmap, &temp, &r->layers[j].fill_brush,
                                    clipmask, rscale, &b, sc);
                    }
                  if (r->layers[j].dostroke)
                    {
                      SplineSet *stroked = RStrokeOutline (&r->layers[j], sc);
                      memset (temp.buffer, 0, temp.pitch * temp.rows);
                      FillOutline (stroked, &outline, &pmax, &cmax,
                                   scale, &b, sc->layers[i].order2, true);
                      err |=
                        (FT_Outline_Get_Bitmap) (ff_ft_context, &outline,
                                                 &temp);
                      MergeBitmaps (&bitmap, &temp,
                                    &r->layers[j].stroke_pen.brush, clipmask,
                                    rscale, &b, sc);
                      SplinePointListsFree (stroked);
                    }
                }
            }
          free (clipmask);
        }
      free (temp.buffer);
    }

  free (outline.points);
  free (outline.tags);
  free (outline.contours);
  bdfc = NULL;
  if (!err)
    {
      bdfc =
        BdfCFromBitmap (&bitmap, (((int) b.minx) + 0x20) >> 6,
                        (((int) b.maxy) + 0x20) >> 6,
                        (int) rint ((ptsize * dpi) / 72.0), depth, sc, NULL);
    }
  free (bitmap.buffer);
  return bdfc;
}

BDFFont *
SplineFontFreeTypeRasterizeNoHints (SplineFont *sf, int layer, int pixelsize,
                                    int depth)
{
  SplineFont *subsf;
  int i, k;
  BDFFont *bdf = SplineFontToBDFHeader (sf, pixelsize, true);

  if (depth != 1)
    BDFClut (bdf, 1 << (depth / 2));

  k = 0;
  do
    {
      if (sf->subfontcnt == 0)
        {
          subsf = sf;
        }
      else
        {
          subsf = sf->subfonts[k];
        }
      for (i = 0; i < subsf->glyphcnt; ++i)
        if (SCWorthOutputting (subsf->glyphs[i]))
          {
            bdf->glyphs[i] =
              SplineCharFreeTypeRasterizeNoHints (subsf->glyphs[i], layer,
                                                  pixelsize, 72, depth);
            if (bdf->glyphs[i] != NULL)
              /* Done */ ;
            else if (depth == 1)
              bdf->glyphs[i] =
                SplineCharRasterize (subsf->glyphs[i], layer, pixelsize);
            else
              bdf->glyphs[i] =
                SplineCharAntiAlias (subsf->glyphs[i], layer, pixelsize,
                                     (1 << (depth / 2)));
            ff_progress_next ();
          }
        else
          bdf->glyphs[i] = NULL;
      ++k;
    }
  while (k < sf->subfontcnt);
  ff_progress_end_indicator ();
  return bdf;
}

void *
FreeTypeFontContext (SplineFont *sf, SplineChar *sc, FontViewBase *fv,
                     int layer)
{
  enum fontformat ff;

  if (sf->subfontcnt != 0)
    ff = ff_otfcid;
  else if (sf->layers[layer].order2)
    ff = ff_ttf;
  else
    ff = ff_otf;

  return _FreeTypeFontContext (sf, sc, fv, layer, ff, 0, NULL);
}

void
FreeType_FreeRaster (struct freetype_raster *raster)
{
  if (raster == NULL || raster == (void *) -1)
    return;
  free (raster->bitmap);
  free (raster);
}
