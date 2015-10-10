#include <config.h>             /* -*- coding: utf-8 -*- */

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

#include <stdbool.h>
#include "fontforgevw.h"
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <unistd.h>
#include <ustring.h>
#include "utype.h"
#include <sys/types.h>
#include <sys/wait.h>
#include "print.h"
#include <xunistring.h>
#include <unistr.h>

/* ************************************************************************** */
/* ***************************** Printing Stuff ***************************** */
/* ************************************************************************** */

static int
pdf_addobject (PI * pi)
{
  if (pi->next_object == 0)
    {
      pi->max_object = 100;
      pi->object_offsets = xmalloc (pi->max_object * sizeof (int));
      pi->object_offsets[pi->next_object++] = 0;        /* Object 0 is magic */
    }
  else if (pi->next_object >= pi->max_object)
    {
      pi->max_object += 100;
      pi->object_offsets =
        xrealloc (pi->object_offsets, pi->max_object * sizeof (int));
    }
  pi->object_offsets[pi->next_object] = ftell (pi->out);
  fprintf (pi->out, "%d 0 obj\n", pi->next_object++);
  return pi->next_object - 1;
}

struct opac_state
{
  int isfill;
  float opacity;
  int obj;
};

struct glyph_res
{
  int pattern_cnt, pattern_max;
  char **pattern_names;
  int *pattern_objs;
  int image_cnt, image_max;
  char **image_names;
  int *image_objs;
  int opacity_cnt, opacity_max;
  struct opac_state *opac_state;
};

#define GLYPH_RES_EMPTY { 0, 0, NULL, NULL, 0, 0, NULL, NULL, 0, 0, NULL }


void
makePatName (char *buffer, RefChar *ref, SplineChar *sc, int layer,
             int isstroke, int isgrad)
{
  /* In PDF patterns (which include gradients) are fixed to the page. They */
  /*  do not alter with the Current Transformation Matrix. So if we have */
  /*  a reference to a glyph, then every reference to the same glyph will */
  /*  need a different pattern description where that description involves */
  /*  the reference's transform matrix */

  if (ref == NULL)
    sprintf (buffer, "%s_ly%d_%s_%s", sc->name, layer,
             isstroke ? "stroke" : "fill", isgrad ? "grad" : "pattern");
  else
    {
      /* PDF names are significant up to 127 chars long and can contain */
      /*  all kinds of odd characters, just no spaces or slashes, so this */
      /*  name should be legal */
      sprintf (buffer, "%s_trans_%g,%g,%g,%g,%g,%g_ly%d_%s_%s", sc->name,
               (double) ref->transform[0], (double) ref->transform[1],
               (double) ref->transform[2], (double) ref->transform[3],
               (double) ref->transform[4], (double) ref->transform[5], layer,
               isstroke ? "stroke" : "fill", isgrad ? "grad" : "pattern");
    }
}

static void
pdf_BrushCheck (PI * pi, struct glyph_res *gr, struct brush *brush,
                int isfill, int layer, SplineChar *sc, RefChar *ref)
{
  char buffer[400];
  int function_obj, shade_obj;
  int i, j;
  struct gradient *grad = brush->gradient;
  struct pattern *pat;

  if (grad != NULL)
    {
      function_obj = pdf_addobject (pi);
      fprintf (pi->out, "<<\n");
      fprintf (pi->out, "  /FunctionType 0\n"); /* Iterpolation between samples */
      fprintf (pi->out, "  /Domain [%g %g]\n",
               (double) grad->grad_stops[0].offset,
               (double) grad->grad_stops[grad->stop_cnt - 1].offset);
      fprintf (pi->out, "  /Range [0 1.0 0 1.0 0 1.0]\n");
      fprintf (pi->out, "  /Size [%d]\n", grad->stop_cnt == 2 ? 2 : 101);
      fprintf (pi->out, "  /BitsPerSample 8\n");
      fprintf (pi->out, "  /Decode [0 1.0 0 1.0 0 1.0]\n");
      fprintf (pi->out, "  /Length %d\n", 3 * (grad->stop_cnt == 2 ? 2 : 101));
      fprintf (pi->out, ">>\n");
      fprintf (pi->out, "stream\n");
      if (grad->stop_cnt == 2)
        {
          int col = grad->grad_stops[0].col;
          if (col == COLOR_INHERITED)
            col = 0x000000;
          putc ((col >> 16) & 0xff, pi->out);
          putc ((col >> 8) & 0xff, pi->out);
          putc ((col) & 0xff, pi->out);
          col = grad->grad_stops[1].col;
          if (col == COLOR_INHERITED)
            col = 0x000000;
          putc ((col >> 16) & 0xff, pi->out);
          putc ((col >> 8) & 0xff, pi->out);
          putc ((col) & 0xff, pi->out);
        }
      else
        {
          /* Rather than try and figure out the minimum common divisor */
          /*  off all the offsets, I'll just assume they are all percent */
          for (i = 0; i <= 100; ++i)
            {
              int col;
              double t =
                grad->grad_stops[0].offset +
                (grad->grad_stops[grad->stop_cnt - 1].offset -
                 grad->grad_stops[0].offset) * i / 100.0;
              for (j = 0; j < grad->stop_cnt; ++j)
                if (t <= grad->grad_stops[j].offset)
                  break;
              if (j == grad->stop_cnt)
                col = grad->grad_stops[j - 1].col;
              else if (t == grad->grad_stops[j].offset)
                col = grad->grad_stops[j].col;
              else
                {
                  double percent =
                    (t -
                     grad->grad_stops[j -
                                      1].offset) /
                    (grad->grad_stops[j].offset -
                     grad->grad_stops[j - 1].offset);
                  uint32_t col1 = grad->grad_stops[j - 1].col;
                  uint32_t col2 = grad->grad_stops[j].col;
                  if (col1 == COLOR_INHERITED)
                    col1 = 0x000000;
                  if (col2 == COLOR_INHERITED)
                    col2 = 0x000000;
                  int red =
                    ((col1 >> 16) & 0xff) * (1 - percent) +
                    ((col2 >> 16) & 0xff) * percent;
                  int green =
                    ((col1 >> 8) & 0xff) * (1 - percent) +
                    ((col2 >> 8) & 0xff) * percent;
                  int blue =
                    ((col1) & 0xff) * (1 - percent) + ((col2) & 0xff) * percent;
                  col = (red << 16) | (green << 8) | blue;
                }
              if (col == COLOR_INHERITED)
                col = 0x000000;
              putc ((col >> 16) & 0xff, pi->out);
              putc ((col >> 8) & 0xff, pi->out);
              putc ((col) & 0xff, pi->out);
            }
        }
      fprintf (pi->out, "\nendstream\n");
      fprintf (pi->out, "endobj\n");

      shade_obj = pdf_addobject (pi);
      fprintf (pi->out, "<<\n");
      fprintf (pi->out, "  /ShadingType %d\n", grad->radius == 0 ? 2 : 3);
      fprintf (pi->out, "  /ColorSpace /DeviceRGB\n");
      if (grad->radius == 0)
        {
          fprintf (pi->out, "  /Coords [%g %g %g %g]\n",
                   (double) grad->start.x, (double) grad->start.y,
                   (double) grad->stop.x, (double) grad->stop.y);
        }
      else
        {
          fprintf (pi->out, "  /Coords [%g %g 0 %g %g %g]\n",
                   (double) grad->start.x, (double) grad->start.y,
                   (double) grad->stop.x, (double) grad->stop.y,
                   (double) grad->radius);
        }
      fprintf (pi->out, "  /Function %d 0 R\n", function_obj);
      fprintf (pi->out, "  /Extend [true true]\n");     /* implies pad */
      fprintf (pi->out, ">>\n");
      fprintf (pi->out, "endobj\n");

      if (gr->pattern_cnt >= gr->pattern_max)
        {
          gr->pattern_names =
            xrealloc (gr->pattern_names,
                      (gr->pattern_max += 100) * sizeof (char *));
          gr->pattern_objs =
            xrealloc (gr->pattern_objs, (gr->pattern_max) * sizeof (int));
        }
      makePatName (buffer, ref, sc, layer, !isfill, true);
      gr->pattern_names[gr->pattern_cnt] = xstrdup_or_null (buffer);
      gr->pattern_objs[gr->pattern_cnt++] = pdf_addobject (pi);
      fprintf (pi->out, "<<\n");
      fprintf (pi->out, "  /Type /Pattern\n");
      fprintf (pi->out, "  /PatternType 2\n");
      fprintf (pi->out, "  /Shading %d 0 R\n", shade_obj);
      fprintf (pi->out, ">>\n");
      fprintf (pi->out, "endobj\n");
    }
  else if ((pat = brush->pattern) != NULL)
    {
      SplineChar *pattern_sc = SFGetChar (sc->parent, -1, pat->pattern);
      DBounds b;
      real scale[6], result[6];
      int respos, resobj;
      int lenpos, lenstart, len;

      if (pattern_sc == NULL)
        LogError (_("No glyph named %s, used as a pattern in %s\n"),
                  pat->pattern, sc->name);
      PatternSCBounds (pattern_sc, &b);

      if (gr->pattern_cnt >= gr->pattern_max)
        {
          gr->pattern_names =
            xrealloc (gr->pattern_names,
                      (gr->pattern_max += 100) * sizeof (char *));
          gr->pattern_objs =
            xrealloc (gr->pattern_objs, (gr->pattern_max) * sizeof (int));
        }
      makePatName (buffer, ref, sc, layer, !isfill, false);
      gr->pattern_names[gr->pattern_cnt] = xstrdup_or_null (buffer);
      gr->pattern_objs[gr->pattern_cnt++] = pdf_addobject (pi);
      fprintf (pi->out, "<<\n");
      fprintf (pi->out, "  /Type /Pattern\n");
      fprintf (pi->out, "  /PatternType 1\n");
      fprintf (pi->out, "  /PaintType 1\n");    /* The intricacies of uncolored tiles are not something into which I wish to delve */
      fprintf (pi->out, "  /TilingType 1\n");
      fprintf (pi->out, "  /BBox [%g %g %g %g]\n", (double) b.minx,
               (double) b.miny, (double) b.maxx, (double) b.maxy);
      fprintf (pi->out, "  /XStep %g\n", (double) (b.maxx - b.minx));
      fprintf (pi->out, "  /YStep %g\n", (double) (b.maxy - b.miny));
      memset (scale, 0, sizeof (scale));
      scale[0] = pat->width / (b.maxx - b.minx);
      scale[3] = pat->height / (b.maxy - b.miny);
      MatMultiply (scale, pat->transform, result);
      fprintf (pi->out, "  /Matrix [%g %g %g %g %g %g]\n", (double) result[0],
               (double) result[1], (double) result[2], (double) result[3],
               (double) result[4], (double) result[5]);
      fprintf (pi->out, "    /Resources ");
      respos = ftell (pi->out);
      fprintf (pi->out, "000000 0 R\n");
      fprintf (pi->out, "    /Length ");
      lenpos = ftell (pi->out);
      fprintf (pi->out, "00000000\n");
      fprintf (pi->out, ">>\n");
      fprintf (pi->out, " stream \n");
      lenstart = ftell (pi->out);
      SC_PSDump ((void (*)(int, void *)) fputc, pi->out, pattern_sc, true,
                 true, ly_all);
      len = ftell (pi->out) - lenstart;
      fprintf (pi->out, " endstream\n");
      fprintf (pi->out, "endobj\n");

      resobj = PdfDumpGlyphResources (pi, pattern_sc);
      fseek (pi->out, respos, SEEK_SET);
      fprintf (pi->out, "%6d", resobj);
      fseek (pi->out, lenpos, SEEK_SET);
      fprintf (pi->out, "%8d", len);
      fseek (pi->out, 0, SEEK_END);
    }
  if (brush->opacity < 1.0 && brush->opacity >= 0)
    {
      for (i = gr->opacity_cnt - 1; i >= 0; --i)
        {
          if (brush->opacity == gr->opac_state[i].opacity
              && isfill == gr->opac_state[i].opacity)
            break;              /* Already done */
        }
      if (i == -1)
        {
          if (gr->opacity_cnt >= gr->opacity_max)
            {
              gr->opac_state =
                xrealloc (gr->opac_state,
                          (gr->opacity_max +=
                           100) * sizeof (struct opac_state));
            }
          gr->opac_state[gr->opacity_cnt].opacity = brush->opacity;
          gr->opac_state[gr->opacity_cnt].isfill = isfill;
          gr->opac_state[gr->opacity_cnt].obj = function_obj =
            pdf_addobject (pi);
          ++gr->opacity_cnt;
          fprintf (pi->out, "<<\n");
          fprintf (pi->out, "  /Type /ExtGState\n");
          if (isfill)
            fprintf (pi->out, "  /ca %g\n", brush->opacity);
          else
            fprintf (pi->out, "  /CA %g\n", brush->opacity);
          fprintf (pi->out, "  /AIS false\n");  /* alpha value */
          fprintf (pi->out, ">>\n");
          fprintf (pi->out, "endobj\n\n");
        }
    }
}

static void
pdf_ImageCheck (PI * pi, struct glyph_res *gr, ImageList *images, int layer,
                SplineChar *sc)
{
  char buffer[400];
  int icnt = 0;
  GImage *img;
  struct _GImage *base;
  int i;

  while (images != NULL)
    {
      img = images->image;
      base = img->list_len == 0 ? img->u.image : img->u.images[1];

      if (gr->image_cnt >= gr->image_max)
        {
          gr->image_names =
            xrealloc (gr->image_names,
                      (gr->image_max += 100) * sizeof (char *));
          gr->image_objs =
            xrealloc (gr->image_objs, (gr->image_max) * sizeof (int));
        }
      sprintf (buffer, "%s_ly%d_%d_image", sc->name, layer, icnt);
      gr->image_names[gr->image_cnt] = xstrdup_or_null (buffer);
      gr->image_objs[gr->image_cnt++] = pdf_addobject (pi);
      ++icnt;

      fprintf (pi->out, "<<\n");
      fprintf (pi->out, "  /Type /XObject\n");
      fprintf (pi->out, "  /Subtype /Image\n");
      fprintf (pi->out, "  /Width %d\n", base->width);
      fprintf (pi->out, "  /Height %d\n", base->height);
      if (base->image_type == it_mono)
        {
          fprintf (pi->out, "  /BitsPerComponent 1\n");
          fprintf (pi->out, "  /ImageMask true\n");
          fprintf (pi->out, "  /Length %d\n",
                   base->height * base->bytes_per_line);
        }
      else if (base->image_type == it_true)
        {
          fprintf (pi->out, "  /BitsPerComponent 8\n");
          fprintf (pi->out, "  /ColorSpace /DeviceRGB\n");
          fprintf (pi->out, "  /Length %d\n", base->height * base->width * 3);
        }
      else if (base->image_type == it_index)
        {
          fprintf (pi->out, "  /BitsPerComponent 8\n");
          fprintf (pi->out, "  /ColorSpace [/Indexed /DeviceRGB %d\n<",
                   base->clut->clut_len);
          for (i = 0; i < base->clut->clut_len; ++i)
            fprintf (pi->out, "%06x ", base->clut->clut[i]);
          fprintf (pi->out, ">\n");
          fprintf (pi->out, "  /Length %d\n", base->height * base->width);
        }
      fprintf (pi->out, ">>\n");
      fprintf (pi->out, "stream\n");
      if (base->image_type != it_true)
        {
          fwrite (base->data, 1, base->height * base->bytes_per_line, pi->out);
        }
      else
        {
          /* My image representation of colors includes a pad byte, pdf's does not */
          uint32_t *pt = (uint32_t *) base->data;
          for (i = 0; i < base->width * base->height; ++i, ++pt)
            {
              int red = (*pt >> 16) & 0xff;
              int green = (*pt >> 8) & 0xff;
              int blue = (*pt) & 0xff;
              putc (red, pi->out);
              putc (green, pi->out);
              putc (blue, pi->out);
            }
        }
      fprintf (pi->out, "\nendstream\n");
      fprintf (pi->out, "endobj\n");
      images = images->next;
    }
}

/* We need different gradients and patterns for different transform */
/*  matrices of references to the same glyph. Sigh. */
int
PdfDumpGlyphResources (PI * pi, SplineChar *sc)
{
  int resobj;
  struct glyph_res gr = GLYPH_RES_EMPTY;
  int i;
  int layer;
  RefChar *ref;

  for (layer = ly_fore; layer < sc->layer_cnt; ++layer)
    {
      if (sc->layers[layer].dofill)
        pdf_BrushCheck (pi, &gr, &sc->layers[layer].fill_brush, true, layer,
                        sc, NULL);
      if (sc->layers[layer].dostroke)
        pdf_BrushCheck (pi, &gr, &sc->layers[layer].stroke_pen.brush, false,
                        layer, sc, NULL);
      pdf_ImageCheck (pi, &gr, sc->layers[layer].images, layer, sc);
      for (ref = sc->layers[layer].refs; ref != NULL; ref = ref->next)
        {
          for (i = 0; i < ref->layer_cnt; ++i)
            {
              if (ref->layers[i].dofill)
                pdf_BrushCheck (pi, &gr, &ref->layers[i].fill_brush, true, i,
                                ref->sc, ref);
              if (ref->layers[i].dostroke)
                pdf_BrushCheck (pi, &gr, &ref->layers[i].stroke_pen.brush,
                                false, i, ref->sc, ref);
              pdf_ImageCheck (pi, &gr, ref->layers[i].images, i, ref->sc);
            }
        }
    }
  resobj = pdf_addobject (pi);
  fprintf (pi->out, "<<\n");
  if (gr.pattern_cnt != 0)
    {
      fprintf (pi->out, "  /Pattern <<\n");
      for (i = 0; i < gr.pattern_cnt; ++i)
        {
          fprintf (pi->out, "    /%s %d 0 R\n", gr.pattern_names[i],
                   gr.pattern_objs[i]);
          free (gr.pattern_names[i]);
        }
      free (gr.pattern_names);
      free (gr.pattern_objs);
      fprintf (pi->out, "  >>\n");
    }
  if (gr.image_cnt != 0)
    {
      fprintf (pi->out, "  /XObject <<\n");
      for (i = 0; i < gr.image_cnt; ++i)
        {
          fprintf (pi->out, "    /%s %d 0 R\n", gr.image_names[i],
                   gr.image_objs[i]);
          free (gr.image_names[i]);
        }
      free (gr.image_names);
      free (gr.image_objs);
      fprintf (pi->out, "  >>\n");
    }
  if (gr.opacity_cnt != 0)
    {
      fprintf (pi->out, "  /ExtGState <<\n");
      for (i = 0; i < gr.opacity_cnt; ++i)
        {
          fprintf (pi->out, "    /gs_%s_opacity_%g %d 0 R\n",
                   gr.opac_state[i].isfill ? "fill" : "stroke",
                   gr.opac_state[i].opacity, gr.opac_state[i].obj);
        }
      free (gr.opac_state);
      fprintf (pi->out, "  >>\n");
    }
  fprintf (pi->out, ">>\n");
  fprintf (pi->out, "endobj\n\n");
  return resobj;
}

/* ************************************************************************** */
/* ***************** Sample Text in Many Scripts/Languages ****************** */
/* ************************************************************************** */

/* English */
static char *_simple[] = {
  " A quick brown fox jumps over the lazy dog.",
  NULL
};

static char *_simplelatnchoices[] = {
/* English */
  " A quick brown fox jumps over the lazy dog.",
  " Few zebras validate my paradox, quoth Jack Xeno",
  " A quick brown vixen jumps for the lazy dog.",
  " The quick blonde doxy jumps over an unfazed wag.",
/* Swedish */
  "flygande bäckasiner söka hwila på mjuka tuvor",
/* German (from http://shiar.net/misc/txt/pangram.en) */
/* Twelve boxing fighters hunted Eva across the great dike of Sylt */
  "zwölf Boxkämpfer jagten Eva quer über den großen Sylter Deich",
/* French (from http://shiar.net/misc/txt/pangram.en) */
/* Carry this old wisky to the blond smoking judge */
  "portez ce vieux whisky au juge blond qui fume",
  "Les naïfs ægithales hâtifs pondant à Noël où il gèle sont sûrs d'être déçus et de voir leurs drôles d'œufs abîmés.",
/* Dutch (from http://shiar.net/misc/txt/pangram.en) */
/* Sexy by body, though scared by the swimsuit */
  " sexy qua lijf, doch bang voor 't zwempak",
/* Polish (from http://shiar.net/misc/txt/pangram.en) */
/* to push a hedgehog or eight bins of figs in this boat */
  " pchnąć w tę łódź jeża lub ośm skrzyń fig ",
/* Slovaka (from http://shiar.net/misc/txt/pangram.en) */
  " starý kôň na hŕbe kníh žuje tíško povädnuté ruže, na stĺpe sa ďateľ učí kvákať novú ódu o živote ",
/* Czech (from http://shiar.net/misc/txt/pangram.en) */
  " příliš žluťoučký kůň úpěl ďábelské kódy ",
  NULL
};

static uint32_t _simplelatnlangs[] = {
  CHR ('E', 'N', 'G', ' '),
  CHR ('E', 'N', 'G', ' '),
  CHR ('E', 'N', 'G', ' '),
  CHR ('E', 'N', 'G', ' '),
  CHR ('S', 'V', 'E', ' '),
  CHR ('D', 'E', 'U', ' '),
  CHR ('F', 'R', 'A', ' '),
  CHR ('F', 'R', 'A', ' '),
  CHR ('N', 'L', 'D', ' '),
  CHR ('P', 'L', 'K', ' '),
  CHR ('S', 'L', 'V', ' '),
  CHR ('C', 'S', 'Y', ' ')
};

/* Hebrew (from http://shiar.net/misc/txt/pangram.en) */
static char *_simplehebrew[] = {
  " ?דג סקרן שט בים מאוכזב ולפתע מצא לו חברה איך הקליטה ",
  NULL
};

/* Katakana (from http://shiar.net/misc/txt/pangram.en) */
static char *_simplekata[] = {
  " イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム/ ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン ",
  NULL
};

/* Hiragana (from http://shiar.net/misc/txt/pangram.en) */
static char *_simplehira[] = {
  " いろはにほへとちりぬるを/ わかよたれそつねならむ/ うゐのおくやまけふこえて/ あさきゆめみしゑひもせす ",
  NULL
};

/* Russian */
static char *_simplecyrill[] = {
  " Съешь ещё этих мягких французских булок, да выпей чаю!",
  NULL
};

static char *_simplecyrillchoices[] = {
/* Eat more those soft french 'little-sweet-breads' and drink tea */
  " Съешь ещё этих мягких французских булок, да выпей чаю!",
/* "In the deep forests of South citrus lived... /answer/Yeah but falsificated one!" */
  " В чащах юга жил-был цитрус -- да, но фальшивый экземпляръ!",
/* A kind lamplighter with grimy face wants to show me a stunt. */
  "Љубазни фењерџија чађавог лица хоће да ми покаже штос.",
/* More frequent filtering through the reticular bag improves
fertilization of genetic hybrids. */
  "Чешће цeђење мрeжастим џаком побољшава фертилизацију генских хибрида.",
  NULL
};

static uint32_t _simplecyrilliclangs[] = {
  CHR ('R', 'U', 'S', ' '),     /* Russian */
  CHR ('R', 'U', 'S', ' '),
  CHR ('S', 'R', 'B', ' '),     /* Serbian */
  CHR ('S', 'R', 'B', ' ')
};

/* Russian */
static char *_annakarenena[] = {
  " Всѣ счастливыя семьи похожи другъ на друга, каждая несчастливая семья несчастлива по-своему.",
  " Все смѣшалось въ домѣ Облонскихъ. Жена узнала, что мужъ былъ связи съ бывшею въ ихъ домѣ француженкою-гувернанткой, и объявила мужу, что не можетъ жить съ нимъ въ одномъ домѣ.",
  NULL
};

/* Serbian (Cyrillic) */
static char *_serbcyriljohn[] = {
  "У почетку беше Реч Божија, и та Реч беше у Бога, и Бог беше Реч.",
  NULL
};

/* Spanish */
static char *_donquixote[] = {
  " En un lugar de la Mancha, de cuyo nombre no quiero acordarme, no ha mucho tiempo que vivía un hidalgo de los de lanza en astillero, adarga antigua, rocín flaco y galgo corredor.",
  NULL
};

/* German */
static char *_faust[] = {
  "Ihr naht euch wieder, schwankende Gestalten,",
  "Die früh sich einst dem trüben Blick gezeigt.",
  "Versuch ich wohl, euch diesmal festzuhalten?",
  "Fühl ich mein Herz noch jenem Wahn geneigt?",
  "Ihr drängt euch zu! Nun gut, so mögt ihr walten,",
  "Wie ihr aus Dunst und Nebel um mich steigt;",
  "Mein Busen fühlt sich jugendlich erschüttert",
  "Vom Zauberhauch, der euren Zug umwittert.",
  NULL
};

/* Anglo Saxon */
static char *_beorwulf[] = {
  "Hwæt, we Gar-Dena  in geardagum",
  "þeodcyninga  þrym gefrunon,",
  "hu ða æþelingas  ellen fremedon.",
  " Oft Scyld Scefing  sceaþena þreatum,",
  "monegum mægþum  meodosetla ofteah;",
  "egsode Eorle.  Syððan ærest wearð",
  "feasceaft funden,  (he þæs frofre gebad)",
  "weox under wolcnum,  weorðmyndum þah,",
  "oðþæt him æghwyle  þara ymbsittendra",
  "ofer hronrade  hyan scolde,",
  "gomban gyldan: þæt wæs god cyning!",
  NULL
};

/* Italian */
static char *_inferno[] = {
  " Nel mezzo del cammin di nostra vita",
  "mi ritrovai per una selva obscura,",
  "ché la diritta via era smarrita.",
  " Ahi quanto a dir qual era è cosa dura",
  "esta selva selvaggia e aspra e forte",
  "che nel pensier rinova la paura!",
  NULL
};

/* Latin */
static char *_debello[] = {
  " Gallia est omnis dīvīsa in partēs trēs, quārum ūnum incolunt Belgae, aliam Aquītānī, tertiam, quī ipsōrum linguā Celtae, nostrā Gallī appelantur. Hī omnēs linguā, īnstitūtīs, lēgibus inter sē differunt. Gallōs ab Aquītānīs Garumna flūmen, ā Belgīs Matrona et Sēquana dīvidit.",
  NULL
};

/* French */
static char *_pheadra[] = {
  "Le dessein en est pris: je pars, cher Théramène,",
  "Et quitte le séjour de l'aimable Trézène.",
  "Dans le doute mortel dont je suis agité,",
  "Je commence à rougir de mon oisiveté.",
  "Depuis plus de six mois éloigné de mon père,",
  "J'ignore le destin d'une tête si chère;",
  "J'ignore jusqu'aux lieux qui le peuvent cacher.",
  NULL
};

/* Classical Greek */
static char *_antigone[] = {
  "Ὦ κοινὸν αὐτάδελφον Ἰσμήνης κάρα,",
  "ἆῤ οἶσθ᾽ ὅτι Ζεὺς τῶν ἀπ᾽ Οἰδίπου κακῶν",
  "ὁποῖον οὐχὶ νῷν ἔτι ζσαιν τελεῖ;",
  NULL
};

                                                                                                                                                             /* Hebrew *//* Seder */
static char *_hebrew[] = {
  "וְאָתָא מַלְאַךְ הַמָּוֶת, וְשָׁחַט לְשּׁוׂחֵט, רְּשָׁחַט לְתוׂרָא, רְּשָׁתַה לְמַּיָּא, דְּכָכָה לְנוּרָא, דְּשָׂרַף לְחוּטְרָא, דְּהִכָּה לְכַלְכָּא, דְּנָשַׁךְ לְשׁוּנְרָא, דְּאָכְלָה לְגַדְיָא, דִּזְבַן אַבָּא בִּתְרֵי זוּזֵי. חַד גַּדְיָא, חַד גַּדְיָא.",
  "וְאָתָא הַקָּדוֹשׁ כָּדוּךְ הוּא, וְשָׁחַט לְמַלְאַךְ הַמָּוֶת, רְּשָׁחַט לְשּׁוׂחֵט, רְּשָׁחַט לְתוׂרָא, רְּשָׁתַה לְמַּיָּא, דְּכָכָה לְנוּרָא, דְּשָׂרַף לְחוּטְרָא, דְּהִכָּה לְכַלְכָּא, דְּנָשַׁךְ לְשׁוּנְרָא, דְּאָכְלָה לְגַדְיָא, דִּזְבַן אַבָּא בִּתְרֵי זוּזֵי. חַד גַּדְיָא, חַד גַּדְיָא.",
  NULL
};

/* Arabic with no dots or vowel marks */
static char *_arabic[] = {
  "لقيت الحكمة العادلة حبا عظيما من الشعب. انسحبت من النادی فی السنة الماضية. وقعت فی الوادی فانكسرت يدی. قابلت صديقی عمرا الكاتب القدير فی السوق فقال لی انه ارسل الى الجامعة عددا من مجلته الجديدة. احتل الامير فيصل مدينة دمسق فی الحرب العالمية ودخلها راكبا على فرسه المحبوبة. ارسل عمر خالدا الى العراق ولاكن بعد مدة قصيرة وجه خالد جيشه الى سورية. قدم على دمشق واستطاع فتحها. قبل احتل عمر القدس وعقد جلستة مع حاكم القدس وقد تكلم معه عن فتح المدينة. ثم رجع عمر الى المدنة المنورة.",
  NULL
};

/* Renaissance English with period ligatures */
static char *_muchado[] = {
  " But till all graces be in one woman, one womã ſhal not com in my grace: rich ſhe ſhall be thats certain, wiſe, or ile none, vertuous, or ile neuer cheapen her.",
  NULL
};

/* Modern (well, Dickens) English */
static char *_chuzzlewit[] = {
  " As no lady or gentleman, with any claims to polite breeding, can"
    " possibly sympathize with the Chuzzlewit Family without being first"
    " assured of the extreme antiquity of the race, it is a great satisfaction"
    " to know that it undoubtedly descended in a direct line from Adam and"
    " Eve; and was, in the very earliest times, closely connected with the"
    " agricultural interest. If it should ever be urged by grudging and"
    " malicious persons, that a Chuzzlewit, in any period of the family"
    " history, displayed an overweening amount of family pride, surely the"
    " weakness will be considered not only pardonable but laudable, when the"
    " immense superiority of the house to the rest of mankind, in respect of"
    " this its ancient origin, is taken into account.",
  NULL
};

/* Middle Welsh */
static char *_mabinogion[] = {
  " Gan fod Argraffiad Rhydychen o'r Mabinogion yn rhoi'r testun yn union fel y digwydd yn y llawysgrifau, ac felly yn cyfarfod â gofyn yr ysgolhaig, bernais mai gwell mewn llawlyfr fel hwn oedd golygu peth arno er mwyn helpu'r ieuainc a'r dibrofiad yn yr hen orgraff.",
  NULL
};

/* Swedish */
static char *_PippiGarOmBord[] = {
  "Om någon människa skulle komma resande till den lilla, lilla staden och så kanske ett tu tre råka förirra sig lite för långt bort åt ena utkanten, då skulle den månniskan få se Villa Villekulla. Inte för att huset var så mycket att titta på just, en rätt fallfårdig gammal villa och en rätt vanskött gammal trädgård runt omkring, men främlingen skulle kanske i alla fall stanna och undra vem som bodde där.",
  NULL
};

/* Czech */
static char *_goodsoldier[] = {
  " „Tak nám zabili Ferdinanda,“ řekla posluhovačka panu Švejkovi, který opustiv před léty vojenskou službu, když byl definitivně prohlášen vojenskou lékařskou komisí za blba, živil se prodejem psů, ošklivých nečistokrevných oblud, kterým padělal rodokmeny.",
  " Kromě tohoto zaměstnání byl stižen revmatismem a mazal si právě kolena opodeldokem.",
  NULL
};

/* Lithuanian */
static char *_lithuanian[] = {
  " Kiekviena šventė yra surišta su praeitimi. Nešvenčiamas gimtadienis, kai, kūdikis gimsta. Ir po keliolikos metų gimtinės arba vardinės nėra tiek reikšmingos, kaip sulaukus 50 ar 75 metų. Juo tolimesnis įvykis, tuo šventė darosi svarbesnė ir iškilmingesnė.",
  NULL
};

/* Polish */
static char *_polish[] = {
  " Język prasłowiański miał w zakresie deklinacji (fleksji imiennej) następujące kategorie gramatyczne: liczby, rodzaju i przypadku. Poza tym istniały w nim (w zakresie fleksji rzeczownika) różne «odmiany», czyli typy deklinacyjne. Im dawniej w czasie, tym owe różnice deklinacyjne miały mniejszy związek z semantyką rzeczownika.",
  NULL
};

/* Slovene */
static char *_slovene[] = {
  " Razvoj glasoslovja je diametralno drugačen od razvoja morfologije.",
  " V govoru si besede slede. V vsaki sintagmi dobi beseda svojo vrednost, če je zvezana z besedo, ki je pred njo, in z besedo, ki ji sledi.",
  NULL
};

/* Macedonian */
static char *_macedonian[] = {
  " Македонскиот јазик во балканската јазична средина и наспрема соседните словенски јаеици. 1. Македонскиот јазик се говори во СР Македонија, и надвор од нејзините граници, во оние делови на Македонија што по балканските војни влегоа во составот на Грција и Бугарија.",
  NULL
};

/* Bulgarian */
static char *_bulgarian[] = {
  " ПРЕДМЕТ И ЗАДАЧИ НА ФОНЕТИКАТА Думата фонетика произлиза от гръцката дума фоне, която означава „звук“, „глас“, „тон“.",
  NULL
};

/* Korean Hangul */
static char *_hangulsijo[] = {
  "어버이 살아신 제 섬길 일란 다 하여라",
  "지나간 후면 애닯다 어이 하리",
  "평생에 고쳐 못할 일이 이뿐인가 하노라",
  "- 정철",
  "",
  "이고 진 저 늙은이 짐 벗어 나를 주오",
  "나는 젊었거니 돌이라 무거울까",
  "늙기도 설워라커든 짐을 조차 지실까",
  "- 정철",
  NULL
};

/* Chinese traditional */
/* https://en.wikipedia.org/wiki/Tao_Te_Ching */
static char *_TaoTeChing[] = {
  "道可道非常道，",
  "名可名非常名。",
  NULL
};

/* http://gan.wikipedia.org/wiki/%E5%B0%87%E9%80%B2%E9%85%92 */
static char *_LiBai[] = {
  "將進酒",
  "",
  "君不見 黃河之水天上來 奔流到海不復回",
  "君不見 高堂明鏡悲白髮 朝如青絲暮成雪",
  "人生得意須盡歡 莫使金樽空對月",
  "天生我材必有用 千金散盡還復來",
  "烹羊宰牛且為樂 會須一飲三百杯",
  "岑夫子 丹丘生 將進酒 君莫停",
  "與君歌一曲 請君為我側耳聽",
  "",
  "鐘鼓饌玉不足貴 但願長醉不願醒",
  "古來聖賢皆寂寞 惟有飲者留其名",
  "陳王昔時宴平樂 斗酒十千恣讙謔",
  "主人何為言少錢 徑須沽取對君酌",
  "五花馬 千金裘 呼兒將出換美酒 與爾同消萬古愁",
  NULL
};

static char *_LiBaiShort[] = {
  "將進酒",
  "",
  "君不見 黃河之水天上來 奔流到海不復回",
  "君不見 高堂明鏡悲白髮 朝如青絲暮成雪",
  NULL
};

/* Japanese */
/* https://ja.wikipedia.org/wiki/%E6%BA%90%E6%B0%8F%E7%89%A9%E8%AA%9E */
static char *_Genji[] = {
  "源氏物語（紫式部）：いづれの御時にか、女御・更衣あまた さぶらひたまひけるなかに、いとやむごとなき 際にはあらぬが、すぐれてときめきたまふありけり。",
  NULL
};

/* http://www.geocities.jp/sybrma/42souseki.neko.html */
static char *_IAmACat[] = {
  "吾輩は猫である（夏目漱石）：吾輩は猫である",
  NULL
};

/* The following translations of the gospel according to John are all from */
/*  Compendium of the world's languages. 1991 Routledge. by George L. Campbell*/

/* Belorussian */
static char *_belorussianjohn[] = {
  "У пачатку было Слова, і Слова было ў Бога, і Богам было Слова. Яно было ў пачатку ў Бога",
  NULL
};

/* basque */
static char *_basquejohn[] = {
  "Asieran Itza ba-zan, ta Itza Yainkoagan zan, ta Itza Yainko zan.",
  "Asieran Bera Yainkoagan zan.",
  NULL
};

/* danish */
static char *_danishjohn[] = {
  "Begyndelsen var Ordet, og Ordet var hos Gud, og Ordet var Gud.",
  "Dette var i Begyndelsen hos Gud.",
  NULL
};

/* dutch */
static char *_dutchjohn[] = {
  "In den beginne was het Woord en het Woord was bij God en het Woord was God.",
  "Dit was in den beginne bij God.",
  NULL
};

/* finnish */
static char *_finnishjohn[] = {
  "Alussa oli Sana, ja Sana oli Jumalan luona, Sana oli Jumala.",
  "ja hä oli alussa Jumalan luona.",
  NULL
};

/* georgian */
    /* Hmm, the first 0x10e0 might be 0x10dd, 0x301 */
static char *_georgianjohn[] = {
  "პირველითგან იყო სიტყუუ̂ა, და სიტყუუ̂ა იგი იყო ღუ̂თისა თანა, და დიერთი იყო სიტყუუ̂ა იგი.",
  "ესე იყო პირველითგან დიერთი თინი.",
  NULL
};

/* icelandic */
static char *_icelandicjohn[] = {
  "Í upphafi var Orðið og Orðið var hjà Guði, og Orðið var Guði.",
  "Það var í upphafi hjá Guði.",
  NULL
};

/* irish */
static char *_irishjohn[] = {
  "Bhí an Briathar(I) ann i dtús báire agus bhí an Briathar in éineacht le Dia, agus ba Dhia an Briathar.",
  "Bhí sé ann i dtús báire in éineacht le Dia.",
  NULL
};

/* Bokmål norwegian */
static char *_norwegianjohn[] = {
  "I begynnelsen var Ordet, Ordet var hos Gud, og Ordet var Gud.",
  "Han var i begynnelsen hos Gud.",
  "Alt er blitt til ved ham; uten ham er ikke noe blitt til av alt som er til.",
  NULL
};

/* Nynorsk norwegian */
static char *_nnorwegianjohn[] = {
  "I opphavet var Ordet, og Ordet var hjå Gud, og Ordet var Gud.",
  "Han var i opphavet hjå Gud.",
  NULL
};

/* old church slavonic */
static char *_churchjohn[] = {
  "Въ нача́лѣ бѣ̀ сло́во и҆ сло́во бѣ̀ къ бг҃ꙋ, и҆ бг҃ъ бѣ̀ сло́во.",
  "Се́й бѣ̀ и҆сконѝ къ бг҃ꙋ.",
  NULL
};

/* swedish */
static char *_swedishjohn[] = {
  "I begynnelsen var Ordet, och Ordet var hos Gud, och Ordet var Gud.",
  "Han var i begynnelsen hos Gud.",
  NULL
};

/* portuguese */
static char *_portjohn[] = {
  "No Principio era a Palavra, e a Palavra estava junto de Deos, e a Palavra era Deos.",
  "Esta estava no principio junto de Deos.",
  NULL
};

/* cherokee */
static char *_cherokeejohn[] = {
  "ᏗᏓᎴᏂᏯᎬ ᎧᏃᎮᏘ ᎡᎮᎢ, ᎠᎴ ᎾᏯᎩ ᎧᏃᎮᏘ ᎤᏁᎳᏅᎯ ᎢᏧᎳᎭ ᎠᏘᎮᎢ, ᎠᎴ ᎾᏯᎩ ᎧᏃᎮᏘ ᎤᏁᎳᏅᎯ ᎨᏎᎢ.",
  "ᏗᏓᎴᏂᏯᎬ ᎾᏯᎩ ᎤᏁᎳᏅᎯ ᎢᏧᎳᎭ ᎠᏘᎮᎢ",
  NULL
};

/* swahili */
static char *_swahilijohn[] = {
  "Hapo mwanzo kulikuwako Neno, naye Neno alikuwako kwa Mungo, naye Neno alikuwa Mungu, Huyo mwanzo alikuwako kwa Mungu.",
  "Vyote vilvanyika kwa huyo; wala pasipo yeye hakikufanyika cho chote kilichofanyiki.",
  NULL
};

                                                                                                                                        /* thai *//* I'm sure I've made transcription errors here, I can't figure out what "0xe27, 0xe38, 0xe4d" really is */
static char *_thaijohn[] = {
  "๏ ในทีเดิมนะนพวุํลอโฆเปนอยู่ แลเปนอยู่ดว้ยกันกับ พวุํเฆ้า",
  NULL
};

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       /* Mayan K'iche' of Guatemala *//* Prolog to Popol Wuj *//* Provided by Daniel Johnson */
static char *_mayanPopolWuj[] = {
  "Are u xe' ojer tzij waral, C'i Che' u bi'. Waral xchikatz'ibaj-wi, xchikatiquiba-wi ojer tzij, u ticaribal, u xe'nabal puch ronojel xban pa tinamit C'i Che', ramak C'i Che' winak.",
  NULL
};

/* I've omitted cornish. no interesting letters. no current speakers */

  /* http://www.ethnologue.com/iso639/codes.asp */
enum scripts
{ sc_latin, sc_greek, sc_cyrillic, sc_georgian, sc_hebrew,
  sc_arabic, sc_hangul, sc_chinesetrad, sc_chinesemod, sc_kanji,
  sc_hiragana, sc_katakana
};
static struct langsamples
{
  char **sample;
  char *iso_lang;               /* ISO 639 two character abbreviation */
  enum scripts script;
  uint32_t otf_script, lang;
} sample[] =
{
  {
  _simple, "various", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('E', 'N', 'G', ' ')},
  {
  _simplecyrill, "various", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('R', 'U', 'S', ' ')},
  {
  _simplehebrew, "he", sc_hebrew, CHR ('h', 'e', 'b', 'r'),
      CHR ('I', 'W', 'R', ' ')},
  {
  _simplekata, "ja", sc_katakana, CHR ('k', 'a', 'n', 'a'),
      CHR ('J', 'A', 'N', ' ')},
  {
  _simplehira, "ja", sc_hiragana, CHR ('k', 'a', 'n', 'a'),
      CHR ('J', 'A', 'N', ' ')},
  {
  _faust, "de", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('D', 'E', 'U', ' ')},
  {
  _pheadra, "fr", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('F', 'R', 'A', ' ')},
  {
  _antigone, "el", sc_greek, CHR ('g', 'r', 'e', 'k'), CHR ('P', 'G', 'R', ' ')},       /* Is this polytonic? */
  {
  _annakarenena, "ru", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('R', 'U', 'S', ' ')},
  {
  _serbcyriljohn, "sr", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('S', 'R', 'B', ' ')},
  {
  _debello, "la", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('L', 'A', 'T', ' ')},
  {
  _hebrew, "he", sc_hebrew, CHR ('h', 'e', 'b', 'r'), CHR ('I', 'W', 'R', ' ')},
  {
  _arabic, "ar", sc_arabic, CHR ('a', 'r', 'a', 'b'), CHR ('A', 'R', 'A', ' ')},
  {
  _hangulsijo, "ko", sc_hangul, CHR ('h', 'a', 'n', 'g'),
      CHR ('K', 'O', 'R', ' ')},
  {
  _TaoTeChing, "zh", sc_chinesetrad, CHR ('h', 'a', 'n', 'i'),
      CHR ('Z', 'H', 'T', ' ')},
  {
  _LiBai, "zh", sc_chinesetrad, CHR ('h', 'a', 'n', 'i'),
      CHR ('Z', 'H', 'T', ' ')},
  {
  _Genji, "ja", sc_kanji, CHR ('h', 'a', 'n', 'i'), CHR ('J', 'A', 'N', ' ')},
  {
  _IAmACat, "ja", sc_kanji, CHR ('h', 'a', 'n', 'i'), CHR ('J', 'A', 'N', ' ')},
  {
  _donquixote, "es", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('E', 'S', 'P', ' ')},
  {
  _inferno, "it", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('I', 'T', 'A', ' ')},
  {
  _beorwulf, "enm", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('E', 'N', 'G', ' ')},      /* 639-2 name for middle english */
  {
  _muchado, "eng", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('E', 'N', 'G', ' ')},       /* 639-2 name for modern english */
  {
  _chuzzlewit, "en", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('E', 'N', 'G', ' ')},     /* 639-2 name for modern english */
  {
  _PippiGarOmBord, "sv", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('S', 'V', 'E', ' ')},
  {
  _mabinogion, "cy", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('W', 'E', 'L', ' ')},
  {
  _goodsoldier, "cs", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('C', 'S', 'Y', ' ')},
  {
  _macedonian, "mk", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('M', 'K', 'D', ' ')},
  {
  _bulgarian, "bg", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('B', 'G', 'R', ' ')},
  {
  _belorussianjohn, "be", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('B', 'E', 'L', ' ')},
  {
  _churchjohn, "cu", sc_cyrillic, CHR ('c', 'y', 'r', 'l'),
      CHR ('C', 'S', 'L', ' ')},
  {
  _lithuanian, "lt", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('L', 'T', 'H', ' ')},
  {
  _polish, "pl", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('P', 'L', 'K', ' ')},
  {
  _slovene, "sl", sc_latin, CHR ('l', 'a', 't', 'n'), CHR ('S', 'L', 'V', ' ')},
  {
  _irishjohn, "ga", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('I', 'R', 'I', ' ')},
  {
  _basquejohn, "eu", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('E', 'U', 'Q', ' ')},
  {
  _portjohn, "pt", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('P', 'T', 'G', ' ')},
  {
  _icelandicjohn, "is", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('I', 'S', 'L', ' ')},
  {
  _danishjohn, "da", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('D', 'A', 'N', ' ')},
  {
  _swedishjohn, "sv", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('S', 'V', 'E', ' ')},
  {
  _norwegianjohn, "no", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('N', 'O', 'R', ' ')},
  {
  _nnorwegianjohn, "no", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('N', 'O', 'R', ' ')},
  {
  _dutchjohn, "nl", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('N', 'L', 'D', ' ')},
  {
  _finnishjohn, "fi", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('F', 'I', 'N', ' ')},
  {
  _cherokeejohn, "chr", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('C', 'H', 'R', ' ')},
  {
  _thaijohn, "th", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('T', 'H', 'A', ' ')},
  {
  _georgianjohn, "ka", sc_georgian, CHR ('g', 'e', 'o', 'r'),
      CHR ('K', 'A', 'T', ' ')},
  {
  _swahilijohn, "sw", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('S', 'W', 'K', ' ')},
  {
  _mayanPopolWuj, "QUT", sc_latin, CHR ('l', 'a', 't', 'n'),
      CHR ('Q', 'U', 'T', ' ')},
  {
  NULL, NULL, 0, 0, 0}
};

static void
OrderSampleByLang (void)
{
  const char *lang = getenv ("LANG");
  char langbuf[12], *pt;
  int i, j;
  int simple_pos;

  if (lang == NULL)
    return;

  strncpy (langbuf, lang, 10);
  langbuf[10] = '\0';
  for (j = 0; j < 3; ++j)
    {
      if (j == 1)
        {
          for (pt = langbuf; *pt != '\0' && *pt != '.'; ++pt);
          *pt = '\0';
        }
      else if (j == 2)
        {
          for (pt = langbuf; *pt != '\0' && *pt != '_'; ++pt);
          *pt = '\0';
        }
      for (i = 0; sample[i].sample != NULL; ++i)
        if (strcmp (sample[i].iso_lang, langbuf) == 0)
          {
            struct langsamples temp;
            temp = sample[i];
            sample[i] = sample[2];
            sample[2] = temp;
            goto out;
          }
    }
out:
  simple_pos = 0;
  if (strcmp (langbuf, "sv") == 0)
    simple_pos = 4;
  else if (strcmp (langbuf, "de") == 0)
    simple_pos = 5;
  else if (strcmp (langbuf, "fr") == 0)
    simple_pos = 6 + (rand () & 1);
  else if (strcmp (langbuf, "nl") == 0)
    simple_pos = 8;
  else if (strcmp (langbuf, "pl") == 0)
    simple_pos = 9;
  else if (strcmp (langbuf, "pl") == 0)
    simple_pos = 10;
  else if (strcmp (langbuf, "cz") == 0)
    simple_pos = 11;
  else
    simple_pos = rand () & 3;
  _simple[0] = _simplelatnchoices[simple_pos];
  sample[0].lang = _simplelatnlangs[simple_pos];

  for (j = 0; _simplecyrillchoices[j] != NULL; ++j);
  simple_pos = rand () % j;
  _simplecyrill[0] = _simplecyrillchoices[simple_pos];
  sample[1].lang = _simplecyrilliclangs[simple_pos];
}

static int
AllChars (SplineFont *sf, const char *str)
{
  int i, ch;
  SplineChar *sc;
  struct altuni *alt;

  if (sf->subfontcnt == 0)
    {
      while ((ch = u8_get_next ((const uint8_t **) &str)) != '\0')
        {
          for (i = 0; i < sf->glyphcnt; ++i)
            if ((sc = sf->glyphs[i]) != NULL)
              {
                if (sc->unicodeenc == ch)
                  break;
                for (alt = sc->altuni; alt != NULL; alt = alt->next)
                  if (alt->vs == -1 && alt->unienc == ch)
                    break;
                if (alt != NULL)
                  break;
              }
          if (i == sf->glyphcnt || !SCWorthOutputting (sf->glyphs[i]))
            return false;
        }
    }
  else
    {
      int max = 0, j;
      for (i = 0; i < sf->subfontcnt; ++i)
        if (sf->subfonts[i]->glyphcnt > max)
          max = sf->subfonts[i]->glyphcnt;
      while ((ch = u8_get_next ((const uint8_t **) &str)) != '\0')
        {
          for (i = 0; i < max; ++i)
            {
              for (j = 0; j < sf->subfontcnt; ++j)
                if (i < sf->subfonts[j]->glyphcnt
                    && sf->subfonts[j]->glyphs[i] != NULL)
                  break;
              if (j != sf->subfontcnt)
                if (sf->subfonts[j]->glyphs[i]->unicodeenc == ch)
                  break;
            }
          if (i == max || !SCWorthOutputting (sf->subfonts[j]->glyphs[i]))
            return false;
        }
    }
  return true;
}

static int
ScriptInList (uint32_t script, uint32_t *scripts, int scnt)
{
  int s;

  for (s = 0; s < scnt; ++s)
    if (script == scripts[s])
      return true;

  return false;
}

uint32_t *
PrtBuildDef (SplineFont *sf, void *tf,
             void (*langsyscallback) (void *tf, int end, uint32_t script,
                                      uint32_t lang))
{
  int i, j, gotem, len, any = 0, foundsomething = 0;
  uint32_t *ret = NULL;
  char **cur;
  uint32_t scriptsdone[100], scriptsthere[100], langs[100];
  char *randoms[100];
  char buffer[220], *pt;
  int scnt, s, therecnt, rcnt;

  OrderSampleByLang ();
  therecnt = SF2Scripts (sf, scriptsthere);

  scnt = 0;

  while (1)
    {
      len = any = 0;
      for (i = 0; sample[i].sample != NULL; ++i)
        {
          gotem = true;
          cur = sample[i].sample;
          for (j = 0; cur[j] != NULL && gotem; ++j)
            gotem = AllChars (sf, cur[j]);
          if (!gotem && sample[i].sample == _LiBai)
            {
              cur = _LiBaiShort;
              gotem = true;
              for (j = 0; cur[j] != NULL && gotem; ++j)
                gotem = AllChars (sf, cur[j]);
            }
          if (gotem)
            {
              for (s = 0; s < scnt; ++s)
                if (scriptsdone[s] == sample[i].otf_script)
                  break;
              if (s == scnt)
                scriptsdone[scnt++] = sample[i].otf_script;

              foundsomething = true;
              ++any;
              for (j = 0; cur[j] != NULL; ++j)
                {
                  if (ret)
                    utf82u_strcpy (ret + len, cur[j]);
                  len += u8_mbsnlen (cur[j], u8_strlen (cur[j]));
                  if (ret)
                    ret[len] = '\n';
                  ++len;
                }
              if (ret)
                ret[len] = '\n';
              ++len;
              if (ret && langsyscallback != NULL)
                (langsyscallback) (tf, len, sample[i].otf_script,
                                   sample[i].lang);
            }
        }

      rcnt = 0;
      for (s = 0; s < therecnt; ++s)
        if (!ScriptInList (scriptsthere[s], scriptsdone, scnt))
          {
            if (ret)
              {
                if (randoms[rcnt] != '\0')
                  {
                    utf82u_strcpy (ret + len, randoms[rcnt]);
                    len += u32_strlen (ret + len);
                    ret[len++] = '\n';
                    ret[len] = '\0';
                    if (langsyscallback != NULL)
                      (langsyscallback) (tf, len, scriptsthere[s], langs[rcnt]);
                  }
                free (randoms[rcnt]);
              }
            else
              {
                randoms[rcnt] =
                  RandomParaFromScript (scriptsthere[s], &langs[rcnt], sf);
                for (pt = randoms[rcnt]; *pt == ' '; ++pt);
                if (*pt == '\0')
                  *randoms[rcnt] = '\0';
                else
                  {
                    len +=
                      u8_mbsnlen (randoms[rcnt], u8_strlen (randoms[rcnt])) + 2;
                    foundsomething = true;
                  }
              }
            ++rcnt;
          }

      if (!foundsomething)
        {
          /* For example, Apostolos's Phaistos Disk font. There is no OT script */
          /*  code assigned for those unicode points */
          int gid;
          SplineChar *sc;

          pt = buffer;
          for (gid = i = 0;
               gid < sf->glyphcnt && pt < buffer + sizeof (buffer) - 4
               && i < 50; ++gid)
            {
              if ((sc = sf->glyphs[gid]) != NULL && sc->unicodeenc != -1)
                {
                  pt = utf8_idpb (pt, sc->unicodeenc);
                  ++i;
                }
            }
          *pt = '\0';
          if (i > 0)
            {
              if (ret)
                {
                  utf82u_strcpy (ret + len, buffer);
                  len += u32_strlen (ret + len);
                  ret[len++] = '\n';
                  ret[len] = '\0';
                  if (langsyscallback != NULL)
                    (langsyscallback) (tf, len, DEFAULT_SCRIPT, DEFAULT_LANG);
                }
              else
                len += u8_mbsnlen (buffer, u8_strlen (buffer)) + 1;
            }
        }

      if (ret)
        {
          ret[len] = '\0';
          return ret;
        }
      if (len == 0)
        {
          /* Er... We didn't find anything?! */
          return xcalloc (1, sizeof (uint32_t));
        }
      ret = xmalloc ((len + 1) * sizeof (uint32_t));
    }
}
