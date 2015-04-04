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

#include <stdbool.h>
#include <fontforgevw.h>
#include <unistd.h>
#include <math.h>
#include <time.h>
#include <utype.h>
#include <chardata.h>
#include <ustring.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sd.h>
#include <xalloc.h>
#include <sortsmill/guile.h>
#include <sortsmill/core.h>
#include <c-strtod.h>
#include <c-vasprintf.h>

/* ************************************************************************** */
/* ****************************    SVG Output    **************************** */
/* ************************************************************************** */

static char *
skip_spaces (char *p)
{
  while (*p == ' ')
    p++;
  return p;
}

static int
svg_pathdump (FILE *file, SplineSet *spl, int lineout,
              int forceclosed, int do_clips)
{
  BasePoint last;
  char *buffer;
  int closed = false;
  Spline *sp, *first;
  /* as I see it there is nothing to be gained by optimizing out the */
  /* command characters, since they just have to be replaced by spaces */
  /* so I don't bother to */

  last.x = last.y = 0;
  while (spl != NULL)
    {
      if ((do_clips && spl->is_clip_path) || (!do_clips && !spl->is_clip_path))
        {
          c_asprintf (&buffer, "M%g %g", (double) spl->first->me.x,
                      (double) spl->first->me.y);
          if (lineout + strlen (buffer) >= 255)
            {
              putc ('\n', file);
              lineout = 0;
            }
          fputs (buffer, file);
          lineout += strlen (buffer);
          free (buffer);
          last = spl->first->me;
          closed = false;

          first = NULL;
          for (sp = spl->first->next; sp != NULL && sp != first;
               sp = sp->to->next)
            {
              if (first == NULL)
                first = sp;
              if (sp->knownlinear)
                {
                  if (sp->to->me.x == sp->from->me.x)
                    c_asprintf (&buffer, "v%g",
                                (double) (sp->to->me.y - last.y));
                  else if (sp->to->me.y == sp->from->me.y)
                    c_asprintf (&buffer, "h%g",
                                (double) (sp->to->me.x - last.x));
                  else if (sp->to->next == first)
                    {
                      c_asprintf (&buffer, "z");
                      closed = true;
                    }
                  else
                    c_asprintf (&buffer, "l%g %g",
                                (double) (sp->to->me.x - last.x),
                                (double) (sp->to->me.y - last.y));
                }
              else if (sp->order2)
                {
                  if (sp->from->prev != NULL && sp->from != spl->first &&
                      sp->from->me.x - sp->from->prevcp.x ==
                      sp->from->nextcp.x - sp->from->me.x
                      && sp->from->me.y - sp->from->prevcp.y ==
                      sp->from->nextcp.y - sp->from->me.y)
                    c_asprintf (&buffer, "t%g %g",
                                (double) (sp->to->me.x - last.x),
                                (double) (sp->to->me.y - last.y));
                  else
                    c_asprintf (&buffer, "q%g %g %g %g",
                                (double) (sp->to->prevcp.x - last.x),
                                (double) (sp->to->prevcp.y - last.y),
                                (double) (sp->to->me.x - last.x),
                                (double) (sp->to->me.y - last.y));
                }
              else
                {
                  if (sp->from->prev != NULL && sp->from != spl->first &&
                      sp->from->me.x - sp->from->prevcp.x ==
                      sp->from->nextcp.x - sp->from->me.x
                      && sp->from->me.y - sp->from->prevcp.y ==
                      sp->from->nextcp.y - sp->from->me.y)
                    c_asprintf (&buffer, "s%g %g %g %g",
                                (double) (sp->to->prevcp.x - last.x),
                                (double) (sp->to->prevcp.y - last.y),
                                (double) (sp->to->me.x - last.x),
                                (double) (sp->to->me.y - last.y));
                  else
                    c_asprintf (&buffer, "c%g %g %g %g %g %g",
                                (double) (sp->from->nextcp.x - last.x),
                                (double) (sp->from->nextcp.y - last.y),
                                (double) (sp->to->prevcp.x - last.x),
                                (double) (sp->to->prevcp.y - last.y),
                                (double) (sp->to->me.x - last.x),
                                (double) (sp->to->me.y - last.y));
                }
              if (lineout + strlen (buffer) >= 255)
                {
                  putc ('\n', file);
                  lineout = 0;
                }
              fputs (buffer, file);
              lineout += strlen (buffer);
              free (buffer);
              last = sp->to->me;
            }
          if (!closed && (forceclosed || spl->first->prev != NULL))
            {
              if (lineout >= 254)
                {
                  putc ('\n', file);
                  lineout = 0;
                }
              putc ('z', file);
              ++lineout;
            }
        }
      spl = spl->next;
    }
  return lineout;
}

static void
svg_dumpstroke (FILE *file, struct pen *cpen, struct pen *fallback,
                char *scname, SplineChar *nested, int layer, int istop)
{
  static char *joins[] = { "miter", "round", "bevel", "inherit", NULL };
  static char *caps[] = { "butt", "round", "square", "inherit", NULL };
  struct pen pen;

  pen = *cpen;
  if (fallback != NULL)
    {
      if (pen.brush.col == COLOR_INHERITED)
        pen.brush.col = fallback->brush.col;
      if (pen.brush.opacity < 0)
        pen.brush.opacity = fallback->brush.opacity;
      if (pen.width == WIDTH_INHERITED)
        pen.width = fallback->width;
      if (pen.linecap == lc_inherited)
        pen.linecap = fallback->linecap;
      if (pen.linejoin == lj_inherited)
        pen.linejoin = fallback->linejoin;
      if (pen.dashes[0] == 0 && pen.dashes[1] == DASH_INHERITED)
        memcpy (pen.dashes, fallback->dashes, sizeof (pen.dashes));
    }

  if (pen.brush.gradient != NULL)
    {
      c_fprintf (file, "stroke=\"url(#%s", scname);
      if (nested != NULL)
        c_fprintf (file, "-%s", nested->name);
      c_fprintf (file, "-ly%d-stroke-grad)\" ", layer);
    }
  else if (pen.brush.pattern != NULL && istop)
    {
      c_fprintf (file, "stroke=\"url(#%s", scname);
      if (nested != NULL)
        c_fprintf (file, "-%s", nested->name);
      c_fprintf (file, "-ly%d-stroke-pattern)\" ", layer);
    }
  else
    {
      if (pen.brush.col != COLOR_INHERITED)
        c_fprintf (file, "stroke=\"#%02x%02x%02x\" ",
                   COLOR_RED (pen.brush.col), COLOR_GREEN (pen.brush.col),
                   COLOR_BLUE (pen.brush.col));
      else
        c_fprintf (file, "stroke=\"currentColor\" ");
      if (pen.brush.opacity >= 0)
        c_fprintf (file, "stroke-opacity=\"%g\" ", pen.brush.opacity);
    }
  if (pen.width != WIDTH_INHERITED)
    c_fprintf (file, "stroke-width=\"%g\" ", pen.width);
  if (pen.linecap != lc_inherited)
    c_fprintf (file, "stroke-linecap=\"%s\" ", caps[pen.linecap]);
  if (pen.linejoin != lc_inherited)
    c_fprintf (file, "stroke-linejoin=\"%s\" ", joins[pen.linejoin]);
/* the current transformation matrix will not affect the fill, but it will */
/*  affect the way stroke looks. So we must include it here. BUT the spline */
/*  set has already been transformed, so we must apply the inverse transform */
/*  to the splineset before outputting it, so that applying the transform */
/*  will give us the splines we desire. */
  if (pen.trans[0] != 1.0 || pen.trans[3] != 1.0 || pen.trans[1] != 0
      || pen.trans[2] != 0)
    c_fprintf (file, "transform=\"matrix(%g, %g, %g, %g, 0, 0)\" ",
               (double) pen.trans[0], (double) pen.trans[1],
               (double) pen.trans[2], (double) pen.trans[3]);
  if (pen.dashes[0] == 0 && pen.dashes[1] == DASH_INHERITED)
    {
      c_fprintf (file, "stroke-dasharray=\"inherit\" ");
    }
  else if (pen.dashes[0] != 0)
    {
      int i;
      c_fprintf (file, "stroke-dasharray=\"");
      for (i = 0; i < DASH_MAX && pen.dashes[i] != 0; ++i)
        c_fprintf (file, "%d ", pen.dashes[i]);
      c_fprintf (file, "\" ");
    }
  else
    {
      /* That's the default, don't need to say it */
      /* c_fprintf( file, "stroke-dasharray=\"none\" " ) */
      ;
    }
}

static void
svg_dumpfill (FILE *file, struct brush *cbrush, struct brush *fallback,
              int dofill, char *scname, SplineChar *nested, int layer,
              int istop)
{
  struct brush brush;

  if (!dofill)
    {
      c_fprintf (file, "fill=\"none\" ");
      return;
    }

  brush = *cbrush;
  if (fallback != NULL)
    {
      if (brush.col == COLOR_INHERITED)
        brush.col = fallback->col;
      if (brush.opacity < 0)
        brush.opacity = fallback->opacity;
    }

  if (brush.gradient != NULL)
    {
      c_fprintf (file, "fill=\"url(#%s", scname);
      if (nested != NULL)
        c_fprintf (file, "-%s", nested->name);
      c_fprintf (file, "-ly%d-fill-grad)\" ", layer);
    }
  else if (brush.pattern != NULL && istop)
    {
      c_fprintf (file, "fill=\"url(#%s", scname);
      if (nested != NULL)
        c_fprintf (file, "-%s", nested->name);
      c_fprintf (file, "-ly%d-fill-pattern)\" ", layer);
    }
  else
    {
      if (brush.col != COLOR_INHERITED)
        c_fprintf (file, "fill=\"#%02x%02x%02x\" ",
                   COLOR_RED (brush.col), COLOR_GREEN (brush.col),
                   COLOR_BLUE (brush.col));
      else
        c_fprintf (file, "fill=\"currentColor\" ");
      if (brush.opacity >= 0)
        c_fprintf (file, "fill-opacity=\"%g\" ", brush.opacity);
    }
}

static SplineSet *
TransBy (SplineSet *ss, real trans[4])
{
  real inversetrans[6], transform[6];

  if (trans[0] == 1.0 && trans[3] == 1.0 && trans[1] == 0 && trans[2] == 0)
    return ss;
  memcpy (transform, trans, 4 * sizeof (real));
  transform[4] = transform[5] = 0;
  MatInverse (inversetrans, transform);
  return (SplinePointListTransform
          (SplinePointListCopy (ss), inversetrans, tpt_AllPoints));
}

static int
svg_sc_any (SplineChar *sc, int layer)
{
  int i, j;
  int any;
  RefChar *ref;
  int first, last;

  first = last = layer;
  if (sc->parent->multilayer)
    last = sc->layer_cnt - 1;
  any = false;
  for (i = first; i <= last && !any; ++i)
    {
      any = sc->layers[i].splines != NULL || sc->layers[i].images != NULL;
      for (ref = sc->layers[i].refs; ref != NULL && !any; ref = ref->next)
        for (j = 0; j < ref->layer_cnt && !any; ++j)
          any = ref->layers[j].splines != NULL;
    }
  return any;
}

static int base64tab[] = {
  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
  'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
  'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/'
};

static void
DataURI_ImageDump (FILE *file, struct gimage *img)
{
  char *mimetype = NULL;
  FILE *imgf;
  int done = false;
  int threechars[3], fourchars[4], i, ch, ch_on_line;
#if !defined( _NO_LIBJPEG)
  struct _GImage *base = img->list_len == 0 ? img->u.image : img->u.images[0];
#endif

  /* Technically we can only put a file into an URI if the whole thing is */
  /*  less than 1024 bytes long. But I shall ignore that issue */
  imgf = tmpfile ();
#if !defined(_NO_LIBJPEG)
  if (base->image_type == it_true)
    {
      done = GImageWrite_Jpeg (img, imgf, 78, false);
      mimetype = "image/jpeg";
    }
#endif
  if (!done)
    {
      done = GImageWrite_Png (img, imgf, false);
      mimetype = "image/png";
    }
  if (!done)
    {
      GImageWrite_Bmp (img, imgf);
      mimetype = "image/bmp";
    }

  c_fprintf (file, "%s;base64,", mimetype);
  rewind (imgf);

  /* Now do base64 output conversion */

  rewind (imgf);
  ch = getc (imgf);
  ch_on_line = 0;
  while (ch != EOF)
    {
      threechars[0] = threechars[1] = threechars[2] = 0;
      for (i = 0; i < 3 && ch != EOF; ++i)
        {
          threechars[i] = ch;
          ch = getc (imgf);
        }
      if (i > 0)
        {
          fourchars[0] = base64tab[threechars[0] >> 2];
          fourchars[1] =
            base64tab[((threechars[0] & 0x3) << 4) | (threechars[1] >> 4)];
          fourchars[2] =
            base64tab[((threechars[1] & 0xf) << 2) | (threechars[2] >> 6)];
          fourchars[3] = base64tab[threechars[2] & 0x3f];
          if (i < 3)
            fourchars[3] = '=';
          if (i < 2)
            fourchars[2] = '=';
          putc (fourchars[0], file);
          putc (fourchars[1], file);
          putc (fourchars[2], file);
          putc (fourchars[3], file);
          ch_on_line += 4;
          if (ch_on_line >= 72)
            {
              putc ('\n', file);
              ch_on_line = 0;
            }
        }
    }
  fclose (imgf);
}

static void
svg_dumpgradient (FILE *file, struct gradient *gradient,
                  char *scname, SplineChar *nested, int layer, int is_fill)
{
  int i;
  Color csame;
  float osame;

  c_fprintf (file, "    <%s ",
             gradient->radius == 0 ? "linearGradient" : "radialGradient");
  if (nested == NULL)
    c_fprintf (file, " id=\"%s-ly%d-%s-grad\"", scname, layer,
               is_fill ? "fill" : "stroke");
  else
    c_fprintf (file, " id=\"%s-%s-ly%d-%s-grad\"", scname, nested->name, layer,
               is_fill ? "fill" : "stroke");
  c_fprintf (file, "\n\tgradientUnits=\"userSpaceOnUse\"");
  if (gradient->radius == 0)
    {
      c_fprintf (file, "\n\tx1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\"",
                 (double) gradient->start.x, (double) gradient->start.y,
                 (double) gradient->stop.x, (double) gradient->stop.y);
    }
  else
    {
      if (gradient->start.x == gradient->stop.x
          && gradient->start.y == gradient->stop.y)
        c_fprintf (file, "\n\tcx=\"%g\" cy=\"%g\" r=\"%g\"",
                   (double) gradient->stop.x, (double) gradient->stop.y,
                   (double) gradient->radius);
      else
        c_fprintf (file, "\n\tfx=\"%g\" fy=\"%g\" cx=\"%g\" cy=\"%g\" r=\"%g\"",
                   (double) gradient->start.x, (double) gradient->start.y,
                   (double) gradient->stop.x, (double) gradient->stop.y,
                   (double) gradient->radius);
    }
  c_fprintf (file, "\n\tspreadMethod=\"%s\">\n",
             gradient->sm == sm_pad ? "pad" :
             gradient->sm == sm_reflect ? "reflect" : "repeat");

  csame = -1;
  osame = -1;
  for (i = 0; i < gradient->stop_cnt; ++i)
    {
      if (csame == -1)
        csame = gradient->grad_stops[i].col;
      else if (csame != gradient->grad_stops[i].col)
        csame = -2;
      if (osame == -1)
        osame = gradient->grad_stops[i].opacity;
      else if (osame != gradient->grad_stops[i].opacity)
        osame = -2;
    }
  for (i = 0; i < gradient->stop_cnt; ++i)
    {
      c_fprintf (file, "      <stop offset=\"%g\"",
                 (double) gradient->grad_stops[i].offset);
      if (csame == -2)
        {
          if (gradient->grad_stops[i].col == COLOR_INHERITED)
            c_fprintf (file, " stop-color=\"inherit\"");
          else
            c_fprintf (file, " stop-color=\"#%06x\"",
                       gradient->grad_stops[i].col);
        }
      if (osame < 0)
        {
          if (gradient->grad_stops[i].opacity == COLOR_INHERITED)
            c_fprintf (file, " stop-opacity=\"inherit\"");
          else
            c_fprintf (file, " stop-opacity=\"%g\"",
                       (double) gradient->grad_stops[i].opacity);
        }
      c_fprintf (file, "/>\n");
    }
  c_fprintf (file, "    </%s>\n",
             gradient->radius == 0 ? "linearGradient" : "radialGradient");
}

static void svg_dumpscdefs (FILE *file, SplineChar *sc, char *name, int istop);
static void svg_dumptype3 (FILE *file, SplineChar *sc, char *name, int istop);

static void
svg_dumppattern (FILE *file, struct pattern *pattern,
                 char *scname, SplineChar *base, SplineChar *nested, int layer,
                 int is_fill)
{
  SplineChar *pattern_sc = SFGetChar (base->parent, -1, pattern->pattern);
  char *patsubname = NULL;

  if (pattern_sc != NULL)
    {
      patsubname = strconcat3 (scname, "-", pattern->pattern);
      svg_dumpscdefs (file, pattern_sc, patsubname, false);
    }
  else
    LogError (_("No glyph named %s, used as a pattern in %s\n"),
              pattern->pattern, scname);

  c_fprintf (file, "    <pattern ");
  if (nested == NULL)
    c_fprintf (file, " id=\"%s-ly%d-%s-pattern\"", scname, layer,
               is_fill ? "fill" : "stroke");
  else
    c_fprintf (file, " id=\"%s-%s-ly%d-%s-pattern\"", scname, nested->name,
               layer, is_fill ? "fill" : "stroke");
  c_fprintf (file, "\n\tpatternUnits=\"userSpaceOnUse\"");
  if (pattern_sc != NULL)
    {
      DBounds b;
      PatternSCBounds (pattern_sc, &b);
      c_fprintf (file, "\n\tviewBox=\"%g %g %g %g\"",
                 (double) b.minx, (double) b.miny,
                 (double) (b.maxx - b.minx), (double) (b.maxy - b.miny));
    }
  c_fprintf (file, "\n\twidth=\"%g\" height=\"%g\"",
             (double) pattern->width, (double) pattern->height);
  if (pattern->transform[0] != 1 || pattern->transform[1] != 0 ||
      pattern->transform[2] != 0 || pattern->transform[3] != 1 ||
      pattern->transform[4] != 0 || pattern->transform[5] != 0)
    {
      c_fprintf (file, "\n\tpatternTransform=\"matrix(%g %g %g %g %g %g)\"",
                 (double) pattern->transform[0], (double) pattern->transform[1],
                 (double) pattern->transform[2], (double) pattern->transform[3],
                 (double) pattern->transform[4],
                 (double) pattern->transform[5]);
    }
  if (pattern_sc != NULL)
    svg_dumpscdefs (file, pattern_sc, patsubname, false);
  c_fprintf (file, "    </pattern>\n");
  free (patsubname);
}

static void
svg_layer_defs (FILE *file, SplineSet *splines, struct brush *fill_brush,
                struct pen *stroke_pen, SplineChar *sc, char *scname,
                SplineChar *nested, int layer, int istop)
{
  if (SSHasClip (splines))
    {
      if (nested == NULL)
        c_fprintf (file, "    <clipPath id=\"%s-ly%d-clip\">\n", scname, layer);
      else
        c_fprintf (file, "    <clipPath id=\"%s-%s-ly%d-clip\">\n", scname,
                   nested->name, layer);
      c_fprintf (file, "      <path d=\"\n");
      svg_pathdump (file, sc->layers[layer].splines, 16, true, true);
      c_fprintf (file, "\"/>\n");
      c_fprintf (file, "    </clipPath>\n");
    }
  if (fill_brush->gradient != NULL)
    svg_dumpgradient (file, fill_brush->gradient, scname, nested, layer, true);
  else if (fill_brush->pattern != NULL && istop)
    svg_dumppattern (file, fill_brush->pattern, scname, sc, nested, layer,
                     true);
  if (stroke_pen->brush.gradient != NULL)
    svg_dumpgradient (file, stroke_pen->brush.gradient, scname, nested, layer,
                      false);
  else if (stroke_pen->brush.pattern != NULL && istop)
    svg_dumppattern (file, stroke_pen->brush.pattern, scname, sc, nested, layer,
                     false);
}

static void
svg_dumpscdefs (FILE *file, SplineChar *sc, char *name, int istop)
{
  int i, j;
  RefChar *ref;

  for (i = ly_fore; i < sc->layer_cnt; ++i)
    {
      svg_layer_defs (file, sc->layers[i].splines, &sc->layers[i].fill_brush,
                      &sc->layers[i].stroke_pen, sc, name, NULL, i, istop);
      for (ref = sc->layers[i].refs; ref != NULL; ref = ref->next)
        {
          for (j = 0; j < ref->layer_cnt; ++j)
            if (ref->layers[j].splines != NULL)
              {
                svg_layer_defs (file, ref->layers[j].splines,
                                &ref->layers[j].fill_brush,
                                &ref->layers[j].stroke_pen, sc, name, ref->sc,
                                j, istop);
              }
        }
    }
}

static void
svg_dumptype3 (FILE *file, SplineChar *sc, char *name, int istop)
{
  int i, j;
  RefChar *ref;
  ImageList *images;
  SplineSet *transed;

  for (i = ly_fore; i < sc->layer_cnt; ++i)
    {
      if (SSHasDrawn (sc->layers[i].splines))
        {
          c_fprintf (file, "  <g ");
          if (SSHasClip (sc->layers[i].splines))
            c_fprintf (file, "clip-path=\"url(#%s-ly%d-clip)\" ", name, i);
          transed = sc->layers[i].splines;
          if (sc->layers[i].dostroke)
            {
              svg_dumpstroke (file, &sc->layers[i].stroke_pen, NULL, name, NULL,
                              i, istop);
              transed = TransBy (transed, sc->layers[i].stroke_pen.trans);
            }
          svg_dumpfill (file, &sc->layers[i].fill_brush, NULL,
                        sc->layers[i].dofill, name, NULL, i, istop);
          c_fprintf (file, ">\n");
          c_fprintf (file, "    <path d=\"\n");
          svg_pathdump (file, transed, 12, !sc->layers[i].dostroke, false);
          c_fprintf (file, "\"/>\n");
          if (transed != sc->layers[i].splines)
            SplinePointListsFree (transed);
          c_fprintf (file, "  </g>\n");
        }
      for (ref = sc->layers[i].refs; ref != NULL; ref = ref->next)
        {
          for (j = 0; j < ref->layer_cnt; ++j)
            if (ref->layers[j].splines != NULL)
              {
                c_fprintf (file, "   <g ");
                transed = ref->layers[j].splines;
                if (SSHasClip (transed))
                  c_fprintf (file, "clip-path=\"url(#%s-%s-ly%d-clip)\" ", name,
                             ref->sc->name, j);
                if (ref->layers[j].dostroke)
                  {
                    svg_dumpstroke (file, &ref->layers[j].stroke_pen,
                                    &sc->layers[i].stroke_pen, sc->name,
                                    ref->sc, j, istop);
                    transed =
                      TransBy (transed, ref->layers[j].stroke_pen.trans);
                  }
                svg_dumpfill (file, &ref->layers[j].fill_brush,
                              &sc->layers[i].fill_brush, ref->layers[j].dofill,
                              sc->name, ref->sc, j, istop);
                c_fprintf (file, ">\n");
                c_fprintf (file, "  <path d=\"\n");
                svg_pathdump (file, transed, 12, !ref->layers[j].dostroke,
                              false);
                c_fprintf (file, "\"/>\n");
                if (transed != ref->layers[j].splines)
                  SplinePointListsFree (transed);
                c_fprintf (file, "   </g>\n");
              }
        }
      for (images = sc->layers[i].images; images != NULL; images = images->next)
        {
          struct _GImage *base;
          c_fprintf (file, "      <image\n");
          base = images->image->list_len == 0 ? images->image->u.image :
            images->image->u.images[0];
          c_fprintf (file, "\twidth=\"%g\"\n\theight=\"%g\"\n",
                     (double) (base->width * images->xscale),
                     (double) (base->height * images->yscale));
          c_fprintf (file, "\tx=\"%g\"\n\ty=\"%g\"\n", (double) images->xoff,
                     (double) images->yoff);
          c_fprintf (file, "\txlink:href=\"data:");
          DataURI_ImageDump (file, images->image);
          c_fprintf (file, "\" />\n");
        }
    }
}

static void
svg_scpathdump (FILE *file, SplineChar *sc, char *endpath, int layer)
{
  RefChar *ref;
  int lineout;
  int i, j;
  int needs_defs = 0;

  if (!svg_sc_any (sc, layer))
    {
      /* I think a space is represented by leaving out the d (path) entirely */
      /*  rather than having d="" */
      fputs (" />\n", file);
    }
  else if (sc->parent->strokedfont)
    {
      /* Can't be done with a path, requires nested elements (I think) */
      c_fprintf (file,
                 ">\n  <g stroke=\"currentColor\" stroke-width=\"%g\" fill=\"none\">\n",
                 (double) sc->parent->strokewidth);
      c_fprintf (file, "    <path d=\"");
      lineout = svg_pathdump (file, sc->layers[layer].splines, 3, false, false);
      for (ref = sc->layers[layer].refs; ref != NULL; ref = ref->next)
        lineout =
          svg_pathdump (file, ref->layers[0].splines, lineout, false, false);
      if (lineout >= 255 - 4)
        putc ('\n', file);
      putc ('"', file);
      fputs (" />\n  </g>\n", file);
      fputs (endpath, file);
    }
  else if (!sc->parent->multilayer)
    {
      c_fprintf (file, "d=\"");
      lineout = svg_pathdump (file, sc->layers[layer].splines, 3, true, false);
      for (ref = sc->layers[layer].refs; ref != NULL; ref = ref->next)
        lineout =
          svg_pathdump (file, ref->layers[0].splines, lineout, true, false);
      if (lineout >= 255 - 4)
        putc ('\n', file);
      putc ('"', file);
      fputs (" />\n", file);
    }
  else
    {
      fputs (">\n", file);
      for (i = ly_fore; i < sc->layer_cnt && !needs_defs; ++i)
        {
          if (SSHasClip (sc->layers[i].splines))
            needs_defs = true;
          else if (sc->layers[i].fill_brush.pattern != NULL ||
                   sc->layers[i].fill_brush.gradient != NULL ||
                   sc->layers[i].stroke_pen.brush.pattern != NULL ||
                   sc->layers[i].stroke_pen.brush.gradient != NULL)
            needs_defs = true;
          for (ref = sc->layers[i].refs; ref != NULL; ref = ref->next)
            {
              for (j = 0; j < ref->layer_cnt; ++j)
                if (ref->layers[j].splines != NULL)
                  {
                    if (SSHasClip (ref->layers[j].splines))
                      needs_defs = true;
                    else if (ref->layers[j].fill_brush.pattern != NULL ||
                             ref->layers[j].fill_brush.gradient != NULL ||
                             ref->layers[j].stroke_pen.brush.pattern != NULL ||
                             ref->layers[j].stroke_pen.brush.gradient != NULL)
                      needs_defs = true;
                  }
            }
        }
      if (needs_defs)
        {
          c_fprintf (file, "  <defs>\n");
          svg_dumpscdefs (file, sc, sc->name, true);
          c_fprintf (file, "  </defs>\n");
        }
      svg_dumptype3 (file, sc, sc->name, true);
      fputs (endpath, file);
    }
}

int
_ExportSVG (FILE *svg, SplineChar *sc, int layer)
{
  char *end;
  int em_size;
  DBounds b;

  SplineCharLayerFindBounds (sc, layer, &b);
  em_size = sc->parent->ascent + sc->parent->descent;
  if (b.minx > 0)
    b.minx = 0;
  if (b.maxx < em_size)
    b.maxx = em_size;
  if (b.miny > -sc->parent->descent)
    b.miny = -sc->parent->descent;
  if (b.maxy < em_size)
    b.maxy = em_size;

  c_fprintf (svg, "<?xml version=\"1.0\" standalone=\"no\"?>\n");
  c_fprintf (svg,
             "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\" >\n");
  c_fprintf (svg, "<svg viewBox=\"%d %d %d %d\">\n", (int) floor (b.minx),
             (int) floor (b.miny), (int) ceil (b.maxx), (int) ceil (b.maxy));
  c_fprintf (svg, "  <g transform=\"matrix(1 0 0 -1 0 %d)\">\n",
             sc->parent->ascent);
#if 0                           /* Used to show the advance width, but as I don't in eps, probably should be consistent */
  c_fprintf (svg, "   <g stroke=\"green\" stroke-width=\"1\">\n");
  c_fprintf (svg, "     <line x1=\"0\" y1=\"0\" x2=\"%d\" y2=\"0\" />\n",
             sc->width);
  c_fprintf (svg, "     <line x1=\"0\" y1=\"10\" x2=\"0\" y2=\"-10\" />\n");
  c_fprintf (svg, "     <line x1=\"%d\" y1=\"10\" x2=\"%d\" y2=\"-10\" />\n",
             sc->width, sc->width);
  c_fprintf (svg, "   </g>\n\n");
#endif
  if (sc->parent->multilayer || sc->parent->strokedfont
      || !svg_sc_any (sc, layer))
    {
      c_fprintf (svg, "   <g ");
      end = "   </g>\n";
    }
  else
    {
      c_fprintf (svg, "   <path fill=\"currentColor\"\n");
      end = "   </path>\n";
    }
  svg_scpathdump (svg, sc, end, layer);
  c_fprintf (svg, "  </g>\n\n");
  c_fprintf (svg, "</svg>\n");

  return !ferror (svg);
}

/* ************************************************************************** */
/* *****************************    SVG Input    **************************** */
/* ************************************************************************** */

#include <libxml/parser.h>

/* Find a node with the given id */
static xmlNodePtr
XmlFindID (xmlNodePtr xml, char *name)
{
  xmlChar *id;
  xmlNodePtr child, ret;

  id = xmlGetProp (xml, "id");
  if (id != NULL && xmlStrcmp (id, name) == 0)
    {
      xmlFree (id);
      return xml;
    }
  if (id != NULL)
    xmlFree (id);

  for (child = xml->children; child != NULL; child = child->next)
    {
      ret = XmlFindID (child, name);
      if (ret != NULL)
        return ret;
    }
  return NULL;
}

static xmlNodePtr
XmlFindURI (xmlNodePtr xml, char *name)
{
  xmlNodePtr ret;
  char *pt, ch;

  if (strncmp (name, "url(#", 5) != 0)
    return NULL;
  name += 5;
  for (pt = name; *pt != ')' && *pt != '\0'; ++pt);
  ch = *pt;
  *pt = '\0';
  ret = XmlFindID (xml, name);
  *pt = ch;
  return ret;
}

// I don't see where the spec says that the separator between numbers
// is comma or whitespace (both is ok, too). But the style sheet spec
// says it, so I probably just missed it.
static char *
skipcomma (char *pt)
{
  while (isspace (*pt))
    ++pt;
  if (*pt == ',')
    ++pt;
  return pt;
}

static void
SVGTraceArc (SplineSet *cur, BasePoint *current,
             double x, double y, double rx, double ry, double axisrot,
             int large_arc, int sweep)
{
  double cosr, sinr;
  double x1p, y1p;
  double lambda, factor;
  double cxp, cyp, cx, cy;
  double tmpx, tmpy, t2x, t2y;
  double startangle, delta, a;
  SplinePoint *final, *sp;
  BasePoint arcp[4], prevcp[4], nextcp[4], firstcp[2];
  int i, j, ia, firstia;
  static double sines[] = { 0, 1, 0, -1, 0, 1, 0, -1, 0, 1, 0, -1 };
  static double cosines[] = { 1, 0, -1, 0, 1, 0, -1, 0, 1, 0, -1, 0 };

  final = SplinePointCreate (x, y);
  if (rx < 0)
    rx = -rx;
  if (ry < 0)
    ry = -ry;
  if (rx != 0 && ry != 0)
    {
      // Page 647 in the SVG 1.1 spec describes how to do this. This
      // is Appendix F (Implementation notes), section 6.5.
      cosr = cos (axisrot);
      sinr = sin (axisrot);
      x1p = cosr * (current->x - x) / 2 + sinr * (current->y - y) / 2;
      y1p = -sinr * (current->x - x) / 2 + cosr * (current->y - y) / 2;
      // Correct for bad radii.
      lambda = x1p * x1p / (rx * rx) + y1p * y1p / (ry * ry);
      if (lambda > 1)
        {
          lambda = sqrt (lambda);
          rx *= lambda;
          ry *= lambda;
        }
      factor = rx * rx * ry * ry - rx * rx * y1p * y1p - ry * ry * x1p * x1p;
      if (RealNear (factor, 0))
        factor = 0;             // Avoid rounding errors that lead to
      // small negative values.
      else
        factor = sqrt (factor / (rx * rx * y1p * y1p + ry * ry * x1p * x1p));
      if (large_arc == sweep)
        factor = -factor;
      cxp = factor * (rx * y1p) / ry;
      cyp = -factor * (ry * x1p) / rx;
      cx = cosr * cxp - sinr * cyp + (current->x + x) / 2;
      cy = sinr * cxp + cosr * cyp + (current->y + y) / 2;

      tmpx = (x1p - cxp) / rx;
      tmpy = (y1p - cyp) / ry;
      startangle = acos (tmpx / sqrt (tmpx * tmpx + tmpy * tmpy));
      if (tmpy < 0)
        startangle = -startangle;
      t2x = (-x1p - cxp) / rx;
      t2y = (-y1p - cyp) / ry;
      delta = (tmpx * t2x + tmpy * t2y) /
        sqrt ((tmpx * tmpx + tmpy * tmpy) * (t2x * t2x + t2y * t2y));
      // We occasionally got rounding errors near -1.
      if (delta <= -1)
        delta = 3.1415926535897932;
      else if (delta >= 1)
        delta = 0;
      else
        delta = acos (delta);
      if (tmpx * t2y - tmpy * t2x < 0)
        delta = -delta;
      if (sweep == 0 && delta > 0)
        delta -= 2 * M_PI;
      if (sweep && delta < 0)
        delta += 2 * M_PI;

      if (delta > 0)
        {
          i = 0;
          ia = firstia = floor (startangle / M_PI_2) + 1;
          for (a = ia * M_PI_2, ia += 4;
               a < startangle + delta && !RealNear (a, startangle + delta);
               a += M_PI_2, ++i, ++ia)
            {
              t2x = rx * cosines[ia];
              t2y = ry * sines[ia];
              arcp[i].x = cosr * t2x - sinr * t2y + cx;
              arcp[i].y = sinr * t2x + cosr * t2y + cy;
              if (t2x == 0)
                {
                  t2x = rx * cosines[ia + 1];
                  t2y = 0;
                }
              else
                {
                  t2x = 0;
                  t2y = ry * sines[ia + 1];
                }
              prevcp[i].x = arcp[i].x - .552 * (cosr * t2x - sinr * t2y);
              prevcp[i].y = arcp[i].y - .552 * (sinr * t2x + cosr * t2y);
              nextcp[i].x = arcp[i].x + .552 * (cosr * t2x - sinr * t2y);
              nextcp[i].y = arcp[i].y + .552 * (sinr * t2x + cosr * t2y);
            }
        }
      else
        {
          i = 0;
          ia = firstia = ceil (startangle / M_PI_2) - 1;
          for (a = ia * M_PI_2, ia += 8;
               a > startangle + delta && !RealNear (a, startangle + delta);
               a -= M_PI_2, ++i, --ia)
            {
              t2x = rx * cosines[ia];
              t2y = ry * sines[ia];
              arcp[i].x = cosr * t2x - sinr * t2y + cx;
              arcp[i].y = sinr * t2x + cosr * t2y + cy;
              if (t2x == 0)
                {
                  t2x = rx * cosines[ia + 1];
                  t2y = 0;
                }
              else
                {
                  t2x = 0;
                  t2y = ry * sines[ia + 1];
                }
              prevcp[i].x = arcp[i].x + .552 * (cosr * t2x - sinr * t2y);
              prevcp[i].y = arcp[i].y + .552 * (sinr * t2x + cosr * t2y);
              nextcp[i].x = arcp[i].x - .552 * (cosr * t2x - sinr * t2y);
              nextcp[i].y = arcp[i].y - .552 * (sinr * t2x + cosr * t2y);
            }
        }
      if (i != 0)
        {
          double firsta = firstia * M_PI_2;
          double d = (firsta - startangle) / 2;
          double th = startangle + d;
          double hypot = 1 / cos (d);
          BasePoint temp;
          t2x = rx * cos (th) * hypot;
          t2y = ry * sin (th) * hypot;
          temp.x = cosr * t2x - sinr * t2y + cx;
          temp.y = sinr * t2x + cosr * t2y + cy;
          firstcp[0].x = cur->last->me.x + .552 * (temp.x - cur->last->me.x);
          firstcp[0].y = cur->last->me.y + .552 * (temp.y - cur->last->me.y);
          firstcp[1].x = arcp[0].x + .552 * (temp.x - arcp[0].x);
          firstcp[1].y = arcp[0].y + .552 * (temp.y - arcp[0].y);
        }
      for (j = 0; j < i; ++j)
        {
          sp = SplinePointCreate (arcp[j].x, arcp[j].y);
          if (j != 0)
            {
              sp->prevcp = prevcp[j];
              cur->last->nextcp = nextcp[j - 1];
            }
          else
            {
              sp->prevcp = firstcp[1];
              cur->last->nextcp = firstcp[0];
            }
          sp->noprevcp = cur->last->nonextcp = false;
          SplineMake (cur->last, sp, false);
          cur->last = sp;
        }
      {
        double hypot, c, s;
        BasePoint temp;
        if (i == 0)
          {
            double th = startangle + delta / 2;
            hypot = 1.0 / cos (delta / 2);
            c = cos (th);
            s = sin (th);
          }
        else
          {
            double lasta = delta < 0 ? a + M_PI_2 : a - M_PI_2;
            double d = (startangle + delta - lasta);
            double th = lasta + d / 2;
            hypot = 1.0 / cos (d / 2);
            c = cos (th);
            s = sin (th);
          }
        t2x = rx * c * hypot;
        t2y = ry * s * hypot;
        temp.x = cosr * t2x - sinr * t2y + cx;
        temp.y = sinr * t2x + cosr * t2y + cy;
        cur->last->nextcp.x =
          cur->last->me.x + .552 * (temp.x - cur->last->me.x);
        cur->last->nextcp.y =
          cur->last->me.y + .552 * (temp.y - cur->last->me.y);
        final->prevcp.x = final->me.x + .552 * (temp.x - final->me.x);
        final->prevcp.y = final->me.y + .552 * (temp.y - final->me.y);
        cur->last->nonextcp = final->noprevcp = false;
      }
    }
  *current = final->me;
  SplineMake (cur->last, final, false);
  cur->last = final;
}

static SplineSet *
SVGParsePath (xmlChar *path)
{
  BasePoint current;
  SplineSet *head = NULL;
  SplineSet *last = NULL;
  SplineSet *cur = NULL;
  SplinePoint *sp;
  int type = 'M';
  double x1;
  double x2;
  double x;
  double y1;
  double y2;
  double y;
  double rx;
  double ry;
  double axisrot;
  int large_arc;
  int sweep;
  int order2 = 0;
  char *end;

  current.x = 0;
  current.y = 0;

  while (*path)
    {
      path = skip_spaces (path);
      while (isalpha (*path))
        type = *path++;
      if (*path == '\0' && type != 'z' && type != 'Z')
        break;
      if (type == 'm' || type == 'M')
        {
          if (cur != NULL && cur->last != cur->first)
            {
              if (RealNear (cur->last->me.x, cur->first->me.x) &&
                  RealNear (cur->last->me.y, cur->first->me.y))
                {
                  cur->first->prevcp = cur->last->prevcp;
                  cur->first->noprevcp = cur->last->noprevcp;
                  cur->first->prev = cur->last->prev;
                  cur->first->prev->to = cur->first;
                  SplinePointFree (cur->last);
                }
              else
                SplineMake (cur->last, cur->first, order2);
              cur->last = cur->first;
            }
          x = c_strtod ((char *) path, &end);
          end = skipcomma (end);
          y = c_strtod (end, &end);
          if (type == 'm')
            {
              x += current.x;
              y += current.y;
            }
          sp = SplinePointCreate (x, y);
          current = sp->me;
          cur = xzalloc (sizeof (SplineSet));
          if (head == NULL)
            head = cur;
          else
            last->next = cur;
          last = cur;
          cur->first = cur->last = sp;
          /* If you omit a command after a moveto then it defaults to lineto */
          if (type == 'm')
            type = 'l';
          else
            type = 'L';
        }
      else if (type == 'z' || type == 'Z')
        {
          if (cur != NULL && cur->last != cur->first)
            {
              if (RealNear (cur->last->me.x, cur->first->me.x) &&
                  RealNear (cur->last->me.y, cur->first->me.y))
                {
                  cur->first->prevcp = cur->last->prevcp;
                  cur->first->noprevcp = cur->last->noprevcp;
                  cur->first->prev = cur->last->prev;
                  cur->first->prev->to = cur->first;
                  SplinePointFree (cur->last);
                }
              else
                SplineMake (cur->last, cur->first, order2);
              cur->last = cur->first;
              current = cur->first->me;
            }
          type = ' ';
          end = (char *) path;
        }
      else
        {
          if (cur == NULL)
            {
              sp = SplinePointCreate (current.x, current.y);
              cur = xzalloc (sizeof (SplineSet));
              if (head == NULL)
                head = cur;
              else
                last->next = cur;
              last = cur;
              cur->first = cur->last = sp;
            }
          switch (type)
            {
            case 'l':
            case 'L':
              x = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 'l')
                {
                  x += current.x;
                  y += current.y;
                }
              sp = SplinePointCreate (x, y);
              current = sp->me;
              SplineMake (cur->last, sp, order2);
              cur->last = sp;
              break;
            case 'h':
            case 'H':
              x = c_strtod ((char *) path, &end);
              y = current.y;
              if (type == 'h')
                x += current.x;
              sp = SplinePointCreate (x, y);
              current = sp->me;
              SplineMake (cur->last, sp, order2);
              cur->last = sp;
              break;
            case 'v':
            case 'V':
              x = current.x;
              y = c_strtod ((char *) path, &end);
              if (type == 'v')
                y += current.y;
              sp = SplinePointCreate (x, y);
              current = sp->me;
              SplineMake (cur->last, sp, order2);
              cur->last = sp;
              break;
            case 'c':
            case 'C':
              x1 = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              y1 = c_strtod (end, &end);
              end = skipcomma (end);
              x2 = c_strtod (end, &end);
              end = skipcomma (end);
              y2 = c_strtod (end, &end);
              end = skipcomma (end);
              x = c_strtod (end, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 'c')
                {
                  x1 += current.x;
                  y1 += current.y;
                  x2 += current.x;
                  y2 += current.y;
                  x += current.x;
                  y += current.y;
                }
              sp = SplinePointCreate (x, y);
              sp->prevcp.x = x2;
              sp->prevcp.y = y2;
              sp->noprevcp = false;
              cur->last->nextcp.x = x1;
              cur->last->nextcp.y = y1;
              cur->last->nonextcp = false;
              current = sp->me;
              SplineMake (cur->last, sp, false);
              cur->last = sp;
              break;
            case 's':
            case 'S':
              x1 = 2 * cur->last->me.x - cur->last->prevcp.x;
              y1 = 2 * cur->last->me.y - cur->last->prevcp.y;
              x2 = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              y2 = c_strtod (end, &end);
              end = skipcomma (end);
              x = c_strtod (end, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 's')
                {
                  x2 += current.x;
                  y2 += current.y;
                  x += current.x;
                  y += current.y;
                }
              sp = SplinePointCreate (x, y);
              sp->prevcp.x = x2;
              sp->prevcp.y = y2;
              sp->noprevcp = false;
              cur->last->nextcp.x = x1;
              cur->last->nextcp.y = y1;
              cur->last->nonextcp = false;
              current = sp->me;
              SplineMake (cur->last, sp, false);
              cur->last = sp;
              break;
            case 'Q':
            case 'q':
              x1 = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              y1 = c_strtod (end, &end);
              end = skipcomma (end);
              x = c_strtod (end, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 'q')
                {
                  x1 += current.x;
                  y1 += current.y;
                  x += current.x;
                  y += current.y;
                }
              sp = SplinePointCreate (x, y);
              sp->prevcp.x = x1;
              sp->prevcp.y = y1;
              sp->noprevcp = false;
              cur->last->nextcp.x = x1;
              cur->last->nextcp.y = y1;
              cur->last->nonextcp = false;
              current = sp->me;
              SplineMake (cur->last, sp, true);
              cur->last = sp;
              order2 = true;
              break;
            case 'T':
            case 't':
              x = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 't')
                {
                  x += current.x;
                  y += current.y;
                }
              x1 = 2 * cur->last->me.x - cur->last->prevcp.x;
              y1 = 2 * cur->last->me.y - cur->last->prevcp.y;
              sp = SplinePointCreate (x, y);
              sp->prevcp.x = x1;
              sp->prevcp.y = y1;
              sp->noprevcp = false;
              cur->last->nextcp.x = x1;
              cur->last->nextcp.y = y1;
              cur->last->nonextcp = false;
              current = sp->me;
              SplineMake (cur->last, sp, true);
              cur->last = sp;
              order2 = true;
              break;
            case 'A':
            case 'a':
              rx = c_strtod ((char *) path, &end);
              end = skipcomma (end);
              ry = c_strtod (end, &end);
              end = skipcomma (end);
              axisrot = c_strtod (end, &end) * 3.1415926535897932 / 180;
              end = skipcomma (end);
              large_arc = strtol (end, &end, 10);
              end = skipcomma (end);
              sweep = strtol (end, &end, 10);
              end = skipcomma (end);
              x = c_strtod (end, &end);
              end = skipcomma (end);
              y = c_strtod (end, &end);
              if (type == 'a')
                {
                  x += current.x;
                  y += current.y;
                }
              if (x != current.x || y != current.y)
                SVGTraceArc (cur, &current, x, y, rx, ry, axisrot, large_arc,
                             sweep);
              break;
            default:
              LogError (_("Unknown type '%c' found in path specification\n"),
                        type);
              break;
            }
        }
      path = skipcomma (end);
    }
  return head;
}

#if 0
static SplineSet *
SVGAddSpiros (xmlChar *path, SplineSet *base)
{
  BasePoint current;
  SplineSet *cur = NULL;
  int type = 'M';
  char *end;
  spiro_cp cp;

  current.x = current.y = 0;

  while (*path)
    {
      path = skip_spaces (path);
      while (isalpha (*path))
        type = *path++;
      if (*path == '\0' && type != 'z' && type != 'Z')
        break;
      if (type == 'm' || type == 'M')
        {
          if (cur == NULL)
            cur = base;
          else
            cur = cur->next;
          if (cur == NULL)
            break;
          cur->spiros = xmalloc ((cur->spiro_max = 10) * sizeof (spiro_cp));
          cp.x = c_strtod ((char *) path, &end);
          end = skipcomma (end);
          cp.y = c_strtod (end, &end);
          if (type == 'm')
            {
              cp.x += current.x;
              cp.y += current.y;
            }
          cp.ty = SPIRO_OPEN_CONTOUR;
          cur->spiros[0] = cp;
          ++(cur->spiro_cnt);
          current.x = cp.x;
          current.y = cp.y;
          /* If you omit a command after a moveto then it defaults to lineto */
          if (type == 'm')
            type = 'l';
          else
            type = 'L';
        }
      else if (type == 'z' || type == 'Z')
        {
          if (cur != NULL && cur->spiros != NULL
              && cur->spiros[0].ty == SPIRO_OPEN_CONTOUR)
            {
              if (RealNear (cur->spiros[0].x, cur->spiros[cur->spiro_cnt - 1].x)
                  && RealNear (cur->spiros[0].y,
                               cur->spiros[cur->spiro_cnt - 1].y))
                {
                  cur->spiros[0].ty = SPIRO_G4;
                }
              else
                cur->spiros[0].ty = SPIRO_CORNER;
              if (cur->spiro_cnt >= cur->spiro_max)
                cur->spiros =
                  xrealloc (cur->spiros,
                            (cur->spiro_max++) * sizeof (spiro_cp));
              cp.x = current.x;
              cp.y = current.y;
              cp.ty = SPIRO_END;
              cur->spiros[cur->spiro_cnt++] = cp;
              current.x = cur->spiros[0].x;
              current.y = cur->spiros[0].y;
            }
          type = ' ';
          end = (char *) path;
        }
      else
        {
          cp.x = c_strtod ((char *) path, &end);
          end = skipcomma (end);
          cp.y = c_strtod (end, &end);
          if (islower (type))
            {
              cp.x += current.x;
              cp.y += current.y;
            }
          switch (type)
            {
            case 'l':
            case 'L':
              cp.ty = SPIRO_CORNER;
              if (cur->spiro_cnt != 1)
                cur->spiros[cur->spiro_cnt - 1].ty = SPIRO_CORNER;
              break;
            case 'c':
            case 'C':
              cp.ty = SPIRO_G4;
              break;
            default:
              LogError (_("Unknown type '%c' found in path specification\n"),
                        type);
              break;
            }
          if (cur->spiro_cnt >= cur->spiro_max)
            cur->spiros =
              xrealloc (cur->spiros,
                        (cur->spiro_max += 10) * sizeof (spiro_cp));
          cur->spiros[cur->spiro_cnt++] = cp;
          current.x = cp.x;
          current.y = cp.y;
        }
      path = skipcomma (end);
    }
  return base;
}
#endif

static SplineSet *
SVGParseExtendedPath (xmlNodePtr svg, xmlNodePtr top)
{
  /* Inkscape exends paths by allowing a spiro representation; but
     their representation looks nothing like spiros and I can't guess
     at it. */
  xmlChar *outline /*, *effect, *spirooutline */ ;
  SplineSet *head = NULL;

  outline = xmlGetProp (svg, "d");
  if (outline != NULL)
    {
#if 1
      // FIXME FIXME FIXME: Handle path data that contains more than
      // one path. (Maybe best done by rewriting SVG support from
      // scratch.)
      head = SVGParsePath (outline);
#else
      SCM contours =
        scm_call_1 (scm_c_public_ref ("sortsmill", "path-data->contours"),
                    scm_from_locale_string ((char *) outline));
      if (!scm_is_null (contours))
        {
          // FIXME FIXME FIXME: Handle path data that contains more
          // than one path. (Maybe best done by rewriting SVG support
          // from scratch.)
          head =
            scm_to_pointer
            (SCM_FF_API_CALL_1
             ("SplinePointList->pointer",
              scm_c_value_ref
              (scm_call_1
               (scm_c_public_ref ("sortsmill",
                                  "contour->malloced-SplinePointList"),
                SCM_CAR (contours)), 0)));
        }
#endif
      xmlFree (outline);
    }
#if 0
  effect = xmlGetProp (svg, "path-effect" /*, "inkscape:" */ );
  spirooutline = xmlGetProp (svg, "original-d" /*, "inkscape:" */ );
  if (effect != NULL && spirooutline != NULL && *effect == '#')
    {
      xmlNodePtr effect_type = XmlFindID (top, effect + 1);
      xmlChar *type = NULL;
      if (effect_type != NULL &&
          (type = xmlGetProp (effect_type, "effect")) != NULL &&
          xmlStrcmp (type, "spiro") == 0)
        SVGAddSpiros (spirooutline, head);
      if (type != NULL)
        xmlFree (type);
    }
  if (effect != NULL)
    xmlFree (effect);
  if (spirooutline != NULL)
    xmlFree (spirooutline);
#endif
  return head;
}

static SplineSet *
SVGParseRect (xmlNodePtr rect)
{
  /* x,y,width,height,rx,ry */
  double x, y, width, height, rx, ry;
  char *num;
  SplinePoint *sp;
  SplineSet *cur;

  num = (char *) xmlGetProp (rect, "x");
  if (num != NULL)
    {
      x = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    x = 0;
  num = (char *) xmlGetProp (rect, "width");
  if (num != NULL)
    {
      width = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    return NULL;
  num = (char *) xmlGetProp (rect, "y");
  if (num != NULL)
    {
      y = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    y = 0;
  num = (char *) xmlGetProp (rect, "height");
  if (num != NULL)
    {
      height = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    return NULL;

  rx = ry = 0;
  num = (char *) xmlGetProp (rect, "rx");
  if (num != NULL)
    {
      ry = rx = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  num = (char *) xmlGetProp (rect, "ry");
  if (num != NULL)
    {
      ry = c_strtod ((char *) num, NULL);
      if (rx == 0)
        ry = rx;
      xmlFree (num);
    }

  if (2 * rx > width)
    rx = width / 2;
  if (2 * ry > height)
    ry = height / 2;

  cur = xzalloc (sizeof (SplineSet));
  if (rx == 0)
    {
      cur->first = SplinePointCreate (x, y + height);
      cur->last = SplinePointCreate (x + width, y + height);
      SplineMake (cur->first, cur->last, true);
      sp = SplinePointCreate (x + width, y);
      SplineMake (cur->last, sp, true);
      cur->last = sp;
      sp = SplinePointCreate (x, y);
      SplineMake (cur->last, sp, true);
      SplineMake (sp, cur->first, true);
      cur->last = cur->first;
      return cur;
    }
  else
    {
      cur->first = SplinePointCreate (x, y + height - ry);
      cur->last = SplinePointCreate (x + rx, y + height);
      cur->first->nextcp.x = x;
      cur->first->nextcp.y = y + height;
      cur->last->prevcp = cur->first->nextcp;
      cur->first->nonextcp = cur->last->noprevcp = false;
      SplineMake (cur->first, cur->last, false);
      if (rx < 2 * width)
        {
          sp = SplinePointCreate (x + width - rx, y + height);
          SplineMake (cur->last, sp, true);
          cur->last = sp;
        }
      sp = SplinePointCreate (x + width, y + height - ry);
      sp->prevcp.x = x + width;
      sp->prevcp.y = y + height;
      cur->last->nextcp = sp->prevcp;
      cur->last->nonextcp = sp->noprevcp = false;
      SplineMake (cur->last, sp, false);
      cur->last = sp;
      if (ry < 2 * height)
        {
          sp = SplinePointCreate (x + width, y + ry);
          SplineMake (cur->last, sp, false);
          cur->last = sp;
        }
      sp = SplinePointCreate (x + width - rx, y);
      sp->prevcp.x = x + width;
      sp->prevcp.y = y;
      cur->last->nextcp = sp->prevcp;
      cur->last->nonextcp = sp->noprevcp = false;
      SplineMake (cur->last, sp, false);
      cur->last = sp;
      if (rx < 2 * width)
        {
          sp = SplinePointCreate (x + rx, y);
          SplineMake (cur->last, sp, false);
          cur->last = sp;
        }
      cur->last->nextcp.x = x;
      cur->last->nextcp.y = y;
      cur->last->nonextcp = false;
      if (ry >= 2 * height)
        {
          cur->first->prevcp = cur->last->nextcp;
          cur->first->noprevcp = false;
        }
      else
        {
          sp = SplinePointCreate (x, y + ry);
          sp->prevcp = cur->last->nextcp;
          sp->noprevcp = false;
          SplineMake (cur->last, sp, false);
          cur->last = sp;
        }
      SplineMake (cur->last, cur->first, false);
      cur->first = cur->last;
      return cur;
    }
}

static SplineSet *
SVGParseLine (xmlNodePtr line)
{
  /* x1,y1, x2,y2 */
  double x, y, x2, y2;
  char *num;
  SplinePoint *sp1, *sp2;
  SplineSet *cur;

  num = (char *) xmlGetProp (line, "x1");
  if (num != NULL)
    {
      x = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    x = 0;
  num = (char *) xmlGetProp (line, "x2");
  if (num != NULL)
    {
      x2 = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    x2 = 0;
  num = (char *) xmlGetProp (line, "y1");
  if (num != NULL)
    {
      y = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    y = 0;
  num = (char *) xmlGetProp (line, "y2");
  if (num != NULL)
    {
      y2 = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    y2 = 0;

  sp1 = SplinePointCreate (x, y);
  sp2 = SplinePointCreate (x2, y2);
  SplineMake (sp1, sp2, false);
  cur = xzalloc (sizeof (SplineSet));
  cur->first = sp1;
  cur->last = sp2;
  return cur;
}

static SplineSet *
SVGParseEllipse (xmlNodePtr ellipse, int iscircle)
{
  double cx;
  double cy;
  double rx;
  double ry;

  char *num = (char *) xmlGetProp (ellipse, "cx");
  if (num != NULL)
    {
      cx = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    cx = 0;
  num = (char *) xmlGetProp (ellipse, "cy");
  if (num != NULL)
    {
      cy = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    cy = 0;
  if (iscircle)
    {
      num = (char *) xmlGetProp (ellipse, "r");
      if (num != NULL)
        {
          rx = c_strtod ((char *) num, NULL);
          ry = rx;
          xmlFree (num);
        }
      else
        return NULL;
    }
  else
    {
      num = (char *) xmlGetProp (ellipse, "rx");
      if (num != NULL)
        {
          rx = c_strtod ((char *) num, NULL);
          xmlFree (num);
        }
      else
        return NULL;
      num = (char *) xmlGetProp (ellipse, "ry");
      if (num != NULL)
        {
          ry = c_strtod ((char *) num, NULL);
          xmlFree (num);
        }
      else
        return NULL;
    }

  rx = fabs (rx);
  ry = fabs (ry);

  // See http://www.tinaja.com/glib/ellipse4.pdf
  const double magic_number = 0.55228475 - 0.00045;

  // Shorter names, for readability.
  SCM (*const on_curve) (double, double) = scm_c_make_on_curve_point;
  SCM (*const off_curve) (double, double) = scm_c_make_off_curve_point;

  SCM points = scm_list_n (on_curve (cx - rx, cy),
                           off_curve (cx - rx, cy + magic_number * ry),
                           off_curve (cx - magic_number * rx, cy + ry),
                           on_curve (cx, cy + ry),
                           off_curve (cx + magic_number * rx, cy + ry),
                           off_curve (cx + rx, cy + magic_number * ry),
                           on_curve (cx + rx, cy),
                           off_curve (cx + rx, cy - magic_number * ry),
                           off_curve (cx + magic_number * rx, cy - ry),
                           on_curve (cx, cy - ry),
                           off_curve (cx - magic_number * rx, cy - ry),
                           off_curve (cx - rx, cy - magic_number * ry),
                           SCM_UNDEFINED);

  // A closed contour of degree 3, with no name.
  SCM contour = scm_c_make_contour (points, true, 3, "");

  // @code{scm_contour_to_malloced_SplinePointList} returns multiple
  // values. We are interested in the first, only.
  SCM results = scm_contour_to_malloced_SplinePointList (contour);
  SCM malloced_SplinePointList = scm_c_value_ref (results, 0);

  // Convert from a Guile ‘SplinePointList’ object to Guile’s generic
  // pointer type, and then to a C pointer.
  return (SplineSet *)
    scm_to_pointer (SCM_FF_API_CALL_1 ("SplinePointList->pointer",
                                       malloced_SplinePointList));
}

static SplineSet *
SVGParsePoly (xmlNodePtr poly, int isgon)
{
  /* points */
  double x, y;
  char *pts, *end;
  SplinePoint *sp;
  SplineSet *cur;

  pts = (char *) xmlGetProp (poly, "points");
  if (pts == NULL)
    return NULL;

  x = c_strtod (pts, &end);
  while (isspace (*end) || *end == ',')
    ++end;
  y = c_strtod (end, &end);
  while (isspace (*end))
    ++end;

  cur = xzalloc (sizeof (SplineSet));
  cur->first = cur->last = SplinePointCreate (x, y);
  while (*end)
    {
      x = c_strtod (end, &end);
      while (isspace (*end) || *end == ',')
        ++end;
      y = c_strtod (end, &end);
      while (isspace (*end))
        ++end;
      sp = SplinePointCreate (x, y);
      SplineMake (cur->last, sp, false);
      cur->last = sp;
    }
  if (isgon)
    {
      if (RealNear (cur->last->me.x, cur->first->me.x) &&
          RealNear (cur->last->me.y, cur->first->me.y))
        {
          cur->first->prev = cur->last->prev;
          cur->first->prev->to = cur->first;
          SplinePointFree (cur->last);
        }
      else
        SplineMake (cur->last, cur->first, false);
      cur->last = cur->first;
    }
  return cur;
}

struct svg_state
{
  double linewidth;
  int dofill, dostroke;
  uint32_t fillcol, strokecol;
  float fillopacity, strokeopacity;
  int isvisible;
  enum linecap lc;
  enum linejoin lj;
  real transform[6];
  DashType dashes[DASH_MAX];
  SplineSet *clippath;
  uint8_t free_clip;
  uint32_t currentColor;
  uint32_t stopColor;
  float stopOpacity;
};

static void
SVGFigureTransform (struct svg_state *st, char *name)
{
  real trans[6], res[6];
  double a, cx, cy;
  char *pt, *paren, *end;
  /* matrix(a,b,c,d,e,f)
     rotate(theta[,cx,cy])
     scale(sx[,sy])
     translate(x,y)
     skewX(theta)
     skewY(theta)
   */

  for (pt = (char *) name; isspace (*pt); ++pt);
  while (*pt)
    {
      paren = strchr (pt, '(');
      if (paren == NULL)
        break;
      if (strncmp (pt, "matrix", paren - pt) == 0)
        {
          trans[0] = c_strtod (paren + 1, &end);
          trans[1] = c_strtod (skipcomma (end), &end);
          trans[2] = c_strtod (skipcomma (end), &end);
          trans[3] = c_strtod (skipcomma (end), &end);
          trans[4] = c_strtod (skipcomma (end), &end);
          trans[5] = c_strtod (skipcomma (end), &end);
        }
      else if (strncmp (pt, "rotate", paren - pt) == 0)
        {
          trans[4] = trans[5] = 0;
          a = c_strtod (paren + 1, &end) * 3.1415926535897932 / 180;
          trans[0] = trans[3] = cos (a);
          trans[1] = sin (a);
          trans[2] = -trans[1];
          while (isspace (*end))
            ++end;
          if (*end != ')')
            {
              cx = c_strtod (skipcomma (end), &end);
              cy = c_strtod (skipcomma (end), &end);
              res[0] = res[3] = 1;
              res[1] = res[2] = 0;
              res[4] = cx;
              res[5] = cy;
              MatMultiply (res, trans, res);
              trans[0] = trans[3] = 1;
              trans[1] = trans[2] = 0;
              trans[4] = -cx;
              trans[5] = -cy;
              MatMultiply (res, trans, res);
              memcpy (trans, res, sizeof (res));
            }
        }
      else if (strncmp (pt, "scale", paren - pt) == 0)
        {
          trans[1] = trans[2] = trans[4] = trans[5] = 0;
          trans[0] = trans[3] = c_strtod (paren + 1, &end);
          while (isspace (*end))
            ++end;
          if (*end != ')')
            trans[3] = c_strtod (skipcomma (end), &end);
        }
      else if (strncmp (pt, "translate", paren - pt) == 0)
        {
          trans[0] = trans[3] = 1;
          trans[1] = trans[2] = trans[5] = 0;
          trans[4] = c_strtod (paren + 1, &end);
          while (isspace (*end))
            ++end;
          if (*end != ')')
            trans[5] = c_strtod (skipcomma (end), &end);
        }
      else if (strncmp (pt, "skewX", paren - pt) == 0)
        {
          trans[0] = trans[3] = 1;
          trans[1] = trans[2] = trans[4] = trans[5] = 0;
          trans[2] =
            tan (c_strtod (paren + 1, &end) * 3.1415926535897932 / 180);
        }
      else if (strncmp (pt, "skewY", paren - pt) == 0)
        {
          trans[0] = trans[3] = 1;
          trans[1] = trans[2] = trans[4] = trans[5] = 0;
          trans[1] =
            tan (c_strtod (paren + 1, &end) * 3.1415926535897932 / 180);
        }
      else
        break;
      while (isspace (*end))
        ++end;
      if (*end != ')')
        break;
      MatMultiply (trans, st->transform, st->transform);
      pt = end + 1;
      while (isspace (*pt))
        ++pt;
    }
}

static void
SVGuseTransform (struct svg_state *st, xmlNodePtr use, xmlNodePtr symbol)
{
  double x, y, uwid, uheight, swid, sheight;
  char *num, *end;
  real trans[6];

  num = (char *) xmlGetProp (use, "x");
  if (num != NULL)
    {
      x = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    x = 0;
  num = (char *) xmlGetProp (use, "y");
  if (num != NULL)
    {
      y = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    y = 0;
  if (x != 0 || y != 0)
    {
      trans[0] = trans[3] = 1;
      trans[1] = trans[2] = 0;
      trans[4] = x;
      trans[5] = y;
      MatMultiply (trans, st->transform, st->transform);
    }
  num = (char *) xmlGetProp (use, "width");
  if (num != NULL)
    {
      uwid = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    uwid = 0;
  num = (char *) xmlGetProp (use, "height");
  if (num != NULL)
    {
      uheight = c_strtod ((char *) num, NULL);
      xmlFree (num);
    }
  else
    uheight = 0;
  num = (char *) xmlGetProp (symbol, "viewBox");
  if (num != NULL)
    {
      x = c_strtod ((char *) num, &end);
      y = c_strtod ((char *) end + 1, &end);
      swid = c_strtod ((char *) end + 1, &end);
      sheight = c_strtod ((char *) end + 1, &end);
      xmlFree (num);
    }
  else
    return;
  if (uwid != 0 || uheight != 0)
    {
      trans[0] = trans[3] = 1;
      trans[1] = trans[2] = trans[4] = trans[5] = 0;
      if (uwid != 0 && swid != 0)
        trans[0] = uwid / swid;
      if (uheight != 0 && sheight != 0)
        trans[3] = uheight / sheight;
      MatMultiply (trans, st->transform, st->transform);
    }
}

static real
parseGCoord (xmlChar *prop, int bb_units, real bb_low, real bb_high)
{
  char *end;
  double val = c_strtod ((char *) prop, &end);

  if (*end == '%')
    val /= 100.0;

  if (bb_units)
    val = bb_low + val * (bb_high - bb_low);
  return val;
}

static int xmlParseColor (xmlChar *name, uint32_t *color, char **url,
                          struct svg_state *st);

static void
xmlParseColorSource (xmlNodePtr top, char *name, DBounds *bbox,
                     struct svg_state *st, struct gradient **_grad,
                     struct epattern **_epat)
{
  xmlNodePtr colour_source = XmlFindURI (top, name);
  int islinear;
  xmlChar *prop;
  xmlNodePtr kid;
  int scnt;

  *_grad = NULL;
  *_epat = NULL;
  if (colour_source == NULL)
    LogError (_("Could not find Color Source with id %s."), name);
  else
    if ((islinear =
         xmlStrcmp (colour_source->name, "linearGradient") == 0)
        || xmlStrcmp (colour_source->name, "radialGradient") == 0)
    {
      struct gradient *grad = xzalloc (sizeof (struct gradient));
      int bbox_units;
      *_grad = grad;

      prop = xmlGetProp (colour_source, "gradientUnits");
      if (prop != NULL)
        {
          bbox_units = xmlStrcmp (prop, "userSpaceOnUse") != 0;
          xmlFree (prop);
        }
      else
        bbox_units = true;

      prop = xmlGetProp (colour_source, "gradientTransform");
      /* I don't support this currently */
      if (prop != NULL)
        xmlFree (prop);

      grad->sm = sm_pad;
      prop = xmlGetProp (colour_source, "spreadMethod");
      if (prop != NULL)
        {
          if (xmlStrcmp (prop, "reflect") == 0)
            grad->sm = sm_reflect;
          else if (xmlStrcmp (prop, "repeat") == 0)
            grad->sm = sm_repeat;
          xmlFree (prop);
        }

      if (islinear)
        {
          grad->start.x = bbox->minx;
          grad->start.y = bbox->miny;
          grad->stop.x = bbox->maxx;
          grad->stop.y = bbox->maxy;

          prop = xmlGetProp (colour_source, "x1");
          if (prop != NULL)
            {
              grad->start.x =
                parseGCoord (prop, bbox_units, bbox->minx, bbox->maxx);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "x2");
          if (prop != NULL)
            {
              grad->stop.x =
                parseGCoord (prop, bbox_units, bbox->minx, bbox->maxx);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "y1");
          if (prop != NULL)
            {
              grad->start.y =
                parseGCoord (prop, bbox_units, bbox->miny, bbox->maxy);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "y2");
          if (prop != NULL)
            {
              grad->stop.y =
                parseGCoord (prop, bbox_units, bbox->miny, bbox->maxy);
              xmlFree (prop);
            }

          grad->radius = 0;
        }
      else
        {
          double offx = (bbox->maxx - bbox->minx) / 2;
          double offy = (bbox->maxy - bbox->miny) / 2;
          grad->stop.x = (bbox->minx + bbox->maxx) / 2;
          grad->stop.y = (bbox->minx + bbox->maxy) / 2;
          grad->radius = sqrt (offx * offx + offy * offy);

          prop = xmlGetProp (colour_source, "cx");
          if (prop != NULL)
            {
              grad->stop.x =
                parseGCoord (prop, bbox_units, bbox->minx, bbox->maxx);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "cy");
          if (prop != NULL)
            {
              grad->stop.y =
                parseGCoord (prop, bbox_units, bbox->miny, bbox->maxy);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "radius");
          if (prop != NULL)
            {
              grad->radius =
                parseGCoord (prop, bbox_units, 0,
                             sqrt (4 * (offx * offx + offy * offy)));
              xmlFree (prop);
            }

          grad->start = grad->stop;
          prop = xmlGetProp (colour_source, "fx");
          if (prop != NULL)
            {
              grad->start.x =
                parseGCoord (prop, bbox_units, bbox->minx, bbox->maxx);
              xmlFree (prop);
            }

          prop = xmlGetProp (colour_source, "fy");
          if (prop != NULL)
            {
              grad->start.y =
                parseGCoord (prop, bbox_units, bbox->miny, bbox->maxy);
              xmlFree (prop);
            }
        }

      scnt = 0;
      for (kid = colour_source->children; kid != NULL; kid = kid->next)
        if (xmlStrcmp (kid->name, "stop") == 0)
          ++scnt;

      if (scnt == 0)
        {
          /* I'm not sure how to use the style stop-color, but I'm guessing */
          /*  this might be it */
          grad->stop_cnt = 1;
          grad->grad_stops = xcalloc (1, sizeof (struct grad_stops));
          grad->grad_stops[scnt].offset = 1;
          grad->grad_stops[scnt].col = st->stopColor;
          grad->grad_stops[scnt].opacity = st->stopOpacity;
          ++scnt;
        }
      else
        {
          grad->stop_cnt = scnt;
          grad->grad_stops = xcalloc (scnt, sizeof (struct grad_stops));
          scnt = 0;
          for (kid = colour_source->children; kid != NULL; kid = kid->next)
            if (xmlStrcmp (kid->name, "stop") == 0)
              {
                grad->grad_stops[scnt].col = st->stopColor;
                grad->grad_stops[scnt].opacity = st->stopOpacity;

                prop = xmlGetProp (kid, "offset");
                if (prop != NULL)
                  {
                    grad->grad_stops[scnt].offset =
                      parseGCoord (prop, false, 0, 1.0);
                    xmlFree (prop);
                  }

                prop = xmlGetProp (kid, "stop-color");
                if (prop != NULL)
                  {
                    xmlParseColor (prop, &grad->grad_stops[scnt].col, NULL, st);
                    xmlFree (prop);
                  }

                prop = xmlGetProp (kid, "stop-opacity");
                if (prop != NULL)
                  {
                    grad->grad_stops[scnt].opacity =
                      c_strtod ((char *) prop, NULL);
                    xmlFree (prop);
                  }
                else
                  grad->grad_stops[scnt].opacity = 1.0;

                ++scnt;
              }
        }
    }
  else if (xmlStrcmp (colour_source->name, "pattern") == 0)
    {
      LogError (_
                ("FontForge does not currently parse pattern Color Sources (%s)."),
                name);
    }
  else
    {
      LogError (_("Color Source with id %s had an unexpected type %s."),
                name, (char *) colour_source->name);
    }
}

/* Annoyingly we can't parse the colour source until we have parsed the contents */
/*  because the colour source needs to know the bounding box of the total item */
/*  This means that for something like <g> we must now go back and figure out */
/*  which components get this gradient/pattern, and which have their own source*/
/*  that overrides the inherited one */
static void
xmlApplyColourSources (xmlNodePtr top, Entity * head,
                       struct svg_state *st,
                       char *fill_colour_source, char *stroke_colour_source)
{
  DBounds b, ssb;
  Entity *e;
  struct gradient *grad;
  struct epattern *epat;

  memset (&b, 0, sizeof (b));
  for (e = head; e != NULL; e = e->next)
    if (e->type == et_splines)
      {
        SplineSetFindBounds (e->u.splines.splines, &ssb);
        if (b.minx == 0 && b.miny == 0 && b.maxx == 0 && b.maxy == 0)
          b = ssb;
        else
          {
            if (b.minx > ssb.minx)
              b.minx = ssb.minx;
            if (b.maxx > ssb.maxx)
              b.maxx = ssb.maxx;
            if (b.miny > ssb.miny)
              b.miny = ssb.miny;
            if (b.maxy > ssb.maxy)
              b.maxy = ssb.maxy;
          }
      }
  if (b.minx == b.maxx)
    b.maxx = b.minx + 1;
  if (b.miny == b.maxy)
    b.maxy = b.maxy + 1;

  if (fill_colour_source != NULL)
    {
      xmlParseColorSource (top, fill_colour_source, &b, st, &grad, &epat);
      free (fill_colour_source);
      for (e = head; e != NULL; e = e->next)
        if (e->type == et_splines)
          {
            if (e->u.splines.fill.grad == NULL && e->u.splines.fill.tile == NULL
                && e->u.splines.fill.col == COLOR_INHERITED)
              {
                e->u.splines.fill.grad = GradientCopy (grad, NULL);
                /*e->u.splines.fill.tile = EPatternCopy(epat,NULL); */
              }
          }
      GradientFree (grad);
      /*EPatternFree(epat); */
    }

  if (stroke_colour_source != NULL)
    {
      xmlParseColorSource (top, stroke_colour_source, &b, st, &grad, &epat);
      free (stroke_colour_source);
      for (e = head; e != NULL; e = e->next)
        if (e->type == et_splines)
          {
            if (e->u.splines.stroke.grad == NULL
                && e->u.splines.stroke.tile == NULL
                && e->u.splines.stroke.col == COLOR_INHERITED)
              {
                e->u.splines.stroke.grad = GradientCopy (grad, NULL);
                /*e->u.splines.stroke.tile = EPatternCopy(epat,NULL); */
              }
          }
      GradientFree (grad);
      /*EPatternFree(epat); */
    }
}

static int
xmlParseColor (xmlChar *name, uint32_t *color, char **url, struct svg_state *st)
{
  int doit, i;
  static struct
  {
    char *name;
    uint32_t col;
  } stdcols[] =
  {
    // *INDENT-OFF*
    {"red", 0xff0000},
    {"green", 0x008000},
    {"blue", 0x0000ff},
    {"cyan", 0x00ffff},
    {"magenta", 0xff00ff},
    {"yellow", 0xffff00},
    {"black", 0x000000},
    {"darkgray", 0x404040},
    {"darkgrey", 0x404040},
    {"gray", 0x808080},
    {"grey", 0x808080},
    {"lightgray", 0xc0c0c0},
    {"lightgrey", 0xc0c0c0},
    {"white", 0xffffff},
    {"maroon", 0x800000},
    {"olive", 0x808000},
    {"navy", 0x000080},
    {"purple", 0x800080},
    {"lime", 0x00ff00},
    {"aqua", 0x00ffff},
    {"teal", 0x008080},
    {"fuchsia", 0xff0080},
    {"silver", 0xc0c0c0},
    {NULL, 0}
    // *INDENT-ON*
  };

  doit = xmlStrcmp (name, "none") != 0;
  if (doit)
    {
      for (i = 0; stdcols[i].name != NULL; ++i)
        if (xmlStrcmp (name, stdcols[i].name) == 0)
          break;
      if (stdcols[i].name != NULL)
        *color = stdcols[i].col;
      else if (xmlStrcmp (name, "currentColor") == 0)
        *color = st->currentColor;
      else if (name[0] == '#')
        {
          unsigned int temp = 0;
          if (sscanf ((char *) name, "#%x", &temp) != 1)
            LogError (_("Bad hex color spec: %s\n"), (char *) name);
          if (strlen ((char *) name) == 4)
            {
              *color = (((temp & 0xf00) * 0x11) << 8) |
                (((temp & 0x0f0) * 0x11) << 4) | (((temp & 0x00f) * 0x11));
            }
          else if (strlen ((char *) name) == 7)
            {
              *color = temp;
            }
          else
            *color = COLOR_INHERITED;
        }
      else if (strncmp ((char *) name, "rgb(", 4) == 0)
        {
          float r = 0, g = 0, b = 0;
          if (sscanf ((char *) name + 4, "%g,%g,%g", &r, &g, &b) != 3)
            LogError (_("Bad RGB color spec: %s\n"), (char *) name);
          if (strchr ((char *) name, '.') != NULL)
            {
              if (r >= 1)
                r = 1;
              else if (r < 0)
                r = 0;
              if (g >= 1)
                g = 1;
              else if (g < 0)
                g = 0;
              if (b >= 1)
                b = 1;
              else if (b < 0)
                b = 0;
              *color = (((int) rint (r * 255)) << 16) |
                (((int) rint (g * 255)) << 8) | (((int) rint (b * 255)));
            }
          else
            {
              if (r >= 255)
                r = 255;
              else if (r < 0)
                r = 0;
              if (g >= 255)
                g = 255;
              else if (g < 0)
                g = 0;
              if (b >= 255)
                b = 255;
              else if (b < 0)
                b = 0;
              *color = (((int) r) << 16) | (((int) g) << 8) | (((int) b));
            }
        }
      else if (url != NULL && strncmp ((char *) name, "url(#", 5) == 0)
        {
          *url = xstrdup_or_null ((char *) name);
          *color = COLOR_INHERITED;
        }
      else
        {
          LogError (_("Failed to parse color %s\n"), (char *) name);
          *color = COLOR_INHERITED;
        }
    }
  return doit;
}

static int
base64ch (int ch)
{
  if (ch >= 'A' && ch <= 'Z')
    return ch - 'A';
  if (ch >= 'a' && ch <= 'z')
    return ch - 'a' + 26;
  if (ch >= '0' && ch <= '9')
    return ch - '0' + 52;
  if (ch == '+')
    return 62;
  if (ch == '/')
    return 63;
  if (ch == '=')
    return 64;

  return -1;
}

static void
DecodeBase64ToFile (FILE *tmp, char *str)
{
  char fourchars[4];
  int i;

  while (*str)
    {
      fourchars[0] = fourchars[1] = fourchars[2] = fourchars[3] = 64;
      for (i = 0; i < 4; ++i)
        {
          while (isspace (*str) || base64ch (*str) == -1)
            ++str;
          if (*str == '\0')
            break;
          fourchars[i] = base64ch (*str++);
        }
      if (fourchars[0] < 64 && fourchars[1] < 64)
        {
          putc ((fourchars[0] << 2) | (fourchars[1] >> 4), tmp);
          if (fourchars[2] < 64)
            {
              putc ((fourchars[1] << 4) | (fourchars[2] >> 2), tmp);
              if (fourchars[3] < 64)
                putc ((fourchars[2] << 6) | fourchars[3], tmp);
            }
        }
    }
}

static GImage *
GImageFromDataURI (char *uri)
{
  char *mimetype;
  int is_base64 = false, ch;
  FILE *tmp;
  GImage *img;

  if (uri == NULL)
    return NULL;
  if (strncmp (uri, "data:", 5) != 0)
    return NULL;
  uri += 5;

  mimetype = uri;
  while (*uri != ',' && *uri != ';' && *uri != '\0')
    ++uri;
  if (*uri == '\0')
    return NULL;
  ch = *uri;
  *uri = '\0';
  if (ch == ';' && strncmp (uri + 1, "base64,", 7) == 0)
    {
      is_base64 = true;
      uri += 6;
      ch = ',';
    }
  else if (ch == ';')           /* Can't deal with other encoding methods */
    return NULL;

  ++uri;
  if (strcmp (mimetype, "image/png") == 0 ||
      strcmp (mimetype, "image/jpeg") == 0 ||
      strcmp (mimetype, "image/bmp") == 0)
    /* These we support (if we've got the libraries) */ ;
  else
    {
      LogError (_("Unsupported mime type in data URI: %s\n"), mimetype);
      return NULL;
    }
  tmp = tmpfile ();
  if (is_base64)
    DecodeBase64ToFile (tmp, uri);
  else
    {
      while (*uri)
        {
          putc (*uri, tmp);
          ++uri;
        }
    }
  rewind (tmp);
  if (strcmp (mimetype, "image/png") == 0)
    img = GImageRead_Png (tmp);
  else
#ifndef _NO_LIBJPEG
  if (strcmp (mimetype, "image/jpeg") == 0)
    img = GImageRead_Jpeg (tmp);
  else
#endif
  if (strcmp (mimetype, "image/bmp") == 0)
    img = GImageRead_Bmp (tmp);
  else
    img = NULL;
  fclose (tmp);
  return img;
}

static Entity *
SVGParseImage (xmlNodePtr svg)
{
  double x = 0, y = 0, width = 1, height = 1;
  GImage *img;
  struct _GImage *base;
  Entity *ent;
  xmlChar *val;

  val = xmlGetProp (svg, "x");
  if (val != NULL)
    {
      x = c_strtod ((char *) val, NULL);
      free (val);
    }
  val = xmlGetProp (svg, "y");
  if (val != NULL)
    {
      y = c_strtod ((char *) val, NULL);
      free (val);
    }

  val = xmlGetProp (svg, "width");
  if (val != NULL)
    {
      width = c_strtod ((char *) val, NULL);
      free (val);
    }
  val = xmlGetProp (svg, "height");
  if (val != NULL)
    {
      height = c_strtod ((char *) val, NULL);
      free (val);
    }

  val = xmlGetProp (svg, /*"xlink:href" */ "href");
  if (val == NULL)
    return NULL;
  if (strncmp ((char *) val, "data:", 5) != 0)
    {
      LogError (_("FontForge only supports embedded images in data: URIs\n"));
      free (val);
      return NULL;              /* I can only handle data URIs */
    }
  img = GImageFromDataURI ((char *) val);
  free (val);
  if (img == NULL)
    return NULL;
  base = img->list_len == 0 ? img->u.image : img->u.images[0];

  ent = xzalloc (sizeof (Entity));
  ent->type = et_image;
  ent->u.image.image = img;
  ent->u.image.transform[1] = ent->u.image.transform[2] = 0;
  ent->u.image.transform[0] = width / base->width;
  ent->u.image.transform[3] = height / base->height;
  ent->u.image.transform[4] = x;
  ent->u.image.transform[5] = y;
  ent->u.image.col = 0xffffffff;
  return ent;
}

static Entity *
EntityCreate (SplinePointList *head, struct svg_state *state)
{
  Entity *ent = xcalloc (1, sizeof (Entity));
  ent->type = et_splines;
  ent->u.splines.splines = head;
  ent->u.splines.cap = state->lc;
  ent->u.splines.join = state->lj;
  ent->u.splines.stroke_width = state->linewidth;
  ent->u.splines.fill.col =
    state->dofill
    ? state->fillcol : (state->dostroke ? 0xffffffff : COLOR_INHERITED);
  ent->u.splines.stroke.col = state->dostroke ? state->strokecol : 0xffffffff;
  ent->u.splines.fill.opacity = state->fillopacity;
  ent->u.splines.stroke.opacity = state->strokeopacity;
  memcpy (ent->u.splines.transform, state->transform, 6 * sizeof (real));
  ent->clippath = SplinePointListCopy (state->clippath);
  return ent;
}

static void
SvgStateFree (struct svg_state *st)
{
  if (st->free_clip)
    SplinePointListFree (st->clippath);
}

static void
SVGFigureStyle (struct svg_state *st, char *name,
                char **fill_colour_source, char **stroke_colour_source)
{
  char *pt;
  char namebuf[200], propbuf[400];

  while (true)
    {
      while (isspace (*name))
        ++name;
      if (*name == ':')
        {
          /* Missing prop name, skip the value */
          while (*name != ';' && *name != '\0')
            ++name;
          if (*name == ';')
            ++name;
        }
      else if (*name != '\0' && *name != ';')
        {
          pt = namebuf;
          while (*name != '\0' && *name != ':' && *name != ';'
                 && !isspace (*name))
            {
              if (pt < namebuf + sizeof (namebuf) - 1)
                *pt++ = *name;
              ++name;
            }
          *pt = '\0';
          while (*name != ':' && *name != ';' && *name != '\0')
            ++name;
          if (*name == ':')
            ++name;
          while (isspace (*name))
            ++name;
          propbuf[0] = '\0';
          if (*name != '\0' && *name != ';')
            {
              pt = propbuf;
              while (*name != '\0' && *name != ';' && !isspace (*name))
                {
                  if (pt < propbuf + sizeof (propbuf) - 1)
                    *pt++ = *name;
                  ++name;
                }
              *pt = '\0';
            }
          while (*name != ';' && *name != '\0')
            ++name;
          if (*name == ';')
            ++name;

          if (strcmp (namebuf, "color") == 0)
            xmlParseColor (propbuf, &st->currentColor, NULL, st);
          else if (strcmp (namebuf, "fill") == 0)
            st->dofill =
              xmlParseColor (propbuf, &st->fillcol, fill_colour_source, st);
          else if (strcmp (namebuf, "visibility") == 0)
            st->isvisible = strcmp (propbuf, "hidden") != 0 &&
              strcmp (propbuf, "collapse") != 0;
          else if (strcmp (namebuf, "fill-opacity") == 0)
            st->fillopacity = c_strtod (propbuf, NULL);
          else if (strcmp (namebuf, "stroke") == 0)
            st->dostroke =
              xmlParseColor (propbuf, &st->strokecol, stroke_colour_source, st);
          else if (strcmp (namebuf, "stroke-opacity") == 0)
            st->strokeopacity = c_strtod ((char *) propbuf, NULL);
          else if (strcmp (namebuf, "stroke-width") == 0)
            st->linewidth = c_strtod ((char *) propbuf, NULL);
          else if (strcmp (namebuf, "stroke-linecap") == 0)
            st->lc = strcmp (propbuf, "butt") ? lc_butt :
              strcmp (propbuf, "round") ? lc_round : lc_square;
          else if (strcmp (namebuf, "stroke-linejoin") == 0)
            st->lj = strcmp (propbuf, "miter") ? lj_miter :
              strcmp (propbuf, "round") ? lj_round : lj_bevel;
          else if (strcmp (namebuf, "stroke-dasharray") == 0)
            {
              if (strcmp (propbuf, "inherit"))
                {
                  st->dashes[0] = 0;
                  st->dashes[1] = DASH_INHERITED;
                }
              else if (strcmp (propbuf, "none"))
                {
                  st->dashes[0] = 0;
                  st->dashes[1] = 0;
                }
              else
                {
                  int i;
                  char *pt, *end;
                  pt = propbuf;
                  for (i = 0; i < DASH_MAX && *pt != '\0'; ++i)
                    {
                      st->dashes[i] = strtol (pt, &end, 10);
                      pt = end;
                    }
                  if (i < DASH_MAX)
                    st->dashes[i] = 0;
                }
            }
          else if (strcmp (namebuf, "stop-color") == 0)
            xmlParseColor (propbuf, &st->stopColor, NULL, st);
          else if (strcmp (namebuf, "stop-opacity") == 0)
            st->stopOpacity = c_strtod (propbuf, NULL);
          else
            {
              /* Lots of props we ignore */
              ;
            }
        }
      else if (*name == ';')
        ++name;
      if (*name == '\0')
        break;
    }
}

static Entity *
_SVGParseSVG (xmlNodePtr svg, xmlNodePtr top, struct svg_state *inherit)
{
  struct svg_state st;
  xmlChar *name;
  xmlNodePtr kid;
  Entity *ehead;
  Entity *elast;
  Entity *eret;
  SplineSet *head;
  int treat_symbol_as_g = false;
  char *fill_colour_source = NULL;
  char *stroke_colour_source = NULL;

  if (svg == NULL)
    return NULL;

  st = *inherit;
  st.free_clip = false;
tail_recurse:
  name = xmlGetProp (svg, "display");
  if (name != NULL)
    {
      int hide = xmlStrcmp (name, "none") == 0;
      xmlFree (name);
      if (hide)
        return NULL;
    }
  name = xmlGetProp (svg, "visibility");
  if (name != NULL)
    {
      st.isvisible = xmlStrcmp (name, "hidden") != 0 &&
        xmlStrcmp (name, "colapse") != 0;
      xmlFree (name);
    }
  name = xmlGetProp (svg, "fill");
  if (name != NULL)
    {
      st.dofill = xmlParseColor (name, &st.fillcol, &fill_colour_source, &st);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "fill-opacity");
  if (name != NULL)
    {
      st.fillopacity = c_strtod ((char *) name, NULL);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke");
  if (name != NULL)
    {
      st.dostroke =
        xmlParseColor (name, &st.strokecol, &stroke_colour_source, &st);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke-opacity");
  if (name != NULL)
    {
      st.strokeopacity = c_strtod ((char *) name, NULL);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke-width");
  if (name != NULL)
    {
      st.linewidth = c_strtod ((char *) name, NULL);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke-linecap");
  if (name != NULL)
    {
      st.lc = xmlStrcmp (name, "butt") ? lc_butt :
        xmlStrcmp (name, "round") ? lc_round : lc_square;
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke-linejoin");
  if (name != NULL)
    {
      st.lj = xmlStrcmp (name, "miter") ? lj_miter :
        xmlStrcmp (name, "round") ? lj_round : lj_bevel;
      xmlFree (name);
    }
  name = xmlGetProp (svg, "stroke-dasharray");
  if (name != NULL)
    {
      if (xmlStrcmp (name, "inherit"))
        {
          st.dashes[0] = 0;
          st.dashes[1] = DASH_INHERITED;
        }
      else if (xmlStrcmp (name, "none"))
        {
          st.dashes[0] = 0;
          st.dashes[1] = 0;
        }
      else
        {
          int i;
          xmlChar *pt, *end;
          pt = name;
          for (i = 0; i < DASH_MAX && *pt != '\0'; ++i)
            {
              st.dashes[i] = strtol ((char *) pt, (char **) &end, 10);
              pt = end;
            }
          if (i < DASH_MAX)
            st.dashes[i] = 0;
        }
      xmlFree (name);
    }
  name = xmlGetProp (svg, "style");
  if (name != NULL)
    {
      SVGFigureStyle (&st, (char *) name, &fill_colour_source,
                      &stroke_colour_source);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "transform");
  if (name != NULL)
    {
      SVGFigureTransform (&st, (char *) name);
      xmlFree (name);
    }
  name = xmlGetProp (svg, "clip-path");
  if (name != NULL)
    {
      xmlNodePtr clip = XmlFindURI (top, (char *) name);
      if (clip != NULL && xmlStrcmp (clip->name, "clipPath") == 0)
        {
          const xmlChar *temp = clip->name;
          struct svg_state null_state;
          memset (&null_state, 0, sizeof (null_state));
          null_state.isvisible = true;
          null_state.transform[0] = null_state.transform[3] = 1;
          clip->name = "g";
          eret = _SVGParseSVG (clip, top, &null_state);
          clip->name = temp;
          if (eret != NULL && eret->type == et_splines)
            {
              st.clippath = eret->u.splines.splines;
              eret->u.splines.splines = NULL;
            }
          free (eret);
        }
      else
        LogError (_("Could not find clippath named %s."), name);
      xmlFree (name);
    }

  if ((treat_symbol_as_g && xmlStrcmp (svg->name, "symbol") == 0) ||
      xmlStrcmp (svg->name, "svg") == 0 ||
      xmlStrcmp (svg->name, "glyph") == 0 ||
      xmlStrcmp (svg->name, "pattern") == 0 || xmlStrcmp (svg->name, "g") == 0)
    {
      ehead = elast = NULL;
      for (kid = svg->children; kid != NULL; kid = kid->next)
        {
          eret = _SVGParseSVG (kid, top, &st);
          if (eret != NULL)
            {
              if (elast == NULL)
                ehead = eret;
              else
                elast->next = eret;
              while (eret->next != NULL)
                eret = eret->next;
              elast = eret;
            }
        }
      SvgStateFree (&st);
      if (fill_colour_source != NULL || stroke_colour_source != NULL)
        xmlApplyColourSources (top, ehead, &st, fill_colour_source,
                               stroke_colour_source);
      return ehead;
    }
  else if (xmlStrcmp (svg->name, "use") == 0)
    {
      name = xmlGetProp (svg, "href");
      kid = NULL;
      if (name != NULL && *name == '#')
        {                       /* Within this file */
          kid = XmlFindID (top, (char *) name + 1);
          treat_symbol_as_g = true;
        }
      SVGuseTransform (&st, svg, kid);
      svg = kid;
      if (name != NULL)
        xmlFree (name);
      if (svg != NULL)
        goto tail_recurse;
      SvgStateFree (&st);
      return NULL;
    }

  if (!st.isvisible)
    {
      SvgStateFree (&st);
      return NULL;
    }

  /* basic shapes */
  head = NULL;
  if (xmlStrcmp (svg->name, "path") == 0)
    head = SVGParseExtendedPath (svg, top);
  else if (xmlStrcmp (svg->name, "rect") == 0)
    head = SVGParseRect (svg);  /* x,y,width,height,rx,ry */
  else if (xmlStrcmp (svg->name, "circle") == 0)
    head = SVGParseEllipse (svg, true); /* cx,cy, r */
  else if (xmlStrcmp (svg->name, "ellipse") == 0)
    head = SVGParseEllipse (svg, false);        /* cx,cy, rx,ry */
  else if (xmlStrcmp (svg->name, "line") == 0)
    head = SVGParseLine (svg);  /* x1,y1, x2,y2 */
  else if (xmlStrcmp (svg->name, "polyline") == 0)
    head = SVGParsePoly (svg, 0);       /* points */
  else if (xmlStrcmp (svg->name, "polygon") == 0)
    head = SVGParsePoly (svg, 1);       /* points */
  else if (xmlStrcmp (svg->name, "image") == 0)
    {
      eret = SVGParseImage (svg);
      if (eret != NULL)
        eret->clippath = st.clippath;
      return eret;
    }
  else
    return NULL;
  if (head == NULL)
    return NULL;

  SPLCategorizePoints (head);

  eret =
    EntityCreate (SplinePointListTransform (head, st.transform, tpt_AllPoints),
                  &st);
  if (fill_colour_source != NULL || stroke_colour_source != NULL)
    xmlApplyColourSources (top, eret, &st, fill_colour_source,
                           stroke_colour_source);
  SvgStateFree (&st);
  return eret;
}

static Entity *
SVGParseSVG (xmlNodePtr svg, int em_size, int ascent)
{
  struct svg_state st;
  char *num, *end;
  double x, y, swidth, sheight, width = 1, height = 1;

  memset (&st, 0, sizeof (st));
  st.lc = lc_inherited;
  st.lj = lj_inherited;
  st.linewidth = WIDTH_INHERITED;
  st.fillcol = COLOR_INHERITED;
  st.strokecol = COLOR_INHERITED;
  st.currentColor = COLOR_INHERITED;
  st.stopColor = COLOR_INHERITED;
  st.isvisible = true;
  st.transform[0] = 1;
  st.transform[3] = -1;         /* The SVG coord system has y increasing down */
  /*  Font coords have y increasing up */
  st.transform[5] = ascent;
  st.strokeopacity = st.fillopacity = 1.0;

  num = (char *) xmlGetProp (svg, "width");
  if (num != NULL)
    {
      width = c_strtod (num, NULL);
      xmlFree (num);
    }
  num = (char *) xmlGetProp (svg, "height");
  if (num != NULL)
    {
      height = c_strtod (num, NULL);
      xmlFree (num);
    }
  if (height <= 0)
    height = 1;
  if (width <= 0)
    width = 1;
  num = (char *) xmlGetProp (svg, "viewBox");
  if (num != NULL)
    {
      x = c_strtod ((char *) num, &end);
      y = c_strtod ((char *) end + 1, &end);
      swidth = c_strtod ((char *) end + 1, &end);
      sheight = c_strtod ((char *) end + 1, &end);
      xmlFree (num);
      if (width > height)
        {
          if (swidth != 0)
            {
              st.transform[0] *= em_size / swidth;
              st.transform[3] *= em_size / swidth;
            }
        }
      else
        {
          if (sheight != 0)
            {
              st.transform[0] *= em_size / sheight;
              st.transform[3] *= em_size / sheight;
            }
        }
    }
  return _SVGParseSVG (svg, svg, &st);
}

static int
SPLFindOrder (SplineSet *ss)
{
  Spline *s, *first;

  while (ss != NULL)
    {
      first = NULL;
      for (s = ss->first->next; s != NULL && s != first; s = s->to->next)
        {
          if (first == NULL)
            first = s;
          if (!s->knownlinear)
            return s->order2;
        }
      ss = ss->next;
    }
  return -1;
}

static int
EntFindOrder (Entity * ent)
{
  int ret;

  while (ent != NULL)
    {
      if (ent->type == et_splines)
        {
          ret = SPLFindOrder (ent->u.splines.splines);
          if (ret != -1)
            return ret;
        }
      ent = ent->next;
    }
  return -1;
}

int
SFFindOrder (SplineFont *sf)
{
  int i, ret;

  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      {
        ret = SPLFindOrder (sf->glyphs[i]->layers[ly_fore].splines);
        if (ret != -1)
          return ret;
      }
  return 0;
}

static void
SPLSetOrder (SplineSet *ss, int order2)
{
  Spline *s, *first;
  SplinePoint *from, *to;

  while (ss != NULL)
    {
      first = NULL;
      for (s = ss->first->next; s != NULL && s != first; s = s->to->next)
        {
          if (first == NULL)
            first = s;
          if (s->order2 != order2)
            {
              if (s->knownlinear)
                {
                  s->from->nextcp = s->from->me;
                  s->to->prevcp = s->to->me;
                  s->from->nonextcp = s->to->noprevcp = true;
                  s->order2 = order2;
                }
              else if (order2)
                {
                  from = SplineTtfApprox (s);
                  s->from->nextcp = from->nextcp;
                  s->from->nonextcp = from->nonextcp;
                  s->from->next = from->next;
                  from->next->from = s->from;
                  SplinePointFree (from);
                  for (to = s->from->next->to; to->next != NULL;
                       to = to->next->to);
                  s->to->prevcp = to->prevcp;
                  s->to->noprevcp = to->noprevcp;
                  s->to->prev = to->prev;
                  to->prev->to = s->to;
                  SplinePointFree (to);
                  to = s->to;
                  from = s->from;
                  SplineFree (s);
                  if (first == s)
                    first = from->next;
                  s = to->prev;
                }
              else
                {
                  s->from->nextcp.x = s->splines[0].c / 3 + s->from->me.x;
                  s->from->nextcp.y = s->splines[1].c / 3 + s->from->me.y;
                  s->to->prevcp.x =
                    s->from->nextcp.x + (s->splines[0].b + s->splines[0].c) / 3;
                  s->to->prevcp.y =
                    s->from->nextcp.y + (s->splines[1].b + s->splines[1].c) / 3;
                  s->order2 = false;
                  SplineRefigure (s);
                }
            }
        }
      ss = ss->next;
    }
}

static void
EntSetOrder (Entity * ent, int order2)
{
  while (ent != NULL)
    {
      if (ent->type == et_splines)
        SPLSetOrder (ent->u.splines.splines, order2);
      SPLSetOrder (ent->clippath, order2);
      ent = ent->next;
    }
}

void
SFSetOrder (SplineFont *sf, int order2)
{
  int i, j;

  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      {
        for (j = ly_fore; j < sf->glyphs[i]->layer_cnt; ++j)
          {
            SPLSetOrder (sf->glyphs[i]->layers[j].splines, order2);
            sf->glyphs[i]->layers[j].order2 = order2;
          }
      }
}

Entity *
EntityInterpretSVG (char *filename, char *memory, int memlen, int em_size,
                    int ascent)
{
  xmlDocPtr doc;
  xmlNodePtr top;
  Entity *ret;
  int order2;

  if (filename != NULL)
    doc = xmlParseFile (filename);
  else
    doc = xmlParseMemory (memory, memlen);
  if (doc == NULL)
    {
      /* Can I get an error message from libxml???? */
      return NULL;
    }

  top = xmlDocGetRootElement (doc);
  if (xmlStrcmp (top->name, "svg") != 0)
    {
      LogError (_("%s does not contain an <svg> element at the top\n"),
                filename);
      xmlFreeDoc (doc);
      return NULL;
    }

  ret = SVGParseSVG (top, em_size, ascent);
  xmlFreeDoc (doc);

  if (loaded_fonts_same_as_new)
    order2 = new_fonts_are_order2;
  else
    order2 = EntFindOrder (ret);
  if (order2 == -1)
    order2 = 0;
  EntSetOrder (ret, order2);

  return ret;
}

SplineSet *
SplinePointListInterpretSVG (char *filename, char *memory, int memlen,
                             int em_size, int ascent, int is_stroked)
{
  Entity *ret = EntityInterpretSVG (filename, memory, memlen, em_size, ascent);
  int flags = -1;
  return SplinesFromEntities (ret, &flags, is_stroked);
}
