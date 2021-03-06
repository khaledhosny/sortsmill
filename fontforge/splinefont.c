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

#include <stdbool.h>
#include "fontforgevw.h"
#include <utype.h>
#include <ustring.h>
#include <math.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <gfile.h>
#include <time.h>
#include "unicoderange.h"
#include "psfont.h"
#include <locale.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <xalloc.h>
#include <sortsmill/core.h>
#include <canonicalize.h>

void
SFUntickAll (SplineFont *sf)
{
  for (size_t i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      sf->glyphs[i]->ticked = false;
}

void
SFOrderBitmapList (SplineFont *sf)
{
  BDFFont *bdf, *prev, *next;
  BDFFont *bdf2, *prev2;

  for (prev = NULL, bdf = sf->bitmaps; bdf != NULL; bdf = bdf->next)
    {
      for (prev2 = NULL, bdf2 = bdf->next; bdf2 != NULL; bdf2 = bdf2->next)
        {
          if (bdf->pixelsize > bdf2->pixelsize ||
              (bdf->pixelsize == bdf2->pixelsize
               && BDFDepth (bdf) > BDFDepth (bdf2)))
            {
              if (prev == NULL)
                sf->bitmaps = bdf2;
              else
                prev->next = bdf2;
              if (prev2 == NULL)
                {
                  bdf->next = bdf2->next;
                  bdf2->next = bdf;
                }
              else
                {
                  next = bdf->next;
                  bdf->next = bdf2->next;
                  bdf2->next = next;
                  prev2->next = bdf;
                }
              next = bdf;
              bdf = bdf2;
              bdf2 = next;
            }
          prev2 = bdf2;
        }
      prev = bdf;
    }
}

SplineChar *
SCBuildDummy (SplineChar *dummy, SplineFont *sf, EncMap *map, int i)
{
  static char namebuf[100];
  static Layer layers[2];

  memset (dummy, '\0', sizeof (*dummy));
  dummy->color = COLOR_DEFAULT;
  dummy->layer_cnt = 2;
  dummy->layers = layers;
  if (sf->cidmaster != NULL)
    {
      /* CID fonts don't have encodings, instead we must look up the cid */
      if (sf->cidmaster->loading_cid_map)
        dummy->unicodeenc = -1;
      else
        dummy->unicodeenc =
          CID2NameUni (FindCidMap
                       (sf->cidmaster->cidregistry, sf->cidmaster->ordering,
                        sf->cidmaster->supplement, sf->cidmaster), i, namebuf,
                       sizeof (namebuf));
    }
  else
    dummy->unicodeenc = UniFromEnc (i, map->enc);

  if (sf->cidmaster != NULL)
    dummy->name = namebuf;
  else if (map->enc->psnames != NULL && i < map->enc->char_cnt &&
           map->enc->psnames[i] != NULL)
    dummy->name = map->enc->psnames[i];
  else if (dummy->unicodeenc == -1)
    dummy->name = NULL;
  else
    dummy->name = (char *) StdGlyphName (namebuf,
                                         dummy->unicodeenc,
                                         sf->uni_interp, sf->for_new_glyphs);
  if (dummy->name == NULL)
    {
      /*if ( dummy->unicodeenc!=-1 || i<256 )
         dummy->name = ".notdef";
         else */
      {
        int j;
        sprintf (namebuf, "NameMe.%d", i);
        j = 0;
        while (SFFindExistingSlot (sf, -1, namebuf) != -1)
          sprintf (namebuf, "NameMe.%d.%d", i, ++j);
        dummy->name = namebuf;
      }
    }
  dummy->width = dummy->vwidth = sf->ascent + sf->descent;
  if (dummy->unicodeenc > 0 && dummy->unicodeenc < 0x10000 &&
      iscombining (dummy->unicodeenc))
    {
      /* Mark characters should be 0 width */
      dummy->width = 0;
      /* Except in monospaced fonts on windows, where they should be the */
      /*  same width as everything else */
    }
  /* Actually, in a monospace font, all glyphs should be the same width */
  /*  whether mark or not */
  if (sf->pfminfo.panose_set && sf->pfminfo.panose[3] == 9 && sf->glyphcnt > 0)
    {
      for (i = sf->glyphcnt - 1; i >= 0; --i)
        if (SCWorthOutputting (sf->glyphs[i]))
          {
            dummy->width = sf->glyphs[i]->width;
            break;
          }
    }
  dummy->parent = sf;
  dummy->orig_pos = 0xffff;
  return dummy;
}

static SplineChar *
_SFMakeChar (SplineFont *sf, EncMap *map, int enc)
{
  SplineChar dummy, *sc;
  SplineFont *ssf;
  int j, real_uni, gid;
  extern const int cns14pua[], amspua[];

  if (enc >= map->enc_limit)
    gid = -1;
  else
    gid = enc_to_gid (map, enc);
  if (sf->subfontcnt != 0 && gid != -1)
    {
      ssf = NULL;
      for (j = 0; j < sf->subfontcnt; ++j)
        if (gid < sf->subfonts[j]->glyphcnt)
          {
            ssf = sf->subfonts[j];
            if (ssf->glyphs[gid] != NULL)
              {
                return ssf->glyphs[gid];
              }
          }
      sf = ssf;
    }

  if (gid == -1 || (sc = sf->glyphs[gid]) == NULL)
    {
      if ((map->enc->is_unicodebmp || map->enc->is_unicodefull) &&
          (enc >= 0xe000 && enc <= 0xf8ff) &&
          (sf->uni_interp == ui_ams || sf->uni_interp == ui_trad_chinese) &&
          (real_uni =
           (sf->uni_interp == ui_ams ? amspua : cns14pua)[enc - 0xe000]) != 0)
        {
          if (real_uni < map->enc_limit)
            {
              SplineChar *sc;
              /* if necessary, create the real unicode code point */
              /*  and then make us be a duplicate of it */
              sc = _SFMakeChar (sf, map, real_uni);
              gid = sc->orig_pos;
              set_enc_to_gid (map, enc, gid);
              SCCharChangedUpdate (sc, ly_all);
              return sc;
            }
        }

      SCBuildDummy (&dummy, sf, map, enc);
      /* Let's say a user has a postscript encoding where the glyph ".notdef" */
      /*  is assigned to many slots. Once the user creates a .notdef glyph */
      /*  all those slots should fill in. If they don't they damn well better */
      /*  when the user clicks on one to edit it */
      /* Used to do that with all encodings. It just confused people */
      if (map->enc->psnames != NULL &&
          (sc = SFGetChar (sf, dummy.unicodeenc, dummy.name)) != NULL)
        {
          set_enc_to_gid (map, enc, sc->orig_pos);
          AltUniAdd (sc, dummy.unicodeenc);
          return sc;
        }
      sc = SFSplineCharCreate (sf);
      sc->unicodeenc = dummy.unicodeenc;
      sc->name = xstrdup_or_null (dummy.name);
      sc->width = dummy.width;
      sc->orig_pos = 0xffff;
      if (sf->cidmaster != NULL)
        sc->altuni =
          CIDSetAltUnis (FindCidMap
                         (sf->cidmaster->cidregistry, sf->cidmaster->ordering,
                          sf->cidmaster->supplement, sf->cidmaster), enc);
      /*SCLigDefault(sc); */
      SFAddGlyphAndEncode (sf, sc, map, enc);
    }
  return sc;
}

SplineChar *
SFMakeChar (SplineFont *sf, EncMap *map, int enc)
{
  int gid;

  if (enc == -1)
    return NULL;
  if (enc >= map->enc_limit)
    gid = -1;
  else
    gid = enc_to_gid (map, enc);
  if (sf->mm != NULL && (gid == -1 || sf->glyphs[gid] == NULL))
    {
      int j;
      _SFMakeChar (sf->mm->normal, map, enc);
      for (j = 0; j < sf->mm->instance_count; ++j)
        _SFMakeChar (sf->mm->instances[j], map, enc);
    }
  return _SFMakeChar (sf, map, enc);
}

struct unicoderange specialnames[] = {
  UNICODERANGE_EMPTY
};

int
NameToEncoding (SplineFont *sf, EncMap *map, const char *name)
{
  int enc, uni, i, ch;
  char *end, *freeme = NULL;
  const char *upt = name;

  ch = u8_get_next ((const uint8_t **) &upt);
  if (*upt == '\0')
    {
      enc = SFFindSlot (sf, map, ch, NULL);
      if (enc != -1)
        return enc;
    }

  enc = uni = -1;

  enc = SFFindSlot (sf, map, -1, name);
  if (enc != -1)
    {
      free (freeme);
      return enc;
    }
  if ((*name == 'U' || *name == 'u') && name[1] == '+')
    {
      uni = strtol (name + 2, &end, 16);
      if (*end != '\0')
        uni = -1;
    }
  else if (name[0] == 'u' && name[1] == 'n' && name[2] == 'i')
    {
      uni = strtol (name + 3, &end, 16);
      if (*end != '\0')
        uni = -1;
    }
  else if (name[0] == 'g' && name[1] == 'l' && name[2] == 'y' && name[3] == 'p'
           && name[4] == 'h')
    {
      int orig = strtol (name + 5, &end, 10);
      if (*end != '\0')
        orig = -1;
      if (orig != -1)
        enc = gid_to_enc (map, orig);
    }
  else if (isdigit (*name))
    {
      enc = strtoul (name, &end, 0);
      if (*end != '\0')
        enc = -1;
      if (map->remap != NULL && enc != -1)
        {
          struct remap *remap = map->remap;
          while (remap->infont != -1)
            {
              if (enc >= remap->firstenc && enc <= remap->lastenc)
                {
                  enc += remap->infont - remap->firstenc;
                  break;
                }
              ++remap;
            }
        }
    }
  else
    {
      if (enc == -1)
        {
          uni = UniFromName (name, sf->uni_interp, map->enc);
          if (uni < 0)
            {
              for (i = 0; specialnames[i].name != NULL; ++i)
                if (strcmp (name, specialnames[i].name) == 0)
                  {
                    uni = specialnames[i].first;
                    break;
                  }
            }
          if (uni < 0 && name[1] == '\0')
            uni = name[0];
        }
    }
  if (enc >= map->enc_limit || enc < 0)
    enc = -1;
  if (enc == -1 && uni != -1)
    enc = SFFindSlot (sf, map, uni, NULL);
  /* Used to have code to remove dotted variant names and apply extensions */
  /*  like ".initial" to get the unicode for arabic init/medial/final variants */
  /*  But that doesn't sound like a good idea. And it would also map "a.sc" */
  /*  to "a" -- which was confusing */
  return enc;
}

void
SFRemoveUndoes (SplineFont *sf, uint8_t *selected, EncMap *map)
{
  SplineFont *main = sf->cidmaster ? sf->cidmaster : sf, *ssf;
  int i, k, max, layer, gid;
  SplineChar *sc;
  BDFFont *bdf;

  if (selected != NULL || main->subfontcnt == 0)
    max = sf->glyphcnt;
  else
    {
      max = 0;
      for (k = 0; k < main->subfontcnt; ++k)
        if (main->subfonts[k]->glyphcnt > max)
          max = main->subfonts[k]->glyphcnt;
    }
  for (i = 0;; ++i)
    {
      if (selected == NULL || main->subfontcnt != 0)
        {
          if (i >= max)
            break;
          gid = i;
        }
      else
        {
          if (i >= map->enc_limit)
            break;
          if (!selected[i])
            continue;
          gid = enc_to_gid (map, i);
          if (gid == -1)
            continue;
        }
      for (bdf = main->bitmaps; bdf != NULL; bdf = bdf->next)
        {
          if (bdf->glyphs[gid] != NULL)
            {
              UndoesFree (bdf->glyphs[gid]->undoes);
              bdf->glyphs[gid]->undoes = NULL;
              UndoesFree (bdf->glyphs[gid]->redoes);
              bdf->glyphs[gid]->redoes = NULL;
            }
        }
      k = 0;
      do
        {
          ssf = main->subfontcnt == 0 ? main : main->subfonts[k];
          if (gid < ssf->glyphcnt && ssf->glyphs[gid] != NULL)
            {
              sc = ssf->glyphs[gid];
              for (layer = 0; layer < sc->layer_cnt; ++layer)
                {
                  UndoesFree (sc->layers[layer].undoes);
                  sc->layers[layer].undoes = NULL;
                  UndoesFree (sc->layers[layer].redoes);
                  sc->layers[layer].redoes = NULL;
                }
            }
          ++k;
        }
      while (k < main->subfontcnt);
    }
}

static void
_SplineFontSetUnChanged (SplineFont *sf)
{
  int i;
  int was = sf->changed;
  BDFFont *bdf;

  sf->changed = false;
  SFClearAutoSave (sf);
  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      if (sf->glyphs[i]->changed)
        {
          sf->glyphs[i]->changed = false;
          SCRefreshTitles (sf->glyphs[i]);
        }
  for (bdf = sf->bitmaps; bdf != NULL; bdf = bdf->next)
    for (i = 0; i < bdf->glyphcnt; ++i)
      if (bdf->glyphs[i] != NULL)
        bdf->glyphs[i]->changed = false;
  if (was)
    FVRefreshAll (sf);
  if (was)
    FVSetTitles (sf);
  for (i = 0; i < sf->subfontcnt; ++i)
    _SplineFontSetUnChanged (sf->subfonts[i]);
}

void
SplineFontSetUnChanged (SplineFont *sf)
{
  int i;

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;
  if (sf->mm != NULL)
    sf = sf->mm->normal;
  _SplineFontSetUnChanged (sf);
  if (sf->mm != NULL)
    for (i = 0; i < sf->mm->instance_count; ++i)
      _SplineFontSetUnChanged (sf->mm->instances[i]);
}

static char *
scaleString (const char *string, double scale)
{
  char *result;
  char *pt;
  char *end;
  double val;

  if (string == NULL)
    return NULL;

  while (*string == ' ')
    ++string;
  result = xmalloc (10 * strlen (string) + 1);
  if (*string != '[')
    {
      val = strtod (string, &end);
      if (end == string)
        {
          free (result);
          return NULL;
        }
      sprintf (result, "%g", val * scale);
      return result;
    }

  pt = result;
  *pt++ = '[';
  ++string;
  while (*string != '\0' && *string != ']')
    {
      val = strtod (string, &end);
      if (end == string)
        {
          free (result);
          return NULL;
        }
      sprintf (pt, "%g ", val * scale);
      pt += strlen (pt);
      string = end;
    }
  if (pt[-1] == ' ')
    pt[-1] = ']';
  else
    *pt++ = ']';
  *pt = '\0';
  return result;
}

static char *
iscaleString (const char *string, double scale)
{
  char *result;
  char *pt;
  char *end;
  double val;

  if (string == NULL)
    return NULL;

  while (*string == ' ')
    ++string;
  result = xmalloc (10 * strlen (string) + 1);
  if (*string != '[')
    {
      val = strtod (string, &end);
      if (end == string)
        {
          free (result);
          return NULL;
        }
      sprintf (result, "%g", rint (val * scale));
      return result;
    }

  pt = result;
  *pt++ = '[';
  ++string;
  while (*string != '\0' && *string != ']')
    {
      val = strtod (string, &end);
      if (end == string)
        {
          free (result);
          return NULL;
        }
      sprintf (pt, "%g ", rint (val * scale));
      pt += strlen (pt);
      string = end;
      while (*string == ' ')
        ++string;
    }
  if (pt[-1] == ' ')
    pt[-1] = ']';
  else
    *pt++ = ']';
  *pt = '\0';
  return result;
}

static void
SFScalePrivate (SplineFont *sf, double scale)
{
  static char *scalethese[] = {
    NULL
  };
  static char *integerscalethese[] = {
    "BlueValues", "OtherBlues",
    "FamilyBlues", "FamilyOtherBlues",
    "BlueShift", "BlueFuzz",
    "StdHW", "StdVW", "StemSnapH", "StemSnapV",
    NULL
  };
  int i;

  for (i = 0; integerscalethese[i] != NULL; ++i)
    {
      const char *str = PSDictHasEntry (sf->private, integerscalethese[i]);
      char *new = iscaleString (str, scale);
      if (new != NULL)
        PSDictChangeEntry (sf->private, integerscalethese[i], new);
      free (new);
    }
  for (i = 0; scalethese[i] != NULL; ++i)
    {
      const char *str = PSDictHasEntry (sf->private, scalethese[i]);
      char *new = scaleString (str, scale);
      if (new != NULL)
        PSDictChangeEntry (sf->private, scalethese[i], new);
      free (new);
    }
}

static void
ScaleBase (struct Base *base, double scale)
{
  struct basescript *bs;
  struct baselangextent *bl, *feat;
  int i;

  for (bs = base->scripts; bs != NULL; bs = bs->next)
    {
      for (i = 0; i < base->baseline_cnt; ++i)
        bs->baseline_pos[i] = (int) rint (bs->baseline_pos[i] * scale);
      for (bl = bs->langs; bl != NULL; bl = bl->next)
        {
          bl->ascent = (int) rint (scale * bl->ascent);
          bl->descent = (int) rint (scale * bl->descent);
          for (feat = bl->features; feat != NULL; feat = feat->next)
            {
              feat->ascent = (int) rint (scale * feat->ascent);
              feat->descent = (int) rint (scale * feat->descent);
            }
        }
    }
}

int
SFScaleToEm (SplineFont *sf, int as, int des)
{
  bigreal scale;
  real transform[6];
  BVTFunc bvts;
  uint8_t *oldselected = sf->fv->selected;

  scale = (as + des) / (bigreal) (sf->ascent + sf->descent);
  sf->pfminfo.hhead_ascent = rint (sf->pfminfo.hhead_ascent * scale);
  sf->pfminfo.hhead_descent = rint (sf->pfminfo.hhead_descent * scale);
  sf->pfminfo.linegap = rint (sf->pfminfo.linegap * scale);
  sf->pfminfo.vlinegap = rint (sf->pfminfo.vlinegap * scale);
  sf->pfminfo.os2_winascent = rint (sf->pfminfo.os2_winascent * scale);
  sf->pfminfo.os2_windescent = rint (sf->pfminfo.os2_windescent * scale);
  sf->pfminfo.os2_typoascent = rint (sf->pfminfo.os2_typoascent * scale);
  sf->pfminfo.os2_typodescent = rint (sf->pfminfo.os2_typodescent * scale);
  sf->pfminfo.os2_typolinegap = rint (sf->pfminfo.os2_typolinegap * scale);

  sf->pfminfo.os2_subxsize = rint (sf->pfminfo.os2_subxsize * scale);
  sf->pfminfo.os2_subysize = rint (sf->pfminfo.os2_subysize * scale);
  sf->pfminfo.os2_subxoff = rint (sf->pfminfo.os2_subxoff * scale);
  sf->pfminfo.os2_subyoff = rint (sf->pfminfo.os2_subyoff * scale);
  sf->pfminfo.os2_supxsize = rint (sf->pfminfo.os2_supxsize * scale);
  sf->pfminfo.os2_supysize = rint (sf->pfminfo.os2_supysize * scale);
  sf->pfminfo.os2_supxoff = rint (sf->pfminfo.os2_supxoff * scale);
  sf->pfminfo.os2_supyoff = rint (sf->pfminfo.os2_supyoff * scale);
  sf->pfminfo.os2_strikeysize = rint (sf->pfminfo.os2_strikeysize * scale);
  sf->pfminfo.os2_strikeypos = rint (sf->pfminfo.os2_strikeypos * scale);
  sf->upos *= scale;
  sf->uwidth *= scale;

  if (sf->private != NULL)
    SFScalePrivate (sf, scale);
  if (sf->horiz_base != NULL)
    ScaleBase (sf->horiz_base, scale);
  if (sf->vert_base != NULL)
    ScaleBase (sf->vert_base, scale);

  if (as + des == sf->ascent + sf->descent)
    {
      if (as != sf->ascent && des != sf->descent)
        {
          sf->ascent = as;
          sf->descent = des;
          sf->changed = true;
        }
      return false;
    }

  transform[0] = transform[3] = scale;
  transform[1] = transform[2] = transform[4] = transform[5] = 0;
  bvts.func = bvt_none;
  sf->fv->selected = xmalloc (sf->fv->map->enc_limit);
  memset (sf->fv->selected, 1, sf->fv->map->enc_limit);

  sf->ascent = as;
  sf->descent = des;

  FVTransFunc (sf->fv, transform, 0, &bvts,
               fvt_alllayers | fvt_round_to_int | fvt_dontsetwidth |
               fvt_scalekernclasses | fvt_scalepstpos | fvt_dogrid);
  free (sf->fv->selected);
  sf->fv->selected = oldselected;

  if (!sf->changed)
    {
      sf->changed = true;
      FVSetTitles (sf);
    }

  return true;
}

void
SFSetModTime (SplineFont *sf)
{
  time_t now;
  time (&now);
  sf->modificationtime = now;
}

static SplineFont *
_SFReadPostScript (FILE *file, char *filename)
{
  FontDict *fd = NULL;
  SplineFont *sf = NULL;

  ff_progress_change_stages (2);
  fd = _ReadPSFont (file);
  ff_progress_next_stage ();
  ff_progress_change_line2 (_("Interpreting Glyphs"));
  if (fd != NULL)
    {
      sf = SplineFontFromPSFont (fd);
      PSFontFree (fd);
      if (sf != NULL)
        CheckAfmOfPostScript (sf, filename, sf->map);
    }
  return sf;
}

static SplineFont *
SFReadPostScript (char *filename)
{
  FontDict *fd = NULL;
  SplineFont *sf = NULL;

  ff_progress_change_stages (2);
  fd = ReadPSFont (filename);
  ff_progress_next_stage ();
  ff_progress_change_line2 (_("Interpreting Glyphs"));
  if (fd != NULL)
    {
      sf = SplineFontFromPSFont (fd);
      PSFontFree (fd);
      if (sf != NULL)
        CheckAfmOfPostScript (sf, filename, sf->map);
    }
  return sf;
}

static char *
ForceFileToHaveName (FILE *file, char *exten)
{
  char tmpfilename[L_tmpnam + 100];
  static int try = 0;
  FILE *newfile;

  while (true)
    {
      sprintf (tmpfilename, P_tmpdir "/fontforge%jd-%d", (intmax_t) getpid (),
               try++);
      if (exten != NULL)
        strcat (tmpfilename, exten);
      if (access (tmpfilename, F_OK) == -1 &&
          (newfile = fopen (tmpfilename, "w")) != NULL)
        {
          char buffer[1024];
          int len;
          while ((len = fread (buffer, 1, sizeof (buffer), file)) > 0)
            fwrite (buffer, 1, len, newfile);
          fclose (newfile);
        }
      return xstrdup_or_null (tmpfilename);     /* The filename does not exist */
    }
}

/* This does not check currently existing fontviews, and should only be used */
/*  by LoadSplineFont (which does) and by RevertFile (which knows what it's doing) */
SplineFont *
_ReadSplineFont (FILE *file, const char *const_filename,
                 enum openflags openflags)
{
  SplineFont *sf;
  char ubuf[250];
  int fromsfd = false;
  char *pt = NULL;
  char *strippedname = NULL;
  char *oldstrippedname = NULL;
  char *tmpfile = NULL;
  char *paren = NULL;
  char *rparen;
  int len;
  int checked;
  int wasurl = false, nowlocal = true;

  if (const_filename == NULL)
    return NULL;

  char *filename = x_gc_strdup (const_filename);
  char *fullname = filename;

  strippedname = filename;
  pt = strrchr (filename, '/');
  if (pt == NULL)
    pt = filename;
  /* Someone gave me a font "Nafees Nastaleeq(Updated).ttf" and complained */
  /*  that ff wouldn't open it */
  /* Now someone will complain about "Nafees(Updated).ttc(fo(ob)ar)" */
  if ((paren = strrchr (pt, '(')) != NULL &&
      (rparen = strrchr (paren, ')')) != NULL && rparen[1] == '\0')
    {
      strippedname = xstrdup_or_null (filename);
      strippedname[paren - filename] = '\0';
    }

  if (strstr (strippedname, "://") != NULL)
    {
      if (file == NULL)
        file = URLToTempFile (strippedname, NULL);
      if (file == NULL)
        return NULL;
      wasurl = true;
      nowlocal = false;
    }

  pt = strrchr (strippedname, '.');
  oldstrippedname = strippedname;

  /* If there are no pfaedit windows, give them something to look at */
  /*  immediately. Otherwise delay a bit */
  strcpy (ubuf, _("Loading font from "));
  len = strlen (ubuf);
  if (!wasurl)       /* If it wasn't a url, then the fullname is reasonable, else use the original name */
    strncat (ubuf, x_gc_u8_strconv_from_locale (GFileBaseName (fullname)), 100);
  else
    strncat (ubuf, x_gc_u8_strconv_from_locale (GFileBaseName (filename)), 100);
  ubuf[100 + len] = '\0';
  ff_progress_start_indicator (FontViewFirst () == NULL ? 0 : 10,
                               _("Loading..."), ubuf, _("Reading Glyphs"), 0, 1,
                               false);
  if (FontViewFirst () == NULL && !no_windowing_ui)
    ff_progress_allow_events ();

  if (file == NULL)
    {
      file = fopen (strippedname, "rb");
      nowlocal = true;
    }

  sf = NULL;
  checked = false;
/* checked == false => not checked */
/* checked == 't'   => TTF/OTF */
/* checked == 'p'   => pfb/general postscript */
/* checked == 'c'   => cff */
/* checked == 'S'   => svg */
/* checked == 'f'   => sfd */
/* checked == 'F'   => sfdir */
/* checked == 'b'   => bdf */
/* checked == 'i'   => ikarus */
  if (!wasurl && GFileIsDir (strippedname))
    {
      char *temp =
        xmalloc (strlen (strippedname) + strlen ("/font.props") + 1);
      strcpy (temp, strippedname);
      strcat (temp, "/font.props");
      if (GFileExists (temp))
        {
          sf = SFDirRead (strippedname);
          checked = 'F';
        }
      free (temp);
      if (file != NULL)
        fclose (file);
    }
  else if (file != NULL)
    {
      /* Try to guess the file type from the first few characters... */
      int ch1 = getc (file);
      int ch2 = getc (file);
      int ch3 = getc (file);
      int ch4 = getc (file);
      int ch5 = getc (file);
      int ch6 = getc (file);
      int ch7 = getc (file);
      int ch9, ch10;
      fseek (file, 98, SEEK_SET);
      ch9 = getc (file);
      ch10 = getc (file);
      rewind (file);
      if ((ch1 == 0 && ch2 == 1 && ch3 == 0 && ch4 == 0) ||
          (ch1 == 'O' && ch2 == 'T' && ch3 == 'T' && ch4 == 'O') ||
          (ch1 == 't' && ch2 == 'r' && ch3 == 'u' && ch4 == 'e') ||
          (ch1 == 't' && ch2 == 't' && ch3 == 'c' && ch4 == 'f'))
        {
          sf = _SFReadTTF (file, 0, openflags, fullname, NULL);
          checked = 't';
        }
      else if ((ch1 == '%' && ch2 == '!') || (ch1 == 0x80 && ch2 == '\01'))
        {                       /* PFB header */
          sf = _SFReadPostScript (file, fullname);
          checked = 'p';
        }
      else if (ch1 == 1 && ch2 == 0 && ch3 == 4)
        {
          int len;
          fseek (file, 0, SEEK_END);
          len = ftell (file);
          fseek (file, 0, SEEK_SET);
          sf = _CFFParse (file, len, NULL);
          checked = 'c';
        }
      else
        if ((ch1 == '<' && ch2 == '?' && (ch3 == 'x' || ch3 == 'X')
             && (ch4 == 'm' || ch4 == 'M')) ||
            /* or UTF-8 SVG with initial byte ordering mark */
            ((ch1 == 0xef && ch2 == 0xbb && ch3 == 0xbf &&
              ch4 == '<' && ch5 == '?' && (ch6 == 'x' || ch6 == 'X')
              && (ch7 == 'm' || ch7 == 'M'))))
        {
          if (nowlocal)
            sf = SFReadSVG (fullname, 0);
          else
            {
              char *spuriousname = ForceFileToHaveName (file, NULL);
              sf = SFReadSVG (spuriousname, 0);
              unlink (spuriousname);
              free (spuriousname);
            }
          checked = 'S';
#if 0                           /* I'm not sure if this is a good test for mf files... */
        }
      else if (ch1 == '%' && ch2 == ' ')
        {
          sf = SFFromMF (fullname);
#endif
        }
      else if (ch1 == 'S' && ch2 == 'p' && ch3 == 'l' && ch4 == 'i')
        {
          sf = _SFDRead (fullname, file);
          file = NULL;
          checked = 'f';
          fromsfd = true;
        }
      else if (ch1 == 'S' && ch2 == 'T' && ch3 == 'A' && ch4 == 'R')
        {
          sf = SFFromBDF (fullname, 0, false);
          checked = 'b';
        }
      else if (ch1 == '\1' && ch2 == 'f' && ch3 == 'c' && ch4 == 'p')
        {
          sf = SFFromBDF (fullname, 2, false);
        }
      else if (ch9 == 'I' && ch10 == 'K' && ch3 == 0 && ch4 == 55)
        {
          /* Ikarus font type appears at word 50 (byte offset 98) */
          /* Ikarus name section length (at word 2, byte offset 2) was 55 in the 80s at URW */
          checked = 'i';
          sf = SFReadIkarus (fullname);
        }                       /* Too hard to figure out a valid mark for a mac resource file */
      if (file != NULL)
        fclose (file);
    }

  if (sf != NULL)
    /* good */ ;
  else if ((strcasecmp (fullname + strlen (fullname) - 4, ".sfd") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 5, ".sfd~") == 0)
           && checked != 'f')
    {
      sf = SFDRead (fullname);
      fromsfd = true;
    }
  else if ((strcasecmp (fullname + strlen (fullname) - 4, ".ttf") == 0 ||
            strcasecmp (fullname + strlen (strippedname) - 4, ".ttc") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".gai") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".otf") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".otb") == 0)
           && checked != 't')
    {
      sf = SFReadTTF (fullname, 0, openflags);
    }
  else if (strcasecmp (fullname + strlen (strippedname) - 4, ".svg") == 0
           && checked != 'S')
    {
      sf = SFReadSVG (fullname, 0);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 4, ".bdf") == 0
           && checked != 'b')
    {
      sf = SFFromBDF (fullname, 0, false);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 2, "pk") == 0)
    {
      sf = SFFromBDF (fullname, 1, true);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 2, "gf") == 0)
    {
      sf = SFFromBDF (fullname, 3, true);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 4, ".pcf") == 0 ||
           strcasecmp (fullname + strlen (fullname) - 4, ".pmf") == 0)
    {
      /* Sun seems to use a variant of the pcf format which they call pmf */
      /*  the encoding actually starts at 0x2000 and the one I examined was */
      /*  for a pixel size of 200. Some sort of printer font? */
      sf = SFFromBDF (fullname, 2, false);
    }
  else if (strcasecmp (fullname + strlen (strippedname) - 4, ".bin") == 0 ||
           strcasecmp (fullname + strlen (strippedname) - 4, ".hqx") == 0 ||
           strcasecmp (fullname + strlen (strippedname) - 6, ".dfont") == 0)
    {
      sf = SFReadMacBinary (fullname, 0, openflags);
    }
  else if (strcasecmp (fullname + strlen (strippedname) - 4, ".fon") == 0 ||
           strcasecmp (fullname + strlen (strippedname) - 4, ".fnt") == 0)
    {
      sf = SFReadWinFON (fullname, 0);
    }
  else if (strcasecmp (fullname + strlen (strippedname) - 4, ".pdb") == 0)
    {
      sf = SFReadPalmPdb (fullname, 0);
    }
  else if ((strcasecmp (fullname + strlen (fullname) - 4, ".pfa") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".pfb") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".pf3") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".cid") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".gsf") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 4, ".pt3") == 0 ||
            strcasecmp (fullname + strlen (fullname) - 3, ".ps") == 0)
           && checked != 'p')
    {
      sf = SFReadPostScript (fullname);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 4, ".cff") == 0
           && checked != 'c')
    {
      sf = CFFParse (fullname);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 3, ".mf") == 0)
    {
      sf = SFFromMF (fullname);
    }
  else if (strcasecmp (fullname + strlen (fullname) - 3, ".ik") == 0
           && checked != 'i')
    {
      sf = SFReadIkarus (fullname);
    }
  else
    {
      sf = SFReadMacBinary (fullname, 0, openflags);
    }
  ff_progress_end_indicator ();

  if (sf != NULL)
    {
      SplineFont *norm = sf->mm != NULL ? sf->mm->normal : sf;
      free (norm->origname);
      if (sf->chosenname != NULL && strippedname == filename)
        {
          norm->origname =
            xmalloc (strlen (filename) + strlen (sf->chosenname) + 8);
          strcpy (norm->origname, filename);
          strcat (norm->origname, "(");
          strcat (norm->origname, sf->chosenname);
          strcat (norm->origname, ")");
        }
      else
        norm->origname = xstrdup_or_null (filename);
      free (norm->chosenname);
      norm->chosenname = NULL;
      if (sf->mm != NULL)
        {
          int j;
          for (j = 0; j < sf->mm->instance_count; ++j)
            {
              free (sf->mm->instances[j]->origname);
              sf->mm->instances[j]->origname = xstrdup_or_null (norm->origname);
            }
        }
    }
  else if (!GFileExists (filename))
    ff_post_error (_("Couldn't open font"),
                   _("The requested file, %.100s, does not exist"),
                   GFileBaseName (filename));
  else if (!GFileReadable (filename))
    ff_post_error (_("Couldn't open font"),
                   _("You do not have permission to read %.100s"),
                   GFileBaseName (filename));
  else
    ff_post_error (_("Couldn't open font"),
                   _("%.100s is not in a known format (or uses features of "
                     "that format fontforge does not support, or is so badly "
                     "corrupted as to be unreadable)"),
                   GFileBaseName (filename));

  if (oldstrippedname != filename)
    free (oldstrippedname);
  if (fullname != filename && fullname != strippedname)
    free (fullname);
  if (tmpfile != NULL)
    {
      unlink (tmpfile);
      free (tmpfile);
    }
  if ((openflags & of_fstypepermitted) && sf != NULL
      && (sf->pfminfo.fstype & 0xff) == 0x0002)
    {
      /* Ok, they have told us from a script they have access to the font */
    }
  else if (!fromsfd && sf != NULL && (sf->pfminfo.fstype & 0xff) == 0x0002)
    {
      char *buts[3];
      buts[0] = _("_Yes");
      buts[1] = _("_No");
      buts[2] = NULL;
      if (ff_ask
          (_("Restricted Font"), (const char **) buts, 1, 1,
           _("This font is marked with an FSType of 2 (Restricted\n"
             "License). That means it is not editable without the\n"
             "permission of the legal owner.\n"
             "\n" "Do you have such permission?")) == 1)
        {
          SplineFontFree (sf);
          return NULL;
        }
    }
  return sf;
}

SplineFont *
ReadSplineFont (const char *filename, enum openflags openflags)
{
  return _ReadSplineFont (NULL, filename, openflags);
}

SplineFont *
LoadSplineFont (const char *filename, enum openflags openflags)
{
  SplineFont *sf;
  const char *pt = NULL;
  char *ept = NULL;
  char *tobefreed1 = NULL;
  char *tobefreed2 = NULL;
  static char *extens[] =
    { ".sfd", ".pfa", ".pfb", ".ttf", ".otf", ".ps", ".cid", ".bin", ".dfont",
    ".PFA", ".PFB", ".TTF", ".OTF", ".PS", ".CID", ".BIN", ".DFONT", NULL
  };
  int i;

  if (filename == NULL)
    return NULL;

  if ((pt = strrchr (filename, '/')) == NULL)
    pt = filename;
  if (strchr (pt, '.') == NULL)
    {
      /* They didn't give an extension. If there's a file with no extension */
      /*  see if it's a valid font file (and if so use the extensionless */
      /*  filename), otherwise guess at an extension */
      /* For some reason Adobe distributes CID keyed fonts (both OTF and */
      /*  postscript) as extensionless files */
      int ok = false;
      FILE *test = fopen (filename, "rb");
      if (test != NULL)
        {
#if 0
          int ch1 = getc (test);
          int ch2 = getc (test);
          int ch3 = getc (test);
          int ch4 = getc (test);
          if (ch1 == '%')
            ok = true;
          else if ((ch1 == 0 && ch2 == 1 && ch3 == 0 && ch4 == 0) ||
                   (ch1 == 0 && ch2 == 2 && ch3 == 0 && ch4 == 0) ||
                   /* Windows 3.1 Chinese version used this version for some arphic fonts */
                   /* See discussion on freetype list, july 2004 */
                   (ch1 == 'O' && ch2 == 'T' && ch3 == 'T' && ch4 == 'O') ||
                   (ch1 == 't' && ch2 == 'r' && ch3 == 'u' && ch4 == 'e') ||
                   (ch1 == 't' && ch2 == 't' && ch3 == 'c' && ch4 == 'f'))
            ok = true;
          else if (ch1 == 'S' && ch2 == 'p' && ch3 == 'l' && ch4 == 'i')
            ok = true;
#endif
          ok = true;            /* Mac resource files are too hard to check for */
          /* If file exists, assume good */
          fclose (test);
        }
      if (!ok)
        {
          tobefreed1 = xmalloc (strlen (filename) + 8);
          strcpy (tobefreed1, filename);
          ept = tobefreed1 + strlen (tobefreed1);
          for (i = 0; extens[i] != NULL; ++i)
            {
              strcpy (ept, extens[i]);
              if (GFileExists (tobefreed1))
                break;
            }
          if (extens[i] != NULL)
            filename = tobefreed1;
          else
            {
              free (tobefreed1);
              tobefreed1 = NULL;
            }
        }
    }
  else
    tobefreed1 = NULL;

  sf = NULL;
  sf = FontWithThisFilename (filename);
  if (sf == NULL && *filename != '/' && strstr (filename, "://") == NULL)
    filename = tobefreed2 =
      XDIE_ON_NULL (canonicalize_filename_mode (filename, CAN_MISSING));

  if (sf == NULL)
    sf = ReadSplineFont (filename, openflags);

  free (tobefreed1);
  free (tobefreed2);
  return sf;
}

/* Use URW 4 letter abbreviations */
const char *knownweights[] = { "Demi", "Bold", "Regu", "Medi", "Book", "Thin",
  "Ligh", "Heav", "Blac", "Ultr", "Nord", "Norm", "Gras", "Stan", "Halb",
  "Fett", "Mage", "Mitt", "Buch", NULL
};

const char *realweights[] =
  { "Demi", "Bold", "Regular", "Medium", "Book", "Thin",
  "Light", "Heavy", "Black", "Ultra", "Nord", "Normal", "Gras", "Standard",
  "Halbfett",
  "Fett", "Mager", "Mittel", "Buchschrift", NULL
};
static const char *moreweights[] = { "ExtraLight", "VeryLight", NULL };
const char **noticeweights[] = { moreweights, realweights, knownweights, NULL };

static const char *modifierlist[] =
  { "Ital", "Obli", "Kursive", "Cursive", "Slanted",
  "Expa", "Cond", NULL
};

static const char *modifierlistfull[] =
  { "Italic", "Oblique", "Kursive", "Cursive", "Slanted",
  "Expanded", "Condensed", NULL
};
static const char **mods[] = { knownweights, modifierlist, NULL };
static const char **fullmods[] = { realweights, modifierlistfull, NULL };


#if 0                           // FIXME: Finish this replacement _GetModifiers() implementation.

// FIXME: Put this somewhere extern and reusable.
static size_t
ascii_match_length (const char *s1, const char *s2)
{
  size_t i = 0;
  while (s1[i] == s2[i])
    i++;
  return i;
}

static char *
modifiers_p (char *fontname, char *familyname)
{
  assert (fontname != NULL && familyname != NULL);

  int i = 0;
  int j = 0;
  while (fontname[i] != '\0' && familyname[j] != '\0')
    {
      size_t match_length = ascii_match_length (fontname + i, familyname + j);
      i += match_length;
      j += match_length;
      if (match_length == 0)
        {
          if (familyname[j] == ' ')
            // Try ignoring a space in the familyname.
            j++;
          else if (fontname[i] == ' ')
            // Try ignoring a space in the fontname.
            i++;
          else if (strchr ("aeiou", familyname[j]) != NULL)
            // Try ignoring a vowel in the familyname.
            j++;
          else
            {
              // Exit the loop, with a mismatch.
              i = strlen (fontname);
              j = strlen (familyname);
            }
        }
    }
  return (fontname[i] != '\0' && familyname[j] == '\0') ? fontname + i : NULL;
}

static bool
is_urw_style_fontname (const char *fontname)
{
  // URW fontnames do not match the familyname; for example:
  // "NimbusSanL-Regu" vs "Nimbus Sans L" (note "San" vs "Sans").
  // So look for a '-' character.
  char *hyphen_p = strchr (fontname, '-');
  return hyphen_p != NULL && *(hyphen_p + 1) != NULL;
}

static char *
urw_modifiers (const char *fontname)
{
  assert (is_urw_style_fontname (fontname));

  return strchr (fontname, '-') + 1;
}

// Search for something like "BoldItalic" in the font name.
static char *
modifiers_within_fontname (const char *fontname, const char *familyname)
{
  char *modifiers = NULL;
  if (is_urw_style_fontname (fontname))
    modifiers = urw_modifiers (fontname);
  else if (familyname != NULL)
    modifiers = modifiers_p (fontname, familyname);
  return modifiers;
}

//char *_GetModifiers(char *fontname, char *familyname, char *weight) {
//     const char *modifiers = modifiers_within_fontname(fontname, familyname);
//     
//}

#endif // FIXME

char *
_GetModifiers (char *fontname, char *familyname, char *weight)
{
  char *pt, *fpt;
  static char space[100];
  int i, j;

  /* URW fontnames don't match the familyname */
  /* "NimbusSanL-Regu" vs "Nimbus Sans L" (note "San" vs "Sans") */
  /* so look for a '-' if there is one and use that as the break point... */

  if ((fpt = strchr (fontname, '-')) != NULL)
    {
      ++fpt;
      if (*fpt == '\0')
        fpt = NULL;
    }
  else if (familyname != NULL)
    {
      for (pt = fontname, fpt = familyname; *fpt != '\0' && *pt != '\0';)
        {
          if (*fpt == *pt)
            {
              ++fpt;
              ++pt;
            }
          else if (*fpt == ' ')
            ++fpt;
          else if (*pt == ' ')
            ++pt;
          else if (*fpt == 'a' || *fpt == 'e' || *fpt == 'i' || *fpt == 'o'
                   || *fpt == 'u')
            ++fpt;              /* allow vowels to be omitted from family when in fontname */
          else
            break;
        }
      if (*fpt == '\0' && *pt != '\0')
        fpt = pt;
      else
        fpt = NULL;
    }

  if (fpt == NULL)
    {
      for (i = 0; mods[i] != NULL; ++i)
        for (j = 0; mods[i][j] != NULL; ++j)
          {
            pt = strstr (fontname, mods[i][j]);
            if (pt != NULL && (fpt == NULL || pt < fpt))
              fpt = pt;
          }
    }
  if (fpt != NULL)
    {
      for (i = 0; mods[i] != NULL; ++i)
        for (j = 0; mods[i][j] != NULL; ++j)
          {
            if (strcmp (fpt, mods[i][j]) == 0)
              {
                strcpy (space, fullmods[i][j]);
                return xstrdup_or_null (space);
              }
          }
      if (strcmp (fpt, "BoldItal") == 0)
        return xstrdup ("BoldItalic");
      else if (strcmp (fpt, "BoldObli") == 0)
        return xstrdup ("BoldOblique");

      return xstrdup_or_null (fpt);
    }

  return xstrdup (weight == NULL || *weight == '\0' ? "Regular" : weight);
}

char *
SFGetModifiers (SplineFont *sf)
{
  return _GetModifiers (sf->fontname, sf->familyname, sf->weight);
}

const uint32_t *
_uGetModifiers (const uint32_t *fontname, const uint32_t *familyname,
                const uint32_t *weight)
{
  const uint32_t *pt, *fpt;
  static uint32_t regular[] = { 'R', 'e', 'g', 'u', 'l', 'a', 'r', 0 };
  static uint32_t space[20];
  int i, j;

  /* URW fontnames don't match the familyname */
  /* "NimbusSanL-Regu" vs "Nimbus Sans L" (note "San" vs "Sans") */
  /* so look for a '-' if there is one and use that as the break point... */

  if ((fpt = u32_strchr (fontname, '-')) != NULL)
    {
      ++fpt;
      if (*fpt == '\0')
        fpt = NULL;
    }
  else if (familyname != NULL)
    {
      for (pt = fontname, fpt = familyname; *fpt != '\0' && *pt != '\0';)
        {
          if (*fpt == *pt)
            {
              ++fpt;
              ++pt;
            }
          else if (*fpt == ' ')
            ++fpt;
          else if (*pt == ' ')
            ++pt;
          else if (*fpt == 'a' || *fpt == 'e' || *fpt == 'i' || *fpt == 'o'
                   || *fpt == 'u')
            ++fpt;              /* allow vowels to be omitted from family when in fontname */
          else
            break;
        }
      if (*fpt == '\0' && *pt != '\0')
        fpt = pt;
      else
        fpt = NULL;
    }

  if (fpt == NULL)
    {
      for (i = 0; mods[i] != NULL; ++i)
        for (j = 0; mods[i][j] != NULL; ++j)
          {
            pt = uc_strstr (fontname, mods[i][j]);
            if (pt != NULL && (fpt == NULL || pt < fpt))
              fpt = pt;
          }
    }

  if (fpt != NULL)
    {
      uint8_t *utf8_fpt = x_gc_u32_to_u8 (u32_force_valid (fpt));

      for (i = 0; mods[i] != NULL; ++i)
        for (j = 0; mods[i][j] != NULL; ++j)
          {
            if (u8_strcmp (utf8_fpt, u8_force_valid (mods[i][j])) == 0)
              {
                u32_strcpy (space, x_gc_u8_to_u32 (fullmods[i][j]));
                return space;
              }
          }
      if (u8_strcmp (utf8_fpt, "BoldItal") == 0)
        {
          u32_strcpy (space, x_gc_u8_to_u32 ("BoldItalic"));
          return space;
        }
      else if (u8_strcmp (utf8_fpt, "BoldObli") == 0)
        {
          u32_strcpy (space, x_gc_u8_to_u32 ("BoldOblique"));
          return space;
        }
      return fpt;
    }

  return weight == NULL || *weight == '\0' ? regular : weight;
}

int
SFIsDuplicatable (SplineFont *sf, SplineChar *sc)
{
  extern const int cns14pua[], amspua[];
  int baseuni = 0;
  const uint32_t *pt;
  const int *pua = NULL;

  if (sf->uni_interp == ui_trad_chinese)
    pua = cns14pua;
  else if (sf->uni_interp == ui_ams)
    pua = amspua;

  if (pua != NULL && sc->unicodeenc >= 0xe000 && sc->unicodeenc <= 0xf8ff)
    baseuni = pua[sc->unicodeenc - 0xe000];
  if (baseuni == 0
      && (pt = SFGetAlternate (sf, sc->unicodeenc, sc, false)) != NULL
      && pt[0] != '\0' && pt[1] == '\0')
    baseuni = pt[0];
  if (baseuni != 0 && SFGetChar (sf, baseuni, NULL) != NULL)
    return true;

  return false;
}

enum flatness
{ mt_flat, mt_round, mt_pointy, mt_unknown };

static bigreal
SPLMaxHeight (SplineSet *spl, enum flatness *isflat)
{
  enum flatness f = mt_unknown;
  bigreal max = -1.0e23;
  Spline *s, *first;
  my_extended ts[2];
  int i;

  for (; spl != NULL; spl = spl->next)
    {
      first = NULL;
      for (s = spl->first->next; s != first && s != NULL; s = s->to->next)
        {
          if (first == NULL)
            first = s;
          if (s->from->me.y >= max ||
              s->to->me.y >= max ||
              s->from->nextcp.y > max || s->to->prevcp.y > max)
            {
              if (!s->knownlinear)
                {
                  if (s->from->me.y > max)
                    {
                      f = mt_round;
                      max = s->from->me.y;
                    }
                  if (s->to->me.y > max)
                    {
                      f = mt_round;
                      max = s->to->me.y;
                    }
                  SplineFindExtrema (&s->splines[1], &ts[0], &ts[1]);
                  for (i = 0; i < 2; ++i)
                    if (ts[i] != -1)
                      {
                        bigreal y =
                          ((s->splines[1].a * ts[i] + s->splines[1].b) * ts[i] +
                           s->splines[1].c) * ts[i] + s->splines[1].d;
                        if (y > max)
                          {
                            f = mt_round;
                            max = y;
                          }
                      }
                }
              else if (s->from->me.y == s->to->me.y)
                {
                  if (s->from->me.y >= max)
                    {
                      max = s->from->me.y;
                      f = mt_flat;
                    }
                }
              else
                {
                  if (s->from->me.y > max)
                    {
                      f = mt_pointy;
                      max = s->from->me.y;
                    }
                  if (s->to->me.y > max)
                    {
                      f = mt_pointy;
                      max = s->to->me.y;
                    }
                }
            }
        }
    }
  *isflat = f;
  return max;
}

static bigreal
SCMaxHeight (SplineChar *sc, int layer, enum flatness *isflat)
{
  /* Find the max height of this layer of the glyph. Also find whether that */
  /* max is flat (as in "z", curved as in "o" or pointy as in "A") */
  enum flatness f = mt_unknown, curf;
  bigreal max = -1.0e23, test;
  RefChar *r;

  max = SPLMaxHeight (sc->layers[layer].splines, &curf);
  f = curf;
  for (r = sc->layers[layer].refs; r != NULL; r = r->next)
    {
      test = SPLMaxHeight (r->layers[0].splines, &curf);
      if (test > max || (test == max && curf == mt_flat))
        {
          max = test;
          f = curf;
        }
    }
  *isflat = f;
  return max;
}

static bigreal
SPLMinHeight (SplineSet *spl, enum flatness *isflat)
{
  enum flatness f = mt_unknown;
  bigreal min = 1.0e23;
  Spline *s, *first;
  my_extended ts[2];
  int i;

  for (; spl != NULL; spl = spl->next)
    {
      first = NULL;
      for (s = spl->first->next; s != first && s != NULL; s = s->to->next)
        {
          if (first == NULL)
            first = s;
          if (s->from->me.y <= min ||
              s->to->me.y <= min ||
              s->from->nextcp.y < min || s->to->prevcp.y < min)
            {
              if (!s->knownlinear)
                {
                  if (s->from->me.y < min)
                    {
                      f = mt_round;
                      min = s->from->me.y;
                    }
                  if (s->to->me.y < min)
                    {
                      f = mt_round;
                      min = s->to->me.y;
                    }
                  SplineFindExtrema (&s->splines[1], &ts[0], &ts[1]);
                  for (i = 0; i < 2; ++i)
                    if (ts[i] != -1)
                      {
                        bigreal y =
                          ((s->splines[1].a * ts[i] + s->splines[1].b) * ts[i] +
                           s->splines[1].c) * ts[i] + s->splines[1].d;
                        if (y < min)
                          {
                            f = mt_round;
                            min = y;
                          }
                      }
                }
              else if (s->from->me.y == s->to->me.y)
                {
                  if (s->from->me.y <= min)
                    {
                      min = s->from->me.y;
                      f = mt_flat;
                    }
                }
              else
                {
                  if (s->from->me.y < min)
                    {
                      f = mt_pointy;
                      min = s->from->me.y;
                    }
                  if (s->to->me.y < min)
                    {
                      f = mt_pointy;
                      min = s->to->me.y;
                    }
                }
            }
        }
    }
  *isflat = f;
  return min;
}

static bigreal
SCMinHeight (SplineChar *sc, int layer, enum flatness *isflat)
{
  /* Find the min height of this layer of the glyph. Also find whether that */
  /* min is flat (as in "z", curved as in "o" or pointy as in "A") */
  enum flatness f = mt_unknown, curf;
  bigreal min = 1.0e23, test;
  RefChar *r;

  min = SPLMinHeight (sc->layers[layer].splines, &curf);
  f = curf;
  for (r = sc->layers[layer].refs; r != NULL; r = r->next)
    {
      test = SPLMinHeight (r->layers[0].splines, &curf);
      if (test < min || (test == min && curf == mt_flat))
        {
          min = test;
          f = curf;
        }
    }
  *isflat = f;
  return min;
}

#define RANGE	0x40ffffff

struct dimcnt
{
  bigreal pos;
  int cnt;
};

static int
dclist_insert (struct dimcnt *arr, int cnt, bigreal val)
{
  int i;

  for (i = 0; i < cnt; ++i)
    {
      if (arr[i].pos == val)
        {
          ++arr[i].cnt;
          return cnt;
        }
    }
  arr[i].pos = val;
  arr[i].cnt = 1;
  return i + 1;
}

static bigreal
SFStandardHeight (SplineFont *sf, int layer, int do_max, uint32_t *list)
{
  struct dimcnt flats[200], curves[200];
  bigreal test;
  enum flatness curf;
  int fcnt = 0, ccnt = 0, cnt, tot, i, useit;
  uint32_t ch, top;
  bigreal result, bestheight, bestdiff, diff, val;
  const char *blues;
  char *end;

  while (*list)
    {
      ch = top = *list;
      if (list[1] == RANGE && list[2] != 0)
        {
          list += 2;
          top = *list;
        }
      for (; ch <= top; ++ch)
        {
          SplineChar *sc = SFGetChar (sf, ch, NULL);
          if (sc != NULL)
            {
              if (do_max)
                test = SCMaxHeight (sc, layer, &curf);
              else
                test = SCMinHeight (sc, layer, &curf);
              if (curf == mt_flat)
                fcnt = dclist_insert (flats, fcnt, test);
              else if (curf != mt_unknown)
                ccnt = dclist_insert (curves, ccnt, test);
            }
        }
      ++list;
    }

  /* All flat surfaces at tops of glyphs are at the same level */
  if (fcnt == 1)
    result = flats[0].pos;
  else if (fcnt > 1)
    {
      cnt = 0;
      for (i = 0; i < fcnt; ++i)
        {
          if (flats[i].cnt > cnt)
            cnt = flats[i].cnt;
        }
      test = 0;
      tot = 0;
      /* find the mode. If multiple values have the same high count, average them */
      for (i = 0; i < fcnt; ++i)
        {
          if (flats[i].cnt == cnt)
            {
              test += flats[i].pos;
              ++tot;
            }
        }
      result = test / tot;
    }
  else if (ccnt == 0)
    return do_max ? -1e23 : 1e23;       /* We didn't find any glyphs */
  else
    {
      /* Italic fonts will often have no flat surfaces for x-height just wavies */
      test = 0;
      tot = 0;
      /* find the mean */
      for (i = 0; i < ccnt; ++i)
        {
          test += curves[i].pos;
          ++tot;
        }
      result = test / tot;
    }

  /* Do we have a BlueValues entry? */
  /* If so, snap height to the closest alignment zone (bottom of the zone) */
  if (sf->private != NULL
      && (blues =
          PSDictHasEntry (sf->private,
                          do_max ? "BlueValues" : "OtherBlues")) != NULL)
    {
      while (*blues == ' ' || *blues == '[')
        ++blues;
      /* Must get at least this close, else we'll just use what we found */
      bestheight = result;
      bestdiff = (sf->ascent + sf->descent) / 100.0;
      useit = true;
      while (*blues != '\0' && *blues != ']')
        {
          val = strtod (blues, &end);
          if (blues == end)
            break;
          blues = end;
          while (*blues == ' ')
            ++blues;
          if (useit)
            {
              if ((diff = val - result) < 0)
                diff = -diff;
              if (diff < bestdiff)
                {
                  bestheight = val;
                  bestdiff = diff;
                }
            }
          useit = !useit;       /* Only interested in every other BV entry */
        }
      result = bestheight;
    }
  return result;
}

static uint32_t capheight_str[] = { 'A', RANGE, 'Z',
  0x391, RANGE, 0x3a9,
  0x402, 0x404, 0x405, 0x406, 0x408, RANGE, 0x40b, 0x40f, RANGE, 0x418, 0x41a,
  0x42f,
  0
};

static uint32_t xheight_str[] =
  { 'a', 'c', 'e', 'g', 'm', 'n', 'o', 'p', 'q', 'r', 's', 'u', 'v', 'w', 'x',
  'y', 'z', 0x131,
  0x3b3, 0x3b9, 0x3ba, 0x3bc, 0x3bd, 0x3c0, 0x3c3, 0x3c4, 0x3c5, 0x3c7, 0x3c8,
  0x3c9,
  0x432, 0x433, 0x438, 0x43a, RANGE, 0x43f, 0x442, 0x443, 0x445, 0x44c, 0x44f,
  0x459, 0x45a,
  0
};

static uint32_t ascender_str[] = { 'b', 'd', 'f', 'h', 'k', 'l',
  0x3b3, 0x3b4, 0x3b6, 0x3b8,
  0x444, 0x452,
  0
};

static uint32_t descender_str[] = { 'g', 'j', 'p', 'q', 'y',
  0x3b2, 0x3b3, 0x3c7, 0x3c8,
  0x434, 0x440, 0x443, 0x444, 0x452, 0x458,
  0
};

bigreal
SFCapHeight (SplineFont *sf, int layer, int return_error)
{
  if (sf->pfminfo.pfmset && sf->pfminfo.os2_capheight)
    return sf->pfminfo.os2_capheight;

  bigreal result = SFStandardHeight (sf, layer, true, capheight_str);

  if (result == -1e23 && !return_error)
    result = (8 * sf->ascent) / 10;
  return result;
}

bigreal
SFXHeight (SplineFont *sf, int layer, int return_error)
{
  if (sf->pfminfo.pfmset && sf->pfminfo.os2_xheight)
    return sf->pfminfo.os2_xheight;

  bigreal result = SFStandardHeight (sf, layer, true, xheight_str);

  if (result == -1e23 && !return_error)
    result = (6 * sf->ascent) / 10;
  return result;
}

bigreal
SFAscender (SplineFont *sf, int layer, int return_error)
{
  bigreal result = SFStandardHeight (sf, layer, true, ascender_str);

  if (result == -1e23 && !return_error)
    result = (81 * sf->ascent) / 100;
  return result;
}

bigreal
SFDescender (SplineFont *sf, int layer, int return_error)
{
  bigreal result = SFStandardHeight (sf, layer, false, descender_str);

  if (result == 1e23 && !return_error)
    result = -sf->descent / 2;
  return result;
}

static void
arraystring (char *buffer, real *array, int cnt)
{
  int i, ei;

  for (ei = cnt; ei > 1 && array[ei - 1] == 0; --ei);
  *buffer++ = '[';
  for (i = 0; i < ei; ++i)
    {
      sprintf (buffer, "%d ", (int) array[i]);
      buffer += strlen (buffer);
    }
  if (buffer[-1] == ' ')
    --buffer;
  *buffer++ = ']';
  *buffer = '\0';
}

static void
SnapSet (struct psdict *private, real stemsnap[12], real snapcnt[12],
         char *name1, char *name2, int which)
{
  int i, mi;
  char buffer[211];

  mi = -1;
  for (i = 0; stemsnap[i] != 0 && i < 12; ++i)
    if (mi == -1)
      mi = i;
    else if (snapcnt[i] > snapcnt[mi])
      mi = i;
  if (mi == -1)
    return;
  if (which < 2)
    {
      sprintf (buffer, "[%d]", (int) stemsnap[mi]);
      PSDictChangeEntry (private, name1, buffer);
    }
  if (which == 0 || which == 2)
    {
      arraystring (buffer, stemsnap, 12);
      PSDictChangeEntry (private, name2, buffer);
    }
}

int
SFPrivateGuess (SplineFont *sf, int layer, struct psdict *private, char *name,
                int onlyone)
{
  real bluevalues[14], otherblues[10];
  real snapcnt[12];
  real stemsnap[12];
  char buffer[211];
  char *oldloc;
  int ret;

  oldloc = xstrdup_or_null (setlocale (LC_NUMERIC, NULL));
  setlocale (LC_NUMERIC, "C");
  ret = true;

  if (strcmp (name, "BlueValues") == 0 || strcmp (name, "OtherBlues") == 0)
    {
      FindBlues (sf, layer, bluevalues, otherblues);
      if (!onlyone || strcmp (name, "BlueValues") == 0)
        {
          arraystring (buffer, bluevalues, 14);
          PSDictChangeEntry (private, "BlueValues", buffer);
        }
      if (!onlyone || strcmp (name, "OtherBlues") == 0)
        {
          if (otherblues[0] != 0 || otherblues[1] != 0)
            {
              arraystring (buffer, otherblues, 10);
              PSDictChangeEntry (private, "OtherBlues", buffer);
            }
          else
            PSDictRemoveEntry (private, "OtherBlues");
        }
    }
  else if (strcmp (name, "StdHW") == 0 || strcmp (name, "StemSnapH") == 0)
    {
      FindHStems (sf, stemsnap, snapcnt);
      SnapSet (private, stemsnap, snapcnt, "StdHW", "StemSnapH",
               !onlyone ? 0 : strcmp (name, "StdHW") == 0 ? 1 : 0);
    }
  else if (strcmp (name, "StdVW") == 0 || strcmp (name, "StemSnapV") == 0)
    {
      FindVStems (sf, stemsnap, snapcnt);
      SnapSet (private, stemsnap, snapcnt, "StdVW", "StemSnapV",
               !onlyone ? 0 : strcmp (name, "StdVW") == 0 ? 1 : 0);
    }
  else if (strcmp (name, "BlueScale") == 0)
    {
      bigreal val = -1;
      if (PSDictFindEntry (private, "BlueValues") != -1)
        {
          /* Can guess BlueScale if we've got a BlueValues */
          val = BlueScaleFigureForced (private, NULL, NULL);
        }
      if (val == -1)
        val = .039625;
      sprintf (buffer, "%g", (double) val);
      PSDictChangeEntry (private, "BlueScale", buffer);
    }
  else if (strcmp (name, "BlueShift") == 0)
    {
      PSDictChangeEntry (private, "BlueShift", "7");
    }
  else if (strcmp (name, "BlueFuzz") == 0)
    {
      PSDictChangeEntry (private, "BlueFuzz", "1");
    }
  else if (strcmp (name, "ForceBold") == 0)
    {
      int isbold = false;
      if (sf->weight != NULL &&
          (strcasestr (sf->weight, "Bold") != NULL ||
           strcasestr (sf->weight, "Heavy") != NULL ||
           strcasestr (sf->weight, "Black") != NULL ||
           strcasestr (sf->weight, "Grass") != NULL ||
           strcasestr (sf->weight, "Fett") != NULL))
        isbold = true;
      if (sf->pfminfo.pfmset && sf->pfminfo.weight >= 700)
        isbold = true;
      PSDictChangeEntry (private, "ForceBold", isbold ? "true" : "false");
    }
  else if (strcmp (name, "LanguageGroup") == 0)
    {
      PSDictChangeEntry (private, "LanguageGroup", "0");
    }
  else if (strcmp (name, "ExpansionFactor") == 0)
    {
      PSDictChangeEntry (private, "ExpansionFactor", "0.06");
    }
  else
    ret = false;

  setlocale (LC_NUMERIC, oldloc);
  free (oldloc);
  return ret;
}

void
SFRemoveLayer (SplineFont *sf, int l)
{
  int gid, i;
  SplineChar *sc;
  CharViewBase *cvs;
  FontViewBase *fvs;
  int layers, any_quads;

  if (sf->subfontcnt != 0 || l <= ly_fore || sf->multilayer)
    return;

  for (layers = ly_fore, any_quads = 0; layers < sf->layer_cnt; ++layers)
    {
      if (layers != l && sf->layers[layers].order2)
        any_quads = true;
    }
  for (gid = 0; gid < sf->glyphcnt; ++gid)
    if ((sc = sf->glyphs[gid]) != NULL)
      {
        LayerFreeContents (sc, l);
        for (i = l + 1; i < sc->layer_cnt; ++i)
          sc->layers[i - 1] = sc->layers[i];
        --sc->layer_cnt;
        for (cvs = sc->views; cvs != NULL; cvs = cvs->next)
          {
            if (cvs->layerheads[dm_back] - sc->layers >= sc->layer_cnt)
              cvs->layerheads[dm_back] = &sc->layers[ly_back];
            if (cvs->layerheads[dm_fore] - sc->layers >= sc->layer_cnt)
              cvs->layerheads[dm_fore] = &sc->layers[ly_fore];
          }
        if (!any_quads)
          {
            free (sc->ttf_instrs);
            sc->ttf_instrs = NULL;
            sc->ttf_instrs_len = 0;
          }
      }

  for (fvs = sf->fv; fvs != NULL; fvs = fvs->next)
    {
      if (fvs->active_layer >= l)
        {
          --fvs->active_layer;
          if (fvs->active_layer + 1 == l)
            FontViewLayerChanged (fvs);
        }
    }
  MVDestroyAll (sf);

  free (sf->layers[l].name);
  for (i = l + 1; i < sf->layer_cnt; ++i)
    sf->layers[i - 1] = sf->layers[i];
  --sf->layer_cnt;
}

void
SFAddLayer (SplineFont *sf, char *name, int order2, int background)
{
  int gid, l;
  SplineChar *sc;
  CharViewBase *cvs;

  if (sf->layer_cnt >= BACK_LAYER_MAX - 1)
    {
      ff_post_error (_("Too many layers"),
                     _("Attempt to have a font with more than %d layers"),
                     BACK_LAYER_MAX);
      return;
    }
  if (name == NULL || *name == '\0')
    name = _("Back");

  l = sf->layer_cnt;
  ++sf->layer_cnt;
  sf->layers = xrealloc (sf->layers, (l + 1) * sizeof (LayerInfo));
  memset (&sf->layers[l], 0, sizeof (LayerInfo));
  sf->layers[l].name = xstrdup_or_null (name);
  sf->layers[l].order2 = order2;
  sf->layers[l].background = background;

  for (gid = 0; gid < sf->glyphcnt; ++gid)
    if ((sc = sf->glyphs[gid]) != NULL)
      {
        Layer *old = sc->layers;
        sc->layers = xrealloc (sc->layers, (l + 1) * sizeof (Layer));
        memset (&sc->layers[l], 0, sizeof (Layer));
        LayerDefault (&sc->layers[l]);
        sc->layers[l].order2 = order2;
        sc->layers[l].background = background;
        ++sc->layer_cnt;
        for (cvs = sc->views; cvs != NULL; cvs = cvs->next)
          {
            cvs->layerheads[dm_back] =
              sc->layers + (cvs->layerheads[dm_back] - old);
            cvs->layerheads[dm_fore] =
              sc->layers + (cvs->layerheads[dm_fore] - old);
          }
      }
}

void
SFLayerSetBackground (SplineFont *sf, int layer, int is_back)
{
  int k, gid;
  SplineFont *_sf;
  SplineChar *sc;

  sf->layers[layer].background = is_back;
  k = 0;
  do
    {
      _sf = sf->subfontcnt == 0 ? sf : sf->subfonts[k];
      for (gid = 0; gid < _sf->glyphcnt; ++gid)
        if ((sc = _sf->glyphs[gid]) != NULL)
          {
            sc->layers[layer].background = is_back;
            if (!is_back && sc->layers[layer].images != NULL)
              {
                ImageListsFree (sc->layers[layer].images);
                sc->layers[layer].images = NULL;
                SCCharChangedUpdate (sc, layer);
              }
          }
      ++k;
    }
  while (k < sf->subfontcnt);
}
