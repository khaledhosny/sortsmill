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
#include "fontforge.h"
#include <utype.h>

#include "ttf.h"

/* ************************************************************************** */
/* *************************    The 'kern' table    ************************* */
/* ************************************************************************** */


struct kerncounts
{
  int cnt;
  int vcnt;
  int mh, mv;
  int hsubs;
  int *hbreaks;
  int vsubs;
  int *vbreaks;
};

static int LookupHasDefault (OTLookup *otl);

static int
CountKerns (struct alltabs *at, SplineFont *sf, struct kerncounts *kcnt)
{
  int i, cnt, vcnt, j, mh, mv;
  KernPair *kp;

  cnt = mh = vcnt = mv = 0;
  for (i = 0; i < at->gi.gcnt; ++i)
    if (at->gi.bygid[i] != -1)
      {
        j = 0;
        for (kp = sf->glyphs[at->gi.bygid[i]]->kerns; kp != NULL; kp = kp->next)
          if (kp->off != 0 && kp->sc->ttf_glyph != -1 &&
              LookupHasDefault (kp->subtable->lookup))
            ++cnt, ++j;
        if (j > mh)
          mh = j;
        j = 0;
        for (kp = sf->glyphs[at->gi.bygid[i]]->vkerns; kp != NULL;
             kp = kp->next)
          if (kp->off != 0 && kp->sc->ttf_glyph != -1
              && LookupHasDefault (kp->subtable->lookup))
            ++vcnt, ++j;
        if (j > mv)
          mv = j;
      }
  kcnt->cnt = cnt;
  kcnt->vcnt = vcnt;
  kcnt->mh = mh;
  kcnt->mv = mv;
  kcnt->hbreaks = kcnt->vbreaks = NULL;
  if (cnt >= 10000)
    {
      /* the sub-table size is 6*cnt+14 or so and needs to be less 65535 */
      /*  so break it up into little bits */
      /* We might not need this when applemode is set because the subtable */
      /*  length is a long. BUT... there's a damn binsearch header with */
      /*  shorts in it still */
      int b = 0;
      kcnt->hbreaks = xmalloc ((at->gi.gcnt + 1) * sizeof (int));
      cnt = 0;
      for (i = 0; i < at->gi.gcnt; ++i)
        if (at->gi.bygid[i] != -1)
          {
            j = 0;
            for (kp = sf->glyphs[at->gi.bygid[i]]->kerns; kp != NULL;
                 kp = kp->next)
              if (kp->off != 0 && LookupHasDefault (kp->subtable->lookup))
                ++j;
            if ((cnt + j) * 6 > 64000L && cnt != 0)
              {
                kcnt->hbreaks[b++] = cnt;
                cnt = 0;
              }
            cnt += j;
          }
      kcnt->hbreaks[b++] = cnt;
      kcnt->hsubs = b;
    }
  else if (cnt != 0)
    kcnt->hsubs = 1;
  else
    kcnt->hsubs = 0;
  if (vcnt >= 10000)
    {
      int b = 0;
      kcnt->vbreaks = xmalloc ((at->gi.gcnt + 1) * sizeof (int));
      vcnt = 0;
      for (i = 0; i < at->gi.gcnt; ++i)
        if (at->gi.bygid[i] != -1)
          {
            j = 0;
            for (kp = sf->glyphs[at->gi.bygid[i]]->vkerns; kp != NULL;
                 kp = kp->next)
              if (kp->off != 0 && LookupHasDefault (kp->subtable->lookup))
                ++j;
            if ((vcnt + j) * 6 > 64000L && vcnt != 0)
              {
                kcnt->vbreaks[b++] = vcnt;
                vcnt = 0;
              }
            vcnt += j;
          }
      kcnt->vbreaks[b++] = vcnt;
      kcnt->vsubs = b;
    }
  else if (vcnt != 0)
    kcnt->vsubs = 1;
  else
    kcnt->vsubs = 0;

  return kcnt->hsubs + kcnt->vsubs;
}

static void
ttf_dumpsfkerns (struct alltabs *at, SplineFont *sf, int version)
{
  struct kerncounts kcnt;
  int i, j, k, m, c, gid, tot, km;
  KernPair *kp;
  uint16_t *glnum, *offsets;
  int isv;
  int b, bmax;
  int *breaks;
  int winfail = 0;

  if (CountKerns (at, sf, &kcnt) == 0)
    return;

  for (isv = 0; isv < 2; ++isv)
    {
      c = isv ? kcnt.vcnt : kcnt.cnt;
      bmax = isv ? kcnt.vsubs : kcnt.hsubs;
      breaks = isv ? kcnt.vbreaks : kcnt.hbreaks;
      if (c != 0)
        {
          km = isv ? kcnt.mv : kcnt.mh;
          glnum = xmalloc (km * sizeof (uint16_t));
          offsets = xmalloc (km * sizeof (uint16_t));
          gid = 0;
          for (b = 0; b < bmax; ++b)
            {
              c = bmax == 1 ? c : breaks[b];
              putshort (at->kern, version);     /* subtable version */
              if (c > 10920)
                ff_post_error (_("Too many kern pairs"),
                               _
                               ("The 'kern' table supports at most 10920 kern pairs in a subtable"));
              putshort (at->kern, (7 + 3 * c) * sizeof (uint16_t));     /* subtable length */
              putshort (at->kern, !isv);        /* coverage, flags=hor/vert&format=0 */
              putshort (at->kern, c);
              for (i = 1, j = 0; i <= c; i <<= 1, ++j);
              i >>= 1;
              --j;
              putshort (at->kern, i * 6);       /* binary search headers */
              putshort (at->kern, j);
              putshort (at->kern, 6 * (c - i));

              for (tot = 0; gid < at->gi.gcnt && tot < c; ++gid)
                if (at->gi.bygid[gid] != -1)
                  {
                    SplineChar *sc = sf->glyphs[at->gi.bygid[gid]];
                    m = 0;
                    for (kp = isv ? sc->vkerns : sc->kerns; kp != NULL;
                         kp = kp->next)
                      {
                        if (kp->off != 0 && kp->sc->ttf_glyph != -1 &&
                            LookupHasDefault (kp->subtable->lookup))
                          {
                            /* order the pairs */
                            for (j = 0; j < m; ++j)
                              if (kp->sc->ttf_glyph < glnum[j])
                                break;
                            for (k = m; k > j; --k)
                              {
                                glnum[k] = glnum[k - 1];
                                offsets[k] = offsets[k - 1];
                              }
                            glnum[j] = kp->sc->ttf_glyph;
                            offsets[j] = kp->off;
                            ++m;
                            /* check if a pair will cause problems on Windows */
                            /* If the glyph is outside BMP, so either unicode >0xffff */
                            /*  or -1. Cast to unsigned catches both */
                            if ((unsigned)
                                (sf->glyphs[at->gi.bygid[gid]]->unicodeenc) >
                                0xFFFF
                                || (unsigned) (sf->
                                               glyphs[at->gi.bygid[glnum[j]]]->
                                               unicodeenc) > 0xFFFF)
                              winfail++;
                          }
                      }
                    for (j = 0; j < m; ++j)
                      {
                        putshort (at->kern, gid);
                        putshort (at->kern, glnum[j]);
                        putshort (at->kern, offsets[j]);
                      }
                    tot += m;
                  }
            }
          free (offsets);
          free (glnum);
        }
    }
  free (kcnt.hbreaks);
  free (kcnt.vbreaks);

  if (winfail > 0)
    ff_post_error (_("Kerning is likely to fail on Windows"),
                   _("On Windows many apps will have problems with this font's "
                     "kerning, because %d of its glyph kern pairs cannot "
                     "be mapped to unicode-BMP kern pairs"), winfail);
}

void
ttf_dumpkerns (struct alltabs *at, SplineFont *sf)
{
  int sum;
  int version;
  struct kerncounts kcnt;

  SFKernClassTempDecompose (sf, false);

  sum = CountKerns (at, sf, &kcnt);
  free (kcnt.hbreaks);
  free (kcnt.vbreaks);
  if (sum == 0)
    {
      SFKernCleanup (sf, false);
      return;
    }

  /* Old kerning format (version 0) uses 16 bit quantities */
  /* Apple's new format (version 0x00010000) uses 32 bit quantities */
  version = 0;
  at->kern = tmpfile ();
  putshort (at->kern, version); /* version */
  putshort (at->kern, sum);     /* number of subtables */

  ttf_dumpsfkerns (at, sf, version);
  SFKernCleanup (sf, false);

  at->kernlen = ftell (at->kern);
  if (at->kernlen & 2)
    putshort (at->kern, 0);     /* pad it */
}

/* ************************************************************************** */
/* *************************    utility routines    ************************* */
/* ************************************************************************** */

static int
scriptsHaveDefault (struct scriptlanglist *sl)
{
  int i;

  for (; sl != NULL; sl = sl->next)
    {
      for (i = 0; i < sl->lang_cnt; ++i)
        {
          if ((i < MAX_LANG && sl->langs[i] == DEFAULT_LANG) ||
              (i >= MAX_LANG && sl->morelangs[i - MAX_LANG] == DEFAULT_LANG))
            {
              return true;
            }
        }
    }
  return false;
}

static int
LookupHasDefault (OTLookup *otl)
{
  FeatureScriptLangList *feats;

  if (otl->def_lang_checked)
    return otl->def_lang_found;

  otl->def_lang_checked = true;
  for (feats = otl->features; feats != NULL; feats = feats->next)
    {
      if (scriptsHaveDefault (feats->scripts))
        {
          otl->def_lang_found = true;
          return true;
        }
    }
  otl->def_lang_found = false;
  return false;
}
