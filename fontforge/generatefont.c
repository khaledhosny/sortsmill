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

#include <config.h>

#include <stdbool.h>
#include "fontforgevw.h"
#include "ustring.h"
#include "gfile.h"
#include "gresource.h"
#include "utype.h"
#include "gio.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "psfont.h"
#include "generatefont.h"

VISIBLE int old_sfnt_flags = ttf_flag_otmode;
VISIBLE int old_ps_flags = ps_flag_afm | ps_flag_round;
VISIBLE int old_psotb_flags = ps_flag_afm;

VISIBLE int oldformatstate = ff_pfb;
VISIBLE int oldbitmapstate = 0;

char *generatefont_extensions[] =
  { ".pfa", ".pfb", ".bin", "%s.pfb", ".pfa", ".pfb", ".pt3", ".ps",
  ".cid", ".cff", ".cid.cff",
  ".t42", ".t11",
  ".ttf", ".ttf", ".ttf.bin", ".ttc", ".dfont", ".otf", ".otf.dfont", ".otf",
  ".otf.dfont", ".svg", ".woff", NULL
};

char *bitmapextensions[] =
  { "-*.bdf", ".ttf", ".dfont", ".ttf", ".otb", ".bmap.bin", ".fon", "-*.fnt",
  ".pdb", "-*.pt3", ".none", NULL
};

static int
WriteAfmFile (char *filename, SplineFont *sf, int formattype,
              EncMap *map, int flags, SplineFont *fullsf, int layer)
{
  char *buf = xmalloc (strlen (filename) + 6), *pt, *pt2;
  FILE *afm;
  int ret;
  int subtype = formattype;

  if ((formattype == ff_mma || formattype == ff_mmb) && sf->mm != NULL)
    {
      sf = sf->mm->normal;
      subtype = ff_pfb;
    }

  strcpy (buf, filename);
  pt = strrchr (buf, '.');
  if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
    pt = NULL;
  if (pt == NULL)
    strcat (buf, ".afm");
  else
    strcpy (pt, ".afm");
  ff_progress_change_line1 (_("Generating AFM File"));
  ff_progress_change_line2 (buf);
  if (strstr (buf, "://") == NULL)
    afm = fopen (buf, "w");
  else
    afm = tmpfile ();
  if (afm == NULL)
    {
      free (buf);
      return false;
    }
  ret =
    AfmSplineFont (afm, sf, subtype, map, flags & ps_flag_afmwithmarks, fullsf,
                   layer);
  if (ret && strstr (buf, "://") != NULL)
    ret = URLFromFile (buf, afm);
  free (buf);
  if (fclose (afm) == -1)
    return false;
  if (!ret)
    return false;

  if ((formattype == ff_mma || formattype == ff_mmb) && sf->mm != NULL)
    {
      MMSet *mm = sf->mm;
      int i;
      for (i = 0; i < mm->instance_count; ++i)
        {
          sf = mm->instances[i];
          buf = xmalloc (strlen (filename) + strlen (sf->fontname) + 4 + 1);
          strcpy (buf, filename);
          pt = strrchr (buf, '/');
          if (pt == NULL)
            pt = buf;
          else
            ++pt;
          strcpy (pt, sf->fontname);
          strcat (pt, ".afm");
          ff_progress_change_line2 (buf);
          afm = fopen (buf, "w");
          free (buf);
          if (afm == NULL)
            return false;
          ret =
            AfmSplineFont (afm, sf, subtype, map, flags & ps_flag_afmwithmarks,
                           NULL, layer);
          if (fclose (afm) == -1)
            return false;
          if (!ret)
            return false;
        }
      buf = xmalloc (strlen (filename) + 8);

      strcpy (buf, filename);
      pt = strrchr (buf, '.');
      if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
        pt = NULL;
      if (pt == NULL)
        strcat (buf, ".amfm");
      else
        strcpy (pt, ".amfm");
      ff_progress_change_line2 (buf);
      afm = fopen (buf, "w");
      free (buf);
      if (afm == NULL)
        return false;
      ret = AmfmSplineFont (afm, mm, formattype, map, layer);
      if (fclose (afm) == -1)
        return false;
    }
  return ret;
}

static int
WriteTfmFile (char *filename, SplineFont *sf, int formattype, EncMap *map,
              int layer)
{
  char *buf = xmalloc (strlen (filename) + 6), *pt, *pt2;
  FILE *tfm, *enc;
  int ret;
  int i;
  char *encname;

  strcpy (buf, filename);
  pt = strrchr (buf, '.');
  if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
    pt = NULL;
  if (pt == NULL)
    strcat (buf, ".tfm");
  else
    strcpy (pt, ".tfm");
  ff_progress_change_line1 (_("Generating TFM File"));
  ff_progress_change_line2 (buf);
  ff_progress_next ();          /* Forces a refresh */
  tfm = fopen (buf, "wb");
  if (tfm == NULL)
    return false;
  ret = TfmSplineFont (tfm, sf, formattype, map, layer);
  if (fclose (tfm) == -1)
    ret = 0;

  pt = strrchr (buf, '.');
  strcpy (pt, ".enc");
  enc = fopen (buf, "wb");
  free (buf);
  if (enc == NULL)
    return false;

  encname = NULL;
  if (sf->subfontcnt == 0 && map->enc != &custom)
    encname = EncodingName (map->enc);
  if (encname == NULL)
    fprintf (enc, "/%s-Enc [\n", sf->fontname);
  else
    fprintf (enc, "/%s [\n", encname);
  for (i = 0; i < map->enc_limit && i < 256; ++i)
    {
      if (enc_to_gid (map, i) == -1
          || !SCWorthOutputting (sf->glyphs[enc_to_gid (map, i)]))
        fprintf (enc, " /.notdef");
      else
        fprintf (enc, " /%s", sf->glyphs[enc_to_gid (map, i)]->name);
      if ((i & 0xf) == 0)
        fprintf (enc, "\t\t%% 0x%02x", i);
      putc ('\n', enc);
    }
  while (i < 256)
    {
      fprintf (enc, " /.notdef");
      if ((i & 0xf0) == 0)
        fprintf (enc, "\t\t%% 0x%02x", i);
      putc ('\n', enc);
      ++i;
    }
  fprintf (enc, "] def\n");

  if (fclose (enc) == -1)
    ret = 0;
  return ret;
}

static int
WriteOfmFile (char *filename, SplineFont *sf, int formattype, EncMap *map,
              int layer)
{
  char *buf = xmalloc (strlen (filename) + 6), *pt, *pt2;
  FILE *tfm, *enc;
  int ret;
  int i;
  char *encname;
  char *texparamnames[] =
    { "SLANT", "SPACE", "STRETCH", "SHRINK", "XHEIGHT", "QUAD", "EXTRASPACE",
    NULL
  };

  strcpy (buf, filename);
  pt = strrchr (buf, '.');
  if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
    pt = NULL;
  if (pt == NULL)
    strcat (buf, ".ofm");
  else
    strcpy (pt, ".ofm");
  ff_progress_change_line1 (_("Generating OFM File"));
  ff_progress_change_line2 (buf);
  ff_progress_next ();          /* Forces a refresh */
  tfm = fopen (buf, "wb");
  if (tfm == NULL)
    return false;
  ret = OfmSplineFont (tfm, sf, formattype, map, layer);
  if (fclose (tfm) == -1)
    ret = 0;

  pt = strrchr (buf, '.');
  strcpy (pt, ".cfg");
  enc = fopen (buf, "wb");
  free (buf);
  if (enc == NULL)
    return false;

  fprintf (enc, "VTITLE %s\n", sf->fontname);
  fprintf (enc, "FAMILY %s\n", sf->familyname);
  encname = NULL;
  if (sf->subfontcnt == 0 && map->enc != &custom)
    encname = EncodingName (map->enc);
  fprintf (enc, "CODINGSCHEME %s\n",
           encname == NULL ? encname : "FONT-SPECIFIC");

  /* OfmSplineFont has already called TeXDefaultParams, so we don't have to */
  fprintf (enc, "EPSILON 0.090\n");     /* I have no idea what this means */
  for (i = 0; texparamnames[i] != NULL; ++i)
    fprintf (enc, "%s %g\n", texparamnames[i],
             sf->texdata.params[i] / (double) (1 << 20));

  for (i = 0; i < map->enc_limit && i < 65536; ++i)
    {
      if (enc_to_gid (map, i) != -1
          && SCWorthOutputting (sf->glyphs[enc_to_gid (map, i)]))
        fprintf (enc, "%04X N %s\n", i, sf->glyphs[enc_to_gid (map, i)]->name);
    }

  if (fclose (enc) == -1)
    ret = 0;
  return ret;
}

#if FONTFORGE_CONFIG_WRITE_PFM
#define WRITE_PFM_ATTRIBUTE VISIBLE
#else
#define WRITE_PFM_ATTRIBUTE static
#endif

WRITE_PFM_ATTRIBUTE int
WritePfmFile (char *filename, SplineFont *sf, int type0, EncMap *map, int layer)
{
  char *buf = xmalloc (strlen (filename) + 6), *pt, *pt2;
  FILE *pfm;
  int ret;

  strcpy (buf, filename);
  pt = strrchr (buf, '.');
  if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
    pt = NULL;
  if (pt == NULL)
    strcat (buf, ".pfm");
  else
    strcpy (pt, ".pfm");
  ff_progress_change_line2 (buf);
  pfm = fopen (buf, "wb");
  free (buf);
  if (pfm == NULL)
    return false;
  ret = PfmSplineFont (pfm, sf, type0, map, layer);
  if (fclose (pfm) == -1)
    return 0;
  return ret;
}

static int
WriteFontLog (char *filename, SplineFont *sf, int formattype,
              EncMap *map, int flags, SplineFont *fullsf)
{
  char *buf = xmalloc (strlen (filename) + 12), *pt;
  FILE *flog;

  if (sf->fontlog == NULL || *sf->fontlog == '\0')
    return true;

  strcpy (buf, filename);
  pt = strrchr (buf, '/');
  if (pt == NULL)
    strcat (buf, "FontLog.txt");
  else
    strcpy (pt + 1, "FontLog.txt");
  flog = fopen (buf, "w");
  free (buf);
  if (flog == NULL)
    return false;

  for (pt = sf->fontlog; *pt; ++pt)
    putc (*pt, flog);
  if (fclose (flog) != 0)
    return false;

  return true;
}

static int
WriteBitmaps (char *filename, SplineFont *sf, int32_t *sizes, int res,
              int bf, EncMap *map)
{
  char *buf = xmalloc (strlen (filename) + 30), *pt, *pt2;
  int i;
  BDFFont *bdf;
  char *ext;
  /* res = -1 => Guess depending on pixel size of font */

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;

  for (i = 0; sizes[i] != 0; ++i);
  ff_progress_change_stages (i);
  for (i = 0; sizes[i] != 0; ++i)
    {
      for (bdf = sf->bitmaps; bdf != NULL &&
           (bdf->pixelsize != (sizes[i] & 0xffff)
            || BDFDepth (bdf) != (sizes[i] >> 16)); bdf = bdf->next);
      if (bdf == NULL)
        {
          ff_post_notice (_("Missing Bitmap"),
                          _
                          ("Attempt to generate a pixel size that has not been created (%d@%d)"),
                          sizes[i] & 0xffff, sizes[i] >> 16);
          free (buf);
          return false;
        }

      if (bf == bf_ptype3 && bdf->clut != NULL)
        {
          ff_post_notice (_("Missing Bitmap"),
                          _
                          ("Currently, FontForge only supports bitmap (not bytemap) type3 output"));
          return false;
        }

      strcpy (buf, filename);
      pt = strrchr (buf, '.');
      if (pt != NULL && (pt2 = strrchr (buf, '/')) != NULL && pt < pt2)
        pt = NULL;
      if (pt == NULL)
        pt = buf + strlen (buf);
      if (strcmp (pt - 4, ".otf.dfont") == 0
          || strcmp (pt - 4, ".ttf.bin") == 0)
        pt -= 4;
      if (pt - 2 > buf && pt[-2] == '-' && pt[-1] == '*')
        pt -= 2;
      ext = bf == bf_bdf ? ".bdf" : bf == bf_ptype3 ? ".pt3" : ".fnt";
      if (bdf->clut == NULL)
        sprintf (pt, "-%d%s", bdf->pixelsize, ext);
      else
        sprintf (pt, "-%d@%d%s", bdf->pixelsize, BDFDepth (bdf), ext);

      ff_progress_change_line2 (buf);
      if (bf == bf_bdf)
        BDFFontDump (buf, bdf, map, res);
      else if (bf == bf_ptype3)
        PSBitmapDump (buf, bdf, map);
      else if (bf == bf_fnt)
        FNTFontDump (buf, bdf, map, res);
      else
        IError ("Unexpected font type");
      ff_progress_next_stage ();
    }
  free (buf);
  return true;
}

static int32_t *
ParseWernerSFDFile (char *wernerfilename, SplineFont *sf, int *max,
                    char ***_names, EncMap *map)
{
  /* one entry for each char, >=1 => that subfont, 0=>not mapped, -1 => end of char mark */
  int cnt = 0, subfilecnt = 0, thusfar;
  int k, warned = false;
  uint32_t r1, r2, i, modi;
  SplineFont *_sf;
  int32_t *mapping;
  FILE *file;
  char buffer[200], *bpt;
  char *end, *pt;
  char *orig;
  struct remap *remap;
  char **names;
  int loop;
  static const char *pfaeditflag = "SplineFontDB:";

  file = fopen (wernerfilename, "r");
  if (file == NULL)
    {
      ff_post_error (_("No Sub Font Definition file"),
                     _("No Sub Font Definition file"));
      return NULL;
    }

  k = 0;
  do
    {
      _sf = sf->subfontcnt == 0 ? sf : sf->subfonts[k++];
      if (_sf->glyphcnt > cnt)
        cnt = _sf->glyphcnt;
    }
  while (k < sf->subfontcnt);

  mapping = xcalloc (cnt + 1, sizeof (int32_t));
  memset (mapping, -1, (cnt + 1) * sizeof (int32_t));
  mapping[cnt] = -2;
  *max = 0;

  while (fgets (buffer, sizeof (buffer), file) != NULL)
    ++subfilecnt;
  names = xmalloc ((subfilecnt + 1) * sizeof (char *));

  rewind (file);
  subfilecnt = 0;
  while (fgets (buffer, sizeof (buffer), file) != NULL)
    {
      if (strncmp (buffer, pfaeditflag, strlen (pfaeditflag)) == 0)
        {
          ff_post_error (_("Wrong type of SFD file"),
                         _
                         ("This looks like one of FontForge's SplineFont DataBase files.\nNot one of TeX's SubFont Definition files.\nAn unfortunate confusion of extensions."));
          free (mapping);
          return NULL;
        }
      pt = buffer + strlen (buffer) - 1;
      bpt = buffer;
      if ((*pt != '\n' && *pt != '\r') || (pt > buffer && pt[-1] == '\\') ||
          (pt > buffer + 1 && pt[-2] == '\\' && isspace (pt[-1])))
        {
          bpt = xstrdup ("");
          while (true)
            {
              loop = false;
              if ((*pt != '\n' && *pt != '\r')
                  || (pt > buffer && pt[-1] == '\\') || (pt > buffer + 1
                                                         && pt[-2] == '\\'
                                                         && isspace (pt[-1])))
                loop = true;
              if (*pt == '\n' || *pt == '\r')
                {
                  if (pt[-1] == '\\')
                    pt[-1] = '\0';
                  else if (pt[-2] == '\\')
                    pt[-2] = '\0';
                }
              bpt = xrealloc (bpt, strlen (bpt) + strlen (buffer) + 10);
              strcat (bpt, buffer);
              if (!loop)
                break;
              if (fgets (buffer, sizeof (buffer), file) == NULL)
                break;
              pt = buffer + strlen (buffer) - 1;
            }
        }
      if (bpt[0] == '#' || bpt[0] == '\0' || isspace (bpt[0]))
        continue;
      for (pt = bpt; !isspace (*pt) && *pt != '\0'; ++pt);
      if (*pt == '\0' || *pt == '\r' || *pt == '\n')
        continue;
      names[subfilecnt] = xstrndup (bpt, pt - bpt);
      if (subfilecnt > *max)
        *max = subfilecnt;
      end = pt;
      thusfar = 0;
      while (*end != '\0')
        {
          while (isspace (*end))
            ++end;
          if (*end == '\0')
            break;
          orig = end;
          r1 = strtoul (end, &end, 0);
          if (orig == end)
            break;
          while (isspace (*end))
            ++end;
          if (*end == ':')
            {
              if (r1 >= 256 || r1 < 0)
                LogError (_("Bad offset: %d for subfont %s\n"), r1,
                          names[subfilecnt]);
              else
                thusfar = r1;
              r1 = strtoul (end + 1, &end, 0);
            }
          if (*end == '_' || *end == '-')
            r2 = strtoul (end + 1, &end, 0);
          else
            r2 = r1;
          for (i = r1; i <= r2; ++i)
            {
              modi = i;
              if (map->remap != NULL)
                {
                  for (remap = map->remap; remap->infont != -1; ++remap)
                    {
                      if (i >= remap->firstenc && i <= remap->lastenc)
                        {
                          modi = i - remap->firstenc + remap->infont;
                          break;
                        }
                    }
                }
              if (modi < map->enc_limit)
                modi = enc_to_gid (map, modi);
              else if (sf->subfontcnt != 0)
                modi = modi;
              else
                modi = -1;
              if (modi < cnt && modi != -1)
                {
                  if (mapping[modi] != -1 && !warned)
                    {
                      if ((i == 0xffff || i == 0xfffe) &&
                          (map->enc->is_unicodebmp || map->enc->is_unicodefull))
                        /* Not a character anyway. just ignore it */ ;
                      else
                        {
                          LogError (_
                                    ("Warning: Encoding %d (0x%x) is mapped to at least two locations (%s@0x%02x and %s@0x%02x)\n Only one will be used here.\n"),
                                    i, i, names[subfilecnt], thusfar,
                                    names[(mapping[modi] >> 8)],
                                    mapping[modi] & 0xff);
                          warned = true;
                        }
                    }
                  mapping[modi] = (subfilecnt << 8) | thusfar;
                }
              thusfar++;
            }
        }
      if (thusfar > 256)
        LogError (_("More than 256 entries in subfont %s\n"),
                  names[subfilecnt]);
      ++subfilecnt;
      if (bpt != buffer)
        free (bpt);
    }
  names[subfilecnt] = NULL;
  *_names = names;
  fclose (file);
  return mapping;
}

static int
GenerateSubFont (SplineFont *sf, char *newname, int32_t *sizes, int res,
                 int32_t *mapping, int subfont, char **names, EncMap *map,
                 int layer)
{
  SplineFont temp;
  SplineChar *chars[256], **newchars;
  SplineFont *_sf;
  int k, i, used, base, extras;
  char *filename;
  char *spt, *pt, buf[8];
  RefChar *ref;
  int err = 0;
  enum fontformat subtype = strstr (newname, ".pfa") != NULL ? ff_pfa : ff_pfb;
  EncMap encmap;

  memset (&encmap, 0, sizeof (encmap));
  encmap.enc_limit = 256;
  make_enc_to_gid (&encmap);
  make_gid_to_enc (&encmap);
  encmap.enc = &custom;

  temp = *sf;
  temp.glyphcnt = 0;
  temp.glyphmax = 256;
  temp.glyphs = chars;
  temp.bitmaps = NULL;
  temp.subfonts = NULL;
  temp.subfontcnt = 0;
  temp.uniqueid = 0;
  memset (chars, 0, sizeof (chars));
  temp.glyphnames = NULL;
  used = 0;
  for (i = 0; mapping[i] != -2; ++i)
    if ((mapping[i] >> 8) == subfont)
      {
        k = 0;
        do
          {
            _sf = sf->subfontcnt == 0 ? sf : sf->subfonts[k++];
            if (i < _sf->glyphcnt && _sf->glyphs[i] != NULL)
              break;
          }
        while (k < sf->subfontcnt);
        if (temp.glyphcnt < 256)
          {
            if (i < _sf->glyphcnt)
              {
                if (_sf->glyphs[i] != NULL)
                  {
                    _sf->glyphs[i]->parent = &temp;
                    _sf->glyphs[i]->orig_pos = temp.glyphcnt;
                    chars[temp.glyphcnt] = _sf->glyphs[i];
                    set_enc_to_gid (&encmap, mapping[i] & 0xff, temp.glyphcnt);
                    set_gid_to_enc (&encmap, temp.glyphcnt, mapping[i] & 0xff);
                    ++temp.glyphcnt;
                    ++used;
                  }
              }
          }
      }
  if (used == 0)
    return 0;

  /* check for any references to things outside this subfont and add them */
  /*  as unencoded chars */
  /* We could just replace with splines, I suppose but that would make */
  /*  korean fonts huge */
  while (true)
    {
      extras = 0;
      for (i = 0; i < temp.glyphcnt; ++i)
        if (temp.glyphs[i] != NULL)
          {
            for (ref = temp.glyphs[i]->layers[ly_fore].refs; ref != NULL;
                 ref = ref->next)
              if (ref->sc->parent != &temp)
                ++extras;
          }
      if (extras == 0)
        break;
      newchars = xcalloc (temp.glyphcnt + extras, sizeof (SplineChar *));
      memcpy (newchars, temp.glyphs, temp.glyphcnt * sizeof (SplineChar *));
      if (temp.glyphs != chars)
        free (temp.glyphs);
      base = temp.glyphcnt;
      temp.glyphs = newchars;
      extras = 0;
      for (i = 0; i < base; ++i)
        if (temp.glyphs[i] != NULL)
          {
            for (ref = temp.glyphs[i]->layers[ly_fore].refs; ref != NULL;
                 ref = ref->next)
              if (ref->sc->parent != &temp)
                {
                  temp.glyphs[base + extras] = ref->sc;
                  ref->sc->parent = &temp;
                  ref->sc->orig_pos = base + extras++;
                }
          }
      temp.glyphcnt += extras;  /* this might be a slightly different value from that found before if some references get reused. N'importe */
      temp.glyphmax = temp.glyphcnt;
    }

  filename = xmalloc (strlen (newname) + strlen (names[subfont]) + 10);
  strcpy (filename, newname);
  pt = strrchr (filename, '.');
  spt = strrchr (filename, '/');
  if (spt == NULL)
    spt = filename;
  else
    ++spt;
  if (pt > spt)
    *pt = '\0';
  pt = strstr (spt, "%d");
  if (pt == NULL)
    pt = strstr (spt, "%s");
  if (pt == NULL)
    strcat (filename, names[subfont]);
  else
    {
      int len = strlen (names[subfont]);
      int l;
      if (len > 2)
        {
          for (l = strlen (pt); l >= 2; --l)
            pt[l + len - 2] = pt[l];
        }
      else if (len < 2)
        strcpy (pt + len, pt + 2);
      memcpy (pt, names[subfont], len);
    }
  temp.fontname = xstrdup_or_null (spt);
  temp.fullname =
    xmalloc (strlen (temp.fullname) + strlen (names[subfont]) + 3);
  strcpy (temp.fullname, sf->fullname);
  strcat (temp.fullname, " ");
  strcat (temp.fullname, names[subfont]);
  strcat (spt, subtype == ff_pfb ? ".pfb" : ".pfa");
  ff_progress_change_line2 (filename);

  if (sf->xuid != NULL)
    {
      sprintf (buf, "%d", subfont);
      temp.xuid = xmalloc (strlen (sf->xuid) + strlen (buf) + 5);
      strcpy (temp.xuid, sf->xuid);
      pt = temp.xuid + strlen (temp.xuid) - 1;
      while (pt > temp.xuid && *pt == ' ')
        --pt;
      if (*pt == ']')
        --pt;
      *pt = ' ';
      strcpy (pt + 1, buf);
      strcat (pt, "]");
    }

  err =
    !WritePSFont (filename, &temp, subtype, old_ps_flags, &encmap, sf, layer);
  if (err)
    ff_post_error (_("Generation Failed"), _("Generation Failed"));
  if (!err && (old_ps_flags & ps_flag_afm) && ff_progress_next_stage ())
    {
      if (!WriteAfmFile
          (filename, &temp, oldformatstate, &encmap, old_ps_flags, sf, layer))
        {
          ff_post_error (_("AFM Generation Failed"),
                         _("AFM Generation Failed"));
          err = true;
        }
    }
  if (!err && (old_ps_flags & ps_flag_tfm))
    {
      if (!WriteTfmFile (filename, &temp, oldformatstate, &encmap, layer))
        {
          ff_post_error (_("TFM Generation Failed"),
                         _("TFM Generation Failed"));
          err = true;
        }
    }
  /* ??? Bitmaps */
  if (!ff_progress_next_stage ())
    err = -1;

  if (temp.glyphs != chars)
    free (temp.glyphs);
  GlyphHashFree (&temp);
  free (temp.xuid);
  free (temp.fontname);
  free (temp.fullname);
  free (filename);

  /* GenerateSubFont messes up the parent and orig_pos fields. Fix 'em up */
  /* Do this after every save, else afm,tfm files might produce extraneous kerns */
  k = 0;
  do
    {
      _sf = sf->subfontcnt == 0 ? sf : sf->subfonts[k++];
      for (i = 0; i < _sf->glyphcnt; ++i)
        if (_sf->glyphs[i] != NULL)
          {
            _sf->glyphs[i]->parent = _sf;
            _sf->glyphs[i]->orig_pos = i;
          }
    }
  while (k < sf->subfontcnt);

  release_enc_to_gid (&encmap);
  release_gid_to_enc (&encmap);

  return err;
}

/* ttf2tfm supports multiple sfd files. I do not. */
static int
WriteMultiplePSFont (SplineFont *sf, char *newname, int32_t *sizes,
                     int res, char *wernerfilename, EncMap *map, int layer)
{
  int err = 0, tofree = false, max, filecnt;
  int32_t *mapping;
  char *path;
  int i;
  char **names;
  char *pt;

  pt = strrchr (newname, '.');
  if (pt == NULL ||
      (strcmp (pt, ".pfa") != 0 && strcmp (pt, ".pfb") != 0
       && strcmp (pt, ".mult") != 0))
    {
      ff_post_error (_("Bad Extension"),
                     _
                     ("You must specify a standard type1 extension (.pfb or .pfa)"));
      return 0;
    }
  if (wernerfilename == NULL)
    return 0;
  mapping = ParseWernerSFDFile (wernerfilename, sf, &max, &names, map);
  if (tofree)
    free (wernerfilename);
  if (mapping == NULL)
    return 1;

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;

  filecnt = 1;
  if ((old_ps_flags & ps_flag_afm))
    filecnt = 2;
#if 0
  if (oldbitmapstate == bf_bdf)
    ++filecnt;
#endif
  path = x_u8_strconv_from_locale (newname);
  ff_progress_start_indicator (10, _("Generating font"),
                               _("Generating Multiple PostScript Fonts"),
                               path, 256, (max + 1) * filecnt, true);
  free (path);

  for (i = 0; i <= max && !err; ++i)
    err =
      GenerateSubFont (sf, newname, sizes, res, mapping, i, names, map, layer);

  free (mapping);
  for (i = 0; names[i] != NULL; ++i)
    free (names[i]);
  free (names);
  free (sizes);
  ff_progress_end_indicator ();
  if (!err)
    SavePrefs (true);
  return err;
}

int
CheckIfTransparent (SplineFont *sf)
{
  /* Type3 doesn't support translucent fills */
  int i, j;
  char *buts[3];
  buts[0] = _("_Yes");
  buts[1] = _("_Cancel");
  buts[2] = NULL;

  for (i = 0; i < sf->glyphcnt; ++i)
    if (sf->glyphs[i] != NULL)
      {
        SplineChar *sc = sf->glyphs[i];
        for (j = ly_fore; j < sc->layer_cnt; ++j)
          {
            if (sc->layers[j].fill_brush.opacity != 1
                || sc->layers[j].stroke_pen.brush.opacity != 1)
              {
                if (ff_ask
                    (_("Bad Drawing Operation"), (const char **) buts, 0, 1,
                     _
                     ("This font contains at least one translucent layer, but type3 does not support that (anything translucent or transparent is treated as opaque). Do you want to proceed anyway?"))
                    == 1)
                  return true;

                return false;
              }
          }
      }
  return false;
}

int
_DoGenerate (SplineFont *sf, char *newname, int32_t *sizes, int res,
             EncMap *map, char *subfontdefinition, int layer)
{
  int err = false;
  int iscid = oldformatstate == ff_cid || oldformatstate == ff_cffcid ||
    oldformatstate == ff_otfcid || oldformatstate == ff_otfciddfont;
  int flags = 0;
  char *buf;

  if (oldformatstate == ff_multiple)
    return (WriteMultiplePSFont
            (sf, newname, sizes, res, subfontdefinition, map, layer));

  if (oldformatstate <= ff_cffcid)
    flags = old_ps_flags;
  else if (oldformatstate <= ff_ttfdfont)
    flags = old_sfnt_flags;
  else if (oldformatstate != ff_none)
    flags = old_sfnt_flags;
  else
    flags = old_sfnt_flags & ~(ttf_flag_ofm);
  if (oldformatstate <= ff_cffcid && oldbitmapstate == bf_otb)
    flags = old_psotb_flags;

  switch (oldformatstate)
    {
    case ff_ttf:
    case ff_ttfsym:
    case ff_ttfmacbin:
      buf = x_gc_strdup (_("Generating TrueType Font"));
      break;
    case ff_otf:
    case ff_otfdfont:
      buf = x_gc_strdup (_("Generating OpenType Font"));
      break;
    case ff_cid:
    case ff_cffcid:
    case ff_otfcid:
    case ff_otfciddfont:
      buf = x_gc_strdup (_("Generating CID keyed font"));
      break;
    case ff_mma:
    case ff_mmb:
      buf = x_gc_strdup (_("Generating multi-master font"));
      break;
    case ff_svg:
      buf = x_gc_strdup (_("Generating SVG font"));
      break;
    default:
      buf = x_gc_strdup (_("Generating PostScript Font"));
      break;
    }

  ff_progress_start_indicator (10, _("Generating font"), buf,
                               x_gc_u8_strconv_from_locale (newname),
                               sf->glyphcnt, 1, true);
  if (oldformatstate != ff_none)
    {
      int oerr = 0;
      int bmap = oldbitmapstate;
      if (bmap == bf_otb)
        bmap = bf_none;
      if (strstr (newname, "://") != NULL)
        {
          if (oldformatstate == ff_pfbmacbin || oldformatstate == ff_ttfmacbin)
            {
              ff_post_error (_("Mac Resource Not Remote"),
                             _
                             ("You may not generate a mac resource file to a remote location"));
              oerr = true;
            }
        }
      if (!oerr)
        switch (oldformatstate)
          {
          case ff_mma:
          case ff_mmb:
            sf = sf->mm->instances[0];
          case ff_pfa:
          case ff_pfb:
          case ff_ptype3:
          case ff_ptype0:
          case ff_cid:
          case ff_type42:
          case ff_type42cid:
            if (sf->multilayer && CheckIfTransparent (sf))
              return true;
            oerr =
              !WritePSFont (newname, sf, oldformatstate, flags, map, NULL,
                            layer);
            break;
          case ff_ttf:
          case ff_ttfsym:
          case ff_otf:
          case ff_otfcid:
          case ff_cff:
          case ff_cffcid:
            oerr = !WriteTTFFont (newname, sf, oldformatstate, sizes, bmap,
                                  flags, map, layer);
            break;
          case ff_pfbmacbin:
            oerr =
              !WriteMacPSFont (newname, sf, oldformatstate, flags, map, layer);
            break;
          case ff_ttfmacbin:
          case ff_ttfdfont:
          case ff_otfdfont:
          case ff_otfciddfont:
            oerr = !WriteMacTTFFont (newname, sf, oldformatstate, sizes,
                                     bmap, flags, map, layer);
            break;
          case ff_svg:
            oerr =
              !WriteSVGFont (newname, sf, oldformatstate, flags, map, layer);
            break;
          }
      if (oerr)
        {
          ff_post_error (_("Generation Failed"), _("Generation Failed"));
          err = true;
        }
    }
  if (!err && (flags & ps_flag_tfm))
    {
      if (!WriteTfmFile (newname, sf, oldformatstate, map, layer))
        {
          ff_post_error (_("TFM Generation Failed"),
                         _("TFM Generation Failed"));
          err = true;
        }
    }
  if (!err && (flags & ttf_flag_ofm))
    {
      if (!WriteOfmFile (newname, sf, oldformatstate, map, layer))
        {
          ff_post_error (_("OFM Generation Failed"),
                         _("OFM Generation Failed"));
          err = true;
        }
    }
  if (!err && (flags & ps_flag_afm))
    {
      ff_progress_increment (-sf->glyphcnt);
      if (!WriteAfmFile (newname, sf, oldformatstate, map, flags, NULL, layer))
        {
          ff_post_error (_("AFM Generation Failed"),
                         _("AFM Generation Failed"));
          err = true;
        }
    }
  if (!err && (flags & ps_flag_outputfontlog))
    {
      /*ff_progress_increment(-sf->glyphcnt); */
      if (!WriteFontLog (newname, sf, oldformatstate, map, flags, NULL))
        {
          ff_post_error (_("FontLog Save Failed"), _("FontLog Save Failed"));
          err = true;
        }
    }
  if (!err && (flags & ps_flag_pfm) && !iscid)
    {
      ff_progress_change_line1 (_("Generating PFM File"));
      ff_progress_increment (-sf->glyphcnt);
      if (!WritePfmFile (newname, sf, oldformatstate == ff_ptype0, map, layer))
        {
          ff_post_error (_("PFM Generation Failed"),
                         _("PFM Generation Failed"));
          err = true;
        }
    }
  if (oldbitmapstate == bf_otb || oldbitmapstate == bf_sfnt_ms)
    {
      char *temp = newname;
      if (newname[strlen (newname) - 1] == '.')
        {
          temp = xmalloc (strlen (newname) + 8);
          strcpy (temp, newname);
          strcat (temp, oldbitmapstate == bf_otb ? "otb" : "ttf");
        }
      if (!WriteTTFFont
          (temp, sf, ff_none, sizes, oldbitmapstate, flags, map, layer))
        err = true;
      if (temp != newname)
        free (temp);
    }
  else if (oldbitmapstate == bf_sfnt_dfont)
    {
      char *temp = newname;
      if (newname[strlen (newname) - 1] == '.')
        {
          temp = xmalloc (strlen (newname) + 8);
          strcpy (temp, newname);
          strcat (temp, "dfont");
        }
      if (!WriteMacTTFFont
          (temp, sf, ff_none, sizes, oldbitmapstate, flags, map, layer))
        err = true;
      if (temp != newname)
        free (temp);
    }
  else if ((oldbitmapstate == bf_bdf || oldbitmapstate == bf_fnt ||
            oldbitmapstate == bf_ptype3) && !err)
    {
      ff_progress_change_line1 (_("Generating Bitmap Font(s)"));
      ff_progress_increment (-sf->glyphcnt);
      if (!WriteBitmaps (newname, sf, sizes, res, oldbitmapstate, map))
        err = true;
    }
  else if (oldbitmapstate == bf_fon && !err)
    {
      if (!FONFontDump (newname, sf, sizes, res, map))
        err = true;
    }
  else if (oldbitmapstate == bf_palm && !err)
    {
      if (!WritePalmBitmaps (newname, sf, sizes, map))
        err = true;
    }
  else
    if ((oldbitmapstate == bf_nfntmacbin /*|| oldbitmapstate==bf_nfntdfont */ )
        && !err)
    {
      if (!WriteMacBitmaps
          (newname, sf, sizes, false /*oldbitmapstate==bf_nfntdfont */ , map))
        err = true;
    }
  free (sizes);
  ff_progress_end_indicator ();
  if (!err)
    SavePrefs (true);
  return err;
}

void
PrepareUnlinkRmOvrlp (SplineFont *sf, char *filename, int layer)
{
  int gid;
  SplineChar *sc;
  RefChar *ref, *refnext;
  extern int no_windowing_ui, maxundoes;
  int old_nwui = no_windowing_ui, old_maxundoes = maxundoes;

#if !defined(_NO_PYTHON)
  PyFF_CallDictFunc (sf->python_temporary, "generateFontPostHook", "fs", sf->fv,
                     filename);
#endif

  if (maxundoes == 0)
    maxundoes = 1;              /* Force undoes */

  for (gid = 0; gid < sf->glyphcnt; ++gid)
    if ((sc = sf->glyphs[gid]) != NULL && sc->unlink_rm_ovrlp_generate_undo)
      {
        if (autohint_before_generate && sc != NULL &&
            sc->changedsincelasthinted && !sc->manualhints)
          {
            no_windowing_ui = true;
            SplineCharAutoHint (sc, layer, NULL);       /* Do this now, else we get an unwanted undo on the stack from hinting */
          }
        no_windowing_ui = false;
        SCPreserveLayer (sc, layer, false);
        no_windowing_ui = true; /* Clustering wants to create an undo that I don't need */
        for (ref = sc->layers[layer].refs; ref != NULL; ref = refnext)
          {
            refnext = ref->next;
            SCRefToSplines (sc, ref, layer);
          }
        SCRoundToCluster (sc, layer, false, .03, .12);
        sc->layers[layer].splines =
          SplineSetRemoveOverlap (sc, sc->layers[layer].splines, over_remove);
        no_windowing_ui = false;
        if (!sc->manualhints)
          sc->changedsincelasthinted = false;
      }
  no_windowing_ui = old_nwui;
  maxundoes = old_maxundoes;
}

void
RestoreUnlinkRmOvrlp (SplineFont *sf, char *filename, int layer)
{
  int gid;
  SplineChar *sc;

  for (gid = 0; gid < sf->glyphcnt; ++gid)
    if ((sc = sf->glyphs[gid]) != NULL && sc->unlink_rm_ovrlp_generate_undo)
      {
        SCDoUndo (sc, layer);
        if (!sc->manualhints)
          sc->changedsincelasthinted = false;
      }
#if !defined(_NO_PYTHON)
  PyFF_CallDictFunc (sf->python_temporary, "generateFontPostHook", "fs", sf->fv,
                     filename);
#endif
}

static int32_t *
AllBitmapSizes (SplineFont *sf)
{
  int32_t *sizes = NULL;
  BDFFont *bdf;
  int i, cnt;

  for (i = 0; i < 2; ++i)
    {
      cnt = 0;
      for (bdf = sf->bitmaps; bdf != NULL; bdf = bdf->next)
        {
          if (sizes != NULL)
            sizes[cnt] = bdf->pixelsize | (BDFDepth (bdf) << 16);
          ++cnt;
        }
      if (i == 1)
        break;
      sizes = xmalloc ((cnt + 1) * sizeof (int32_t));
    }
  sizes[cnt] = 0;
  return sizes;
}

int
GenerateScript (SplineFont *sf, char *filename, char *bitmaptype, int fmflags,
                int res, char *subfontdefinition, struct sflist *sfs,
                EncMap *map, NameList * rename_to, int layer)
{
  int i;
  static char *bitmaps[] =
    { "bdf", "ttf", "dfont", "ttf", "otb", "bin", "fon", "fnt", "pdb", "pt3",
    NULL
  };
  int32_t *sizes = NULL;
  char *end = filename + strlen (filename);
  struct sflist *sfi;
  char *freeme = NULL;
  int ret;
  struct sflist *sfl;
  char **former;

  if (sf->bitmaps == NULL)
    i = bf_none;
  else if (strcasecmp (bitmaptype, "otf") == 0)
    i = bf_ttf;
  else if (strcasecmp (bitmaptype, "ms") == 0)
    i = bf_ttf;
  else if (strcasecmp (bitmaptype, "apple") == 0)
    i = bf_ttf;
  else if (strcasecmp (bitmaptype, "sbit") == 0)
    i = bf_sfnt_dfont;
  else if (strcasecmp (bitmaptype, "nfnt") == 0)
    i = bf_nfntmacbin;
  else if (strcasecmp (bitmaptype, "ps") == 0)
    i = bf_ptype3;
  else
    for (i = 0; bitmaps[i] != NULL; ++i)
      {
        if (strcasecmp (bitmaptype, bitmaps[i]) == 0)
          break;
      }
  oldbitmapstate = i;

  for (i = 0; generatefont_extensions[i] != NULL; ++i)
    {
      if (strlen (generatefont_extensions[i]) > 0 &&
          end - filename >= strlen (generatefont_extensions[i]) &&
          strcasecmp (end - strlen (generatefont_extensions[i]),
                      generatefont_extensions[i]) == 0)
        break;
    }
  if (end - filename > 8
      && strcasecmp (end - strlen (".ttf.bin"), ".ttf.bin") == 0)
    i = ff_ttfmacbin;
  else if (end - filename > 5 && strcasecmp (end - strlen (".suit"), ".suit") == 0)     /* Different extensions for Mac/non Mac, support both always */
    i = ff_ttfmacbin;
  else if (end - filename > 4
           && strcasecmp (end - strlen (".bin"), ".bin") == 0)
    i = ff_pfbmacbin;
  else if (end - filename > 4
           && strcasecmp (end - strlen (".res"), ".res") == 0)
    i = ff_pfbmacbin;
  else if (end - filename > 8
           && strcasecmp (end - strlen (".sym.ttf"), ".sym.ttf") == 0)
    i = ff_ttfsym;
  else if (end - filename > 8
           && strcasecmp (end - strlen (".cid.cff"), ".cid.cff") == 0)
    i = ff_cffcid;
  else if (end - filename > 8
           && strcasecmp (end - strlen (".cid.t42"), ".cid.t42") == 0)
    i = ff_type42cid;
  else if (end - filename > 7
           && strcasecmp (end - strlen (".mm.pfa"), ".mm.pfa") == 0)
    i = ff_mma;
  else if (end - filename > 7
           && strcasecmp (end - strlen (".mm.pfb"), ".mm.pfb") == 0)
    i = ff_mmb;
  else if (end - filename > 7
           && strcasecmp (end - strlen (".mult"), ".mult") == 0)
    i = ff_multiple;
  else if ((i == ff_pfa || i == ff_pfb) && strstr (filename, "%s") != NULL)
    i = ff_multiple;
  if (generatefont_extensions[i] == NULL)
    {
      for (i = 0; bitmaps[i] != NULL; ++i)
        {
          if (end - filename > strlen (bitmaps[i]) &&
              strcasecmp (end - strlen (bitmaps[i]), bitmaps[i]) == 0)
            break;
        }
      if (*filename == '\0' || end[-1] == '.')
        i = ff_none;
      else if (bitmaps[i] == NULL)
        i = ff_pfb;
      else
        {
          oldbitmapstate = i;
          i = ff_none;
        }
    }
  if (i == ff_ttfdfont
      && strcasecmp (end - strlen (".otf.dfont"), ".otf.dfont") == 0)
    i = ff_otfdfont;
  if (sf->cidmaster != NULL)
    {
      if (i == ff_otf)
        i = ff_otfcid;
      else if (i == ff_otfdfont)
        i = ff_otfciddfont;
    }
  if ((i == ff_none || sf->onlybitmaps) && oldbitmapstate == bf_ttf)
    oldbitmapstate = bf_sfnt_ms;
  oldformatstate = i;

  if (oldformatstate == ff_none && end[-1] == '.' &&
      (oldbitmapstate == bf_ttf || oldbitmapstate == bf_sfnt_dfont
       || oldbitmapstate == bf_otb))
    {
      freeme = xmalloc (strlen (filename) + 8);
      strcpy (freeme, filename);
      if (strcasecmp (bitmaptype, "otf") == 0)
        strcat (freeme, "otf");
      else if (oldbitmapstate == bf_otb)
        strcat (freeme, "otb");
      else if (oldbitmapstate == bf_sfnt_dfont)
        strcat (freeme, "dfont");
      else
        strcat (freeme, "ttf");
      filename = freeme;
    }
  else if (sf->onlybitmaps && sf->bitmaps != NULL &&
           (oldformatstate == ff_ttf || oldformatstate == ff_otf) &&
           (oldbitmapstate == bf_none || oldbitmapstate == bf_ttf ||
            oldbitmapstate == bf_sfnt_dfont || oldbitmapstate == bf_otb))
    {
      if (oldbitmapstate == ff_ttf)
        oldbitmapstate = bf_ttf;
      oldformatstate = ff_none;
    }

  if (oldbitmapstate == bf_sfnt_dfont)
    oldformatstate = ff_none;

  if (fmflags == -1)
    {
      /* Default to what we did last time */
    }
  else
    {
      if (oldformatstate == ff_ttf && (fmflags & 0x2000))
        oldformatstate = ff_ttfsym;
      if (oldformatstate <= ff_cffcid)
        {
          old_ps_flags = 0;
          if (fmflags & 1)
            old_ps_flags |= ps_flag_afm;
          if (fmflags & 2)
            old_ps_flags |= ps_flag_pfm;
          if (fmflags & 0x10000)
            old_ps_flags |= ps_flag_tfm;
          if (fmflags & 0x20000)
            old_ps_flags |= ps_flag_nohintsubs;
          if (fmflags & 0x40000)
            old_ps_flags |= ps_flag_noflex;
          if (fmflags & 0x80000)
            old_ps_flags |= ps_flag_nohints;
          if (fmflags & 0x100000)
            old_ps_flags |= ps_flag_restrict256;
          if (fmflags & 0x200000)
            old_ps_flags |= ps_flag_round;
          if (fmflags & 0x400000)
            old_ps_flags |= ps_flag_afmwithmarks;
          if (i == bf_otb)
            {
              old_sfnt_flags = 0;
              switch (fmflags & 0x90)
                {
                case 0x80:
                  old_sfnt_flags |= ttf_flag_applemode | ttf_flag_otmode;
                  break;
                case 0x90:
                  /* Neither */ ;
                  break;
                case 0x10:
                  old_sfnt_flags |= ttf_flag_applemode;
                  break;
                case 0x00:
                  old_sfnt_flags |= ttf_flag_otmode;
                  break;
                }
              if (fmflags & 4)
                old_sfnt_flags |= ttf_flag_shortps;
              if (fmflags & 0x20)
                old_sfnt_flags |= ttf_flag_pfed_comments;
              if (fmflags & 0x40)
                old_sfnt_flags |= ttf_flag_pfed_colors;
              if (fmflags & 0x200)
                old_sfnt_flags |= ttf_flag_TeXtable;
              if (fmflags & 0x400)
                old_sfnt_flags |= ttf_flag_ofm;
              if ((fmflags & 0x800) && !(old_sfnt_flags & ttf_flag_applemode))
                old_sfnt_flags |= ttf_flag_oldkern;
              if (fmflags & 0x2000)
                old_sfnt_flags |= ttf_flag_symbol;
              if (fmflags & 0x4000)
                old_sfnt_flags |= ttf_flag_dummyDSIG;
              if (fmflags & 0x800000)
                old_sfnt_flags |= ttf_flag_pfed_lookupnames;
              if (fmflags & 0x1000000)
                old_sfnt_flags |= ttf_flag_pfed_guides;
              if (fmflags & 0x2000000)
                old_sfnt_flags |= ttf_flag_pfed_layers;
              if (fmflags & 0x4000000)
                old_sfnt_flags |= ttf_flag_nomacnames;
            }
        }
      else
        {
          old_sfnt_flags = 0;
          /* Applicable postscript flags */
          if (fmflags & 1)
            old_sfnt_flags |= ps_flag_afm;
          if (fmflags & 2)
            old_sfnt_flags |= ps_flag_pfm;
          if (fmflags & 0x20000)
            old_sfnt_flags |= ps_flag_nohintsubs;
          if (fmflags & 0x40000)
            old_sfnt_flags |= ps_flag_noflex;
          if (fmflags & 0x80000)
            old_sfnt_flags |= ps_flag_nohints;
          if (fmflags & 0x200000)
            old_sfnt_flags |= ps_flag_round;
          if (fmflags & 0x400000)
            old_sfnt_flags |= ps_flag_afmwithmarks;
          /* Applicable truetype flags */
          switch (fmflags & 0x90)
            {
            case 0x80:
              old_sfnt_flags |= ttf_flag_applemode | ttf_flag_otmode;
              break;
            case 0x90:
              /* Neither */ ;
              break;
            case 0x10:
              old_sfnt_flags |= ttf_flag_applemode;
              break;
            case 0x00:
              old_sfnt_flags |= ttf_flag_otmode;
              break;
            }
          if (fmflags & 4)
            old_sfnt_flags |= ttf_flag_shortps;
          if (fmflags & 8)
            old_sfnt_flags |= ttf_flag_nohints;
          if (fmflags & 0x20)
            old_sfnt_flags |= ttf_flag_pfed_comments;
          if (fmflags & 0x40)
            old_sfnt_flags |= ttf_flag_pfed_colors;
          if (fmflags & 0x100)
            old_sfnt_flags |= ttf_flag_glyphmap;
          if (fmflags & 0x200)
            old_sfnt_flags |= ttf_flag_TeXtable;
          if (fmflags & 0x400)
            old_sfnt_flags |= ttf_flag_ofm;
          if ((fmflags & 0x800) && !(old_sfnt_flags & ttf_flag_applemode))
            old_sfnt_flags |= ttf_flag_oldkern;
          if (fmflags & 0x2000)
            old_sfnt_flags |= ttf_flag_symbol;
          if (fmflags & 0x4000)
            old_sfnt_flags |= ttf_flag_dummyDSIG;
          if (fmflags & 0x800000)
            old_sfnt_flags |= ttf_flag_pfed_lookupnames;
          if (fmflags & 0x1000000)
            old_sfnt_flags |= ttf_flag_pfed_guides;
          if (fmflags & 0x2000000)
            old_sfnt_flags |= ttf_flag_pfed_layers;
          if (fmflags & 0x4000000)
            old_sfnt_flags |= ttf_flag_nomacnames;
        }
    }

  if (oldbitmapstate != bf_none)
    {
      if (sfs != NULL)
        {
          for (sfi = sfs; sfi != NULL; sfi = sfi->next)
            sfi->sizes = AllBitmapSizes (sfi->sf);
        }
      else
        sizes = AllBitmapSizes (sf);
    }

  former = NULL;
  if (sfs != NULL)
    {
      for (sfl = sfs; sfl != NULL; sfl = sfl->next)
        {
          PrepareUnlinkRmOvrlp (sfl->sf, filename, layer);
          if (rename_to != NULL)
            sfl->former_names =
              SFTemporaryRenameGlyphsToNamelist (sfl->sf, rename_to);
        }
    }
  else
    {
      PrepareUnlinkRmOvrlp (sf, filename, layer);
      if (rename_to != NULL)
        former = SFTemporaryRenameGlyphsToNamelist (sf, rename_to);
    }

  if (sfs != NULL)
    {
      int flags = 0;
      if (oldformatstate <= ff_cffcid)
        flags = old_ps_flags;
      else
        flags = old_sfnt_flags;
      ret =
        WriteMacFamily (filename, sfs, oldformatstate, oldbitmapstate, flags,
                        layer);
    }
  else
    {
      ret =
        !_DoGenerate (sf, filename, sizes, res, map, subfontdefinition, layer);
    }
  free (freeme);

  if (sfs != NULL)
    {
      for (sfl = sfs; sfl != NULL; sfl = sfl->next)
        {
          RestoreUnlinkRmOvrlp (sfl->sf, filename, layer);
          if (rename_to != NULL)
            SFTemporaryRestoreGlyphNames (sfl->sf, sfl->former_names);
        }
    }
  else
    {
      RestoreUnlinkRmOvrlp (sf, filename, layer);
      if (rename_to != NULL)
        SFTemporaryRestoreGlyphNames (sf, former);
    }

  if (oldbitmapstate != bf_none)
    {
      if (sfs != NULL)
        {
          for (sfi = sfs; sfi != NULL; sfi = sfi->next)
            free (sfi->sizes);
        }
    }
  return ret;
}
