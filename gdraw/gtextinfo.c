#include <config.h>

// Copyright (C) 2015 Khaled Hosny and Barry Schwartz
//
// This file is part of Sorts Mill Tools.
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

#include <stdlib.h>
#include <stdbool.h>
#include <glib.h>
#include <libguile.h>
#include "gdraw.h"
#include "ggadgetP.h"
#include "gfile.h"
#include "utype.h"
#include "ustring.h"
#include "gresource.h"

#define _SHORTCUTS_MODULE "sortsmill editor keyboard-shortcuts"

int
GTextInfoGetWidth (GWindow base, GTextInfo *ti, FontInstance * font)
{
  int width = 0;
  int iwidth = 0;
  int skip = 0;

  if (ti->text != NULL)
    {
      if (ti->font != NULL)
        font = ti->font;

      if (font != NULL)
        GDrawSetFont (base, font);
      width = GDrawGetTextWidth (base, ti->text, -1);
    }
  if (ti->image != NULL)
    {
      iwidth = MENU_ICON_SIZE;
      if (ti->text != NULL)
        skip = GDrawPointsToPixels (base, MENU_ICON_SEP);
    }
  return (width + iwidth + skip);
}

int
GTextInfoGetMaxWidth (GWindow base, GTextInfo **ti, FontInstance * font)
{
  int width = 0, temp;
  int i;

  for (i = 0; ti[i]->text != NULL || ti[i]->image != NULL; ++i)
    {
      if ((temp = GTextInfoGetWidth (base, ti[i], font)) > width)
        width = temp;
    }
  return (width);
}

int
GTextInfoGetHeight (GWindow base, GTextInfo *ti, FontInstance * font)
{
  int fh = 0, as = 0, ds = 0, ld;
  int iheight = 0;
  int height;
  GTextBounds bounds;

  if (ti->font != NULL)
    font = ti->font;
  GDrawGetFontMetrics (base, font, &as, &ds, &ld);
  if (ti->text != NULL)
    {
      GDrawSetFont (base, font);
      GDrawGetTextBounds (base, ti->text, -1, &bounds);
      if (as < bounds.as)
        as = bounds.as;
      if (ds < bounds.ds)
        ds = bounds.ds;
    }
  fh = as + ds;
  if (ti->image != NULL)
    {
      iheight = MENU_ICON_SIZE;
      iheight += 1;
    }
  if ((height = fh) < iheight)
    height = iheight;
  return (height);
}

int
GTextInfoGetMaxHeight (GWindow base, GTextInfo **ti, FontInstance * font,
                       int *allsame)
{
  int height = 0, temp, same = 1;
  int i;

  for (i = 0; ti[i]->text != NULL || ti[i]->image != NULL; ++i)
    {
      temp = GTextInfoGetHeight (base, ti[i], font);
      if (height != 0 && height != temp)
        same = 0;
      if (height < temp)
        height = temp;
    }
  *allsame = same;
  return (height);
}

int
GTextInfoGetAs (GWindow base, GTextInfo *ti, FontInstance * font)
{
  int fh = 0, as = 0, ds = 0, ld;
  int iheight = 0;
  int height;
  GTextBounds bounds;

  GDrawGetFontMetrics (base, font, &as, &ds, &ld);
  if (ti->text != NULL)
    {
      GDrawSetFont (base, font);
      GDrawGetTextBounds (base, ti->text, -1, &bounds);
      if (as < bounds.as)
        as = bounds.as;
      if (ds < bounds.ds)
        ds = bounds.ds;
    }
  fh = as + ds;
  if (ti->image != NULL)
    {
      iheight = MENU_ICON_SIZE;
    }
  if ((height = fh) < iheight)
    height = iheight;

  if (ti->text != NULL)
    return (as + (height > fh ? (height - fh) / 2 : 0));

  return (iheight);
}

int
GTextInfoDraw (GWindow base, int x, int y, GTextInfo *ti,
               FontInstance * font, Color fg, Color sel, int ymax)
{
  int fh = 0, as = 0, ds = 0, ld;
  int iwidth = 0, iheight = 0;
  int height, skip = 0;
  GTextBounds bounds;
  GRect r, old;

  GTextInfoImageLookup (ti);
  GDrawGetFontMetrics (base, font, &as, &ds, &ld);
  if (ti->text != NULL)
    {
      if (ti->font != NULL)
        font = ti->font;
      if (ti->fg != COLOR_DEFAULT && ti->fg != COLOR_UNKNOWN)
        fg = ti->fg;

      GDrawSetFont (base, font);
      GDrawGetTextBounds (base, ti->text, -1, &bounds);
      if (as < bounds.as)
        as = bounds.as;
      if (ds < bounds.ds)
        ds = bounds.ds;
    }
  fh = as + ds;
  if (fg == COLOR_DEFAULT)
    fg = GDrawGetDefaultForeground (GDrawGetDisplayOfWindow (base));
  if (ti->image != NULL)
    {
      iwidth = MENU_ICON_SIZE;
      iheight = MENU_ICON_SIZE + 1;
      if (ti->text != NULL)
        skip = GDrawPointsToPixels (base, MENU_ICON_SEP);
    }
  if ((height = fh) < iheight)
    height = iheight;

  r.y = y;
  r.height = height;
  r.x = 0;
  r.width = 10000;

  if (ti->line)
    {
      _GGroup_Init ();
      GDrawGetClip (base, &r);
      r.x += GDrawPointsToPixels (base, 2);
      r.width -= 2 * GDrawPointsToPixels (base, 2);
      GDrawPushClip (base, &r, &old);
      r.y = y;
      r.height = height;
      r.x = x;
      r.width = 10000;
      GBoxDrawHLine (base, &r, &_GGroup_LineBox);
      GDrawPopClip (base, &old);
    }
  else
    {
      if ((ti->selected && sel != COLOR_DEFAULT)
          || (ti->bg != COLOR_DEFAULT && ti->bg != COLOR_UNKNOWN))
        {
          Color bg = ti->bg;
          if (ti->selected)
            {
              if (sel == COLOR_DEFAULT)
                sel = fg;
              bg = sel;
              if (sel == fg)
                {
                  fg = ti->bg;
                  if (fg == COLOR_DEFAULT || fg == COLOR_UNKNOWN)
                    fg =
                      GDrawGetDefaultBackground (GDrawGetDisplayOfWindow
                                                 (base));
                }
            }
          GDrawFillRect (base, &r, bg);
        }

      if (ti->image != NULL && ti->image_precedes)
        {
          GDrawDrawScaledImage (base, ti->image, x,
                                y + (iheight > as ? 0 : as - iheight));
          x += iwidth + skip;
        }
      if (ti->text != NULL)
        {
          int ypos = y + as + (height > fh ? (height - fh) / 2 : 0);
          int width = GDrawDrawText (base, x, ypos, ti->text, -1, fg);
          _ggadget_underlineMnemonic (base, x, ypos, ti->text, ti->mnemonic, fg,
                                      ymax);
          x += width + skip;
        }
      if (ti->image != NULL && !ti->image_precedes)
        GDrawDrawScaledImage (base, ti->image, x,
                              y + (iheight > as ? 0 : as - iheight));
    }

  return (height);
}

uint32_t *
utf82u_mncopy (const char *utf8buf, uint32_t *mn)
{
  int len = strlen (utf8buf);
  uint32_t *ubuf = xmalloc ((len + 1) * sizeof (uint32_t));
  uint32_t *upt = ubuf, *uend = ubuf + len;
  const uint8_t *pt = (const uint8_t *) utf8buf, *end = pt + strlen (utf8buf);
  int w;
  int was_mn = false;

  *mn = '\0';
  while (pt < end && *pt != '\0' && upt < uend)
    {
      if (*pt <= 127)
        {
          if (*pt != '_')
            *upt = *pt++;
          else
            {
              was_mn = 2;
              ++pt;
              --upt;
            }
        }
      else if (*pt <= 0xdf)
        {
          *upt = ((*pt & 0x1f) << 6) | (pt[1] & 0x3f);
          pt += 2;
        }
      else if (*pt <= 0xef)
        {
          *upt = ((*pt & 0xf) << 12) | ((pt[1] & 0x3f) << 6) | (pt[2] & 0x3f);
          pt += 3;
        }
      else if (upt + 1 < uend)
        {
          /* Um... I don't support surrogates */
          w = (((*pt & 0x7) << 2) | ((pt[1] & 0x30) >> 4)) - 1;
          *upt++ =
            0xd800 | (w << 6) | ((pt[1] & 0xf) << 2) | ((pt[2] & 0x30) >> 4);
          *upt = 0xdc00 | ((pt[2] & 0xf) << 6) | (pt[3] & 0x3f);
          pt += 4;
        }
      else
        {
          /* no space for surrogate */
          pt += 4;
        }
      ++upt;
      if (was_mn == 1)
        {
          *mn = upt[-1];
          if (islower (*mn))
            *mn = toupper (*mn);
        }
      --was_mn;
    }
  *upt = '\0';
  return (ubuf);
}

GTextInfo *
GTextInfoCopy (GTextInfo *ti)
{
  GTextInfo *copy;

  copy = xmalloc (sizeof (GTextInfo));
  *copy = *ti;
  copy->text_is_1byte = false;
  if (copy->fg == 0 && copy->bg == 0)
    {
      copy->fg = copy->bg = COLOR_UNKNOWN;
    }
  if (ti->text != NULL)
    {
      if (ti->text_is_1byte && ti->text_has_mnemonic)
        {
          copy->text = utf82u_mncopy ((char *) copy->text, &copy->mnemonic);
          copy->text_has_mnemonic = false;
          copy->text_is_1byte = false;
        }
      else if (ti->text_is_1byte)
        {
          copy->text = utf82u_copy ((char *) copy->text);
          copy->text_is_1byte = false;
        }
      else
        copy->text = x_u32_strdup_or_null (copy->text);
    }
  return (copy);
}

static char *imagedir = "fontforge-pixmaps";    /* This is the system pixmap directory */
static char **imagepath;        /* May contain user directories too */

struct image_bucket
{
  struct image_bucket *next;
  char *filename, *absname;
  GImage *image;
};

#define IC_SIZE	127
static struct image_bucket *imagecache[IC_SIZE];

static int
hash_filename (char *_pt)
{
  unsigned char *pt = (unsigned char *) _pt;
  int val = 0;

  while (*pt)
    {
      val <<= 1;
      if (val & 0x8000)
        {
          val &= ~0x8000;
          val ^= 1;
        }
      val ^= *pt++;
    }
  return (val % IC_SIZE);
}

static void
ImagePathDefault (void)
{
  extern char *_GGadget_ImagePath;

  if (imagepath == NULL)
    {
      imagepath = xmalloc (2 * sizeof (void *));
      imagepath[0] = xstrdup_or_null (imagedir);
      imagepath[1] = NULL;
      free (_GGadget_ImagePath);
      _GGadget_ImagePath = xstrdup ("=");
    }
}

char **
_GGadget_GetImagePath (void)
{
  ImagePathDefault ();
  return (imagepath);
}

int
_GGadget_ImageInCache (GImage *image)
{
  int i;
  struct image_bucket *bucket;

  for (i = 0; i < IC_SIZE; ++i)
    {
      for (bucket = imagecache[i]; bucket != NULL; bucket = bucket->next)
        if (bucket->image == image)
          return (true);
    }
  return (false);
}

static void
ImageCacheReload (void)
{
  int i, k;
  struct image_bucket *bucket;
  char *path = NULL;
  GImage *temp, hold;

  ImagePathDefault ();

  /* Try to reload the cache from the new directory */
  /* If a file doesn't exist in the new dir when it did in the old then */
  /*  retain the old copy (people may hold pointers to it) */
  for (i = 0; i < IC_SIZE; ++i)
    {
      for (bucket = imagecache[i]; bucket != NULL; bucket = bucket->next)
        {
          temp = NULL;
          k = 0;
          while (temp == NULL && imagepath[k] != NULL)
            {
              path = x_gc_strjoin (imagepath[k], "/", bucket->filename, NULL);
              temp = GImageRead (path);
              k++;
            }
          if (temp != NULL)
            {
              if (bucket->image == NULL)
                bucket->image = temp;
              else
                {
                  /* Need to retain the GImage point, but update its */
                  /*  contents, and free the old stuff */
                  hold = *(bucket->image);
                  *bucket->image = *temp;
                  *temp = hold;
                  GImageDestroy (temp);
                }
              free (bucket->absname);
              bucket->absname = xstrdup_or_null (path);
            }
        }
    }
  free (path);
}

void
GGadgetSetImageDir (char *dir)
{
  int k;
  extern char *_GGadget_ImagePath;

  if (dir != NULL && strcmp (imagedir, dir) != 0)
    {
      char *old = imagedir;
      imagedir = xstrdup_or_null (dir);
      if (imagepath != NULL)
        {
          for (k = 0; imagepath[k] != NULL; ++k)
            if (strcmp (imagepath[k], old) == 0)
              break;
          if (imagepath[k] != NULL)
            {
              free (imagepath[k]);
              imagepath[k] = imagedir;
              ImageCacheReload ();
            }
          free (_GGadget_ImagePath);
          _GGadget_ImagePath = xstrdup ("=");
        }
    }
}

static char *
ImagePathFigureElement (char *start, int len)
{
  if (*start == '=' && len == 1)
    return (imagedir);
  else if (*start == '~' && start[1] == '/' && len >= 2)
    {
      int hlen = strlen (GFileGetHomeDir ());
      char *absname = xmalloc (hlen + len + 8);
      strcpy (absname, GFileGetHomeDir ());
      strncpy (absname + hlen, start + 1, len - 1);
      absname[hlen + len - 1] = '\0';
      return (absname);
    }
  else
    return (xstrndup (start, len));
}

#ifndef PATH_SEPARATOR
#define PATH_SEPARATOR ':'
#endif

void
GGadgetSetImagePath (char *path)
{
  int cnt, k;
  char *pt, *end;
  extern char *_GGadget_ImagePath;

  if (path == NULL)
    return;
  free (_GGadget_ImagePath);

  if (imagepath != NULL)
    {
      for (k = 0; imagepath[k] != NULL; ++k)
        free (imagepath[k]);
      free (imagepath);
    }
  for (cnt = 0, pt = path; (end = strchr (pt, PATH_SEPARATOR)) != NULL;
       ++cnt, pt = end + 1);
  imagepath = xmalloc ((cnt + 2) * sizeof (char *));
  for (cnt = 0, pt = path; (end = strchr (pt, PATH_SEPARATOR)) != NULL;
       ++cnt, pt = end + 1)
    imagepath[cnt] = ImagePathFigureElement (pt, end - pt);
  imagepath[cnt] = ImagePathFigureElement (pt, strlen (pt));
  imagepath[cnt + 1] = NULL;
  ImageCacheReload ();
  _GGadget_ImagePath = xstrdup_or_null (path);
}

static GImage *
_GGadgetImageCache (char *filename, char **foundname)
{
  int index = hash_filename (filename);
  struct image_bucket *bucket;
  int k;

  for (bucket = imagecache[index]; bucket != NULL; bucket = bucket->next)
    {
      if (strcmp (bucket->filename, filename) == 0)
        {
          if (foundname != NULL)
            *foundname = xstrdup_or_null (bucket->absname);
          return (bucket->image);
        }
    }
  bucket = xcalloc (1, sizeof (struct image_bucket));
  bucket->next = imagecache[index];
  imagecache[index] = bucket;
  bucket->filename = xstrdup_or_null (filename);

  ImagePathDefault ();

  char *path = NULL;
  if (g_path_is_absolute (filename))
    {
      path = filename;
      bucket->image = GImageRead (path);
    }
  else
    {
      bucket->image = NULL;
      k = 0;
      while (bucket->image == NULL && imagepath[k] != NULL)
        {
          path = x_gc_strjoin (imagepath[k], "/", filename, NULL);
          bucket->image = GImageRead (path);
          k++;
        }
    }
  if (bucket->image != NULL)
    {
      bucket->absname = xstrdup_or_null (path);

      /* Play with the clut to make white be transparent */
      struct _GImage *base = bucket->image->u.image;
      if (base->image_type == it_mono && base->clut == NULL)
        base->trans = 1;
      else if (base->image_type != it_true && base->clut != NULL
               && base->trans == 0xffffffff)
        {
          int i;
          for (i = 0; i < base->clut->clut_len; ++i)
            {
              if (base->clut->clut[i] == 0xffffff)
                {
                  base->trans = i;
                  break;
                }
            }
        }
    }
  if (foundname != NULL && bucket->image != NULL)
    *foundname = xstrdup_or_null (bucket->absname);
  return (bucket->image);
}

GImage *
GGadgetImageCache (char *filename)
{
  return (_GGadgetImageCache (filename, NULL));
}

/* Substitutes an image contents with what's found in cache. */
/* That is, unless there is nothing found in the cache.      */
bool
TryGGadgetImageCache (GImage *image, char *name)
{
  GImage *loaded = GGadgetImageCache (name);
  if (loaded != NULL)
    *image = *loaded;
  else
    fprintf (stderr, "Warning: image '%s' was not found\n", name);

  return (loaded != NULL);
}

GResImage *
GGadgetResourceFindImage (char *name, char *def)
{
  GImage *ret;
  char *fname;
  GResImage *ri;

  fname = GResourceFindString (name);
  if (fname == NULL)
    fname = xstrdup_or_null (def);
  if (fname == NULL)
    return (NULL);

  ri = xcalloc (1, sizeof (GResImage));
  ri->filename = fname;

  if (*fname == '/')
    ret = GImageRead (fname);
  else if (*fname == '~' && fname[1] == '/')
    {
      char *absname =
        xmalloc (strlen (GFileGetHomeDir ()) + strlen (fname) + 8);
      strcpy (absname, GFileGetHomeDir ());
      strcat (absname, fname + 1);
      ret = GImageRead (absname);
      free (fname);
      ri->filename = fname = absname;
    }
  else
    {
      char *absname;
      ret = _GGadgetImageCache (fname, &absname);
      if (ret)
        {
          free (fname);
          ri->filename = fname = absname;
        }
    }
  if (ret == NULL)
    {
      ri->filename = NULL;
      free (fname);
    }
  else
    ri->image = ret;

  return (ri);
}

void
GTextInfoImageLookup (GTextInfo *ti)
{
  char *pt;
  int any;

  if (ti->image == NULL)
    return;

  /* Image might be an image pointer, or it might be a filename we want to */
  /*  read and convert into an image. If it's an image it will begin with */
  /*  a short containing a small number (usually 1), which won't look like */
  /*  a filename */
  any = 0;
  for (pt = (char *) (ti->image); *pt != '\0'; ++pt)
    {
      if (*pt < ' ' || *pt >= 0x7f)
        return;
      if (*pt == '.')
        any = 1;
    }
  if (!any)                     /* Must have an extension */
    return;

  ti->image = GGadgetImageCache ((char *) (ti->image));
}

GTextInfo **
GTextInfoArrayFromList (GTextInfo *ti, uint16_t *cnt)
{
  int i;
  GTextInfo **arr;

  i = 0;
  if (ti != NULL)
    for (; ti[i].text != NULL || ti[i].image != NULL || ti[i].line; ++i);
  if (i == 0)
    {
      arr = xmalloc (sizeof (GTextInfo *));
      i = 0;
    }
  else
    {
      arr = xmalloc ((i + 1) * sizeof (GTextInfo *));
      for (i = 0; ti[i].text != NULL || ti[i].image != NULL || ti[i].line; ++i)
        arr[i] = GTextInfoCopy (&ti[i]);
    }
  arr[i] = xcalloc (1, sizeof (GTextInfo));
  if (cnt != NULL)
    *cnt = i;
  return (arr);
}

GTextInfo **
GTextInfoArrayCopy (GTextInfo **ti)
{
  int i;
  GTextInfo **arr;

  if (ti == NULL
      || (ti[0]->image == NULL && ti[0]->text == NULL && !ti[0]->line))
    {
      arr = xmalloc (sizeof (GTextInfo *));
      i = 0;
    }
  else
    {
      for (i = 0; ti[i]->text != NULL || ti[i]->image != NULL || ti[i]->line;
           ++i);
      arr = xmalloc ((i + 1) * sizeof (GTextInfo *));
      for (i = 0; ti[i]->text != NULL || ti[i]->image != NULL || ti[i]->line;
           ++i)
        arr[i] = GTextInfoCopy (ti[i]);
    }
  arr[i] = xcalloc (1, sizeof (GTextInfo));
  return (arr);
}

int
GTextInfoArrayCount (GTextInfo **ti)
{
  int i;

  for (i = 0; ti[i]->text || ti[i]->image || ti[i]->line; ++i);
  return (i);
}

void
GTextInfoFree (GTextInfo *ti)
{
  if (!ti->text_has_mnemonic)
    free (ti->text);
  free (ti);
}

void
GTextInfoListFree (GTextInfo *ti)
{
  int i;

  if (ti == NULL)
    return;

  for (i = 0; ti[i].text != NULL || ti[i].image != NULL || ti[i].line; ++i)
    if (!ti[i].text_has_mnemonic)
      free (ti[i].text);
  free (ti);
}

void
GTextInfoArrayFree (GTextInfo **ti)
{
  int i;

  if (ti == NULL)
    return;
  for (i = 0; ti[i]->text || ti[i]->image || ti[i]->line; ++i)
    GTextInfoFree (ti[i]);
  GTextInfoFree (ti[i]);        /* And free the null entry at end */
  free (ti);
}

int
GTextInfoCompare (GTextInfo *ti1, GTextInfo *ti2)
{
  if (ti1->sort_me_first_in_list != ti2->sort_me_first_in_list)
    {
      if (ti1->sort_me_first_in_list)
        return (-1);
      else
        return (1);
    }

  if (ti1->text == NULL && ti2->text == NULL)
    return (0);
  else if (ti1->text == NULL)
    return (-1);
  else if (ti2->text == NULL)
    return (1);
  else
    {
      int ret;
      int error = u32_normcoll (ti1->text, u32_strlen (ti1->text),
                                ti2->text, u32_strlen (ti2->text),
                                UNINORM_NFC, &ret);
      ret = (error == 0) ? ret : 0;
      return (ret);
    }
}

GTextInfo **
GTextInfoFromChars (char **array, int len)
{
  int i;
  GTextInfo **ti;

  if (array == NULL || len == 0)
    return (NULL);
  if (len == -1)
    {
      for (len = 0; array[len] != NULL; ++len);
    }
  else
    {
      for (i = 0; i < len && array[i] != NULL; ++i);
      len = i;
    }
  ti = xmalloc ((i + 1) * sizeof (GTextInfo *));
  for (i = 0; i < len; ++i)
    {
      ti[i] = xcalloc (1, sizeof (GTextInfo));
      ti[i]->text = x_u8_to_u32 (u8_force_valid (array[i]));
      ti[i]->fg = ti[i]->bg = COLOR_DEFAULT;
    }
  ti[i] = xcalloc (1, sizeof (GTextInfo));
  return (ti);
}

void
GMenuItemArrayFree (GMenuItem *mi)
{
  int i;

  if (mi == NULL)
    return;
  for (i = 0; mi[i].ti.text || mi[i].ti.image || mi[i].ti.line; ++i)
    {
      GMenuItemArrayFree (mi[i].sub);
      free (mi[i].ti.text);
    }
  free (mi);
}

VISIBLE GMenuItem *
GMenuItemArrayCopy (GMenuItem *mi, uint16_t *cnt)
{
  int i;
  GMenuItem *arr;

  if (mi == NULL)
    return (NULL);
  for (i = 0; mi[i].ti.text != NULL || mi[i].ti.image != NULL || mi[i].ti.line;
       ++i);
  if (i == 0)
    return (NULL);
  arr = xmalloc ((i + 1) * sizeof (GMenuItem));
  for (i = 0; mi[i].ti.text != NULL || mi[i].ti.image != NULL || mi[i].ti.line;
       ++i)
    {
      arr[i] = mi[i];
      if (mi[i].shortcut != NULL)
        GMenuItemParseShortCut (&arr[i], mi[i].shortcut);
      GTextInfoImageLookup (&arr[i].ti);
      if (mi[i].ti.text != NULL)
        {
          if (mi[i].ti.text_has_mnemonic && mi[i].ti.text_is_1byte)
            arr[i].ti.text =
              utf82u_mncopy ((char *) mi[i].ti.text, &arr[i].ti.mnemonic);
          else if (mi[i].ti.text_is_1byte)
            arr[i].ti.text = utf82u_copy ((char *) mi[i].ti.text);
          else
            arr[i].ti.text = x_u32_strdup_or_null (mi[i].ti.text);
          arr[i].ti.text_has_mnemonic = arr[i].ti.text_is_1byte = false;
        }
      if (islower (arr[i].ti.mnemonic))
        arr[i].ti.mnemonic = toupper (arr[i].ti.mnemonic);
      if (islower (arr[i].shortcut_char))
        arr[i].shortcut_char = toupper (arr[i].shortcut_char);
      if (mi[i].sub != NULL)
        arr[i].sub = GMenuItemArrayCopy (mi[i].sub, NULL);
    }
  memset (&arr[i], '\0', sizeof (GMenuItem));
  if (cnt != NULL)
    *cnt = i;
  return (arr);
}

int
GMenuItemArrayMask (GMenuItem *mi)
{
  int mask = 0;
  int i;

  for (i = 0; mi[i].ti.text != NULL || mi[i].ti.image != NULL || mi[i].ti.line;
       ++i)
    {
      if (mi[i].sub != NULL)
        mask |= GMenuItemArrayMask (mi[i].sub);
      else
        mask |= mi[i].short_mask;
    }
  return (mask);
}

int
GMenuItemArrayAnyUnmasked (GMenuItem *mi)
{
  int i;

  for (i = 0; mi[i].ti.text != NULL || mi[i].ti.image != NULL || mi[i].ti.line;
       ++i)
    {
      if (mi[i].sub != NULL)
        {
          if (GMenuItemArrayAnyUnmasked (mi[i].sub))
            return (true);
        }
      else
        {
          if ((mi[i].short_mask & ~ksm_shift) == 0 && mi[i].shortcut_char != 0)
            return (true);
        }
    }
  return (false);
}

static const char *shortcut_domain = "shortcuts";

void
GMenuSetShortcutDomain (const char *domain)
{
  shortcut_domain = domain;
}

const char *
GMenuGetShortcutDomain (void)
{
  return (shortcut_domain);
}

static struct
{
  char *modifier;
  int mask;
  char *alt;
} modifiers[] =
{
  {
  "Ctl+", ksm_control, NULL},
  {
  "Control+", ksm_control, NULL},
  {
  "Shft+", ksm_shift, NULL},
  {
  "Shift+", ksm_shift, NULL},
  {
  "CapsLk+", ksm_capslock, NULL},
  {
  "CapsLock+", ksm_capslock, NULL},
  {
  "Meta+", ksm_meta, NULL},
  {
  "Alt+", ksm_meta, NULL},
  {
  "Flag0x01+", 0x01, NULL},
  {
  "Flag0x02+", 0x02, NULL},
  {
  "Flag0x04+", 0x04, NULL},
  {
  "Flag0x08+", 0x08, NULL},
  {
  "Flag0x10+", 0x10, NULL},
  {
  "Flag0x20+", 0x20, NULL},
  {
  "Flag0x40+", 0x40, NULL},
  {
  "Flag0x80+", 0x80, NULL},
  {
  "Opt+", ksm_meta, NULL},
  {
  "Option+", ksm_meta, NULL},
    /* We used to map command to control on the mac, no longer, let it be itself */
  {
  "Command+", ksm_cmdmacosx, NULL},
  {
  "Cmd+", ksm_cmdmacosx, NULL},
  {
  "NumLk+", ksm_cmdmacosx, NULL},       /* This is unfortunate. Numlock should be ignored, Command should not */
  {
  "NumLock+", ksm_cmdmacosx, NULL},
  {
  NULL, 0, NULL}
  /* Windows flag key=Super (keysym ffeb/ffec) key maps to 0x40 on my machine */
};

static void
initmods (void)
{
  if (modifiers[0].alt == NULL)
    {
      int i;
      for (i = 0; modifiers[i].modifier != NULL; ++i)
        modifiers[i].alt = dgettext (shortcut_domain, modifiers[i].modifier);
    }
}

int
GMenuItemParseMask (char *shortcut)
{
  char *pt, *sh;
  int mask, temp, i;

  sh = dgettext (shortcut_domain, shortcut);
  if (sh == shortcut && strlen (shortcut) > 2 && shortcut[2] == '*')
    {
      sh = dgettext (shortcut_domain, shortcut + 3);
      if (sh == shortcut + 3)
        sh = shortcut;
    }
  pt = strchr (sh, '|');
  if (pt != NULL)
    sh = pt + 1;
  if (*sh == '\0' || strcmp (sh, "No Shortcut") == 0
      || strcmp (sh, "None") == 0)
    return (0);

  initmods ();

  mask = 0;
  while (true)
    {
      pt = strchr (sh, '+');
      if (pt == sh || *sh == '\0')
        return (mask);
      if (pt == NULL)
        pt = sh + strlen (sh);
      for (i = 0; modifiers[i].modifier != NULL; ++i)
        {
          if (strncasecmp (sh, modifiers[i].modifier, pt - sh) == 0)
            break;
        }
      if (modifiers[i].modifier == NULL)
        {
          for (i = 0; modifiers[i].alt != NULL; ++i)
            {
              if (strncasecmp (sh, modifiers[i].alt, pt - sh) == 0)
                break;
            }
        }
      if (modifiers[i].modifier != NULL)
        mask |= modifiers[i].mask;
      else if (sscanf (sh, "0x%x", &temp) == 1)
        mask |= temp;
      else
        {
          fprintf (stderr, "Could not parse short cut: %s\n", shortcut);
          return (0);
        }
      sh = pt + 1;
    }
}

static const char *
get_keyboard_shortcut (const char *key)
{
  SCM proc = scm_c_public_ref (_SHORTCUTS_MODULE, "shortcut-ref");
  SCM k = scm_from_utf8_string (key);
  SCM v = scm_call_1 (proc, k);
  return
    (scm_is_true (v)) ? (x_gc_grabstr (scm_to_utf8_stringn (v, NULL))) : NULL;
}

void
GMenuItemParseShortCut (GMenuItem *mi, char *shortcut)
{
  const char *pt, *sh;
  int mask, temp, i;

  mi->short_mask = 0;
  mi->shortcut_char = '\0';

  sh = get_keyboard_shortcut (shortcut);
  if (sh == NULL)
    sh = dgettext (shortcut_domain, shortcut);
  /* shortcut might be "Open|Ctl+O" meaning the Open menu item is bound to ^O */
  /*  or "CV*Open|Ctl+O" meaning that in the charview the Open menu item ... */
  /*  but if CV*Open|Ctl+O isn't found then check simple "Open|Ctl+O" as a default */
  if (sh == shortcut && strlen (shortcut) > 2 && shortcut[2] == '*')
    {
      sh = dgettext (shortcut_domain, shortcut + 3);
      if (sh == shortcut + 3)
        sh = shortcut;
    }
  pt = strchr (sh, '|');
  if (pt != NULL)
    sh = pt + 1;
  if (*sh == '\0' || strcmp (sh, "No Shortcut") == 0
      || strcmp (sh, "None") == 0)
    return;

  initmods ();

  mask = 0;
  while ((pt = strchr (sh, '+')) != NULL && pt != sh)
    {                           /* A '+' can also occur as the short cut char itself */
      for (i = 0; modifiers[i].modifier != NULL; ++i)
        {
          if (strncasecmp (sh, modifiers[i].modifier, pt - sh) == 0)
            break;
        }
      if (modifiers[i].modifier == NULL)
        {
          for (i = 0; modifiers[i].alt != NULL; ++i)
            {
              if (strncasecmp (sh, modifiers[i].alt, pt - sh) == 0)
                break;
            }
        }
      if (modifiers[i].modifier != NULL)
        mask |= modifiers[i].mask;
      else if (sscanf (sh, "0x%x", &temp) == 1)
        mask |= temp;
      else
        {
          fprintf (stderr, "Could not parse short cut: %s\n", shortcut);
          return;
        }
      sh = pt + 1;
    }
  mi->short_mask = mask;
  for (i = 0; i < 0x100; ++i)
    {
      if (GDrawKeysyms[i] != NULL
          && u8_strcmp (x_gc_u32_to_u8 (u32_force_valid (GDrawKeysyms[i])),
                        u8_force_valid (sh)) == 0)
        {
          mi->shortcut_char = 0xff00 + i;
          break;
        }
    }
  if (i == 0x100)
    {
      if (mask == 0)
        {
          static int first = true;
          if (first)
            {
              fprintf (stderr, "Warning: No modifiers in short cut: %s\n",
                       shortcut);
              first = false;
            }
        }
      mi->shortcut_char = u8_get_next ((const uint8_t **) &sh);
      if (*sh != '\0')
        {
          fprintf (stderr, "Unexpected characters at end of short cut: %s\n",
                   shortcut);
          return;
        }
    }
}

int
GIntGetResource (enum int_res index)
{
  static int ret;
  const char *pt;
  char *end;
  if (index == _NUM_Buttonsize)
    {
/* TRANSLATORS:
 * This is an unusual string. It is used to get around a limitation in
 * FontForge's widget set. You should put a number here.  The number should be
 * the number of points used for a standard sized button.  It should be big
 * enough to contain "OK", "Cancel", "New...", "Edit...", "Delete" (in their
 * translated forms of course).
 */
      pt = C_ ("GGadget button size", "55");
      ret = strtol (pt, &end, 10);
      if (pt == end || ret > 4000)
        ret = 55;
    }
  else if (index == _NUM_ScaleFactor)
    {
/* TRANSLATORS:
 * This is an unusual string. It is used to get around a limitation in
 * FontForge's widget set. You should put a number here.  The number should be
 * a percentage and indicates the ratio of the length of a string in your
 * language to the same string's length in English.  Suppose it takes 116
 * pixels to say "Ne pas enregistrer" in French but only 67 pixels to say
 * "Don't Save" in English. Then a value for ScaleFactor might be
 * 116*100/67 = 173
 */
      pt = C_ ("GGadget scale factor", "100");
      ret = strtol (pt, &end, 10);
      if (pt == end || ret < 20 || ret > 4000)
        ret = 100;
    }
  else
    {
      GDrawIError ("Unknown integer resource: %d", index);
      ret = 0;
    }

  return ret;
}
