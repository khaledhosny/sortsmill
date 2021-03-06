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

/* Copyright (C) 2007-2012 by George Williams */
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
#include "fontforgeui.h"
#include <chardata.h>
#include <utype.h>
#include <ustring.h>
#include <math.h>
#include <locale.h>
#include <stdlib.h>
#include "ttf.h"
#include <gkeysym.h>
#include "lookups.h"
#include <assert.h>

int default_autokern_dlg = true;

/* ************************************************************************** */
/* ******************************* UI routines ****************************** */
/* ************************************************************************** */

GTextInfo **
SFLookupListFromType (SplineFont *sf, int lookup_type)
{
  int isgpos = (lookup_type >= gpos_start);
  int k, cnt;
  OTLookup *otl;
  GTextInfo **ti;

  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
           otl = otl->next)
        {
          if (lookup_type == gsub_start || lookup_type == gpos_start
              || otl->lookup_type == lookup_type)
            {
              if (k)
                {
                  ti[cnt] = xcalloc (1, sizeof (GTextInfo));
                  ti[cnt]->userdata = (void *) otl;
                  ti[cnt]->fg = ti[cnt]->bg = COLOR_DEFAULT;
                  ti[cnt]->text = utf82u_copy (otl->lookup_name);
                }
              ++cnt;
            }
        }
      if (!k)
        ti = xcalloc (cnt + 2, sizeof (GTextInfo *));
      else
        ti[cnt] = xcalloc (1, sizeof (GTextInfo));
    }
  return ti;
}

GTextInfo *
SFLookupArrayFromType (SplineFont *sf, int lookup_type)
{
  int isgpos = (lookup_type >= gpos_start);
  int k, cnt;
  OTLookup *otl;
  GTextInfo *ti;

  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
           otl = otl->next)
        {
          if (lookup_type == gsub_start || lookup_type == gpos_start
              || otl->lookup_type == lookup_type)
            {
              if (k)
                {
                  ti[cnt].userdata = (void *) otl;
                  ti[cnt].fg = ti[cnt].bg = COLOR_DEFAULT;
                  ti[cnt].text = utf82u_copy (otl->lookup_name);
                }
              ++cnt;
            }
        }
      if (!k)
        ti = xcalloc (cnt + 2, sizeof (GTextInfo));
    }
  return ti;
}

GTextInfo *
SFLookupArrayFromMask (SplineFont *sf, int mask)
{
  int k, cnt;
  OTLookup *otl;
  GTextInfo *ti;
  int isgpos;

  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (isgpos = 0; isgpos < 2; ++isgpos)
        {
          for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
               otl = otl->next)
            {
              int lmask =
                isgpos ? (gpos_single_mask << (otl->lookup_type - gpos_single))
                : (gsub_single_mask << (otl->lookup_type - gsub_single));
              if (mask == 0 || (mask & lmask))
                {
                  if (k)
                    {
                      ti[cnt].userdata = (void *) otl;
                      ti[cnt].fg = ti[cnt].bg = COLOR_DEFAULT;
                      ti[cnt].text_is_1byte = true;
                      ti[cnt].text =
                        (uint32_t *) xstrdup_or_null (otl->lookup_name);
                    }
                  ++cnt;
                }
            }
        }
      if (!k)
        ti = xcalloc (cnt + 2, sizeof (GTextInfo));
    }
  return ti;
}

/* ************************************************************************** */
/* ********************** Lookup dialog and subdialogs ********************** */
/* ************************************************************************** */

static GTextInfo gsub_lookuptypes[] = {
  {(uint32_t *) NC_ ("Lookup Type", "Unspecified"), NULL, 0, 0,
   (void *) ot_undef, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Single Substitution"), NULL, 0, 0,
   (void *) gsub_single, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Multiple Substitution"), NULL, 0, 0,
   (void *) gsub_multiple, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Alternate Substitution"), NULL, 0, 0,
   (void *) gsub_alternate, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Ligature Substitution"), NULL, 0, 0,
   (void *) gsub_ligature, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Contextual Substitution"), NULL, 0, 0,
   (void *) gsub_context, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Contextual Chaining Substitution"), NULL,
   0, 0, (void *) gsub_contextchain, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Reverse Chaining Substitution"), NULL, 0,
   0, (void *) gsub_reversecchain, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  GTEXTINFO_EMPTY
};

static GTextInfo gpos_lookuptypes[] = {
  {(uint32_t *) NC_ ("Lookup Type", "Unspecified"), NULL, 0, 0,
   (void *) ot_undef, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Single Position"), NULL, 0, 0,
   (void *) gpos_single, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Pair Position (kerning)"), NULL, 0, 0,
   (void *) gpos_pair, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Cursive Position"), NULL, 0, 0,
   (void *) gpos_cursive, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Mark to Base Position"), NULL, 0, 0,
   (void *) gpos_mark2base, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Mark to Ligature Position"), NULL, 0, 0,
   (void *) gpos_mark2ligature, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Mark to Mark Position"), NULL, 0, 0,
   (void *) gpos_mark2mark, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Contextual Position"), NULL, 0, 0,
   (void *) gpos_context, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Lookup Type", "Contextual Chaining Position"), NULL, 0, 0,
   (void *) gpos_contextchain, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  GTEXTINFO_EMPTY
};
static GTextInfo *lookuptypes[3] = { gsub_lookuptypes, gpos_lookuptypes, NULL };

    /* see also list in tottfgpos.c mapping code points to scripts */
    /* see also list in lookups.c for non-ui access to these data */
GTextInfo scripts[] = {
  {(uint32_t *) NC_ ("Script", "Arabic"), NULL, 0, 0,
   (void *) CHR ('a', 'r', 'a', 'b'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Aramaic"), NULL, 0, 0,
   (void *) CHR ('a', 'r', 'a', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Armenian"), NULL, 0, 0,
   (void *) CHR ('a', 'r', 'm', 'n'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Avestan"), NULL, 0, 0,
   (void *) CHR ('a', 'v', 'e', 's'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Balinese"), NULL, 0, 0,
   (void *) CHR ('b', 'a', 'l', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Batak"), NULL, 0, 0,
   (void *) CHR ('b', 'a', 't', 'k'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Bengali"), NULL, 0, 0,
   (void *) CHR ('b', 'e', 'n', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Bengali2"), NULL, 0, 0,
   (void *) CHR ('b', 'n', 'g', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Bliss Symbolics"), NULL, 0, 0,
   (void *) CHR ('b', 'l', 'i', 's'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Bopomofo"), NULL, 0, 0,
   (void *) CHR ('b', 'o', 'p', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Brāhmī"), NULL, 0, 0,
   (void *) CHR ('b', 'r', 'a', 'h'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Braille"), NULL, 0, 0,
   (void *) CHR ('b', 'r', 'a', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Buginese"), NULL, 0, 0,
   (void *) CHR ('b', 'u', 'g', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Buhid"), NULL, 0, 0,
   (void *) CHR ('b', 'u', 'h', 'd'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Byzantine Music"), NULL, 0, 0,
   (void *) CHR ('b', 'y', 'z', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Canadian Syllabics"), NULL, 0, 0,
   (void *) CHR ('c', 'a', 'n', 's'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cham"), NULL, 0, 0,
   (void *) CHR ('c', 'h', 'a', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cherokee"), NULL, 0, 0,
   (void *) CHR ('c', 'h', 'e', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cirth"), NULL, 0, 0,
   (void *) CHR ('c', 'i', 'r', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "CJK Ideographic"), NULL, 0, 0,
   (void *) CHR ('h', 'a', 'n', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Coptic"), NULL, 0, 0,
   (void *) CHR ('c', 'o', 'p', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cypro-Minoan"), NULL, 0, 0,
   (void *) CHR ('c', 'p', 'r', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cypriot syllabary"), NULL, 0, 0,
   (void *) CHR ('c', 'p', 'm', 'n'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Cyrillic"), NULL, 0, 0,
   (void *) CHR ('c', 'y', 'r', 'l'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Default"), NULL, 0, 0,
   (void *) CHR ('D', 'F', 'L', 'T'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Deseret (Mormon)"), NULL, 0, 0,
   (void *) CHR ('d', 's', 'r', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Devanagari"), NULL, 0, 0,
   (void *) CHR ('d', 'e', 'v', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Devanagari2"), NULL, 0, 0,
   (void *) CHR ('d', 'e', 'v', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Egyptian demotic"), NULL, 0, 0, (void *) CHR('e','g','y','d'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'}, */
/*  { (uint32_t *) NC_("Script", "Egyptian hieratic"), NULL, 0, 0, (void *) CHR('e','g','y','h'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'}, */
/* TRANSLATORS: Someone asked if FontForge actually was prepared generate hieroglyph output */
/* because of this string. No. But OpenType and Unicode have placeholders for */
/* dealing with these scripts against the day someone wants to use them. So */
/* FontForge must be prepared to deal with those placeholders if nothing else. */
/*  { (uint32_t *) NC_("Script", "Egyptian hieroglyphs"), NULL, 0, 0, (void *) CHR('e','g','y','p'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'}, */
  {(uint32_t *) NC_ ("Script", "Ethiopic"), NULL, 0, 0,
   (void *) CHR ('e', 't', 'h', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Georgian"), NULL, 0, 0,
   (void *) CHR ('g', 'e', 'o', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Glagolitic"), NULL, 0, 0,
   (void *) CHR ('g', 'l', 'a', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Gothic"), NULL, 0, 0,
   (void *) CHR ('g', 'o', 't', 'h'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Greek"), NULL, 0, 0,
   (void *) CHR ('g', 'r', 'e', 'k'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Gujarati"), NULL, 0, 0,
   (void *) CHR ('g', 'u', 'j', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Gujarati2"), NULL, 0, 0,
   (void *) CHR ('g', 'j', 'r', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Gurmukhi"), NULL, 0, 0,
   (void *) CHR ('g', 'u', 'r', 'u'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Gurmukhi2"), NULL, 0, 0,
   (void *) CHR ('g', 'u', 'r', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Hangul Jamo"), NULL, 0, 0,
   (void *) CHR ('j', 'a', 'm', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Hangul"), NULL, 0, 0,
   (void *) CHR ('h', 'a', 'n', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Hanunóo"), NULL, 0, 0,
   (void *) CHR ('h', 'a', 'n', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Hebrew"), NULL, 0, 0,
   (void *) CHR ('h', 'e', 'b', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Pahawh Hmong"), NULL, 0, 0, (void *) CHR('h','m','n','g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
/*  { (uint32_t *) NC_("Script", "Indus (Harappan)"), NULL, 0, 0, (void *) CHR('i','n','d','s'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
  {(uint32_t *) NC_ ("Script", "Javanese"), NULL, 0, 0,
   (void *) CHR ('j', 'a', 'v', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Kayah Li"), NULL, 0, 0, (void *) CHR('k','a','l','i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
  {(uint32_t *) NC_ ("Script", "Hiragana & Katakana"), NULL, 0, 0,
   (void *) CHR ('k', 'a', 'n', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Kharoṣṭhī"), NULL, 0, 0,
   (void *) CHR ('k', 'h', 'a', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Kannada"), NULL, 0, 0,
   (void *) CHR ('k', 'n', 'd', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Kannada2"), NULL, 0, 0,
   (void *) CHR ('k', 'n', 'd', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Khmer"), NULL, 0, 0,
   (void *) CHR ('k', 'h', 'm', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Kharosthi"), NULL, 0, 0,
   (void *) CHR ('k', 'h', 'a', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Lao"), NULL, 0, 0,
   (void *) CHR ('l', 'a', 'o', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Latin"), NULL, 0, 0,
   (void *) CHR ('l', 'a', 't', 'n'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Lepcha (Róng)"), NULL, 0, 0,
   (void *) CHR ('l', 'e', 'p', 'c'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Limbu"), NULL, 0, 0, (void *) CHR ('l', 'i', 'm', 'b'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'}, /* Not in ISO 15924 !!!!!, just guessing */
  {(uint32_t *) NC_ ("Script", "Linear A"), NULL, 0, 0,
   (void *) CHR ('l', 'i', 'n', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Linear B"), NULL, 0, 0,
   (void *) CHR ('l', 'i', 'n', 'b'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Mandaean"), NULL, 0, 0,
   (void *) CHR ('m', 'a', 'n', 'd'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Mayan hieroglyphs"), NULL, 0, 0, (void *) CHR('m','a','y','a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
  {(uint32_t *) NC_ ("Script", "Malayālam"), NULL, 0, 0,
   (void *) CHR ('m', 'l', 'y', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Malayālam2"), NULL, 0, 0,
   (void *) CHR ('m', 'l', 'm', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Mathematical Alphanumeric Symbols"), NULL, 0, 0,
   (void *) CHR ('m', 'a', 't', 'h'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Mongolian"), NULL, 0, 0,
   (void *) CHR ('m', 'o', 'n', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Musical"), NULL, 0, 0,
   (void *) CHR ('m', 'u', 's', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Myanmar"), NULL, 0, 0,
   (void *) CHR ('m', 'y', 'm', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "N'Ko"), NULL, 0, 0,
   (void *) CHR ('n', 'k', 'o', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Ogham"), NULL, 0, 0,
   (void *) CHR ('o', 'g', 'a', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Old Italic (Etruscan, Oscan, etc.)"), NULL, 0,
   0, (void *) CHR ('i', 't', 'a', 'l'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Old Permic"), NULL, 0, 0,
   (void *) CHR ('p', 'e', 'r', 'm'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Old Persian cuneiform"), NULL, 0, 0,
   (void *) CHR ('x', 'p', 'e', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Oriya"), NULL, 0, 0,
   (void *) CHR ('o', 'r', 'y', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Oriya2"), NULL, 0, 0,
   (void *) CHR ('o', 'r', 'y', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Osmanya"), NULL, 0, 0,
   (void *) CHR ('o', 's', 'm', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Pahlavi"), NULL, 0, 0,
   (void *) CHR ('p', 'a', 'l', 'v'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Phags-pa"), NULL, 0, 0,
   (void *) CHR ('p', 'h', 'a', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Phoenician"), NULL, 0, 0,
   (void *) CHR ('p', 'h', 'n', 'x'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Phaistos"), NULL, 0, 0,
   (void *) CHR ('p', 'h', 's', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Pollard Phonetic"), NULL, 0, 0,
   (void *) CHR ('p', 'l', 'r', 'd'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Rongorongo"), NULL, 0, 0,
   (void *) CHR ('r', 'o', 'r', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Runic"), NULL, 0, 0,
   (void *) CHR ('r', 'u', 'n', 'r'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Shavian"), NULL, 0, 0,
   (void *) CHR ('s', 'h', 'a', 'w'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Sinhala"), NULL, 0, 0,
   (void *) CHR ('s', 'i', 'n', 'h'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Sumero-Akkadian Cuneiform"), NULL, 0, 0,
   (void *) CHR ('x', 's', 'u', 'x'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Syloti Nagri"), NULL, 0, 0,
   (void *) CHR ('s', 'y', 'l', 'o'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Syriac"), NULL, 0, 0,
   (void *) CHR ('s', 'y', 'r', 'c'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tagalog"), NULL, 0, 0,
   (void *) CHR ('t', 'g', 'l', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tagbanwa"), NULL, 0, 0,
   (void *) CHR ('t', 'a', 'g', 'b'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tai Le"), NULL, 0, 0, (void *) CHR ('t', 'a', 'l', 'e'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},        /* Not in ISO 15924 !!!!!, just guessing */
  {(uint32_t *) NC_ ("Script", "Tai Lu"), NULL, 0, 0, (void *) CHR ('t', 'a', 'l', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},        /* Not in ISO 15924 !!!!!, just guessing */
  {(uint32_t *) NC_ ("Script", "Tamil"), NULL, 0, 0,
   (void *) CHR ('t', 'a', 'm', 'l'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tamil2"), NULL, 0, 0,
   (void *) CHR ('t', 'm', 'l', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Telugu"), NULL, 0, 0,
   (void *) CHR ('t', 'e', 'l', 'u'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Telugu2"), NULL, 0, 0,
   (void *) CHR ('t', 'e', 'l', '2'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tengwar"), NULL, 0, 0,
   (void *) CHR ('t', 'e', 'n', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Thaana"), NULL, 0, 0,
   (void *) CHR ('t', 'h', 'a', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Thai"), NULL, 0, 0,
   (void *) CHR ('t', 'h', 'a', 'i'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tibetan"), NULL, 0, 0,
   (void *) CHR ('t', 'i', 'b', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Tifinagh (Berber)"), NULL, 0, 0,
   (void *) CHR ('t', 'f', 'n', 'g'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Ugaritic"), NULL, 0, 0, (void *) CHR ('u', 'g', 'r', 't'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},      /* Not in ISO 15924 !!!!!, just guessing */
  {(uint32_t *) NC_ ("Script", "Vai"), NULL, 0, 0,
   (void *) CHR ('v', 'a', 'i', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Visible Speech"), NULL, 0, 0, (void *) CHR('v','i','s','p'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
  {(uint32_t *) NC_ ("Script", "Cuneiform, Ugaritic"), NULL, 0, 0,
   (void *) CHR ('x', 'u', 'g', 'a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Script", "Yi"), NULL, 0, 0,
   (void *) CHR ('y', 'i', ' ', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
/*  { (uint32_t *) NC_("Script", "Private Use Script 1")  , NULL, 0, 0, (void *) CHR('q','a','a','a'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
/*  { (uint32_t *) NC_("Script", "Private Use Script 2")  , NULL, 0, 0, (void *) CHR('q','a','a','b'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
/*  { (uint32_t *) NC_("Script", "Undetermined Script")  , NULL, 0, 0, (void *) CHR('z','y','y','y'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
/*  { (uint32_t *) NC_("Script", "Uncoded Script")  , NULL, 0, 0, (void *) CHR('z','z','z','z'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},*/
  GTEXTINFO_EMPTY
};

GTextInfo languages[] = {
  {(uint32_t *) NC_ ("Language", "Abaza"), NULL, 0, 0,
   (void *) CHR ('A', 'B', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Abkhazian"), NULL, 0, 0,
   (void *) CHR ('A', 'B', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Adyghe"), NULL, 0, 0,
   (void *) CHR ('A', 'D', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Afrikaans"), NULL, 0, 0,
   (void *) CHR ('A', 'F', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Afar"), NULL, 0, 0,
   (void *) CHR ('A', 'F', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Agaw"), NULL, 0, 0,
   (void *) CHR ('A', 'G', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Alsatian"), NULL, 0, 0,
   (void *) CHR ('A', 'L', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Altai"), NULL, 0, 0,
   (void *) CHR ('A', 'L', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Americanist IPA"), NULL, 0, 0,
   (void *) CHR ('A', 'M', 'P', 'H'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Amharic"), NULL, 0, 0,
   (void *) CHR ('A', 'M', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Arabic"), NULL, 0, 0,
   (void *) CHR ('A', 'R', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Aari"), NULL, 0, 0,
   (void *) CHR ('A', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Arakanese"), NULL, 0, 0,
   (void *) CHR ('A', 'R', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Assamese"), NULL, 0, 0,
   (void *) CHR ('A', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Athapaskan"), NULL, 0, 0,
   (void *) CHR ('A', 'T', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Avar"), NULL, 0, 0,
   (void *) CHR ('A', 'V', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Awadhi"), NULL, 0, 0,
   (void *) CHR ('A', 'W', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Aymara"), NULL, 0, 0,
   (void *) CHR ('A', 'Y', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Azeri"), NULL, 0, 0,
   (void *) CHR ('A', 'Z', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Badaga"), NULL, 0, 0,
   (void *) CHR ('B', 'A', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Baghelkhandi"), NULL, 0, 0,
   (void *) CHR ('B', 'A', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Balkar"), NULL, 0, 0,
   (void *) CHR ('B', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Baule"), NULL, 0, 0,
   (void *) CHR ('B', 'A', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Berber"), NULL, 0, 0,
   (void *) CHR ('B', 'B', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bench"), NULL, 0, 0,
   (void *) CHR ('B', 'C', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bible Cree"), NULL, 0, 0,
   (void *) CHR ('B', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Belarussian"), NULL, 0, 0,
   (void *) CHR ('B', 'E', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bemba"), NULL, 0, 0,
   (void *) CHR ('B', 'E', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bengali"), NULL, 0, 0,
   (void *) CHR ('B', 'E', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bulgarian"), NULL, 0, 0,
   (void *) CHR ('B', 'G', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bhili"), NULL, 0, 0,
   (void *) CHR ('B', 'H', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bhojpuri"), NULL, 0, 0,
   (void *) CHR ('B', 'H', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bikol"), NULL, 0, 0,
   (void *) CHR ('B', 'I', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bilen"), NULL, 0, 0,
   (void *) CHR ('B', 'I', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Blackfoot"), NULL, 0, 0,
   (void *) CHR ('B', 'K', 'F', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Balochi"), NULL, 0, 0,
   (void *) CHR ('B', 'L', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Balante"), NULL, 0, 0,
   (void *) CHR ('B', 'L', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Balti"), NULL, 0, 0,
   (void *) CHR ('B', 'L', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bambara"), NULL, 0, 0,
   (void *) CHR ('B', 'M', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bamileke"), NULL, 0, 0,
   (void *) CHR ('B', 'M', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bosnian"), NULL, 0, 0,
   (void *) CHR ('B', 'O', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Breton"), NULL, 0, 0,
   (void *) CHR ('B', 'R', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Brahui"), NULL, 0, 0,
   (void *) CHR ('B', 'R', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Braj Bhasha"), NULL, 0, 0,
   (void *) CHR ('B', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Burmese"), NULL, 0, 0,
   (void *) CHR ('B', 'R', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Bashkir"), NULL, 0, 0,
   (void *) CHR ('B', 'S', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Beti"), NULL, 0, 0,
   (void *) CHR ('B', 'T', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Catalan"), NULL, 0, 0,
   (void *) CHR ('C', 'A', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Cebuano"), NULL, 0, 0,
   (void *) CHR ('C', 'E', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chechen"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chaha Gurage"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chattisgarhi"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chichewa"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chukchi"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chipewyan"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Cherokee"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chuvash"), NULL, 0, 0,
   (void *) CHR ('C', 'H', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Comorian"), NULL, 0, 0,
   (void *) CHR ('C', 'M', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Coptic"), NULL, 0, 0,
   (void *) CHR ('C', 'O', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Corsican"), NULL, 0, 0,
   (void *) CHR ('C', 'O', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Cree"), NULL, 0, 0,
   (void *) CHR ('C', 'R', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Carrier"), NULL, 0, 0,
   (void *) CHR ('C', 'R', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Crimean Tatar"), NULL, 0, 0,
   (void *) CHR ('C', 'R', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Church Slavonic"), NULL, 0, 0,
   (void *) CHR ('C', 'S', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Czech"), NULL, 0, 0,
   (void *) CHR ('C', 'S', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Danish"), NULL, 0, 0,
   (void *) CHR ('D', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dargwa"), NULL, 0, 0,
   (void *) CHR ('D', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Default"), NULL, 0, 0, (void *) DEFAULT_LANG,
   NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Woods Cree"), NULL, 0, 0,
   (void *) CHR ('D', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "German (Standard)"), NULL, 0, 0,
   (void *) CHR ('D', 'E', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dogri"), NULL, 0, 0,
   (void *) CHR ('D', 'G', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dhivehi (Obsolete)"), NULL, 0, 0,
   (void *) CHR ('D', 'H', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dhivehi"), NULL, 0, 0,
   (void *) CHR ('D', 'I', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Djerma"), NULL, 0, 0,
   (void *) CHR ('D', 'J', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dangme"), NULL, 0, 0,
   (void *) CHR ('D', 'N', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dinka"), NULL, 0, 0,
   (void *) CHR ('D', 'N', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dari"), NULL, 0, 0,
   (void *) CHR ('D', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dungan"), NULL, 0, 0,
   (void *) CHR ('D', 'U', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dzongkha"), NULL, 0, 0,
   (void *) CHR ('D', 'Z', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ebira"), NULL, 0, 0,
   (void *) CHR ('E', 'B', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Eastern Cree"), NULL, 0, 0,
   (void *) CHR ('E', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Edo"), NULL, 0, 0,
   (void *) CHR ('E', 'D', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Efik"), NULL, 0, 0,
   (void *) CHR ('E', 'F', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Greek"), NULL, 0, 0,
   (void *) CHR ('E', 'L', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "English"), NULL, 0, 0,
   (void *) CHR ('E', 'N', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Erzya"), NULL, 0, 0,
   (void *) CHR ('E', 'R', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Spanish"), NULL, 0, 0,
   (void *) CHR ('E', 'S', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Estonian"), NULL, 0, 0,
   (void *) CHR ('E', 'T', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Basque"), NULL, 0, 0,
   (void *) CHR ('E', 'U', 'Q', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Evenki"), NULL, 0, 0,
   (void *) CHR ('E', 'V', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Even"), NULL, 0, 0,
   (void *) CHR ('E', 'V', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ewe"), NULL, 0, 0,
   (void *) CHR ('E', 'W', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "French Antillean"), NULL, 0, 0,
   (void *) CHR ('F', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Farsi"), NULL, 0, 0,
   (void *) CHR ('F', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Finnish"), NULL, 0, 0,
   (void *) CHR ('F', 'I', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Fijian"), NULL, 0, 0,
   (void *) CHR ('F', 'J', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Flemish"), NULL, 0, 0,
   (void *) CHR ('F', 'L', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Forest Nenets"), NULL, 0, 0,
   (void *) CHR ('F', 'N', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Fon"), NULL, 0, 0,
   (void *) CHR ('F', 'O', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Faroese"), NULL, 0, 0,
   (void *) CHR ('F', 'O', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "French (Standard)"), NULL, 0, 0,
   (void *) CHR ('F', 'R', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Frisian"), NULL, 0, 0,
   (void *) CHR ('F', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Friulian"), NULL, 0, 0,
   (void *) CHR ('F', 'R', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Futa"), NULL, 0, 0,
   (void *) CHR ('F', 'T', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Fulani"), NULL, 0, 0,
   (void *) CHR ('F', 'U', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ga"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gaelic"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gagauz"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Galician"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Garshuni"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Garhwali"), NULL, 0, 0,
   (void *) CHR ('G', 'A', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ge'ez"), NULL, 0, 0,
   (void *) CHR ('G', 'E', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gilyak"), NULL, 0, 0,
   (void *) CHR ('G', 'I', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gumuz"), NULL, 0, 0,
   (void *) CHR ('G', 'M', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gondi"), NULL, 0, 0,
   (void *) CHR ('G', 'O', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Greenlandic"), NULL, 0, 0,
   (void *) CHR ('G', 'R', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Garo"), NULL, 0, 0,
   (void *) CHR ('G', 'R', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Guarani"), NULL, 0, 0,
   (void *) CHR ('G', 'U', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Gujarati"), NULL, 0, 0,
   (void *) CHR ('G', 'U', 'J', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Haitian"), NULL, 0, 0,
   (void *) CHR ('H', 'A', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Halam"), NULL, 0, 0,
   (void *) CHR ('H', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Harauti"), NULL, 0, 0,
   (void *) CHR ('H', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hausa"), NULL, 0, 0,
   (void *) CHR ('H', 'A', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hawaiin"), NULL, 0, 0,
   (void *) CHR ('H', 'A', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hammer-Banna"), NULL, 0, 0,
   (void *) CHR ('H', 'B', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hiligaynon"), NULL, 0, 0,
   (void *) CHR ('H', 'I', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hindi"), NULL, 0, 0,
   (void *) CHR ('H', 'I', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "High Mari"), NULL, 0, 0,
   (void *) CHR ('H', 'M', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hindko"), NULL, 0, 0,
   (void *) CHR ('H', 'N', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ho"), NULL, 0, 0,
   (void *) CHR ('H', 'O', ' ', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Harari"), NULL, 0, 0,
   (void *) CHR ('H', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Croatian"), NULL, 0, 0,
   (void *) CHR ('H', 'R', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hungarian"), NULL, 0, 0,
   (void *) CHR ('H', 'U', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Armenian"), NULL, 0, 0,
   (void *) CHR ('H', 'Y', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Igbo"), NULL, 0, 0,
   (void *) CHR ('I', 'B', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ijo"), NULL, 0, 0,
   (void *) CHR ('I', 'J', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ilokano"), NULL, 0, 0,
   (void *) CHR ('I', 'L', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Indonesian"), NULL, 0, 0,
   (void *) CHR ('I', 'N', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ingush"), NULL, 0, 0,
   (void *) CHR ('I', 'N', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Inuktitut"), NULL, 0, 0,
   (void *) CHR ('I', 'N', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "IPA usage"), NULL, 0, 0,
   (void *) CHR ('I', 'P', 'P', 'H'), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Irish"), NULL, 0, 0,
   (void *) CHR ('I', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Irish Traditional"), NULL, 0, 0,
   (void *) CHR ('I', 'R', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Icelandic"), NULL, 0, 0,
   (void *) CHR ('I', 'S', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Inari Sami"), NULL, 0, 0,
   (void *) CHR ('I', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Italian"), NULL, 0, 0,
   (void *) CHR ('I', 'T', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Hebrew"), NULL, 0, 0,
   (void *) CHR ('I', 'W', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Javanese"), NULL, 0, 0,
   (void *) CHR ('J', 'A', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Yiddish"), NULL, 0, 0,
   (void *) CHR ('J', 'I', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Japanese"), NULL, 0, 0,
   (void *) CHR ('J', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Judezmo"), NULL, 0, 0,
   (void *) CHR ('J', 'U', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Jula"), NULL, 0, 0,
   (void *) CHR ('J', 'U', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kabardian"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kachchi"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'C', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kalenjin"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kannada"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Karachay"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Georgian"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kazakh"), NULL, 0, 0,
   (void *) CHR ('K', 'A', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kebena"), NULL, 0, 0,
   (void *) CHR ('K', 'E', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khutsuri Georgian"), NULL, 0, 0,
   (void *) CHR ('K', 'G', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khakass"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khanty-Kazim"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khmer"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khanty-Shurishkar"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khanty-Vakhi"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khowar"), NULL, 0, 0,
   (void *) CHR ('K', 'H', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kikuyu"), NULL, 0, 0,
   (void *) CHR ('K', 'I', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kirghiz"), NULL, 0, 0,
   (void *) CHR ('K', 'I', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kisii"), NULL, 0, 0,
   (void *) CHR ('K', 'I', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kokni"), NULL, 0, 0,
   (void *) CHR ('K', 'K', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kalmyk"), NULL, 0, 0,
   (void *) CHR ('K', 'L', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kamba"), NULL, 0, 0,
   (void *) CHR ('K', 'M', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kumaoni"), NULL, 0, 0,
   (void *) CHR ('K', 'M', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Komo"), NULL, 0, 0,
   (void *) CHR ('K', 'M', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Komso"), NULL, 0, 0,
   (void *) CHR ('K', 'M', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kanuri"), NULL, 0, 0,
   (void *) CHR ('K', 'N', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kodagu"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Korean Old Hangul"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Konkani"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kikongo"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Komi-Permyak"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Korean"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Komi-Zyrian"), NULL, 0, 0,
   (void *) CHR ('K', 'O', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kpelle"), NULL, 0, 0,
   (void *) CHR ('K', 'P', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Krio"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Karakalpak"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Karelian"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Karaim"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Karen"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Koorete"), NULL, 0, 0,
   (void *) CHR ('K', 'R', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kashmiri"), NULL, 0, 0,
   (void *) CHR ('K', 'S', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Khasi"), NULL, 0, 0,
   (void *) CHR ('K', 'S', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kildin Sami"), NULL, 0, 0,
   (void *) CHR ('K', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kui"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kulvi"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kumyk"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kurdish"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kurukh"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Kuy"), NULL, 0, 0,
   (void *) CHR ('K', 'U', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Koryak"), NULL, 0, 0,
   (void *) CHR ('K', 'Y', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ladin"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lahuli"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lak"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lambani"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lao"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Latin"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Laz"), NULL, 0, 0,
   (void *) CHR ('L', 'A', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "L-Cree"), NULL, 0, 0,
   (void *) CHR ('L', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ladakhi"), NULL, 0, 0,
   (void *) CHR ('L', 'D', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lezgi"), NULL, 0, 0,
   (void *) CHR ('L', 'E', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lingala"), NULL, 0, 0,
   (void *) CHR ('L', 'I', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Low Mari"), NULL, 0, 0,
   (void *) CHR ('L', 'M', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Limbu"), NULL, 0, 0,
   (void *) CHR ('L', 'M', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lomwe"), NULL, 0, 0,
   (void *) CHR ('L', 'M', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lower Sorbian"), NULL, 0, 0,
   (void *) CHR ('L', 'S', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lule Sami"), NULL, 0, 0,
   (void *) CHR ('L', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Lithuanian"), NULL, 0, 0,
   (void *) CHR ('L', 'T', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Luxembourgish"), NULL, 0, 0,
   (void *) CHR ('L', 'T', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Luba"), NULL, 0, 0,
   (void *) CHR ('L', 'U', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Luganda"), NULL, 0, 0,
   (void *) CHR ('L', 'U', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Luhya"), NULL, 0, 0,
   (void *) CHR ('L', 'U', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Luo"), NULL, 0, 0,
   (void *) CHR ('L', 'U', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Latvian"), NULL, 0, 0,
   (void *) CHR ('L', 'V', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Majang"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'J', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Makua"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Malayalam Traditional"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mansi"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Marathi"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mapudungun"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Marwari"), NULL, 0, 0,
   (void *) CHR ('M', 'A', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mbundu"), NULL, 0, 0,
   (void *) CHR ('M', 'B', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Manchu"), NULL, 0, 0,
   (void *) CHR ('M', 'C', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Moose Cree"), NULL, 0, 0,
   (void *) CHR ('M', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mende"), NULL, 0, 0,
   (void *) CHR ('M', 'D', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Me'en"), NULL, 0, 0,
   (void *) CHR ('M', 'E', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mizo"), NULL, 0, 0,
   (void *) CHR ('M', 'I', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Macedonian"), NULL, 0, 0,
   (void *) CHR ('M', 'K', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Male"), NULL, 0, 0,
   (void *) CHR ('M', 'L', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Malagasy"), NULL, 0, 0,
   (void *) CHR ('M', 'L', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Malinke"), NULL, 0, 0,
   (void *) CHR ('M', 'L', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Malayalam Reformed"), NULL, 0, 0,
   (void *) CHR ('M', 'L', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Malay"), NULL, 0, 0,
   (void *) CHR ('M', 'L', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mandinka"), NULL, 0, 0,
   (void *) CHR ('M', 'N', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mongolian"), NULL, 0, 0,
   (void *) CHR ('M', 'N', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Manipuri"), NULL, 0, 0,
   (void *) CHR ('M', 'N', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Maninka"), NULL, 0, 0,
   (void *) CHR ('M', 'N', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Manx Gaelic"), NULL, 0, 0,
   (void *) CHR ('M', 'N', 'X', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mohawk"), NULL, 0, 0,
   (void *) CHR ('M', 'O', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Moksha"), NULL, 0, 0,
   (void *) CHR ('M', 'O', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Moldavian"), NULL, 0, 0,
   (void *) CHR ('M', 'O', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mon"), NULL, 0, 0,
   (void *) CHR ('M', 'O', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Moroccan"), NULL, 0, 0,
   (void *) CHR ('M', 'O', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Maori"), NULL, 0, 0,
   (void *) CHR ('M', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Maithili"), NULL, 0, 0,
   (void *) CHR ('M', 'T', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Maltese"), NULL, 0, 0,
   (void *) CHR ('M', 'T', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Mundari"), NULL, 0, 0,
   (void *) CHR ('M', 'U', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Naga-Assamese"), NULL, 0, 0,
   (void *) CHR ('N', 'A', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nanai"), NULL, 0, 0,
   (void *) CHR ('N', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Naskapi"), NULL, 0, 0,
   (void *) CHR ('N', 'A', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "N-Cree"), NULL, 0, 0,
   (void *) CHR ('N', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ndebele"), NULL, 0, 0,
   (void *) CHR ('N', 'D', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ndonga"), NULL, 0, 0,
   (void *) CHR ('N', 'D', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nepali"), NULL, 0, 0,
   (void *) CHR ('N', 'E', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Newari"), NULL, 0, 0,
   (void *) CHR ('N', 'E', 'W', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nagari"), NULL, 0, 0,
   (void *) CHR ('N', 'G', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Norway House Cree"), NULL, 0, 0,
   (void *) CHR ('N', 'H', 'C', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nisi"), NULL, 0, 0,
   (void *) CHR ('N', 'I', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Niuean"), NULL, 0, 0,
   (void *) CHR ('N', 'I', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nkole"), NULL, 0, 0,
   (void *) CHR ('N', 'K', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "N'Ko"), NULL, 0, 0,
   (void *) CHR ('N', 'K', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Dutch"), NULL, 0, 0,
   (void *) CHR ('N', 'L', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nogai"), NULL, 0, 0,
   (void *) CHR ('N', 'O', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Norwegian"), NULL, 0, 0,
   (void *) CHR ('N', 'O', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Northern Sami"), NULL, 0, 0,
   (void *) CHR ('N', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Northern Tai"), NULL, 0, 0,
   (void *) CHR ('N', 'T', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Esperanto"), NULL, 0, 0,
   (void *) CHR ('N', 'T', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Nynorsk"), NULL, 0, 0,
   (void *) CHR ('N', 'Y', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Occitan"), NULL, 0, 0,
   (void *) CHR ('O', 'C', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Oji-Cree"), NULL, 0, 0,
   (void *) CHR ('O', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ojibway"), NULL, 0, 0,
   (void *) CHR ('O', 'J', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Oriya"), NULL, 0, 0,
   (void *) CHR ('O', 'R', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Oromo"), NULL, 0, 0,
   (void *) CHR ('O', 'R', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ossetian"), NULL, 0, 0,
   (void *) CHR ('O', 'S', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Palestinian Aramaic"), NULL, 0, 0,
   (void *) CHR ('P', 'A', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Pali"), NULL, 0, 0,
   (void *) CHR ('P', 'A', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Punjabi"), NULL, 0, 0,
   (void *) CHR ('P', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Palpa"), NULL, 0, 0,
   (void *) CHR ('P', 'A', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Pashto"), NULL, 0, 0,
   (void *) CHR ('P', 'A', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Polytonic Greek"), NULL, 0, 0,
   (void *) CHR ('P', 'G', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Pilipino (Filipino)"), NULL, 0, 0,
   (void *) CHR ('P', 'I', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Palaung"), NULL, 0, 0,
   (void *) CHR ('P', 'L', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Polish"), NULL, 0, 0,
   (void *) CHR ('P', 'L', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Provencal"), NULL, 0, 0,
   (void *) CHR ('P', 'R', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Portuguese"), NULL, 0, 0,
   (void *) CHR ('P', 'T', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chin"), NULL, 0, 0,
   (void *) CHR ('Q', 'I', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Rajasthani"), NULL, 0, 0,
   (void *) CHR ('R', 'A', 'J', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "R-Cree"), NULL, 0, 0,
   (void *) CHR ('R', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Russian Buriat"), NULL, 0, 0,
   (void *) CHR ('R', 'B', 'U', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Riang"), NULL, 0, 0,
   (void *) CHR ('R', 'I', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Rhaeto-Romanic"), NULL, 0, 0,
   (void *) CHR ('R', 'M', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Romanian"), NULL, 0, 0,
   (void *) CHR ('R', 'O', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Romany"), NULL, 0, 0,
   (void *) CHR ('R', 'O', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Rusyn"), NULL, 0, 0,
   (void *) CHR ('R', 'S', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ruanda"), NULL, 0, 0,
   (void *) CHR ('R', 'U', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Russian"), NULL, 0, 0,
   (void *) CHR ('R', 'U', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sadri"), NULL, 0, 0,
   (void *) CHR ('S', 'A', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sanskrit"), NULL, 0, 0,
   (void *) CHR ('S', 'A', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Santali"), NULL, 0, 0,
   (void *) CHR ('S', 'A', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sayisi"), NULL, 0, 0,
   (void *) CHR ('S', 'A', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sekota"), NULL, 0, 0,
   (void *) CHR ('S', 'E', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Selkup"), NULL, 0, 0,
   (void *) CHR ('S', 'E', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sango"), NULL, 0, 0,
   (void *) CHR ('S', 'G', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Shan"), NULL, 0, 0,
   (void *) CHR ('S', 'H', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sibe"), NULL, 0, 0,
   (void *) CHR ('S', 'I', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sidamo"), NULL, 0, 0,
   (void *) CHR ('S', 'I', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Silte Gurage"), NULL, 0, 0,
   (void *) CHR ('S', 'I', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Skolt Sami"), NULL, 0, 0,
   (void *) CHR ('S', 'K', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Slovak"), NULL, 0, 0,
   (void *) CHR ('S', 'K', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Slavey"), NULL, 0, 0,
   (void *) CHR ('S', 'L', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Slovenian"), NULL, 0, 0,
   (void *) CHR ('S', 'L', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Somali"), NULL, 0, 0,
   (void *) CHR ('S', 'M', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Samoan"), NULL, 0, 0,
   (void *) CHR ('S', 'M', 'O', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sena"), NULL, 0, 0,
   (void *) CHR ('S', 'N', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sindhi"), NULL, 0, 0,
   (void *) CHR ('S', 'N', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sinhalese"), NULL, 0, 0,
   (void *) CHR ('S', 'N', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Soninke"), NULL, 0, 0,
   (void *) CHR ('S', 'N', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sodo Gurage"), NULL, 0, 0,
   (void *) CHR ('S', 'O', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sotho"), NULL, 0, 0,
   (void *) CHR ('S', 'O', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Albanian"), NULL, 0, 0,
   (void *) CHR ('S', 'Q', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Serbian"), NULL, 0, 0,
   (void *) CHR ('S', 'R', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Saraiki"), NULL, 0, 0,
   (void *) CHR ('S', 'R', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Serer"), NULL, 0, 0,
   (void *) CHR ('S', 'R', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "South Slavey"), NULL, 0, 0,
   (void *) CHR ('S', 'S', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Southern Sami"), NULL, 0, 0,
   (void *) CHR ('S', 'S', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Suri"), NULL, 0, 0,
   (void *) CHR ('S', 'U', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Svan"), NULL, 0, 0,
   (void *) CHR ('S', 'V', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Swedish"), NULL, 0, 0,
   (void *) CHR ('S', 'V', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Swadaya Aramaic"), NULL, 0, 0,
   (void *) CHR ('S', 'W', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Swahili"), NULL, 0, 0,
   (void *) CHR ('S', 'W', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Swazi"), NULL, 0, 0,
   (void *) CHR ('S', 'W', 'Z', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Sutu"), NULL, 0, 0,
   (void *) CHR ('S', 'X', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Syriac"), NULL, 0, 0,
   (void *) CHR ('S', 'Y', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tabasaran"), NULL, 0, 0,
   (void *) CHR ('T', 'A', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tajiki"), NULL, 0, 0,
   (void *) CHR ('T', 'A', 'J', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tamil"), NULL, 0, 0,
   (void *) CHR ('T', 'A', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tatar"), NULL, 0, 0,
   (void *) CHR ('T', 'A', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "TH-Cree"), NULL, 0, 0,
   (void *) CHR ('T', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Telugu"), NULL, 0, 0,
   (void *) CHR ('T', 'E', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tongan"), NULL, 0, 0,
   (void *) CHR ('T', 'G', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tigre"), NULL, 0, 0,
   (void *) CHR ('T', 'G', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tigrinya"), NULL, 0, 0,
   (void *) CHR ('T', 'G', 'Y', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Thai"), NULL, 0, 0,
   (void *) CHR ('T', 'H', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tahitian"), NULL, 0, 0,
   (void *) CHR ('T', 'H', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tibetan"), NULL, 0, 0,
   (void *) CHR ('T', 'I', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Turkmen"), NULL, 0, 0,
   (void *) CHR ('T', 'K', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Temne"), NULL, 0, 0,
   (void *) CHR ('T', 'M', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tswana"), NULL, 0, 0,
   (void *) CHR ('T', 'N', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tundra Nenets"), NULL, 0, 0,
   (void *) CHR ('T', 'N', 'E', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tonga"), NULL, 0, 0,
   (void *) CHR ('T', 'N', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Todo"), NULL, 0, 0,
   (void *) CHR ('T', 'O', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Turkish"), NULL, 0, 0,
   (void *) CHR ('T', 'R', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tsonga"), NULL, 0, 0,
   (void *) CHR ('T', 'S', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Turoyo Aramaic"), NULL, 0, 0,
   (void *) CHR ('T', 'U', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tulu"), NULL, 0, 0,
   (void *) CHR ('T', 'U', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tuvin"), NULL, 0, 0,
   (void *) CHR ('T', 'U', 'V', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Twi"), NULL, 0, 0,
   (void *) CHR ('T', 'W', 'I', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Udmurt"), NULL, 0, 0,
   (void *) CHR ('U', 'D', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Ukrainian"), NULL, 0, 0,
   (void *) CHR ('U', 'K', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Urdu"), NULL, 0, 0,
   (void *) CHR ('U', 'R', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Upper Sorbian"), NULL, 0, 0,
   (void *) CHR ('U', 'S', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Uyghur"), NULL, 0, 0,
   (void *) CHR ('U', 'Y', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Uzbek"), NULL, 0, 0,
   (void *) CHR ('U', 'Z', 'B', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Venda"), NULL, 0, 0,
   (void *) CHR ('V', 'E', 'N', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Vietnamese"), NULL, 0, 0,
   (void *) CHR ('V', 'I', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Wa"), NULL, 0, 0,
   (void *) CHR ('W', 'A', ' ', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Wagdi"), NULL, 0, 0,
   (void *) CHR ('W', 'A', 'G', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "West-Cree"), NULL, 0, 0,
   (void *) CHR ('W', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Welsh"), NULL, 0, 0,
   (void *) CHR ('W', 'E', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Wolof"), NULL, 0, 0,
   (void *) CHR ('W', 'L', 'F', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Tai Lue"), NULL, 0, 0,
   (void *) CHR ('X', 'B', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Xhosa"), NULL, 0, 0,
   (void *) CHR ('X', 'H', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Yakut"), NULL, 0, 0,
   (void *) CHR ('Y', 'A', 'K', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Yoruba"), NULL, 0, 0,
   (void *) CHR ('Y', 'B', 'A', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Y-Cree"), NULL, 0, 0,
   (void *) CHR ('Y', 'C', 'R', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Yi Classic"), NULL, 0, 0,
   (void *) CHR ('Y', 'I', 'C', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Yi Modern"), NULL, 0, 0,
   (void *) CHR ('Y', 'I', 'M', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chinese Hong Kong"), NULL, 0, 0,
   (void *) CHR ('Z', 'H', 'H', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chinese Phonetic"), NULL, 0, 0,
   (void *) CHR ('Z', 'H', 'P', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chinese Simplified"), NULL, 0, 0,
   (void *) CHR ('Z', 'H', 'S', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Chinese Traditional"), NULL, 0, 0,
   (void *) CHR ('Z', 'H', 'T', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Zande"), NULL, 0, 0,
   (void *) CHR ('Z', 'N', 'D', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  {(uint32_t *) NC_ ("Language", "Zulu"), NULL, 0, 0,
   (void *) CHR ('Z', 'U', 'L', ' '), NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0, '\0'},
  GTEXTINFO_EMPTY
};

static char *LK_LangsDlg (GGadget *, int r, int c);
static char *LK_ScriptsDlg (GGadget *, int r, int c);
static struct col_init scriptci[] = {
  {me_stringchoicetag, NULL, scripts, NULL, NC_ ("writing system", "Script")},
  {me_funcedit, LK_LangsDlg, NULL, NULL, N_("Language(s)")},
  COL_INIT_EMPTY
};

static struct col_init featureci[] = {
  {me_stringchoicetrans, NULL, NULL, NULL, N_("Feature")},
  {me_funcedit, LK_ScriptsDlg, NULL, NULL, N_("Script(s) & Language(s)")},
  COL_INIT_EMPTY
};

void
LookupUIInit (void)
{
  static int done = false;
  int i, j;

  if (done)
    return;
  done = true;
  for (i = 0; scripts[i].text != NULL || scripts[i].line; ++i)
    if (scripts[i].text != NULL)
      scripts[i].text =
        (uint32_t *) g_dpgettext2 (NULL, "Script", (char *) scripts[i].text);

  for (i = 0; languages[i].text != NULL || languages[i].line; ++i)
    if (languages[i].text != NULL)
      languages[i].text =
        (uint32_t *) g_dpgettext2 (NULL, "Language",
                                   (char *) languages[i].text);

  for (j = 0; lookuptypes[j] != NULL; ++j)
    {
      for (i = 0; lookuptypes[j][i].text != NULL || lookuptypes[j][i].line; ++i)
        if (lookuptypes[j][i].text != NULL)
          lookuptypes[j][i].text =
            (uint32_t *) g_dpgettext2 (NULL, "Lookup Type",
                                       (char *) lookuptypes[j][i].text);
    }

  LookupInit ();

  featureci[0].title = _(featureci[0].title);
  featureci[1].title = _(featureci[1].title);
  scriptci[0].title =
    (char *) g_dpgettext2 (NULL, "writing system", scriptci[0].title);
  scriptci[1].title = _(scriptci[1].title);
}

#define CID_LookupType		1000
#define CID_LookupFeatures	1001
#define CID_Lookup_R2L		1002
#define CID_Lookup_IgnBase	1003
#define CID_Lookup_IgnLig	1004
#define CID_Lookup_IgnMark	1005
#define CID_Lookup_ProcessMark	1006
#define CID_LookupName		1007
#define CID_LookupAfm		1008
#define CID_OK			1009
#define CID_Cancel		1010
#define CID_Lookup_ProcessSet	1011

#define CID_FeatureScripts	1020
#define CID_ShowAnchors		1021

struct lookup_dlg
{
  OTLookup *orig;
  SplineFont *sf;
  GWindow gw, scriptgw;
  int isgpos;
  int done, scriptdone, name_has_been_set;
  int ok;
  char *scriptret;
};

static int
langs_e_h (GWindow gw, GEvent *event)
{
  int *done = GDrawGetUserData (gw);

  if (event->type == et_close)
    {
      *done = true;
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("lookups.html#scripts-dlg");
          return true;
        }
      return false;
    }
  else if (event->type == et_controlevent
           && event->u.control.subtype == et_buttonactivate)
    {
      switch (GGadgetGetCid (event->u.control.g))
        {
        case CID_OK:
          *done = 2;
          break;
        case CID_Cancel:
          *done = true;
          break;
        }
    }
  return true;
}

static char *
LK_LangsDlg (GGadget *g, int r, int c)
{
  int rows, i;
  struct matrix_data *strings = GMatrixEditGet (g, &rows);
  char *langstr = strings[2 * r + c].u.md_str, *pt, *start;
  unsigned char tagstr[4], warnstr[8];
  uint32_t tag;
  int warn_cnt = 0;
  int j, done = 0;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[5], *varray[6], *harray[7], boxes[3];
  GTextInfo label[5];
  GRect pos;
  GWindow gw;
  int32_t len;
  GTextInfo **ti;
  char *ret;

  for (i = 0; languages[i].text != NULL; ++i)
    languages[i].selected = false;

  for (start = langstr; *start;)
    {
      memset (tagstr, ' ', sizeof (tagstr));
      for (pt = start, j = 0; *pt != '\0' && *pt != ','; ++pt, ++j)
        {
          if (j < 4)
            tagstr[j] = *pt;
        }
      if (*pt == ',')
        ++pt;
      tag =
        (tagstr[0] << 24) | (tagstr[1] << 16) | (tagstr[2] << 8) | tagstr[3];
      for (i = 0; languages[i].text != NULL; ++i)
        if (languages[i].userdata == (void *) (intptr_t) tag)
          {
            languages[i].selected = true;
            break;
          }
      if (languages[i].text == NULL)
        {
          ++warn_cnt;
          memcpy (warnstr, tagstr, 4);
          warnstr[4] = '\0';
        }
      start = pt;
    }
  if (warn_cnt != 0)
    {
      if (warn_cnt == 1)
        ff_post_error (_("Unknown Language"),
                       _
                       ("The language, '%s', is not in the list of known languages and will be omitted"),
                       warnstr);
      else
        ff_post_error (_("Unknown Language"),
                       _
                       ("Several language tags, including '%s', are not in the list of known languages and will be omitted"),
                       warnstr);
    }

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Language List");
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = GGadgetScale (GDrawPointsToPixels (NULL, 150));
  pos.height = GDrawPointsToPixels (NULL, 193);
  gw = GDrawCreateTopWindow (NULL, &pos, langs_e_h, &done, &wattrs);

  memset (&gcd, 0, sizeof (gcd));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));

  i = 0;
  gcd[i].gd.pos.x = 10;
  gcd[i].gd.pos.y = 5;
  gcd[i].gd.pos.height = 12 * 12 + 6;
  gcd[i].gd.flags =
    gg_enabled | gg_visible | gg_list_alphabetic | gg_list_multiplesel |
    gg_utf8_popup;
  gcd[i].gd.u.list = languages;
  gcd[i].gd.cid = 0;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("Select as many languages as needed\n"
                   "Hold down the control key when clicking\n"
                   "to make disjoint selections.");
  varray[0] = &gcd[i];
  varray[1] = NULL;
  gcd[i++].creator = GListCreate;

  gcd[i].gd.pos.x = 15 - 3;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + gcd[i - 1].gd.pos.height + 5;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.pos.height = 0;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[i].text = (uint32_t *) _("_OK");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.cid = CID_OK;
  harray[0] = GCD_Glue;
  harray[1] = &gcd[i];
  harray[2] = GCD_Glue;
  gcd[i++].creator = GButtonCreate;

  gcd[i].gd.pos.x = -15;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 3;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.pos.height = 0;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[i].text = (uint32_t *) _("_Cancel");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.cid = CID_Cancel;
  harray[3] = GCD_Glue;
  harray[4] = &gcd[i];
  harray[5] = GCD_Glue;
  harray[6] = NULL;
  gcd[i++].creator = GButtonCreate;

  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = harray;
  boxes[2].creator = GHBoxCreate;
  varray[2] = &boxes[2];
  varray[3] = NULL;
  varray[4] = NULL;

  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = varray;
  boxes[0].creator = GHVGroupCreate;

  GGadgetsCreate (gw, boxes);
  GHVBoxSetExpandableRow (boxes[0].ret, 0);
  GHVBoxSetExpandableCol (boxes[2].ret, gb_expandgluesame);
  GHVBoxFitWindow (boxes[0].ret);

  GDrawSetVisible (gw, true);
retry:
  while (!done)
    GDrawProcessOneEvent (NULL);
  ret = NULL;
  ti = GGadgetGetList (gcd[0].ret, &len);
  if (done == 2)
    {
      int lcnt = 0;
      for (i = 0; i < len; ++i)
        {
          if (ti[i]->selected)
            ++lcnt;
        }
      if (lcnt == 0)
        {
          ff_post_error (_("Language Missing"),
                         _
                         ("You must select at least one language.\nUse the \"Default\" language if nothing else fits."));
          done = 0;
          goto retry;
        }
      ret = xmalloc (5 * lcnt + 1);
      *ret = '\0';
      pt = ret;
      for (i = 0; i < len; ++i)
        {
          if (done == 2 && ti[i]->selected)
            {
              uint32_t tag = (uint32_t) (intptr_t) (ti[i]->userdata);
              *pt++ = tag >> 24;
              *pt++ = tag >> 16;
              *pt++ = tag >> 8;
              *pt++ = tag & 0xff;
              *pt++ = ',';
            }
        }
      if (pt != ret)
        pt[-1] = '\0';
    }
  GDrawDestroyWindow (gw);
  return ret;
}

static void
LK_NewScript (GGadget *g, int row)
{
  int rows;
  struct matrix_data *strings = GMatrixEditGet (g, &rows);
  /* What's a good default lang list for a new script? */
  /*  well it depends on what the script is, but we don't know that yet */
  /* dflt is safe */

  strings[2 * row + 1].u.md_str = xstrdup ("dflt");
}

static void
ScriptMatrixInit (struct matrixinit *mi, char *scriptstr)
{
  struct matrix_data *md;
  int k, cnt;
  char *start, *scriptend, *langsend;

  memset (mi, 0, sizeof (*mi));
  mi->col_cnt = 2;
  mi->col_init = scriptci;

  md = NULL;
  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (start = scriptstr; *start;)
        {
          for (scriptend = start; *scriptend != '\0' && *scriptend != '{';
               ++scriptend);
          langsend = scriptend;
          if (*scriptend == '{')
            for (langsend = scriptend + 1;
                 *langsend != '\0' && *langsend != '}'; ++langsend);
          if (k)
            {
              md[2 * cnt + 0].u.md_str = xstrndup (start, scriptend - start);
              if (*scriptend == '{')
                md[2 * cnt + 1].u.md_str =
                  xstrndup (scriptend + 1, langsend - (scriptend + 1));
              else
                md[2 * cnt + 1].u.md_str = xstrdup ("");
            }
          ++cnt;
          if (*langsend == '}')
            ++langsend;
          if (*langsend == ' ')
            ++langsend;
          start = langsend;
        }
      if (md == NULL)
        md = xcalloc (2 * (cnt + 10), sizeof (struct matrix_data));
    }
  mi->matrix_data = md;
  mi->initial_row_cnt = cnt;

  mi->initrow = LK_NewScript;
  mi->finishedit = NULL;
  mi->candelete = NULL;
  mi->popupmenu = NULL;
  mi->handle_key = NULL;
  mi->bigedittitle = NULL;
}

static int
Script_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      int rows, i, j, script_cnt, lang_cnt;
      struct matrix_data *strings =
        GMatrixEditGet (GWidgetGetControl (ld->scriptgw, CID_FeatureScripts),
                        &rows);
      char *pt, *start, *ret, *rpt;
      char foo[4];

      if (rows == 0)
        {
          ff_post_error (_("No scripts"),
                         _
                         ("You must select at least one script if you provide a feature tag."));
          return true;
        }
      script_cnt = rows;
      lang_cnt = 0;
      for (i = 0; i < rows; ++i)
        {
          if (strlen (strings[2 * i + 0].u.md_str) > 4)
            {
              ff_post_error (_("Bad script tag"),
                             _
                             ("The script tag on line %d (%s) is too long.  It may be at most 4 letters"),
                             i + 1, strings[2 * i + 0].u.md_str);
              return true;
            }
          else
            {
              for (pt = strings[2 * i + 0].u.md_str; *pt != '\0'; ++pt)
                if (*pt > 0x7e)
                  {
                    ff_post_error (_("Bad script tag"),
                                   _
                                   ("The script tag on line %d (%s) should be in ASCII.\n"),
                                   i + 1, strings[2 * i + 0].u.md_str);
                    return true;
                  }
            }
          /* Now check the languages */
          if (*strings[2 * i + 1].u.md_str == '\0')
            {
              ff_post_error (_("No languages"),
                             _
                             ("You must select at least one language for each script."));
              return true;
            }
          for (start = strings[2 * i + 1].u.md_str; *start != '\0';)
            {
              for (pt = start; *pt != ',' && *pt != '\0'; ++pt)
                {
                  if (*pt > 0x7e)
                    {
                      ff_post_error (_("Bad language tag"),
                                     _
                                     ("A language tag on line %d (%s) should be in ASCII.\n"),
                                     i + 1, strings[2 * i + 1].u.md_str);
                      return true;
                    }
                }
              if (pt - start > 4)
                {
                  ff_post_error (_("Bad language tag"),
                                 _
                                 ("A language tag on line %d (%s) is too long.  It may be at most 4 letters"),
                                 i + 1, strings[2 * i + 0].u.md_str);
                  return true;
                }
              ++lang_cnt;
              start = pt;
              if (*start == ',')
                ++start;
            }
        }

      /* Ok, we validated the script lang list. Now parse it */
      rpt = ret = xmalloc (6 * script_cnt + 5 * lang_cnt + 10);
      for (i = 0; i < rows; ++i)
        {
          memset (foo, ' ', sizeof (foo));
          for (j = 0, pt = strings[2 * i + 0].u.md_str; j < 4 && *pt;
               foo[j++] = *pt++);
          strncpy (rpt, foo, 4);
          rpt += 4;
          *rpt++ = '{';
          /* Now do the languages */
          for (start = strings[2 * i + 1].u.md_str; *start != '\0';)
            {
              memset (foo, ' ', sizeof (foo));
              for (j = 0, pt = start; *pt != ',' && *pt != '\0'; ++pt)
                foo[j++] = *pt;
              strncpy (rpt, foo, 4);
              rpt += 4;
              *rpt++ = ',';
              if (*pt == ',')
                ++pt;
              start = pt;
            }
          if (rpt > ret && rpt[-1] == ',')
            rpt[-1] = '}';
          *rpt++ = ' ';
        }
      if (rpt > ret && rpt[-1] == ' ')
        rpt[-1] = '\0';
      else
        *rpt = '\0';
      ld->scriptdone = true;
      ld->scriptret = ret;
    }
  return true;
}

static int
Script_Cancel (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      ld->scriptdone = true;
      ld->scriptret = NULL;
    }
  return true;
}

static int
script_e_h (GWindow gw, GEvent *event)
{

  if (event->type == et_close)
    {
      struct lookup_dlg *ld = GDrawGetUserData (gw);
      ld->scriptdone = true;
      ld->scriptret = NULL;
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("lookups.html#scripts-dlg");
          return true;
        }
      return false;
    }

  return true;
}

static char *
LK_ScriptsDlg (GGadget *g, int r, int c)
{
  int rows, i, k, j;
  struct matrix_data *strings = GMatrixEditGet (g, &rows);
  char *scriptstr = strings[2 * r + c].u.md_str;
  struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[4], boxes[3];
  GGadgetCreateData *varray[6], *harray3[8];
  GTextInfo label[4];
  struct matrixinit mi;

  ld->scriptdone = 0;
  ld->scriptret = NULL;

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.is_dlg = true;
  wattrs.restrict_input_to_me = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Script(s)");
  pos.x = pos.y = 0;
  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (268));
  pos.height = GDrawPointsToPixels (NULL, 375);
  ld->scriptgw = gw =
    GDrawCreateTopWindow (NULL, &pos, script_e_h, ld, &wattrs);

  ScriptMatrixInit (&mi, scriptstr);

  memset (&gcd, 0, sizeof (gcd));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));
  k = j = 0;
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = gcd[1].gd.pos.y + 14;
  gcd[k].gd.pos.width = 300;
  gcd[k].gd.pos.height = 90;
  gcd[k].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[k].gd.cid = CID_FeatureScripts;
  gcd[k].gd.u.matrix = &mi;
  gcd[k].gd.popup_msg =
    (uint32_t *) _("Each feature is active for a specific set of\n"
                   "scripts and languages.\n"
                   "Usually only one script is specified, but\n"
                   "occasionally more will be.\n"
                   "A script is a four letter OpenType script tag\n");
  gcd[k].creator = GMatrixEditCreate;
  varray[j++] = &gcd[k++];
  varray[j++] = NULL;

  gcd[k].gd.pos.x = 30 - 3;
  gcd[k].gd.pos.width = -1;
  gcd[k].gd.pos.height = 0;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.handle_controlevent = Script_OK;
  gcd[k].gd.cid = CID_OK;
  gcd[k++].creator = GButtonCreate;

  gcd[k].gd.pos.x = -30;
  gcd[k].gd.pos.width = -1;
  gcd[k].gd.pos.height = 0;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.handle_controlevent = Script_Cancel;
  gcd[k].gd.cid = CID_Cancel;
  gcd[k++].creator = GButtonCreate;

  harray3[0] = harray3[2] = harray3[3] = harray3[4] = harray3[6] = GCD_Glue;
  harray3[7] = NULL;
  harray3[1] = &gcd[k - 2];
  harray3[5] = &gcd[k - 1];

  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = harray3;
  boxes[0].creator = GHBoxCreate;
  varray[j++] = &boxes[0];
  varray[j++] = NULL;
  varray[j] = NULL;

  boxes[1].gd.pos.x = boxes[1].gd.pos.y = 2;
  boxes[1].gd.flags = gg_enabled | gg_visible;
  boxes[1].gd.u.boxelements = varray;
  boxes[1].creator = GHVGroupCreate;

  GGadgetsCreate (gw, boxes + 1);

  for (i = 0; i < mi.initial_row_cnt; ++i)
    {
      free (mi.matrix_data[2 * i + 0].u.md_str);
      free (mi.matrix_data[2 * i + 1].u.md_str);
    }
  free (mi.matrix_data);

  GMatrixEditSetNewText (gcd[0].ret, C_ ("OpenTypeFeature", "New"));
  GHVBoxSetExpandableRow (boxes[1].ret, 0);
  GHVBoxSetExpandableCol (boxes[0].ret, gb_expandgluesame);

  GHVBoxFitWindow (boxes[1].ret);

  GDrawSetVisible (gw, true);
  while (!ld->scriptdone)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);
  return ld->scriptret;
}

static FeatureScriptLangList *
LK_ParseFL (struct matrix_data *strings, int rows)
{
  int i, j;
  char *pt, *start;
  FeatureScriptLangList *fl, *fhead, *flast;
  struct scriptlanglist *sl, *slast;
  unsigned char foo[4];
  uint32_t *langs = NULL;
  int lmax = 0, lcnt = 0;

  fhead = flast = NULL;
  for (i = 0; i < rows; ++i)
    {
      fl = xzalloc (sizeof (FeatureScriptLangList));
      memset (foo, ' ', sizeof (foo));
      for (j = 0, pt = strings[2 * i + 0].u.md_str; j < 4 && *pt;
           foo[j++] = *pt++);
      fl->featuretag = (foo[0] << 24) | (foo[1] << 16) | (foo[2] << 8) | foo[3];
      if (flast == NULL)
        fhead = fl;
      else
        flast->next = fl;
      flast = fl;
      /* Now do the script langs */
      slast = NULL;
      for (start = strings[2 * i + 1].u.md_str; *start != '\0';)
        {
          memset (foo, ' ', sizeof (foo));
          for (j = 0, pt = start; *pt != '{' && *pt != '\0'; ++pt)
            foo[j++] = *pt;
          sl =
            xzalloc (sizeof (struct scriptlanglist));
          sl->script = (foo[0] << 24) | (foo[1] << 16) | (foo[2] << 8) | foo[3];
          if (slast == NULL)
            fl->scripts = sl;
          else
            slast->next = sl;
          slast = sl;
          if (*pt != '{')
            {
              sl->lang_cnt = 1;
              sl->langs[0] = DEFAULT_LANG;
              start = pt;
            }
          else
            {
              lcnt = 0;
              for (start = pt + 1; *start != '}' && *start != '\0';)
                {
                  memset (foo, ' ', sizeof (foo));
                  for (j = 0, pt = start;
                       *pt != '}' && *pt != ',' && *pt != '\0';
                       foo[j++] = *pt++);
                  if (lcnt >= lmax)
                    langs = xrealloc (langs, (lmax += 20) * sizeof (uint32_t));
                  langs[lcnt++] =
                    (foo[0] << 24) | (foo[1] << 16) | (foo[2] << 8) | foo[3];
                  start = (*pt == ',') ? pt + 1 : pt;
                }
              if (*start == '}')
                ++start;
              for (j = 0; j < lcnt && j < MAX_LANG; ++j)
                sl->langs[j] = langs[j];
              if (lcnt > MAX_LANG)
                {
                  sl->morelangs =
                    xmalloc ((lcnt - MAX_LANG) * sizeof (uint32_t));
                  for (; j < lcnt; ++j)
                    sl->morelangs[j - MAX_LANG] = langs[j];
                }
              sl->lang_cnt = lcnt;
            }
          while (*start == ' ')
            ++start;
        }
    }
  free (langs);
  return fhead;
}

static void
LK_FinishEdit (GGadget *g, int row, int col, int wasnew)
{
  struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));

  if (col == 0)
    {
      int rows;
      struct matrix_data *strings = GMatrixEditGet (g, &rows);

      if (!ld->isgpos
          && (strcmp (strings[row].u.md_str, "liga") == 0
              || strcmp (strings[row].u.md_str, "rlig") == 0))
        GGadgetSetChecked (GWidgetGetControl (ld->gw, CID_LookupAfm), true);
    }
  if (row == 0 && !ld->name_has_been_set && ld->orig->features == NULL)
    {
      int rows;
      struct matrix_data *strings = GMatrixEditGet (g, &rows);
      OTLookup *otl = ld->orig;
      int old_type = otl->lookup_type;
      FeatureScriptLangList *oldfl = otl->features;

      otl->lookup_type =
        (intptr_t)
        GGadgetGetListItemSelected (GWidgetGetControl
                                    (ld->gw, CID_LookupType))->userdata;
      otl->features = LK_ParseFL (strings, rows);
      NameOTLookup (otl, ld->sf);
      GGadgetSetTitle8 (GWidgetGetControl (ld->gw, CID_LookupName),
                        otl->lookup_name);
      free (otl->lookup_name);
      otl->lookup_name = NULL;
      FeatureScriptLangListFree (otl->features);
      otl->features = oldfl;
      otl->lookup_type = old_type;
    }
}

static void
LK_NewFeature (GGadget *g, int row)
{
  int rows;
  struct matrix_data *strings = GMatrixEditGet (g, &rows);
  struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
  SplineFont *sf, *_sf = ld->sf;
  SplineChar *sc;
  /* What's a good default script / lang list for a new feature? */
  /*  well it depends on what the feature is, and on what's inside the lookup */
  /*  Neither of those has been set yet. */
  /* So... give them everything we can think of. They can remove stuff */
  /*  which is inappropriate */
  uint32_t scripts[20], script, *langs;
  int scnt = 0, i, l;
  int gid, k;
  char *buf;
  int bmax, bpos;

  k = 0;
  do
    {
      sf = k < _sf->subfontcnt ? _sf->subfonts[k] : _sf;
      for (gid = 0; gid < sf->glyphcnt; ++gid)
        if ((sc = sf->glyphs[gid]) != NULL)
          {
            script = SCScriptFromUnicode (sc);
            for (i = 0; i < scnt; ++i)
              if (script == scripts[i])
                break;
            if (i == scnt)
              {
                scripts[scnt++] = script;
                if (scnt >= 20) /* If they've got lots of scripts, enumerating them all won't be much use... */
                  break;
              }
          }
      ++k;
    }
  while (k < _sf->subfontcnt && scnt < 20);

  if (scnt == 0)
    scripts[scnt++] = DEFAULT_SCRIPT;

  buf = xmalloc (bmax = 100);
  bpos = 0;
  for (i = 0; i < scnt; ++i)
    {
      langs = SFLangsInScript (sf, -1, scripts[i]);
      for (l = 0; langs[l] != 0; ++l);
      if (bpos + 5 + 5 * l + 4 > bmax)
        buf = xrealloc (buf, bmax += 5 + 5 * l + 100);
      sprintf (buf + bpos, "%c%c%c%c{", scripts[i] >> 24, scripts[i] >> 16,
               scripts[i] >> 8, scripts[i]);
      bpos += 5;
      for (l = 0; langs[l] != 0; ++l)
        {
          sprintf (buf + bpos, "%c%c%c%c,", langs[l] >> 24, langs[l] >> 16,
                   langs[l] >> 8, langs[l]);
          bpos += 5;
        }
      if (l > 0)
        --bpos;
      strcpy (buf + bpos, "} ");
      bpos += 2;
      free (langs);
    }
  if (bpos > 0)
    buf[bpos - 1] = '\0';

  strings[2 * row + 1].u.md_str = xstrdup_or_null (buf);
  free (buf);
}

static void
LKMatrixInit (struct matrixinit *mi, OTLookup *otl)
{
  struct matrix_data *md;
  int k, cnt, j;
  FeatureScriptLangList *fl;
  struct scriptlanglist *sl;
  char featbuf[32], *buf = NULL;
  int blen = 0, bpos = 0;

  memset (mi, 0, sizeof (*mi));
  mi->col_cnt = 2;
  mi->col_init = featureci;

  md = NULL;
  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (fl = otl->features; fl != NULL; fl = fl->next)
        {
          if (k)
            {
              sprintf (featbuf, "%c%c%c%c", fl->featuretag >> 24,
                       fl->featuretag >> 16, fl->featuretag >> 8,
                       fl->featuretag);
              md[2 * cnt + 0].u.md_str = xstrdup_or_null (featbuf);
              bpos = 0;
              for (sl = fl->scripts; sl != NULL; sl = sl->next)
                {
                  if (bpos + 4 /*script */  + 1 /*open brace */  +
                      5 * sl->lang_cnt + 1 + 2 > blen)
                    buf = xrealloc (buf, blen +=
                                    5 + 5 * sl->lang_cnt + 3 + 200);
                  sprintf (buf + bpos, "%c%c%c%c{", sl->script >> 24,
                           sl->script >> 16, sl->script >> 8, sl->script);
                  bpos += 5;
                  for (j = 0; j < sl->lang_cnt; ++j)
                    {
                      uint32_t tag =
                        j <
                        MAX_LANG ? sl->langs[j] : sl->morelangs[j - MAX_LANG];
                      sprintf (buf + bpos, "%c%c%c%c,", tag >> 24, tag >> 16,
                               tag >> 8, tag);
                      bpos += 5;
                    }
                  if (sl->lang_cnt != 0)
                    --bpos;
                  buf[bpos++] = '}';
                  buf[bpos++] = ' ';
                }
              if (bpos > 0)
                --bpos;
              if (buf)
                {
                  buf[bpos] = '\0';
                  md[2 * cnt + 1].u.md_str = xstrdup_or_null (buf);
                }
              else
                {
                  md[2 * cnt + 1].u.md_str = NULL;
                }
            }
          ++cnt;
        }
      if (md == NULL)
        md = xcalloc (2 * (cnt + 10), sizeof (struct matrix_data));
    }
  mi->matrix_data = md;
  mi->initial_row_cnt = cnt;

  mi->initrow = LK_NewFeature;
  mi->finishedit = LK_FinishEdit;
  mi->candelete = NULL;
  mi->popupmenu = NULL;
  mi->handle_key = NULL;
  mi->bigedittitle = NULL;
  free (buf);
}

static GTextInfo *
SFMarkClassList (SplineFont *sf, int class)
{
  int i;
  GTextInfo *ti;

  if (sf->cidmaster)
    sf = sf->cidmaster;
  else if (sf->mm != NULL)
    sf = sf->mm->normal;

  i = sf->mark_class_cnt;
  ti = xcalloc (i + 4, sizeof (GTextInfo));
  ti[0].text = utf82u_copy (_("All"));
  ti[0].selected = class == 0;
  for (i = 1; i < sf->mark_class_cnt; ++i)
    {
      ti[i].text = (uint32_t *) xstrdup_or_null (sf->mark_class_names[i]);
      ti[i].userdata = (void *) (intptr_t) i;
      ti[i].text_is_1byte = true;
      if (i == class)
        ti[i].selected = true;
    }
  return ti;
}

static GTextInfo *
SFMarkSetList (SplineFont *sf, int set)
{
  int i;
  GTextInfo *ti;

  if (sf->cidmaster)
    sf = sf->cidmaster;
  else if (sf->mm != NULL)
    sf = sf->mm->normal;

  i = sf->mark_set_cnt;
  ti = xcalloc (i + 4, sizeof (GTextInfo));
  ti[0].text = utf82u_copy (_("All"));
  ti[0].userdata = (void *) (intptr_t) -1;
  ti[0].selected = set == -1;
  for (i = 0; i < sf->mark_set_cnt; ++i)
    {
      ti[i + 1].text = (uint32_t *) xstrdup_or_null (sf->mark_set_names[i]);
      ti[i + 1].userdata = (void *) (intptr_t) i;
      ti[i + 1].text_is_1byte = true;
      if (i == set)
        ti[i + 1].selected = true;
    }
  return ti;
}

static int
MaskFromLookupType (int lookup_type)
{
  switch (lookup_type)
    {
    case gsub_single:
    case gsub_multiple:
    case gsub_alternate:
    case gsub_ligature:
    case gsub_context:
    case gsub_contextchain:
      return 1 << (lookup_type - 1);
    case gsub_reversecchain:
      return gsub_reversecchain_mask;
    case gpos_single:
    case gpos_pair:
    case gpos_cursive:
    case gpos_mark2base:
    case gpos_mark2ligature:
    case gpos_mark2mark:
    case gpos_context:
    case gpos_contextchain:
      return gpos_single_mask << (lookup_type - gpos_single);
    default:
      return 0;
    }
}

static GTextInfo *
FeatureListFromLookupType (int lookup_type)
{
  int k, i, cnt, mask;
  GTextInfo *ti;

  mask = MaskFromLookupType (lookup_type);
  for (k = 0; k < 2; ++k)
    {
      cnt = 0;
      for (i = 0; friendlies[i].tag != 0; ++i)
        if (friendlies[i].masks & mask)
          {
            if (k)
              {
                ti[cnt].text =
                  (uint32_t *) xstrdup_or_null (friendlies[i].friendlyname);
                ti[cnt].text_is_1byte = true;
                ti[cnt].userdata = friendlies[i].tagstr;
              }
            ++cnt;
          }
      if (cnt == 0)
        {
          if (k)
            {
              ti[cnt].text =
                (uint32_t *)
                xstrdup_or_null (_("You must choose a lookup type"));
              ti[cnt].text_is_1byte = true;
              ti[cnt].userdata = "????";
            }
          ++cnt;
        }
      if (!k)
        ti = xcalloc (cnt + 1, sizeof (GTextInfo));
    }
  return ti;
}

static int
Lookup_NameChanged (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_textchanged)
    {
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      if (*_GGadgetGetTitle (g) != '\0')
        ld->name_has_been_set = true;
    }
  return true;
}

static int
LK_TypeChanged (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_listselected)
    {
      int lookup_type = (intptr_t) GGadgetGetListItemSelected (g)->userdata;
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      GTextInfo *ti = FeatureListFromLookupType (lookup_type);
      GMatrixEditSetColumnChoices (GWidgetGetControl
                                   (ld->gw, CID_LookupFeatures), 0, ti);
      GTextInfoListFree (ti);
      if (!ld->isgpos)
        {
          GGadgetSetEnabled (GWidgetGetControl (ld->gw, CID_LookupAfm),
                             lookup_type == gsub_ligature);
          if (lookup_type != gsub_ligature)
            GGadgetSetChecked (GWidgetGetControl (ld->gw, CID_LookupAfm),
                               false);
        }
    }
  return true;
}

static int
Lookup_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      int lookup_type =
        (intptr_t)
        GGadgetGetListItemSelected (GWidgetGetControl
                                    (ld->gw, CID_LookupType))->userdata;
      int rows, i, isgpos;
      struct matrix_data *strings =
        GMatrixEditGet (GWidgetGetControl (ld->gw, CID_LookupFeatures), &rows);
      char *pt, *start, *name;
      OTLookup *otl = ld->orig, *test;
      int flags, afm;
      FeatureScriptLangList *fhead;
      int feat, set;

      if (lookup_type == ot_undef)
        {
          ff_post_error (_("No Lookup Type Selected"),
                         _("You must select a Lookup Type."));
          return true;
        }
      if (*_GGadgetGetTitle (GWidgetGetControl (ld->gw, CID_LookupName)) ==
          '\0')
        {
          ff_post_error (_("Unnamed lookup"), _("You must name the lookup."));
          return true;
        }
      for (i = 0; i < rows; ++i)
        {
          if (sscanf (strings[2 * i + 0].u.md_str, "<%d,%d>", &feat, &set) == 2)
            /* It's a mac feature/setting */ ;
          else if (strlen (strings[2 * i + 0].u.md_str) > 4)
            {
              ff_post_error (_("Bad feature tag"),
                             _
                             ("The feature tag on line %d (%s) is too long.  It may be at most 4 letters (or it could be a mac feature setting, two numbers in brokets <3,4>)"),
                             i + 1, strings[2 * i + 0].u.md_str);
              return true;
            }
          else
            {
              for (pt = strings[2 * i + 0].u.md_str; *pt != '\0'; ++pt)
                if (*pt > 0x7e)
                  {
                    ff_post_error (_("Bad feature tag"),
                                   _
                                   ("The feature tag on line %d (%s) should be in ASCII.\n"),
                                   i + 1, strings[2 * i + 0].u.md_str);
                    return true;
                  }
            }
          /* Now check the script langs */
          for (start = strings[2 * i + 1].u.md_str; *start != '\0';)
            {
              for (pt = start; *pt != '{' && *pt != '\0'; ++pt)
                {
                  if (*pt > 0x7e)
                    {
                      ff_post_error (_("Bad script tag"),
                                     _
                                     ("A script tag on line %d (%s) should be in ASCII.\n"),
                                     i + 1, strings[2 * i + 1].u.md_str);
                      return true;
                    }
                }
              if (pt - start > 4)
                {
                  ff_post_error (_("Bad script tag"),
                                 _
                                 ("A script tag on line %d (%s) is too long.  It may be at most 4 letters"),
                                 i + 1, strings[2 * i + 0].u.md_str);
                  return true;
                }
              if (*pt == '{')
                {
                  for (start = pt + 1; *start != '}' && *start != '\0';)
                    {
                      for (pt = start; *pt != '}' && *pt != ',' && *pt != '\0';
                           ++pt)
                        {
                          if (*pt > 0x7e)
                            {
                              ff_post_error (_("Bad language tag"),
                                             _
                                             ("A language tag on line %d (%s) should be in ASCII.\n"),
                                             i + 1,
                                             strings[2 * i + 1].u.md_str);
                              return true;
                            }
                        }
                      if (pt - start > 4)
                        {
                          ff_post_error (_("Bad language tag"),
                                         _
                                         ("A language tag on line %d (%s) is too long.  It may be at most 4 letters"),
                                         i + 1, strings[2 * i + 0].u.md_str);
                          return true;
                        }
                      start = (*pt == ',') ? pt + 1 : pt;
                    }
                  if (*start == '}')
                    ++start;
                }
              else
                start = pt;
              while (*start == ' ')
                ++start;
            }
        }
      name = GGadgetGetTitle8 (GWidgetGetControl (ld->gw, CID_LookupName));
      for (isgpos = 0; isgpos < 2; ++isgpos)
        {
          for (test = isgpos ? ld->sf->gpos_lookups : ld->sf->gsub_lookups;
               test != NULL; test = test->next)
            {
              if (test != otl && strcmp (test->lookup_name, name) == 0)
                {
                  ff_post_error (_("Lookup name already used"),
                                 _
                                 ("This name has already been used for another lookup.\nLookup names must be unique."));
                  free (name);
                  return true;
                }
            }
        }

      flags = 0;
      if (GGadgetIsChecked (GWidgetGetControl (ld->gw, CID_Lookup_R2L)))
        flags |= pst_r2l;
      if (GGadgetIsChecked (GWidgetGetControl (ld->gw, CID_Lookup_IgnBase)))
        flags |= pst_ignorebaseglyphs;
      if (GGadgetIsChecked (GWidgetGetControl (ld->gw, CID_Lookup_IgnLig)))
        flags |= pst_ignoreligatures;
      if (GGadgetIsChecked (GWidgetGetControl (ld->gw, CID_Lookup_IgnMark)))
        flags |= pst_ignorecombiningmarks;
      flags |=
        ((intptr_t)
         GGadgetGetListItemSelected (GWidgetGetControl
                                     (ld->gw,
                                      CID_Lookup_ProcessMark))->userdata) << 8;
      set =
        ((intptr_t)
         GGadgetGetListItemSelected (GWidgetGetControl
                                     (ld->gw,
                                      CID_Lookup_ProcessSet))->userdata);
      if (set != -1)
        flags |= pst_usemarkfilteringset | (set << 16);

      if (!ld->isgpos)
        afm = GGadgetIsChecked (GWidgetGetControl (ld->gw, CID_LookupAfm));
      else
        afm = false;

      /* Ok, we validated the feature script lang list. Now parse it */
      fhead = LK_ParseFL (strings, rows);
      free (otl->lookup_name);
      FeatureScriptLangListFree (otl->features);
      otl->lookup_name = name;
      otl->lookup_type = lookup_type;
      otl->lookup_flags = flags;
      otl->features = FLOrder (fhead);
      otl->store_in_afm = afm;
      ld->done = true;
      ld->ok = true;
    }
  return true;
}

static int
Lookup_Cancel (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct lookup_dlg *ld = GDrawGetUserData (GGadgetGetWindow (g));
      ld->done = true;
      ld->ok = false;
    }
  return true;
}

static int
lookup_e_h (GWindow gw, GEvent *event)
{

  if (event->type == et_close)
    {
      struct lookup_dlg *ld = GDrawGetUserData (gw);
      ld->done = true;
      ld->ok = false;
    }
  else if (event->type == et_char)
    {
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("lookups.html#Add-Lookup");
          return true;
        }
      return false;
    }

  return true;
}

int
EditLookup (OTLookup *otl, int isgpos, SplineFont *sf)
{
  /* Ok we must provide a lookup type (but only if otl->type==ot_undef) */
  /* a set of lookup flags */
  /* a name */
  /* A potentially empty set of features, scripts and languages */
  /* afm */
  /* check to make sure the name isn't a duplicate */
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[16], boxes[7];
  GGadgetCreateData *harray1[4], *varray[14], *flaghvarray[10], *flagarray[12],
    *harray2[4], *harray3[8];
  GTextInfo label[16];
  struct lookup_dlg ld;
  struct matrixinit mi;
  int class, i, k, vpos;

  LookupUIInit ();

  memset (&ld, 0, sizeof (ld));
  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.is_dlg = true;
  wattrs.restrict_input_to_me = 1;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Lookup");
  pos.x = pos.y = 0;
  pos.width = GDrawPointsToPixels (NULL, GGadgetScale (268));
  pos.height = GDrawPointsToPixels (NULL, 375);
  ld.gw = gw = GDrawCreateTopWindow (NULL, &pos, lookup_e_h, &ld, &wattrs);

  ld.orig = otl;
  ld.sf = sf;
  ld.isgpos = isgpos;

  memset (boxes, 0, sizeof (boxes));
  memset (gcd, 0, sizeof (gcd));
  memset (label, 0, sizeof (label));

  label[0].text = (uint32_t *) _("Type:");
  label[0].text_is_1byte = true;
  label[0].text_has_mnemonic = true;
  gcd[0].gd.label = &label[0];
  gcd[0].gd.pos.x = 12;
  gcd[0].gd.pos.y = 6 + 6;
  gcd[0].gd.flags = gg_visible | gg_enabled;
  gcd[0].creator = GLabelCreate;

  gcd[1].gd.pos.x = 100;
  gcd[1].gd.pos.y = 6;
  gcd[1].gd.pos.width = 200;
  gcd[1].gd.flags = otl->lookup_type != ot_undef
    && otl->subtables !=
    NULL ? (gg_visible | gg_utf8_popup) : (gg_visible | gg_enabled |
                                           gg_utf8_popup);
  gcd[1].gd.cid = CID_LookupType;
  gcd[1].gd.u.list = lookuptypes[isgpos];
  gcd[1].gd.handle_controlevent = LK_TypeChanged;
  gcd[1].gd.popup_msg =
    (uint32_t *) _("Each lookup may contain many transformations,\n"
                   "but each transformation must be of the same type.");
  gcd[1].creator = GListButtonCreate;

  for (i = 0;
       lookuptypes[isgpos][i].text != NULL || lookuptypes[isgpos][i].line; ++i)
    {
      if ((void *) otl->lookup_type == lookuptypes[isgpos][i].userdata
          && !lookuptypes[isgpos][i].line)
        {
          lookuptypes[isgpos][i].selected = true;
          gcd[1].gd.label = &lookuptypes[isgpos][i];
        }
      else
        lookuptypes[isgpos][i].selected = false;
    }
  harray1[0] = &gcd[0];
  harray1[1] = &gcd[1];
  harray1[2] = GCD_Glue;
  harray1[3] = NULL;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = harray1;
  boxes[0].creator = GHBoxCreate;
  varray[0] = &boxes[0];
  varray[1] = NULL;

  LKMatrixInit (&mi, otl);
  featureci[0].enum_vals = FeatureListFromLookupType (otl->lookup_type);

  gcd[2].gd.pos.x = 10;
  gcd[2].gd.pos.y = gcd[1].gd.pos.y + 14;
  gcd[2].gd.pos.width = 300;
  gcd[2].gd.pos.height = 90;
  gcd[2].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[2].gd.cid = CID_LookupFeatures;
  gcd[2].gd.u.matrix = &mi;
  gcd[2].gd.popup_msg =
    (uint32_t *) _("Most lookups will be attached to a feature\n"
                   "active in a specific script for certain languages.\n"
                   "In some cases lookups will not be attached to any\n"
                   "feature, but will be invoked by another lookup,\n"
                   "a conditional one. In other cases a lookup might\n"
                   "be attached to several features.\n"
                   "A feature is either a four letter OpenType feature\n"
                   "tag, or a two number mac <feature,setting> combination.");
  gcd[2].creator = GMatrixEditCreate;
  varray[2] = &gcd[2];
  varray[3] = NULL;


  gcd[3].gd.pos.x = 5;
  gcd[3].gd.pos.y = 5;
  gcd[3].gd.flags =
    gg_visible | gg_enabled | (otl->lookup_flags & pst_r2l ? gg_cb_on : 0);
  label[3].text = (uint32_t *) _("Right To Left");
  label[3].text_is_1byte = true;
  gcd[3].gd.label = &label[3];
  gcd[3].gd.cid = CID_Lookup_R2L;
  gcd[3].creator = GCheckBoxCreate;
  flagarray[0] = &gcd[3];
  flagarray[1] = NULL;

  gcd[4].gd.pos.x = 5;
  gcd[4].gd.pos.y = gcd[3].gd.pos.y + 15;
  gcd[4].gd.flags =
    gg_visible | gg_enabled | (otl->lookup_flags & pst_ignorebaseglyphs ?
                               gg_cb_on : 0);
  label[4].text = (uint32_t *) _("Ignore Base Glyphs");
  label[4].text_is_1byte = true;
  gcd[4].gd.label = &label[4];
  gcd[4].gd.cid = CID_Lookup_IgnBase;
  gcd[4].creator = GCheckBoxCreate;
  flagarray[2] = &gcd[4];
  flagarray[3] = NULL;

  gcd[5].gd.pos.x = 5;
  gcd[5].gd.pos.y = gcd[4].gd.pos.y + 15;
  gcd[5].gd.flags =
    gg_visible | gg_enabled | (otl->lookup_flags & pst_ignoreligatures ?
                               gg_cb_on : 0);
  label[5].text = (uint32_t *) _("Ignore Ligatures");
  label[5].text_is_1byte = true;
  gcd[5].gd.label = &label[5];
  gcd[5].gd.cid = CID_Lookup_IgnLig;
  gcd[5].creator = GCheckBoxCreate;
  flagarray[4] = &gcd[5];
  flagarray[5] = NULL;

  gcd[6].gd.pos.x = 5;
  gcd[6].gd.pos.y = gcd[5].gd.pos.y + 15;
  gcd[6].gd.flags =
    gg_visible | gg_enabled | (otl->lookup_flags & pst_ignorecombiningmarks ?
                               gg_cb_on : 0);
  label[6].text = (uint32_t *) _("Ignore Combining Marks");
  label[6].text_is_1byte = true;
  gcd[6].gd.label = &label[6];
  gcd[6].gd.cid = CID_Lookup_IgnMark;
  gcd[6].creator = GCheckBoxCreate;
  flagarray[6] = &gcd[6];
  flagarray[7] = NULL;

/* TRANSLATORS: Process is a verb here and Mark is a noun. */
/* Marks of the given mark class are to be processed */
  label[7].text = (uint32_t *) _("Mark Class:");
  label[7].text_is_1byte = true;
  gcd[7].gd.label = &label[7];
  gcd[7].gd.pos.x = 5;
  gcd[7].gd.pos.y = gcd[6].gd.pos.y + 16;
  gcd[7].gd.flags =
    sf->mark_class_cnt <= 1 ? gg_visible : (gg_enabled | gg_visible);
  gcd[7].creator = GLabelCreate;
  flaghvarray[0] = &gcd[7];

  gcd[8].gd.pos.x = 10;
  gcd[8].gd.pos.y = gcd[7].gd.pos.y;
  gcd[8].gd.pos.width = 140;
  gcd[8].gd.flags = gcd[7].gd.flags;
  if ((class = ((otl->lookup_flags & 0xff00) >> 8)) >= sf->mark_class_cnt)
    class = 0;
  gcd[8].gd.u.list = SFMarkClassList (sf, class);
  gcd[8].gd.label = &gcd[8].gd.u.list[class];
  gcd[8].gd.cid = CID_Lookup_ProcessMark;
  gcd[8].creator = GListButtonCreate;
  flaghvarray[1] = &gcd[8];
  flaghvarray[2] = GCD_Glue;
  flaghvarray[3] = NULL;

/* TRANSLATORS: Mark is a noun here and Set is also a noun. */
  label[9].text = (uint32_t *) _("Mark Set:");
  label[9].text_is_1byte = true;
  gcd[9].gd.label = &label[9];
  gcd[9].gd.flags =
    sf->mark_set_cnt < 1 ? gg_visible : (gg_enabled | gg_visible);
  gcd[9].creator = GLabelCreate;
  flaghvarray[4] = &gcd[9];

  gcd[10].gd.pos.width = 140;
  gcd[10].gd.flags = gcd[9].gd.flags;
  class = (otl->lookup_flags >> 16) & 0xffff;
  if (!(otl->lookup_flags & pst_usemarkfilteringset)
      || class >= sf->mark_set_cnt)
    class = -1;
  gcd[10].gd.u.list = SFMarkSetList (sf, class);
  gcd[10].gd.label = &gcd[10].gd.u.list[class + 1];
  gcd[10].gd.cid = CID_Lookup_ProcessSet;
  gcd[10].creator = GListButtonCreate;
  flaghvarray[5] = &gcd[10];
  flaghvarray[6] = GCD_Glue;
  flaghvarray[7] = NULL;
  flaghvarray[8] = NULL;

  boxes[1].gd.flags = gg_enabled | gg_visible;
  boxes[1].gd.u.boxelements = flaghvarray;
  boxes[1].creator = GHVBoxCreate;

  flagarray[8] = &boxes[1];
  flagarray[9] = NULL;
  flagarray[10] = NULL;

  boxes[2].gd.pos.x = boxes[2].gd.pos.y = 2;
  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = flagarray;
  boxes[2].creator = GHVGroupCreate;
  varray[4] = &boxes[2];
  varray[5] = NULL;

  label[11].text = (uint32_t *) _("Lookup Name:");
  label[11].text_is_1byte = true;
  gcd[11].gd.label = &label[11];
  gcd[11].gd.pos.x = 5;
  gcd[11].gd.pos.y = gcd[8].gd.pos.y + 16;
  gcd[11].gd.flags = gg_enabled | gg_visible;
  gcd[11].creator = GLabelCreate;
  harray2[0] = &gcd[11];

  label[12].text = (uint32_t *) otl->lookup_name;
  label[12].text_is_1byte = true;
  gcd[12].gd.pos.x = 10;
  gcd[12].gd.pos.y = gcd[11].gd.pos.y;
  gcd[12].gd.pos.width = 140;
  gcd[12].gd.flags = gcd[11].gd.flags;
  gcd[12].gd.label = otl->lookup_name == NULL ? NULL : &label[12];
  gcd[12].gd.cid = CID_LookupName;
  gcd[12].gd.handle_controlevent = Lookup_NameChanged;
  gcd[12].creator = GTextFieldCreate;
  harray2[1] = &gcd[12];
  harray2[2] = NULL;
  if (otl->lookup_name != NULL && *otl->lookup_name != '\0')
    ld.name_has_been_set = true;

  boxes[3].gd.flags = gg_enabled | gg_visible;
  boxes[3].gd.u.boxelements = harray2;
  boxes[3].creator = GHBoxCreate;
  varray[6] = &boxes[3];
  varray[7] = NULL;

  k = 13;
  vpos = 8;
  if (!isgpos)
    {
      gcd[13].gd.pos.x = 5;
      gcd[13].gd.pos.y = gcd[5].gd.pos.y + 15;
      gcd[13].gd.flags =
        otl->lookup_type !=
        gsub_ligature ? gg_visible : otl->store_in_afm ? (gg_visible |
                                                          gg_enabled | gg_cb_on)
        : (gg_visible | gg_enabled);
      label[13].text = (uint32_t *) _("Store ligature data in AFM files");
      label[13].text_is_1byte = true;
      gcd[13].gd.label = &label[13];
      gcd[13].gd.cid = CID_LookupAfm;
      gcd[13].creator = GCheckBoxCreate;
      varray[8] = &gcd[13];
      varray[9] = NULL;
      k = 14;
      vpos = 10;
    }

  gcd[k].gd.pos.x = 30 - 3;
  gcd[k].gd.pos.width = -1;
  gcd[k].gd.pos.height = 0;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.handle_controlevent = Lookup_OK;
  gcd[k].gd.cid = CID_OK;
  gcd[k].creator = GButtonCreate;

  gcd[k + 1].gd.pos.x = -30;
  gcd[k + 1].gd.pos.width = -1;
  gcd[k + 1].gd.pos.height = 0;
  gcd[k + 1].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[k + 1].text = (uint32_t *) _("_Cancel");
  label[k + 1].text_is_1byte = true;
  label[k + 1].text_has_mnemonic = true;
  gcd[k + 1].gd.label = &label[k + 1];
  gcd[k + 1].gd.handle_controlevent = Lookup_Cancel;
  gcd[k + 1].gd.cid = CID_Cancel;
  gcd[k + 1].creator = GButtonCreate;

  harray3[0] = harray3[2] = harray3[3] = harray3[4] = harray3[6] = GCD_Glue;
  harray3[7] = NULL;
  harray3[1] = &gcd[k];
  harray3[5] = &gcd[k + 1];

  boxes[4].gd.flags = gg_enabled | gg_visible;
  boxes[4].gd.u.boxelements = harray3;
  boxes[4].creator = GHBoxCreate;
  varray[vpos++] = &boxes[4];
  varray[vpos++] = NULL;
  varray[vpos] = NULL;

  boxes[5].gd.pos.x = boxes[5].gd.pos.y = 2;
  boxes[5].gd.flags = gg_enabled | gg_visible;
  boxes[5].gd.u.boxelements = varray;
  boxes[5].creator = GHVGroupCreate;

  GGadgetsCreate (gw, boxes + 5);

  GTextInfoListFree (gcd[8].gd.u.list);
  GTextInfoListFree (gcd[10].gd.u.list);

  for (i = 0; i < mi.initial_row_cnt; ++i)
    {
      free (mi.matrix_data[2 * i + 0].u.md_str);
      free (mi.matrix_data[2 * i + 1].u.md_str);
    }
  free (mi.matrix_data);

  GMatrixEditSetNewText (gcd[2].ret, C_ ("OpenTypeFeature", "New"));
  GHVBoxSetExpandableRow (boxes[5].ret, 1);
  GHVBoxSetExpandableCol (boxes[4].ret, gb_expandgluesame);
  GHVBoxSetExpandableCol (boxes[3].ret, 1);
  GHVBoxSetExpandableCol (boxes[1].ret, 1);
  GHVBoxSetExpandableCol (boxes[0].ret, gb_expandglue);

  GHVBoxFitWindow (boxes[5].ret);

  GDrawSetVisible (gw, true);
  while (!ld.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (gw);

  return ld.ok;
}

static OTLookup *
CreateAndSortNewLookupOfType (SplineFont *sf, int lookup_type)
{
  OTLookup *newotl;
  int isgpos = lookup_type >= gpos_start;

  newotl = xzalloc (sizeof (OTLookup));
  newotl->lookup_type = lookup_type;
  if (!EditLookup (newotl, isgpos, sf))
    {
      free (newotl);
      return NULL;
    }
  SortInsertLookup (sf, newotl);
  return newotl;
}

/* ************************************************************************** */
/* ***************************** Anchor Subtable **************************** */
/* ************************************************************************** */
typedef struct anchorclassdlg
{
  SplineFont *sf;
  int def_layer;
  struct lookup_subtable *sub;
  GWindow gw;
  int mag, pixelsize;
  int orig_pos, orig_value, down;
  BDFFont *display;
  int done;
  int popup_r;
  GCursor cursor_current;
  /* Used during autokern */
  int rows_at_start;
  int rows, cols;               /* Total rows, columns allocated */
  int next_row;
  struct matrix_data *psts;
} AnchorClassDlg, PSTKernDlg;
#define CID_Anchors	2001

#define CID_PSTList	2001
#define CID_Alpha	2002
#define CID_Unicode	2003
#define CID_Scripts	2004
#define CID_BaseChar	2005
#define CID_Suffix	2006
#define CID_AllSame	2007
#define CID_Separation	2008
#define CID_MinKern	2009
#define CID_Touched	2010
#define CID_OnlyCloser	2011
#define CID_Autokern	2012

#define CID_KernDisplay		2022
#define CID_PixelSize		2023
#define CID_Magnification	2024

static GTextInfo magnifications[] = {
  {(uint32_t *) "100%", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 1, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "200%", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "300%", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  {(uint32_t *) "400%", NULL, 0, 0, NULL, NULL, 0, 0, 0, 0, 0, 0, 1, 0, 0,
   '\0'},
  GTEXTINFO_EMPTY
};

static AnchorClass *
SFAddAnchorClass (SplineFont *sf, struct lookup_subtable *sub, char *name)
{
  assert (sf != NULL);
  assert (sub != NULL);
  assert (name != NULL);

  AnchorClass *ac = xzalloc (sizeof (AnchorClass));
  ac->name = xstrdup (name);
  ac->subtable = sub;
  ac->next = sf->anchor;
  sf->anchor = ac;
  return ac;
}

static int
AnchorClassD_ShowAnchors (GGadget *g, GEvent *e)
{
  AnchorClassDlg *acd;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct matrix_data *classes;
      int32_t class_cnt;
      AnchorClass *ac;

      acd = GDrawGetUserData (GGadgetGetWindow (g));
      classes =
        GMatrixEditGet (GWidgetGetControl (acd->gw, CID_Anchors), &class_cnt);
      GGadget *thing = GWidgetGetControl (acd->gw, CID_Anchors);
      int cols = GMatrixEditGetColCnt (thing);
      int row = GMatrixEditGetActiveRow (thing);
      if (row == -1)
        return true;

      ac = classes[cols * row + 1].u.md_addr;
      if (ac == NULL)
        {
          ac = SFAddAnchorClass (acd->sf, acd->sub,
                                 classes[cols * row + 0].u.md_str);
          classes[cols * row + 1].u.md_addr = ac;
        }
      else if (ac->subtable != acd->sub)
        {
          ff_post_error (_("Name in use"),
                         _("The name, %.80s, has already been used to "
                           "identify an anchor class in a different "
                           "lookup subtable (%.80s)"),
                         ac->name, ac->subtable->subtable_name);
          return true;
        }
      AnchorControlClass (acd->sf, ac, acd->def_layer);
    }
  return true;
}

static void
AC_EnableOtherButtons (GGadget *g, int r, int c)
{
  GGadgetSetEnabled (GWidgetGetControl (GGadgetGetWindow (g), CID_ShowAnchors),
                     r != -1);
}

static int
AC_OK (GGadget *g, GEvent *e)
{
  AnchorClassDlg *acd;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct matrix_data *classes;
      int32_t class_cnt;
      int i, j;
      AnchorClass *ac, *acnext, *actest;

      acd = GDrawGetUserData (GGadgetGetWindow (g));
      classes =
        GMatrixEditGet (GWidgetGetControl (acd->gw, CID_Anchors), &class_cnt);
      acd->sub->anchor_classes = true /*class_cnt!=0 */ ;
      for (i = 0; i < class_cnt; ++i)
        {
          if (*classes[2 * i + 0].u.md_str == '\0')
            continue;           /* Ignore blank lines. They pressed return once too often or something */
          for (j = i + 1; j < class_cnt; ++j)
            {
              if (strcmp
                  (classes[2 * i + 0].u.md_str,
                   classes[2 * j + 0].u.md_str) == 0)
                {
                  ff_post_error (_("Name used twice"),
                                 _
                                 ("The name, %.80s, appears twice in this list.\nEach anchor class must have a distinct name."),
                                 classes[2 * i + 0].u.md_str);
                  return true;
                }
            }
          for (actest = acd->sf->anchor; actest != NULL; actest = actest->next)
            if (actest->subtable != acd->sub
                && strcmp (actest->name, classes[2 * i + 0].u.md_str) == 0)
              break;
          if (actest != NULL)
            {
              ff_post_error (_("Name in use"),
                             _
                             ("The name, %.80s, has already been used to identify an anchor class in a different lookup subtable (%.80s)"),
                             actest->name, actest->subtable->subtable_name);
              return true;
            }
        }

      for (ac = acd->sf->anchor; ac != NULL; ac = ac->next)
        ac->processed = false;
      for (i = 0; i < class_cnt; ++i)
        {
          ac = classes[2 * i + 1].u.md_addr;
          if (ac != NULL)
            ac->processed = true;
        }
      for (ac = acd->sf->anchor; ac != NULL; ac = ac->next)
        {
          if (!ac->processed && ac->subtable == acd->sub)
            {
              char *buts[3];
              buts[0] = _("_Remove");
              buts[1] = _("_Cancel");
              buts[2] = NULL;
              if (gwwv_ask
                  (_("Remove Anchor Class?"), (const char **) buts, 0, 1,
                   _
                   ("Do you really want to remove the anchor class, %.80s?\nThis will remove all anchor points associated with that class."),
                   ac->name) == 1)
                return true;
            }
        }

      for (i = 0; i < class_cnt; ++i)
        {
          if (*classes[2 * i + 0].u.md_str == '\0')
            continue;           /* Ignore blank lines. They pressed return once too much or something */
          ac = classes[2 * i + 1].u.md_addr;
          if (ac == NULL)
            {
              ac =
                SFAddAnchorClass (acd->sf, acd->sub,
                                  classes[2 * i + 0].u.md_str);
              ac->processed = true;
            }
          else
            {
              free (ac->name);
              ac->name = xstrdup_or_null (classes[2 * i + 0].u.md_str);
              ac->processed = true;
            }
        }
      for (ac = acd->sf->anchor; ac != NULL; ac = acnext)
        {
          acnext = ac->next;
          if (!ac->processed && ac->subtable == acd->sub)
            {
              SFRemoveAnchorClass (acd->sf, ac);
            }
        }
      acd->done = true;
    }
  return true;
}

static void
AC_DoCancel (AnchorClassDlg * acd)
{
  acd->done = true;
}

static int
AC_Cancel (GGadget *g, GEvent *e)
{
  AnchorClassDlg *acd;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      acd = GDrawGetUserData (GGadgetGetWindow (g));
      AC_DoCancel (acd);
    }
  return true;
}

static int
acd_e_h (GWindow gw, GEvent *event)
{
  AnchorClassDlg *acd = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_close:
      AC_DoCancel (acd);
      break;
    case et_char:
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("lookups.html#Anchor");
          return true;
        }
      return false;
      break;
    case et_destroy:
      break;
    case et_mouseup:
    case et_mousemove:
    case et_mousedown:
      break;
    case et_expose:
      break;
    case et_resize:
      break;
    }
  return true;
}

static void
ACDMatrixInit (struct matrixinit *mi, SplineFont *sf,
               struct lookup_subtable *sub)
{
  int cnt;
  AnchorClass *ac;
  struct matrix_data *md;
  static struct col_init ci[] = {
    {me_string, NULL, NULL, NULL, N_("Anchor Class Name")},
    {me_addr, NULL, NULL, NULL, "Anchor Class Pointer, hidden"},
    COL_INIT_EMPTY
  };
  static int initted = false;

  if (!initted)
    {
      initted = true;
      ci[0].title = _(ci[0].title);
    }

  memset (mi, 0, sizeof (*mi));
  mi->col_cnt = 2;
  mi->col_init = ci;

  for (ac = sf->anchor, cnt = 0; ac != NULL; ac = ac->next)
    if (ac->subtable == sub)
      ++cnt;
  if (cnt == 0)
    {
      md = xcalloc (1, sizeof (struct matrix_data));
      mi->initial_row_cnt = 0;
    }
  else
    {
      md = xcalloc (2 * cnt, sizeof (struct matrix_data));
      for (ac = sf->anchor, cnt = 0; ac != NULL; ac = ac->next)
        if (ac->subtable == sub)
          {
            md[2 * cnt + 0].u.md_str = ac->name;
            md[2 * cnt++ + 1].u.md_addr = ac;
          }
      mi->initial_row_cnt = cnt;
    }
  mi->matrix_data = md;
}

/* Anchor list, [new], [delete], [edit], [show first mark/entry], [show first base/exit] */
/*  [ok], [cancel] */
static void
AnchorClassD (SplineFont *sf, struct lookup_subtable *sub, int def_layer)
{
  GRect pos;
  GWindowAttrs wattrs;
  AnchorClassDlg acd;
  GWindow gw;
  char buffer[200];
  GGadgetCreateData gcd[6], buttonbox, mainbox[2];
  GGadgetCreateData *buttonarray[8], *varray[7];
  GTextInfo label[6];
  int i;
  struct matrixinit mi;

  memset (&acd, 0, sizeof (acd));
  acd.sf = sf;
  acd.def_layer = def_layer;
  acd.sub = sub;

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&buttonbox, 0, sizeof (buttonbox));
  memset (&mainbox, 0, sizeof (mainbox));
  memset (&label, 0, sizeof (label));

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = true;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  snprintf (buffer, sizeof (buffer), _("Anchor classes in subtable %.80s"),
            sub->subtable_name);
  wattrs.utf8_window_title = buffer;
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = GGadgetScale (GDrawPointsToPixels (NULL, 300));
  pos.height = GDrawPointsToPixels (NULL, 200);
  acd.gw = gw = GDrawCreateTopWindow (NULL, &pos, acd_e_h, &acd, &wattrs);

  i = 0;

  ACDMatrixInit (&mi, sf, sub);
  gcd[i].gd.pos.width = 300;
  gcd[i].gd.pos.height = 200;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.cid = CID_Anchors;
  gcd[i].gd.u.matrix = &mi;
  gcd[i].data = &acd;
  gcd[i++].creator = GMatrixEditCreate;
  varray[0] = &gcd[i - 1];
  varray[1] = NULL;

  gcd[i].gd.pos.x = 10;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 24 + 3;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[i].text = (uint32_t *) _("_OK");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = AC_OK;
  gcd[i].gd.cid = CID_OK;
  gcd[i++].creator = GButtonCreate;

  gcd[i].gd.pos.x = -10;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 3;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[i].text = (uint32_t *) _("_Cancel");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = AC_Cancel;
  gcd[i].gd.cid = CID_Cancel;
  gcd[i++].creator = GButtonCreate;

  buttonarray[0] = GCD_Glue;
  buttonarray[1] = &gcd[i - 2];
  buttonarray[2] = GCD_Glue;
  buttonarray[3] = GCD_Glue;
  buttonarray[4] = &gcd[i - 1];
  buttonarray[5] = GCD_Glue;
  buttonarray[6] = NULL;
  buttonbox.gd.flags = gg_enabled | gg_visible;
  buttonbox.gd.u.boxelements = buttonarray;
  buttonbox.creator = GHBoxCreate;
  varray[2] = &buttonbox;
  varray[3] = NULL;
  varray[4] = NULL;

  mainbox[0].gd.pos.x = mainbox[0].gd.pos.y = 2;
  mainbox[0].gd.flags = gg_enabled | gg_visible;
  mainbox[0].gd.u.boxelements = varray;
  mainbox[0].creator = GHVGroupCreate;

  GGadgetsCreate (acd.gw, mainbox);
  GHVBoxSetExpandableRow (mainbox[0].ret, 0);
  GHVBoxSetExpandableCol (buttonbox.ret, gb_expandgluesame);
  GMatrixEditShowColumn (gcd[0].ret, 1, false);

  gcd[i].gd.flags = gg_visible | gg_enabled;
  label[i].text = (uint32_t *) _("Anchor Control...");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.cid = CID_ShowAnchors;
  gcd[i].gd.handle_controlevent = AnchorClassD_ShowAnchors;
  gcd[i].creator = GButtonCreate;

  GMatrixEditAddButtons (gcd[0].ret, gcd + i);
  GMatrixEditSetOtherButtonEnable (gcd[0].ret, AC_EnableOtherButtons);
  GMatrixEditSetNewText (gcd[0].ret, _("New Anchor Class"));

  GDrawSetVisible (acd.gw, true);
  while (!acd.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (acd.gw);
}

/* ************************************************************************** */
/* ********************** Single/Double Glyph Subtables ********************* */
/* ************************************************************************** */
static int isalphabetic = true, byscripts = true, stemming = true;
int lookup_hideunused = true;
static int ispair;

struct sortinfo
{
  char *glyphname;              /* We replace the glyph name with a pointer to */
  /* this structure, but must restore it when */
  /* finished sorting */
  SplineChar *sc;               /* The glyph itself */
  SplineChar *base;             /* The base of Agrave would be A */
  uint32_t script;
};

static void
SortPrep (PSTKernDlg * pstkd, struct matrix_data *md, struct sortinfo *si)
{

  si->glyphname = md->u.md_str;
  md->u.md_ival = (intptr_t) si;

  si->sc = SFGetChar (pstkd->sf, -1, si->glyphname);
  if (si->sc == NULL)
    return;
  if (byscripts)
    si->script = SCScriptFromUnicode (si->sc);
  if (stemming)
    {
      const uint32_t *alt = NULL;
      int uni = si->sc->unicodeenc;
      char *pt;

      if (uni != -1 && uni < 0x10000 && isdecompositionnormative (uni)
          && unicode_alternates[uni >> 8] != NULL
          && (alt = unicode_alternates[uni >> 8][uni & 0xff]) != NULL)
        si->base = SFGetChar (pstkd->sf, alt[0], NULL);
      if (si->base == NULL
          && ((pt = strchr (si->glyphname, '.')) != NULL
              || (pt = strchr (si->glyphname, '_')) != NULL))
        {
          int ch = *pt;
          *pt = '\0';
          si->base = SFGetChar (pstkd->sf, -1, si->glyphname);
          *pt = ch;
        }
      if (si->base == NULL)
        si->base = si->sc;
    }
}

static void
SortUnPrep (struct matrix_data *md)
{
  struct sortinfo *si = (struct sortinfo *) (md->u.md_ival);

  md->u.md_str = si->glyphname;
}

static int
_md_cmp (const struct sortinfo *md1, const struct sortinfo *md2)
{

  if (md1->sc == NULL || md2->sc == NULL)
    {
      if (md1->sc != NULL)
        return 1;
      else if (md2->sc != NULL)
        return -1;
      else
        return strcmp (md1->glyphname, md2->glyphname);
    }

  if (byscripts && md1->script != md2->script)
    {
      if (md1->script == DEFAULT_SCRIPT)
        return 1;
      else if (md2->script == DEFAULT_SCRIPT)
        return -1;
      if (md1->script > md2->script)
        return 1;
      else
        return -1;
    }

  if (!isalphabetic)
    {
      int uni1;
      int uni2;

      if (stemming)
        {
          /* First ignore case */
          uni1 =
            (md1->base->unicodeenc != -1) ? md1->base->unicodeenc : 0xffffff;
          uni2 =
            (md2->base->unicodeenc != -1) ? md2->base->unicodeenc : 0xffffff;
          if (uni1 < 0x10000 && islower (uni1))
            uni1 = toupper (uni1);
          if (uni2 < 0x10000 && islower (uni2))
            uni2 = toupper (uni2);

          if (uni1 > uni2)
            return 1;
          else if (uni1 < uni2)
            return -1;

          uni1 =
            (md1->base->unicodeenc != -1) ? md1->base->unicodeenc : 0xffffff;
          uni2 =
            (md2->base->unicodeenc != -1) ? md2->base->unicodeenc : 0xffffff;
          if (uni1 > uni2)
            return 1;
          else if (uni1 < uni2)
            return -1;
        }

      uni1 = (md1->sc->unicodeenc != -1) ? md1->sc->unicodeenc : 0xffffff;
      uni2 = (md2->sc->unicodeenc != -1) ? md2->sc->unicodeenc : 0xffffff;
      if (uni1 > uni2)
        return 1;
      else if (uni1 < uni2)
        return -1;
    }
  else
    {
      if (stemming)
        {
          int ret;
          ret = strcasecmp (md1->base->name, md2->base->name);
          if (ret != 0)
            return ret;
          ret = strcmp (md1->base->name, md2->base->name);
          if (ret != 0)
            return ret;
        }
    }
  return strcmp (md1->glyphname, md2->glyphname);
}

static int
md_cmp (const void *_md1, const void *_md2)
{
  const struct matrix_data *md1 = _md1, *md2 = _md2;
  int ret = _md_cmp ((struct sortinfo *) md1->u.md_ival,
                     (struct sortinfo *) md2->u.md_ival);

  if (ret == 0 && ispair)
    ret =
      _md_cmp ((struct sortinfo *) md1[1].u.md_ival,
               (struct sortinfo *) md2[1].u.md_ival);
  return ret;
}

static void
PSTKD_DoSort (PSTKernDlg * pstkd, struct matrix_data *psts, int rows, int cols)
{
  struct sortinfo *primary, *secondary = NULL;
  int i;

  if (pstkd->gw != NULL && GWidgetGetControl (pstkd->gw, CID_Alpha) != NULL)
    {
      isalphabetic =
        GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Alpha));
      byscripts = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Scripts));
      stemming = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_BaseChar));
    }
  primary = xcalloc (rows, sizeof (struct sortinfo));
  ispair = pstkd->sub->lookup->lookup_type == gpos_pair;
  if (ispair)
    secondary = xcalloc (rows, sizeof (struct sortinfo));
  for (i = 0; i < rows; ++i)
    {
      SortPrep (pstkd, &psts[i * cols + 0], &primary[i]);
      if (ispair)
        SortPrep (pstkd, &psts[i * cols + 1], &secondary[i]);
    }
  qsort (psts, rows, cols * sizeof (struct matrix_data), md_cmp);
  for (i = 0; i < rows; ++i)
    {
      SortUnPrep (&psts[i * cols + 0]);
      if (ispair)
        SortUnPrep (&psts[i * cols + 1]);
    }
  free (primary);
  free (secondary);
}

static void
PST_FreeImage (const void *_pstkd, GImage *img)
{
  GImageDestroy (img);
}

static GImage *
_PST_GetImage (const void *_pstkd)
{
  PSTKernDlg *pstkd = (PSTKernDlg *) _pstkd;
  GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
  int rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = GMatrixEditGet (pstk, &rows);
  SplineChar *sc =
    SFGetChar (pstkd->sf, -1, old[cols * pstkd->popup_r].u.md_str);

  return (PST_GetImage
          (pstk, pstkd->sf, pstkd->def_layer, pstkd->sub, pstkd->popup_r, sc));
}

static void
PST_PopupPrepare (GGadget *g, int r, int c)
{
  PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
  int rows, cols = GMatrixEditGetColCnt (g);
  struct matrix_data *old = GMatrixEditGet (g, &rows);
  if (c != 0 && pstkd->sub->lookup->lookup_type == gpos_single)
    return;
  if (c < 0 || c >= cols || r < 0 || r >= rows || old[cols * r].u.md_str == NULL
      || SFGetChar (pstkd->sf, -1, old[cols * r].u.md_str) == NULL)
    return;
  pstkd->popup_r = r;
  GGadgetPreparePopupImage (GGadgetGetWindow (g), NULL, pstkd, _PST_GetImage,
                            PST_FreeImage);
}

static void PSTKD_FinishSuffixedEdit (GGadget *g, int row, int col, int wasnew);
static void PSTKD_FinishBoundsEdit (GGadget *g, int row, int col, int wasnew);
static void PSTKD_FinishKernEdit (GGadget *g, int row, int col, int wasnew);
static void PSTKD_InitSameAsRow (GGadget *g, int row);

static int
is_boundsFeat (struct lookup_subtable *sub)
{
  FeatureScriptLangList *features = sub->lookup->features, *testf;

  if (sub->lookup->lookup_type == gpos_single)
    {
      for (testf = features; testf != NULL; testf = testf->next)
        {
          if (testf->featuretag == CHR ('l', 'f', 'b', 'd'))
            return 1;
          else if (testf->featuretag == CHR ('r', 't', 'b', 'd'))
            return -1;
        }
    }
  return 0;
}

static void
PSTMatrixInit (struct matrixinit *mi, SplineFont *_sf,
               struct lookup_subtable *sub, PSTKernDlg * pstkd)
{
  int cnt;
  struct matrix_data *md;
  static struct col_init simplesubsci[] = {
    {me_string, NULL, NULL, NULL, N_("Base Glyph Name")},
    {me_string, NULL, NULL, NULL, N_("Replacement Glyph Name")},
    COL_INIT_EMPTY
  };
  static struct col_init ligatureci[] = {
    {me_string, NULL, NULL, NULL, N_("Ligature Glyph Name")},
    {me_string, NULL, NULL, NULL, N_("Source Glyph Names")},
    COL_INIT_EMPTY
  };
  static struct col_init altmultsubsci[] = {
    {me_string, NULL, NULL, NULL, N_("Base Glyph Name")},
    {me_string, NULL, NULL, NULL, N_("Replacement Glyph Names")},
    COL_INIT_EMPTY
  };
#define SIM_DX		1
#define SIM_DY		3
#define SIM_DX_ADV	5
#define SIM_DY_ADV	7
#define PAIR_DX1	2
#define PAIR_DY1	4
#define PAIR_DX_ADV1	6
#define PAIR_DY_ADV1	8
#define PAIR_DX2	10
#define PAIR_DY2	12
#define PAIR_DX_ADV2	14
#define PAIR_DY_ADV2	16
  static struct col_init simpleposci[] = {
    {me_string, NULL, NULL, NULL, N_("Base Glyph Name")},
    {me_int, NULL, NULL, NULL, N_("∆x")},     /* delta-x */
/* TRANSLATORS: "Adjust" here means Device Table based pixel adjustments, an OpenType */
/* concept which allows small corrections for small pixel sizes where */
/* rounding errors (in kerning for example) may smush too glyphs together */
/* or space them too far apart. Generally not a problem for big pixelsizes*/
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y")},     /* delta-y */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆x_adv")}, /* delta-x-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y_adv")}, /* delta-y-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    COL_INIT_EMPTY
  };
  static struct col_init pairposci[] = {
    {me_string, NULL, NULL, NULL, N_("First Glyph Name")},
    {me_string, NULL, NULL, NULL, N_("Second Glyph Name")},
    {me_int, NULL, NULL, NULL, N_("∆x #1")},  /* delta-x */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y #1")},  /* delta-y */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆x_adv #1")},      /* delta-x-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y_adv #1")},      /* delta-y-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆x #2")},  /* delta-x */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y #2")},  /* delta-y */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆x_adv #2")},      /* delta-x-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    {me_int, NULL, NULL, NULL, N_("∆y_adv #2")},      /* delta-y-adv */
    {me_funcedit, DevTab_Dlg, NULL, NULL, N_("Adjust")},
    COL_INIT_EMPTY
  };
  static struct
  {
    int ltype;
    int cnt;
    struct col_init *ci;
  } fuf[] =
  {
    {
    gsub_single, sizeof (simplesubsci) / sizeof (struct col_init) - 1,
        simplesubsci},
    {
    gsub_multiple, sizeof (altmultsubsci) / sizeof (struct col_init) - 1,
        altmultsubsci},
    {
    gsub_alternate, sizeof (altmultsubsci) / sizeof (struct col_init) - 1,
        altmultsubsci},
    {
    gsub_ligature, sizeof (ligatureci) / sizeof (struct col_init) - 1,
        ligatureci},
    {
    gpos_single, sizeof (simpleposci) / sizeof (struct col_init) - 1,
        simpleposci},
    {
    gpos_pair, sizeof (pairposci) / sizeof (struct col_init) - 1, pairposci},
    {
    0, 0, NULL}
  };
  static int initted = false;
  int lookup_type = sub->lookup->lookup_type;
  int isv;
  int i, j, gid, k;
  SplineFont *sf;
  SplineChar *sc;
  PST *pst;
  KernPair *kp;

  if (!initted)
    {
      initted = true;
      for (i = 0; fuf[i].ltype != 0; ++i)
        if (fuf[i].ltype != gsub_alternate)
          {
            for (j = 0; j < fuf[i].cnt; ++j)
              fuf[i].ci[j].title = _(fuf[i].ci[j].title);
          }
    }

  memset (mi, 0, sizeof (*mi));
  for (i = 0; fuf[i].ltype != 0 && fuf[i].ltype != lookup_type; ++i);
  if (fuf[i].ltype == 0)
    {
      IError ("Unknown lookup type in PSTMatrixInit");
      i -= 2;
    }
  mi->col_cnt = fuf[i].cnt;
  mi->col_init = fuf[i].ci;

  for (j = 0; j < 2; ++j)
    {
      cnt = 0;
      k = 0;
      do
        {
          sf = _sf->subfontcnt == 0 ? _sf : _sf->subfonts[k];
          for (gid = 0; gid < sf->glyphcnt; ++gid)
            if ((sc = sf->glyphs[gid]) != NULL)
              {
                for (pst = sc->possub; pst != NULL; pst = pst->next)
                  {
                    if (pst->subtable == sub)
                      {
                        if (j)
                          {
                            md[cnt * mi->col_cnt].u.md_str =
                              xstrdup_or_null (sc->name);
                            switch (lookup_type)
                              {
                              case gsub_single:
                                md[cnt * mi->col_cnt + 1].u.md_str =
                                  xstrdup_or_null (pst->u.subs.variant);
                                break;
                              case gsub_multiple:
                              case gsub_alternate:
                                md[cnt * mi->col_cnt + 1].u.md_str =
                                  xstrdup_or_null (pst->u.mult.components);
                                break;
                              case gsub_ligature:
                                md[cnt * mi->col_cnt + 1].u.md_str =
                                  xstrdup_or_null (pst->u.lig.components);
                                break;
                              case gpos_single:
                                md[cnt * mi->col_cnt + SIM_DX].u.md_ival =
                                  pst->u.pos.xoff;
                                md[cnt * mi->col_cnt + SIM_DY].u.md_ival =
                                  pst->u.pos.yoff;
                                md[cnt * mi->col_cnt + SIM_DX_ADV].u.md_ival =
                                  pst->u.pos.h_adv_off;
                                md[cnt * mi->col_cnt + SIM_DY_ADV].u.md_ival =
                                  pst->u.pos.v_adv_off;
                                ValDevTabToStrings (md,
                                                    cnt * mi->col_cnt + SIM_DX +
                                                    1, pst->u.pos.adjust);
                                break;
                              case gpos_pair:
                                md[cnt * mi->col_cnt + 1].u.md_str =
                                  xstrdup_or_null (pst->u.pair.paired);
                                md[cnt * mi->col_cnt + PAIR_DX1].u.md_ival =
                                  pst->u.pair.vr[0].xoff;
                                md[cnt * mi->col_cnt + PAIR_DY1].u.md_ival =
                                  pst->u.pair.vr[0].yoff;
                                md[cnt * mi->col_cnt + PAIR_DX_ADV1].u.md_ival =
                                  pst->u.pair.vr[0].h_adv_off;
                                md[cnt * mi->col_cnt + PAIR_DY_ADV1].u.md_ival =
                                  pst->u.pair.vr[0].v_adv_off;
                                md[cnt * mi->col_cnt + PAIR_DX2].u.md_ival =
                                  pst->u.pair.vr[1].xoff;
                                md[cnt * mi->col_cnt + PAIR_DY2].u.md_ival =
                                  pst->u.pair.vr[1].yoff;
                                md[cnt * mi->col_cnt + PAIR_DX_ADV2].u.md_ival =
                                  pst->u.pair.vr[1].h_adv_off;
                                md[cnt * mi->col_cnt + PAIR_DY_ADV2].u.md_ival =
                                  pst->u.pair.vr[1].v_adv_off;
                                ValDevTabToStrings (md,
                                                    cnt * mi->col_cnt +
                                                    PAIR_DX1 + 1,
                                                    pst->u.pair.vr[0].adjust);
                                ValDevTabToStrings (md,
                                                    cnt * mi->col_cnt +
                                                    PAIR_DX2 + 1,
                                                    pst->u.pair.vr[1].adjust);
                                break;
                              }
                          }
                        ++cnt;
                      }
                  }
                if (lookup_type == gpos_pair)
                  {
                    for (isv = 0; isv < 2; ++isv)
                      {
                        for (kp = isv ? sc->vkerns : sc->kerns; kp != NULL;
                             kp = kp->next)
                          {
                            if (kp->subtable == sub)
                              {
                                if (j)
                                  {
                                    md[cnt * mi->col_cnt + 0].u.md_str =
                                      xstrdup_or_null (sc->name);
                                    md[cnt * mi->col_cnt + 1].u.md_str =
                                      xstrdup_or_null (kp->sc->name);
                                    if (isv)
                                      {
                                        md[cnt * mi->col_cnt +
                                           PAIR_DY_ADV1].u.md_ival = kp->off;
                                        DevTabToString (&md
                                                        [cnt * mi->col_cnt +
                                                         PAIR_DY_ADV1 +
                                                         1].u.md_str,
                                                        kp->adjust);
                                      }
                                    else
                                      {
                                        md[cnt * mi->col_cnt +
                                           PAIR_DX_ADV1].u.md_ival = kp->off;
                                        DevTabToString (&md
                                                        [cnt * mi->col_cnt +
                                                         PAIR_DX_ADV1 +
                                                         1].u.md_str,
                                                        kp->adjust);
                                      }
                                  }
                                ++cnt;
                              }
                          }
                      }
                  }
              }
          ++k;
        }
      while (k < _sf->subfontcnt);
      if (!j)
        {
          mi->initial_row_cnt = cnt;
          if (cnt == 0)
            {
              md = xcalloc (mi->col_cnt, sizeof (struct matrix_data));
              break;
            }
          else
            {
              md = xcalloc (mi->col_cnt * cnt, sizeof (struct matrix_data));
            }
        }
    }
  PSTKD_DoSort (pstkd, md, cnt, mi->col_cnt);
  mi->matrix_data = md;
  if (lookup_type == gsub_single)
    mi->finishedit = PSTKD_FinishSuffixedEdit;
  else if (lookup_type == gpos_single)
    {
      mi->initrow = PSTKD_InitSameAsRow;
      if (is_boundsFeat (sub))
        mi->finishedit = PSTKD_FinishBoundsEdit;
    }
  else if (lookup_type == gpos_pair && !sub->vertical_kerning)
    mi->finishedit = PSTKD_FinishKernEdit;
}

static void
PSTKD_FinishSuffixedEdit (GGadget *g, int row, int col, int wasnew)
{
  PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
  int rows, cols = GMatrixEditGetColCnt (g);
  struct matrix_data *psts = _GMatrixEditGet (g, &rows);
  char *suffix = GGadgetGetTitle8 (GWidgetGetControl (pstkd->gw, CID_Suffix));
  SplineChar *alt, *sc;

  if (col != 0 || !wasnew || psts[row * cols + 0].u.md_str == NULL)
    return;
  if (*suffix == '\0' || (suffix[0] == '.' && suffix[1] == '\0'))
    return;
  sc = SFGetChar (pstkd->sf, -1, psts[row * cols + 0].u.md_str);
  if (sc == NULL)
    return;
  alt = SuffixCheck (sc, suffix);
  if (alt != NULL)
    psts[row * cols + 1].u.md_str = xstrdup_or_null (alt->name);
}

static void
PSTKD_FinishBoundsEdit (GGadget *g, int row, int col, int wasnew)
{
  PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
  int rows, cols = GMatrixEditGetColCnt (g);
  struct matrix_data *psts = _GMatrixEditGet (g, &rows);
  int is_bounds = is_boundsFeat (pstkd->sub);
  SplineChar *sc;
  real loff, roff;

  if (col != 0 || !wasnew || psts[row * cols + 0].u.md_str == NULL)
    return;
  if (is_bounds == 0)
    return;
  sc = SFGetChar (pstkd->sf, -1, psts[row * cols + 0].u.md_str);
  if (sc == NULL)
    return;

  GuessOpticalOffset (sc, pstkd->def_layer, &loff, &roff, 0);
  if (is_bounds > 0)
    {
      psts[row * cols + SIM_DX].u.md_ival = -loff;
      psts[row * cols + SIM_DX_ADV].u.md_ival = -loff;
    }
  else
    psts[row * cols + SIM_DX_ADV].u.md_ival = -roff;
}

static void
PSTKD_AddKP (void *data, SplineChar *left, SplineChar *right, int off)
{
  PSTKernDlg *pstkd = data;
  struct matrix_data *psts = pstkd->psts;
  int cols = pstkd->cols;
  int i;
  SplineChar *first = left, *second = right;

  for (i = 0; i < pstkd->rows_at_start; ++i)
    {
      /* If the user has already got this combination in the lookup */
      /*  then don't add a new guess, and don't override the old */
      if (psts[i * cols + 0].u.md_str != NULL
          && psts[i * cols + 1].u.md_str != NULL
          && strcmp (psts[i * cols + 0].u.md_str, first->name) == 0
          && strcmp (psts[i * cols + 1].u.md_str, second->name) == 0)
        return;
    }

  i = pstkd->next_row++;
  if (i >= pstkd->rows)
    pstkd->psts = psts =
      xrealloc (psts,
                (pstkd->rows += 100) * cols * sizeof (struct matrix_data));
  memset (psts + i * cols, 0, cols * sizeof (struct matrix_data));
  psts[i * cols + 0].u.md_str = xstrdup_or_null (first->name);
  psts[i * cols + 1].u.md_str = xstrdup_or_null (second->name);
  psts[i * cols + PAIR_DX_ADV1].u.md_ival = off;
  if (strcmp (psts[0 + 0].u.md_str, "T") != 0)
    second = left;
}

static void
PSTKD_FinishKernEdit (GGadget *g, int row, int col, int wasnew)
{
  PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
  int rows, cols = GMatrixEditGetColCnt (g);
  struct matrix_data *psts = _GMatrixEditGet (g, &rows);
  SplineChar *sc1, *sc2;
  SplineChar *lefts[2], *rights[2];
  int err, touch, separation;

  if (col > 1 || psts[row * cols + 0].u.md_str == NULL
      || psts[row * cols + 1].u.md_str == NULL
      || psts[row * cols + PAIR_DX1].u.md_ival != 0
      || psts[row * cols + PAIR_DX_ADV1].u.md_ival != 0
      || psts[row * cols + PAIR_DY_ADV1].u.md_ival != 0
      || psts[row * cols + PAIR_DX_ADV2].u.md_ival != 0)
    return;
  sc1 = SFGetChar (pstkd->sf, -1, psts[row * cols + 0].u.md_str);
  sc2 = SFGetChar (pstkd->sf, -1, psts[row * cols + 1].u.md_str);
  if (sc1 == NULL || sc2 == NULL)
    return;
  lefts[1] = rights[1] = NULL;
  lefts[0] = sc1;
  rights[0] = sc2;

  err = false;
  touch = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Touched));
  separation = GetInt8 (pstkd->gw, CID_Separation, _("Separation"), &err);
  if (err)
    return;

  pstkd->cols = GMatrixEditGetColCnt (g);
  pstkd->psts = psts;
  pstkd->rows_at_start = 0;
  pstkd->next_row = row;
  pstkd->rows = rows;

  AutoKern2 (pstkd->sf, pstkd->def_layer, lefts, rights, pstkd->sub, separation, 0, touch, 0, 0,        /* Don't bother with minkern,onlyCloser they asked for this, they get it, whatever it may be */
             PSTKD_AddKP, pstkd);
  if (pstkd->psts != psts)
    IError ("AutoKern added too many pairs, was only supposed to add one");
  else
    GGadgetRedraw (g);
}

static void
PSTKD_InitSameAsRow (GGadget *g, int row)
{
  GWidget gw = GGadgetGetWindow (g);
  int rows, cols = GMatrixEditGetColCnt (g);
  struct matrix_data *psts = GMatrixEditGet (g, &rows);

  if (row == 0)
    return;
  if (!GGadgetIsChecked (GWidgetGetControl (gw, CID_AllSame)))
    return;
  psts[row * cols + SIM_DX].u.md_ival = psts[0 + SIM_DX].u.md_ival;
  psts[row * cols + SIM_DY].u.md_ival = psts[0 + SIM_DY].u.md_ival;
  psts[row * cols + SIM_DX_ADV].u.md_ival = psts[0 + SIM_DX_ADV].u.md_ival;
  psts[row * cols + SIM_DY_ADV].u.md_ival = psts[0 + SIM_DY_ADV].u.md_ival;
}

static int
PSTKD_Sort (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_radiochanged)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int rows, cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *old = GMatrixEditGet (pstk, &rows);
      PSTKD_DoSort (pstkd, old, rows, cols);
      GGadgetRedraw (pstk);
    }
  return true;
}

static void
PSTKD_DoHideUnused (PSTKernDlg * pstkd)
{
  GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
  int rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = GMatrixEditGet (pstk, &rows);
  uint8_t cols_used[20];
  int r, col, startc, tot;

  startc = (pstkd->sub->lookup->lookup_type == gpos_single) ? 1 : 2;
  if (lookup_hideunused)
    {
      memset (cols_used, 0, sizeof (cols_used));
      for (r = 0; r < rows; ++r)
        {
          for (col = startc; col < cols; col += 2)
            {
              if (old[cols * r + col].u.md_ival != 0)
                cols_used[col] = true;
              if (old[cols * r + col + 1].u.md_str != NULL
                  && *old[cols * r + col + 1].u.md_str != '\0')
                cols_used[col + 1] = true;
            }
        }
      /* If no columns used (no info yet, all info is to preempt a kernclass and sets to 0) */
      /*  then show what we expect to be the default column for this kerning mode */
      for (col = startc, tot = 0; col < cols; ++col)
        tot += cols_used[col];
      if (tot == 0)
        {
          if (startc == 1)
            {
              cols_used[SIM_DX] = cols_used[SIM_DY] = cols_used[SIM_DX_ADV] =
                cols_used[SIM_DY_ADV] = true;
            }
          else
            {
              if (pstkd->sub->vertical_kerning)
                cols_used[PAIR_DY_ADV1] = true;
              else
                cols_used[PAIR_DX_ADV1] = true;
            }
        }
      for (col = startc; col < cols; ++col)
        GMatrixEditShowColumn (pstk, col, cols_used[col]);
    }
  else
    {
      for (col = startc; col < cols; ++col)
        GMatrixEditShowColumn (pstk, col, true);
    }
  GWidgetToDesiredSize (pstkd->gw);

  GGadgetRedraw (pstk);
}

static int
PSTKD_HideUnused (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_radiochanged)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      lookup_hideunused = GGadgetIsChecked (g);
      PSTKD_DoHideUnused (pstkd);
      GGadgetRedraw (GWidgetGetControl (pstkd->gw, CID_PSTList));
    }
  return true;
}

static int
PSTKD_MagnificationChanged (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_listselected)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      int mag = GGadgetGetFirstListSelectedItem (g);

      if (mag != -1 && mag != pstkd->mag - 1)
        {
          pstkd->mag = mag + 1;
          GGadgetRedraw (GWidgetGetControl (pstkd->gw, CID_KernDisplay));
        }
    }
  return true;
}

static int
PSTKD_DisplaySizeChanged (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_textchanged)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      const uint32_t *ret =
        _GGadgetGetTitle (GWidgetGetControl (pstkd->gw, CID_PixelSize));
      uint32_t *end;
      int pixelsize = u32_strtol (ret, &end, 10);

      while (*end == ' ')
        ++end;
      if (pixelsize > 4 && pixelsize < 400 && *end == '\0')
        {
          pstkd->pixelsize = pixelsize;
          if (pstkd->display != NULL)
            {
              BDFFontFree (pstkd->display);
              pstkd->display = NULL;
            }
          GGadgetRedraw (GWidgetGetControl (pstkd->gw, CID_KernDisplay));
        }
    }
  return true;
}

static void
PSTKD_METextChanged (GGadget *g, int r, int c, GGadget *text)
{
  PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
  GGadgetRedraw (GWidgetGetControl (pstkd->gw, CID_KernDisplay));
}

static int
FigureValue (struct matrix_data *old, int rcol, int c, int startc, GGadget *tf,
             double scale, int pixelsize)
{
  int val;
  char *str, *freeme = NULL;
  DeviceTable dt;

  if (c == startc && tf != NULL)
    val = u32_strtol (_GGadgetGetTitle (tf), NULL, 10);
  else
    val = old[rcol + startc].u.md_ival;
  val = rint (val * scale);
  if (c == startc + 1 && tf != NULL)
    str = freeme = GGadgetGetTitle8 (tf);
  else
    str = old[rcol + startc + 1].u.md_str;
  memset (&dt, 0, sizeof (dt));
  DeviceTableParse (&dt, str);
  if (pixelsize >= dt.first_pixel_size && pixelsize <= dt.last_pixel_size
      && dt.corrections != NULL)
    val += dt.corrections[pixelsize - dt.first_pixel_size];
  free (dt.corrections);
  free (freeme);
  return val;
}

static int
ParsePSTKVR (PSTKernDlg * pstkd, GGadget *pstk, int startc, ValueRecord *vr)
{
  int rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = _GMatrixEditGet (pstk, &rows);
  GGadget *tf = _GMatrixEditGetActiveTextField (pstk);
  int r = GMatrixEditGetActiveRow (pstk);
  int c = GMatrixEditGetActiveCol (pstk);
  double scale =
    pstkd->pixelsize / (double) (pstkd->sf->ascent + pstkd->sf->descent);

  vr->xoff =
    FigureValue (old, r * cols, c, startc, tf, scale, pstkd->pixelsize);
  vr->yoff =
    FigureValue (old, r * cols, c, startc + 2, tf, scale, pstkd->pixelsize);
  vr->h_adv_off =
    FigureValue (old, r * cols, c, startc + 4, tf, scale, pstkd->pixelsize);
  vr->v_adv_off =
    FigureValue (old, r * cols, c, startc + 6, tf, scale, pstkd->pixelsize);
  return true;
}

static void
PSTKern_DrawGlyph (GWindow pixmap, int x, int y, BDFChar *bc, int mag)
{
  /* x,y is the location of the origin of the glyph, they need to */
  /*  be adjusted by the images xmin, ymax, etc. */
  struct _GImage base;
  GImage gi;
  GClut clut;
  int scale, l;
  Color fg, bg;

  scale = bc->depth == 8 ? 8 : 4;

  memset (&gi, '\0', sizeof (gi));
  memset (&base, '\0', sizeof (base));
  memset (&clut, '\0', sizeof (clut));
  gi.u.image = &base;
  base.clut = &clut;
  base.data = bc->bitmap;
  base.bytes_per_line = bc->bytes_per_line;
  base.width = bc->xmax - bc->xmin + 1;
  base.height = bc->ymax - bc->ymin + 1;
  base.image_type = it_index;
  clut.clut_len = 1 << scale;
  bg = GDrawGetDefaultBackground (NULL);
  fg = GDrawGetDefaultForeground (NULL);
  for (l = 0; l < (1 << scale); ++l)
    clut.clut[l] =
      COLOR_CREATE (COLOR_RED (bg) +
                    (l * (COLOR_RED (fg) - COLOR_RED (bg))) / ((1 << scale) -
                                                               1),
                    COLOR_GREEN (bg) +
                    (l * (COLOR_GREEN (fg) - COLOR_GREEN (bg))) /
                    ((1 << scale) - 1),
                    COLOR_BLUE (bg) +
                    (l * (COLOR_BLUE (fg) - COLOR_BLUE (bg))) / ((1 << scale) -
                                                                 1));

  x += bc->xmin;
  y -= bc->ymax;

  if (mag == 1)
    GDrawDrawImage (pixmap, &gi, NULL, x, y);
  else
    GDrawDrawImageMagnified (pixmap, &gi, NULL, x * mag, y * mag,
                             base.width * mag, base.height * mag);
}

static void
PSTKern_Expose (GWindow pixmap, PSTKernDlg * pstkd)
{
  GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
  int r;
  int rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = _GMatrixEditGet (pstk, &rows);
  SplineChar *sc1, *sc2;
  BDFChar *bc1, *bc2;
  ValueRecord vr1, vr2;
  int xorig, yorig;
  GRect size;
  int mag = pstkd->mag;

  if ((r = GMatrixEditGetActiveRow (pstk)) == -1)
    return;                     /* No kerning pair is active */
  if (old[r * cols + 0].u.md_str == NULL || old[r * cols + 1].u.md_str == NULL)
    return;                     /* No glyphs specified to kern */
  sc1 = SFGetChar (pstkd->sf, -1, old[r * cols + 0].u.md_str);
  sc2 = SFGetChar (pstkd->sf, -1, old[r * cols + 1].u.md_str);
  if (sc1 == NULL || sc2 == NULL)
    return;                     /* The names specified weren't in the font */

  if (!ParsePSTKVR (pstkd, pstk, PAIR_DX1, &vr1)
      || !ParsePSTKVR (pstkd, pstk, PAIR_DX2, &vr2))
    return;                     /* Couldn't parse the numeric kerning info */

  if (pstkd->display == NULL)
    pstkd->display =
      SplineFontPieceMeal (pstkd->sf, pstkd->def_layer, pstkd->pixelsize, 72,
                           pf_antialias, NULL);
  bc1 = BDFPieceMealCheck (pstkd->display, sc1->orig_pos);
  bc2 = BDFPieceMealCheck (pstkd->display, sc2->orig_pos);

  GDrawGetSize (GDrawableGetWindow
                (GWidgetGetControl (pstkd->gw, CID_KernDisplay)), &size);
  if (pstkd->sub->vertical_kerning)
    {
      double scale =
        pstkd->pixelsize / (double) (pstkd->sf->ascent + pstkd->sf->descent);
      int vwidth1 = rint (sc1->vwidth * scale), vwidth2 =
        rint (sc2->vwidth * scale);
      xorig = size.width / 10;
      yorig = size.height / 20;
      xorig /= mag;
      yorig /= mag;
      PSTKern_DrawGlyph (pixmap, xorig + vr1.xoff, yorig + vwidth1 - vr1.yoff,
                         bc1, mag);
      PSTKern_DrawGlyph (pixmap, xorig + vr2.xoff,
                         yorig + vwidth1 + vwidth2 + vr1.v_adv_off - vr2.yoff,
                         bc2, mag);
    }
  else
    {
      xorig = size.width / 10;
      yorig =
        pstkd->sf->ascent * size.height / (pstkd->sf->ascent +
                                           pstkd->sf->descent);
      GDrawDrawLine (pixmap, xorig, 0, xorig, size.height, 0x808080);
      GDrawDrawLine (pixmap, 0, yorig, size.width, yorig, 0x808080);
      xorig /= mag;
      yorig /= mag;
      PSTKern_DrawGlyph (pixmap, xorig + vr1.xoff, yorig - vr1.yoff, bc1, mag);
      PSTKern_DrawGlyph (pixmap, xorig + bc1->width + vr1.h_adv_off + vr2.xoff,
                         yorig - vr2.yoff, bc2, mag);
    }
}

static void
PSTKern_Mouse (PSTKernDlg * pstkd, GEvent *event)
{
  GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
  int r;
  int rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = _GMatrixEditGet (pstk, &rows);
  int c = GMatrixEditGetActiveCol (pstk);
  GGadget *tf = _GMatrixEditGetActiveTextField (pstk);
  double scale =
    pstkd->pixelsize / (double) (pstkd->sf->ascent + pstkd->sf->descent);
  int diff, col;
  char buffer[20];
  GCursor ct = ct_pointer;
  GRect size;

  if ((r = GMatrixEditGetActiveRow (pstk)) == -1)
    return;                     /* No kerning pair is active */

  if (pstkd->sub->vertical_kerning)
    {
      diff = event->u.mouse.y - pstkd->orig_pos;
      col = PAIR_DY_ADV1;
    }
  else
    {
      diff = event->u.mouse.x - pstkd->orig_pos;
      col = PAIR_DX_ADV1;
    }

  if (event->type == et_mousedown)
    {
      pstkd->down = true;
      pstkd->orig_pos =
        pstkd->sub->vertical_kerning ? event->u.mouse.y : event->u.mouse.x;
      pstkd->orig_value = FigureValue (old, r * cols, c, col, tf, 1.0, -1);
    }
  else if (pstkd->down)
    {
      diff = rint (diff / scale);
      if (col == c && tf != NULL)
        {
          sprintf (buffer, "%d", pstkd->orig_value + diff);
          GGadgetSetTitle8 (tf, buffer);
          GGadgetRedraw (tf);
        }
      else
        {
          old[r * cols + col].u.md_ival = pstkd->orig_value + diff;
          GGadgetRedraw (pstk);
        }
      GGadgetRedraw (GWidgetGetControl (pstkd->gw, CID_KernDisplay));
      if (event->type == et_mouseup)
        pstkd->down = false;
    }
  else if (event->type == et_mousemove)
    {
      SplineChar *sc1 = SFGetChar (pstkd->sf, -1, old[r * cols + 0].u.md_str);
      if (sc1 != NULL)
        {
          GDrawGetSize (event->w, &size);
          if (col == PAIR_DX_ADV1
                   && event->u.mouse.x - size.width / 10 > sc1->width * scale)
            {
              ct = ct_kerning;
            }
        }
    }
  if (ct != pstkd->cursor_current)
    {
      GDrawSetCursor (event->w, ct);
      pstkd->cursor_current = ct;
    }
}

static int
pstkern_e_h (GWindow gw, GEvent *event)
{
  switch (event->type)
    {
    case et_char:
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          help ("lookups.html#Pair");
          return true;
        }
      return false;
    case et_expose:
      PSTKern_Expose (gw, GDrawGetUserData (gw));
      return true;
    case et_mousedown:
    case et_mouseup:
    case et_mousemove:
      PSTKern_Mouse (GDrawGetUserData (gw), event);
      return true;
    }
  return true;
}

static int
SCReasonable (SplineChar *sc)
{
  if (sc == NULL)
    return false;
  if (strcmp (sc->name, ".notdef") == 0 || strcmp (sc->name, ".null") == 0
      || strcmp (sc->name, "nonmarkingreturn") == 0)
    return false;

  return true;
}

static struct matrix_data *
MDCopy (struct matrix_data *old, int rows, int cols)
{
  struct matrix_data *md = xmalloc (rows * cols * sizeof (struct matrix_data));
  int r;

  memcpy (md, old, rows * cols * sizeof (struct matrix_data));
  for (r = 0; r < rows; ++r)
    {
      md[r * cols + 0].u.md_str = xstrdup_or_null (md[r * cols + 0].u.md_str);
      if (cols == 2 /* subs, lig, alt, etc. */  || cols >= 10 /* kerning */ )
        md[r * cols + 1].u.md_str = xstrdup_or_null (md[r * cols + 1].u.md_str);
    }
  return md;
}

static int
SCNameUnused (char *name, struct matrix_data *old, int rows, int cols)
{
  int r;

  for (r = 0; r < rows; ++r)
    {
      if (old[r * cols + 0].u.md_str != NULL
          && strcmp (old[r * cols + 0].u.md_str, name) == 0)
        {
          if ((cols == 2
               && (old[r * cols + 1].u.md_str == NULL
                   || *old[r * cols + 1].u.md_str == '\0'))
              || (cols == 5 && old[r * cols + 1].u.md_ival == 0
                  && old[r * cols + 2].u.md_ival == 0
                  && old[r * cols + 3].u.md_ival == 0
                  && old[r * cols + 4].u.md_ival == 0))
            return r;         /* There's an entry, but it's blank, fill it if we can */
          else
            return -1;
        }
    }
  return r;                   /* Off end of list */
}

static int
SCIsLigature (SplineChar *sc)
{
  int len;
  const uint32_t *alt = NULL;

  if (strchr (sc->name, '_') != NULL)
    return true;
  len = strlen (sc->name);
  if (strncmp (sc->name, "uni", 3) == 0 && (len - 3) % 4 == 0 && len > 7)
    return true;

  if (sc->unicodeenc == -1 || sc->unicodeenc >= 0x10000)
    return false;
  else if (isdecompositionnormative (sc->unicodeenc)
           && unicode_alternates[sc->unicodeenc >> 8] != NULL
           && (alt =
               unicode_alternates[sc->
                                  unicodeenc >> 8][sc->unicodeenc & 0xff]) !=
           NULL)
    {
      if (alt[1] == '\0')
        return false;         /* Single replacements aren't ligatures */
      else if (iscombining (alt[1]) && (alt[2] == '\0' || iscombining (alt[2])))
        return false;         /* Nor am I interested in accented letters */
      else
        return true;
    }
  else
    return false;
}

enum pop_type
{ pt_all, pt_suffixed, pt_selected };

static void
PSTKD_DoPopulate (PSTKernDlg * pstkd, char *suffix, enum pop_type pt)
{
  GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
  int rows, row_max, old_rows, cols = GMatrixEditGetColCnt (pstk);
  struct matrix_data *old = GMatrixEditGet (pstk, &rows), *psts;
  int pos;
  int gid, k;
  SplineChar *sc, *alt;
  SplineFont *sf = pstkd->sf;
  FontView *fv = (FontView *) sf->fv;
  EncMap *map = fv->b.map;
  GGadget *gallsame = GWidgetGetControl (pstkd->gw, CID_AllSame);
  int allsame = false;
  FeatureScriptLangList *features = pstkd->sub->lookup->features;

  if (gallsame != NULL)
    allsame = GGadgetIsChecked (gallsame);

  psts = MDCopy (old, rows, cols);
  old_rows = row_max = rows;
  k = 0;
  do
    {
      sf = pstkd->sf->subfontcnt == 0 ? pstkd->sf : pstkd->sf->subfonts[k];
      for (gid = 0; gid < sf->glyphcnt; ++gid)
        {
          if (SCReasonable (sc = sf->glyphs[gid])
              && (pt == pt_selected
                  || ScriptInFeatureScriptList
                  (SCScriptFromUnicode (sc), features))
              && (pt != pt_selected
                  || (gid < fv->b.sf->glyphcnt
                      && gid_to_enc_is_set (map, gid)
                      && fv->b.selected[gid_to_enc (map, gid)]))
              && (pos = SCNameUnused (sc->name, old, old_rows, cols)) != -1
              && (pstkd->sub->lookup->lookup_type != gsub_ligature
                  || SCIsLigature (sc)))
            {
              alt = NULL;
              if (suffix != NULL)
                {
                  alt = SuffixCheck (sc, suffix);
                  if (pt == pt_suffixed && alt == NULL)
                    continue;
                }
              if (pos == old_rows)
                {
                  pos = rows;
                  if (rows >= row_max)
                    {
                      if (row_max < sf->glyphcnt - 10)
                        row_max = sf->glyphcnt;
                      else
                        row_max += 15;
                      psts =
                        xrealloc (psts,
                                  row_max * cols * sizeof (struct matrix_data));
                    }
                  memset (psts + rows * cols, 0,
                          cols * sizeof (struct matrix_data));
                  psts[rows * cols + 0].u.md_str = xstrdup_or_null (sc->name);
                  ++rows;
                }
              if (alt != NULL)
                psts[pos * cols + 1].u.md_str = xstrdup_or_null (alt->name);
              else if (allsame && pos != 0)
                {
                  psts[pos * cols + SIM_DX].u.md_ival =
                    psts[0 + SIM_DX].u.md_ival;
                  psts[pos * cols + SIM_DY].u.md_ival =
                    psts[0 + SIM_DY].u.md_ival;
                  psts[pos * cols + SIM_DX_ADV].u.md_ival =
                    psts[0 + SIM_DX_ADV].u.md_ival;
                  psts[pos * cols + SIM_DY_ADV].u.md_ival =
                    psts[0 + SIM_DY_ADV].u.md_ival;
                }
              else if (pstkd->sub->lookup->lookup_type != gpos_pair)
                SCSubtableDefaultSubsCheck (sc, pstkd->sub, psts, cols, pos,
                                            pstkd->def_layer);
            }
        }
      ++k;
    }
  while (k < pstkd->sf->subfontcnt);
  if (rows < row_max)
    psts = xrealloc (psts, rows * cols * sizeof (struct matrix_data));
  PSTKD_DoSort (pstkd, psts, rows, cols);
  GMatrixEditSet (pstk, psts, rows, false);
  GGadgetRedraw (pstk);
}

static void
PSTKD_SetSuffix (PSTKernDlg * pstkd)
{
  char *suffix;

  if (pstkd->sub->lookup->lookup_type != gsub_single)
    return;                     /* Not applicable */

  suffix = GGadgetGetTitle8 (GWidgetGetControl (pstkd->gw, CID_Suffix));
  if (*suffix != '\0' && (suffix[0] != '.' || suffix[1] != '\0'))
    {
      free (pstkd->sub->suffix);
      pstkd->sub->suffix =
        (*suffix == '.') ? xstrdup (suffix + 1) : xstrdup (suffix);
      free (suffix);
    }
}

static int
PSTKD_PopulateWithSuffix (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      char *suffix =
        GGadgetGetTitle8 (GWidgetGetControl (pstkd->gw, CID_Suffix));
      if (*suffix != '\0' && (suffix[0] != '.' || suffix[1] != '\0'))
        {
          PSTKD_DoPopulate (pstkd, suffix, pt_suffixed);
          PSTKD_SetSuffix (pstkd);
        }
      free (suffix);
    }
  return true;
}

static int
PSTKD_Populate (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *gsuffix = GWidgetGetControl (pstkd->gw, CID_Suffix);
      char *suffix = NULL;
      if (gsuffix != NULL)
        {
          suffix = GGadgetGetTitle8 (gsuffix);
          if (*suffix == '\0' || (suffix[0] == '.' && suffix[1] == '\0'))
            {
              free (suffix);
              suffix = NULL;
            }
        }
      PSTKD_SetSuffix (pstkd);
      PSTKD_DoPopulate (pstkd, suffix, pt_all);
      free (suffix);
    }
  return true;
}

static int
PSTKD_PopulateSelected (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *gsuffix = GWidgetGetControl (pstkd->gw, CID_Suffix);
      char *suffix = NULL;
      if (gsuffix != NULL)
        {
          suffix = GGadgetGetTitle8 (gsuffix);
          if (*suffix == '\0' || (suffix[0] == '.' && suffix[1] == '\0'))
            {
              free (suffix);
              suffix = NULL;
            }
        }
      PSTKD_SetSuffix (pstkd);
      PSTKD_DoPopulate (pstkd, suffix, pt_selected);
      free (suffix);
    }
  return true;
}

static int
PSTKD_DoAutoKern (PSTKernDlg * pstkd, SplineChar **glyphlist)
{
  int err, touch, separation, minkern, onlyCloser;

  if (!GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Autokern)))
    return false;

  err = false;
  touch = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Touched));
  separation = GetInt8 (pstkd->gw, CID_Separation, _("Separation"), &err);
  minkern = GetInt8 (pstkd->gw, CID_MinKern, _("Min Kern"), &err);
  onlyCloser = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_OnlyCloser));
  if (err)
    return false;
  AutoKern2 (pstkd->sf, pstkd->def_layer, glyphlist, glyphlist, pstkd->sub,
             separation, minkern, touch, onlyCloser, 0, PSTKD_AddKP, pstkd);
  return true;
}

static int
PSTKD_AutoKern (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int rows, cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *old = GMatrixEditGet (pstk, &rows);
      SplineFont *sf = pstkd->sf;
      int i, gid, cnt;
      SplineChar **list, *sc;
      FeatureScriptLangList *features = pstkd->sub->lookup->features, *testf;
      struct scriptlanglist *scripts;
      uint32_t *scripttags;

      pstkd->cols = GMatrixEditGetColCnt (pstk);
      pstkd->psts = MDCopy (old, rows, cols);
      pstkd->rows_at_start = pstkd->rows = pstkd->next_row = rows;

      for (testf = features, cnt = 0; testf != NULL; testf = testf->next)
        {
          for (scripts = testf->scripts; scripts != NULL;
               scripts = scripts->next)
            ++cnt;
        }
      if (cnt == 0)
        {
          ff_post_error (_("No scripts"),
                         _
                         ("There are no scripts bound to features bound to this lookup. So nothing happens."));
          return true;
        }
      scripttags = xmalloc ((cnt + 1) * sizeof (uint32_t));
      for (testf = features, cnt = 0; testf != NULL; testf = testf->next)
        {
          for (scripts = testf->scripts; scripts != NULL;
               scripts = scripts->next)
            {
              for (i = 0; i < cnt; ++i)
                if (scripttags[i] == scripts->script)
                  break;
              if (i == cnt)
                scripttags[cnt++] = scripts->script;
            }
        }
      scripttags[cnt] = 0;

      list = xmalloc ((sf->glyphcnt + 1) * sizeof (SplineChar *));
      for (i = 0; scripttags[i] != 0; ++i)
        {
          uint32_t script = scripttags[i];

          for (cnt = gid = 0; gid < sf->glyphcnt; ++gid)
            {
              if ((sc = sf->glyphs[gid]) != NULL && SCWorthOutputting (sc)
                  && SCScriptFromUnicode (sc) == script)
                list[cnt++] = sc;
            }
          list[cnt] = NULL;
          PSTKD_DoAutoKern (pstkd, list);
        }
      free (list);
      free (scripttags);
      if (pstkd->next_row < pstkd->rows)
        pstkd->psts =
          xrealloc (pstkd->psts,
                    pstkd->rows * cols * sizeof (struct matrix_data));
      PSTKD_DoSort (pstkd, pstkd->psts, pstkd->next_row, cols);
      GMatrixEditSet (pstk, pstkd->psts, pstkd->next_row, false);
      GGadgetRedraw (pstk);
    }
  return true;
}

static int
PSTKD_AutoKernSelected (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int rows, cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *old = GMatrixEditGet (pstk, &rows);
      SplineFont *sf = pstkd->sf;
      FontViewBase *fv = sf->fv;
      int enc, gid, cnt;
      SplineChar **list, *sc;

      pstkd->cols = GMatrixEditGetColCnt (pstk);
      pstkd->psts = MDCopy (old, rows, cols);
      pstkd->rows_at_start = pstkd->rows = pstkd->next_row = rows;

      for (enc = 0, cnt = 0; enc < fv->map->enc_limit; ++enc)
        {
          if (fv->selected[enc] && (gid = enc_to_gid (fv->map, enc)) != -1
              && SCWorthOutputting (sc = sf->glyphs[gid]))
            ++cnt;
        }
      list = xmalloc ((cnt + 1) * sizeof (SplineChar *));
      for (enc = 0, cnt = 0; enc < fv->map->enc_limit; ++enc)
        {
          if (fv->selected[enc] && (gid = enc_to_gid (fv->map, enc)) != -1
              && SCWorthOutputting (sc = sf->glyphs[gid]))
            list[cnt++] = sc;
        }
      list[cnt] = NULL;
      PSTKD_DoAutoKern (pstkd, list);
      free (list);
      if (pstkd->next_row < pstkd->rows)
        pstkd->psts =
          xrealloc (pstkd->psts,
                    pstkd->rows * cols * sizeof (struct matrix_data));
      PSTKD_DoSort (pstkd, pstkd->psts, pstkd->next_row, cols);
      GMatrixEditSet (pstk, pstkd->psts, pstkd->next_row, false);
      GGadgetRedraw (pstk);
    }
  return true;
}

static int
PSTKD_RemoveAll (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *psts = NULL;

      psts = xcalloc (cols, sizeof (struct matrix_data));
      GMatrixEditSet (pstk, psts, 0, false);
    }
  return true;
}

static int
PSTKD_RemoveEmpty (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int rows, cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *old = GMatrixEditGet (pstk, &rows), *psts = NULL;
      int r, empty, rm_cnt, j;

      for (r = rows - 1, rm_cnt = 0; r >= 0; --r)
        {
          if (pstkd->sub->lookup->lookup_type == gpos_single)
            empty = old[r * cols + SIM_DX].u.md_ival == 0
              && old[r * cols + SIM_DY].u.md_ival == 0
              && old[r * cols + SIM_DX_ADV].u.md_ival == 0
              && old[r * cols + SIM_DY_ADV].u.md_ival == 0;
          else
            empty = old[r * cols + 1].u.md_str == NULL
              || *old[r * cols + 1].u.md_str == '\0';
          if (empty)
            {
              if (psts == NULL)
                psts = MDCopy (old, rows, cols);
              free (psts[r * cols + 0].u.md_str);
              if (cols != 5)
                free (psts[r * cols + 1].u.md_str);
              for (j = r + 1; j < rows - rm_cnt; ++j)
                memcpy (psts + (j - 1) * cols, psts + j * cols,
                        cols * sizeof (struct matrix_data));
              ++rm_cnt;
            }
        }
      if (rm_cnt != 0)
        {
          /* Some reallocs explode if given a size of 0 */
          psts =
            xrealloc (psts,
                      (rows - rm_cnt + 1) * cols * sizeof (struct matrix_data));
          GMatrixEditSet (pstk, psts, rows - rm_cnt, false);
        }
    }
  return true;
}

static int
PSTKD_Ok (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      PSTKernDlg *pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      GGadget *pstk = GWidgetGetControl (pstkd->gw, CID_PSTList);
      int rows, cols = GMatrixEditGetColCnt (pstk);
      struct matrix_data *psts = GMatrixEditGet (pstk, &rows);
      int r, r1, k, gid, isv, ch;
      char *pt, *start;
      int lookup_type = pstkd->sub->lookup->lookup_type;
      SplineFont *sf = NULL;
      SplineChar *sc, *found;
      char *buts[3];
      KernPair *kp, *kpprev, *kpnext;
      PST *pst, *pstprev, *pstnext;
      int err, touch = 0, separation = 0, minkern = 0, onlyCloser =
        0, autokern = 0;
      int _t =
        lookup_type == gpos_single ? pst_position : lookup_type ==
        gpos_pair ? pst_pair : lookup_type ==
        gsub_single ? pst_substitution : lookup_type ==
        gsub_alternate ? pst_alternate : lookup_type ==
        gsub_multiple ? pst_multiple : pst_ligature;

      /* First check for errors */
      if (lookup_type == gpos_pair)
        {
          /* bad metadata */
          err = false;
          touch = GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Touched));
          separation =
            GetInt8 (pstkd->gw, CID_Separation, _("Separation"), &err);
          minkern = GetInt8 (pstkd->gw, CID_MinKern, _("Min Kern"), &err);
          onlyCloser =
            GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_OnlyCloser));
          autokern =
            GGadgetIsChecked (GWidgetGetControl (pstkd->gw, CID_Autokern));
          if (err)
            return true;
        }

      /* Glyph names that aren't in the font */
      for (r = 0; r < rows; ++r)
        {
          if (SFGetChar (pstkd->sf, -1, psts[r * cols + 0].u.md_str) == NULL)
            {
              ff_post_error (_("Missing glyph"),
                             _("There is no glyph named %s in the font"),
                             psts[cols * r + 0].u.md_str);
              return true;
            }
        }
      /* Empty entries */
      if (cols == 2 || cols == 10)
        {
          for (r = 0; r < rows; ++r)
            {
              start = psts[cols * r + 1].u.md_str;
              if (start == NULL)
                start = "";
              while (*start == ' ')
                ++start;
              if (*start == '\0')
                {
                  ff_post_error (_("Missing glyph name"),
                                 _
                                 ("You must specify a replacement glyph for %s"),
                                 psts[cols * r + 0].u.md_str);
                  return true;
                }
              /* Replacements which aren't in the font */
              while (*start)
                {
                  for (pt = start; *pt != '\0' && *pt != ' ' && *pt != '(';
                       ++pt);
                  ch = *pt;
                  *pt = '\0';
                  found = SFGetChar (pstkd->sf, -1, start);
                  if (found == NULL)
                    {
                      buts[0] = _("_Yes");
                      buts[1] = _("_Cancel");
                      buts[2] = NULL;
                      if (gwwv_ask
                          (_("Missing glyph"), (const char **) buts, 0, 1,
                           _
                           ("For glyph %.60s you refer to a glyph named %.80s, which is not in the font yet. Was this intentional?"),
                           psts[cols * r + 0].u.md_str, start) == 1)
                        {
                          *pt = ch;
                          return true;
                        }
                    }
                  *pt = ch;
                  if (ch == '(')
                    {
                      while (*pt != ')' && *pt != '\0')
                        ++pt;
                      if (*pt == ')')
                        ++pt;
                    }
                  while (*pt == ' ')
                    ++pt;
                  start = pt;
                }
            }
        }
      /* Duplicate entries */
      for (r = 0; r < rows; ++r)
        {
          for (r1 = r + 1; r1 < rows; ++r1)
            {
              if (strcmp
                  (psts[r * cols + 0].u.md_str,
                   psts[r1 * cols + 0].u.md_str) == 0)
                {
                  if (lookup_type == gpos_pair || lookup_type == gsub_ligature)
                    {
                      if (strcmp
                          (psts[r * cols + 1].u.md_str,
                           psts[r1 * cols + 1].u.md_str) == 0)
                        {
                          ff_post_error (_("Duplicate data"),
                                         _
                                         ("There are two entries for the same glyph set (%.80s and %.80s)"),
                                         psts[cols * r + 0].u.md_str,
                                         psts[cols * r + 1].u.md_str);
                          return true;
                        }
                    }
                  else
                    {
                      ff_post_error (_("Duplicate data"),
                                     _
                                     ("There are two entries for the same glyph (%.80s)"),
                                     psts[cols * r + 0].u.md_str);
                      return true;
                    }
                }
            }
        }

      /* Check for badly specified device tables */
      if (_t == pst_position || _t == pst_pair)
        {
          int startc = _t == pst_position ? SIM_DX + 1 : PAIR_DX1 + 1;
          int low, high, c;
          for (r = 0; r < rows; ++r)
            {
              for (c = startc; c < cols; c += 2)
                {
                  if (!DeviceTableOK (psts[r * cols + c].u.md_str, &low, &high))
                    {
                      ff_post_error (_("Bad Device Table Adjustment"),
                                     _
                                     ("A device table adjustment specified for %.80s is invalid"),
                                     psts[cols * r + 0].u.md_str);
                      return true;
                    }
                }
            }
        }

      /* Ok, if we get here then there should be no errors and we can parse */
      /* First mark all the current things as unused */
      k = 0;
      do
        {
          sf = pstkd->sf->subfontcnt == 0 ? pstkd->sf : pstkd->sf->subfonts[k];
          for (gid = 0; gid < sf->glyphcnt; ++gid)
            if ((sc = sf->glyphs[gid]) != NULL)
              {
                for (pst = sc->possub; pst != NULL; pst = pst->next)
                  pst->ticked = false;
                for (isv = 0; isv < 2; ++isv)
                  for (kp = isv ? sc->vkerns : sc->kerns; kp != NULL;
                       kp = kp->next)
                    kp->kcid = 0;
              }
          ++k;
        }
      while (k < pstkd->sf->subfontcnt);

      if (lookup_type != gpos_pair)
        {
          for (r = 0; r < rows; ++r)
            {
              sc = SFGetChar (pstkd->sf, -1, psts[cols * r + 0].u.md_str);
              for (pst = sc->possub; pst != NULL; pst = pst->next)
                {
                  if (pst->subtable == pstkd->sub && !pst->ticked)
                    break;
                }
              if (pst == NULL)
                {
                  pst = xzalloc (sizeof (PST));
                  pst->type = _t;
                  pst->subtable = pstkd->sub;
                  pst->next = sc->possub;
                  sc->possub = pst;
                }
              else if (lookup_type != gpos_single)
                free (pst->u.subs.variant);
              pst->ticked = true;
              if (lookup_type == gpos_single)
                {
                  VRDevTabParse (&pst->u.pos, &psts[cols * r + SIM_DX + 1]);
                  pst->u.pos.xoff = psts[cols * r + SIM_DX].u.md_ival;
                  pst->u.pos.yoff = psts[cols * r + SIM_DY].u.md_ival;
                  pst->u.pos.h_adv_off = psts[cols * r + SIM_DX_ADV].u.md_ival;
                  pst->u.pos.v_adv_off = psts[cols * r + SIM_DY_ADV].u.md_ival;
                }
              else
                {
                  pst->u.subs.variant = xstrdup (psts[cols * r + 1].u.md_str);
                  if (lookup_type == gsub_ligature)
                    pst->u.lig.lig = sc;
                }
            }
        }
      else if (lookup_type == gpos_pair)
        {
          for (r = 0; r < rows; ++r)
            {
              sc = SFGetChar (pstkd->sf, -1, psts[cols * r + 0].u.md_str);
              KpMDParse (sc, pstkd->sub, psts, rows, cols, r);
            }
        }

      /* Now free anything with this subtable which did not get ticked */
      k = 0;
      do
        {
          sf = pstkd->sf->subfontcnt == 0 ? pstkd->sf : pstkd->sf->subfonts[k];
          for (gid = 0; gid < sf->glyphcnt; ++gid)
            if ((sc = sf->glyphs[gid]) != NULL)
              {
                for (pstprev = NULL, pst = sc->possub; pst != NULL;
                     pst = pstnext)
                  {
                    pstnext = pst->next;
                    if (pst->ticked || pst->subtable != pstkd->sub)
                      pstprev = pst;
                    else
                      {
                        if (pstprev == NULL)
                          sc->possub = pstnext;
                        else
                          pstprev->next = pstnext;
                        pst->next = NULL;
                        PSTFree (pst);
                      }
                  }
                for (isv = 0; isv < 2; ++isv)
                  {
                    for (kpprev = NULL, kp = isv ? sc->vkerns : sc->kerns;
                         kp != NULL; kp = kpnext)
                      {
                        kpnext = kp->next;
                        if (kp->kcid != 0 || kp->subtable != pstkd->sub)
                          kpprev = kp;
                        else
                          {
                            if (kpprev != NULL)
                              kpprev->next = kpnext;
                            else if (isv)
                              sc->vkerns = kpnext;
                            else
                              sc->kerns = kpnext;
                            kp->next = NULL;
                            KernPairsFree (kp);
                          }
                      }
                  }
              }
          ++k;
        }
      while (k < pstkd->sf->subfontcnt);

      /* The field we use to tick kern pairs must be reset to false */
      k = 0;
      do
        {
          for (gid = 0; gid < sf->glyphcnt; ++gid)
            if ((sc = sf->glyphs[gid]) != NULL)
              {
                for (isv = 0; isv < 2; ++isv)
                  {
                    for (kp = isv ? sc->vkerns : sc->kerns; kp != NULL;
                         kp = kp->next)
                      {
                        kp->kcid = false;
                      }
                  }
              }
          ++k;
        }
      while (k < pstkd->sf->subfontcnt);
      PSTKD_SetSuffix (pstkd);
      if (lookup_type == gpos_pair)
        {
          pstkd->sub->separation = separation;
          pstkd->sub->minkern = minkern;
          pstkd->sub->kerning_by_touch = touch;
          pstkd->sub->onlyCloser = onlyCloser;
          pstkd->sub->dontautokern = !autokern;
        }
      pstkd->done = true;
    }
  return true;
}

static void
PSTKD_DoCancel (PSTKernDlg * pstkd)
{
  pstkd->done = true;
}

static int
PSTKD_Cancel (GGadget *g, GEvent *e)
{
  PSTKernDlg *pstkd;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      pstkd = GDrawGetUserData (GGadgetGetWindow (g));
      PSTKD_DoCancel (pstkd);
    }
  return true;
}

uint32_t **
SFGlyphNameCompletion (SplineFont *sf, GGadget *t, int from_tab,
                       int new_name_after_space)
{
  uint32_t *pt, *spt, *basept, *wild;
  uint32_t **ret;
  int gid, cnt, doit, match_len;
  SplineChar *sc;
  int do_wildcards;

  pt = spt = basept = (uint32_t *) _GGadgetGetTitle (t);
  if (pt == NULL || *pt == '\0')
    return NULL;
  if (new_name_after_space)
    {
      if ((spt = u32_strrchr (spt, ' ')) == NULL)
        spt = basept;
      else
        {
          pt = ++spt;
          if (*pt == '\0')
            return NULL;
        }
    }
  while (*pt && *pt != '*' && *pt != '?' && *pt != '[' && *pt != '{')
    ++pt;
  do_wildcards = *pt != '\0';

  if ((!do_wildcards && pt - spt == 1 && (*spt >= 0x10000 || !isalpha (*spt)))
      || (!from_tab && do_wildcards && pt - spt == 2 && spt[1] == ' '))
    {
      sc = SFGetChar (sf, *spt, NULL);
      /* One unicode character which isn't a glyph name (so not "A") and */
      /*  isn't a wildcard (so not "*") gets expanded to its glyph name. */
      /*  (so "," becomes "comma(,)" */
      /* Or, a single wildcard followed by a space gets expanded to glyph name */
      if (sc != NULL)
        {
          ret = xmalloc ((2) * sizeof (uint32_t *));
          ret[0] = x_u8_to_u32 (sc->name);
          ret[1] = NULL;
          return ret;
        }
    }
  if (do_wildcards && !from_tab)
    return NULL;

  wild = NULL;
  if (do_wildcards)
    {
      pt = spt;
      wild = xmalloc ((u32_strlen (spt) + 2) * sizeof (uint32_t));
      u32_strcpy (wild, pt);
      u32_strcat (wild, x_gc_u8_to_u32 ("*"));
    }

  match_len = u32_strlen (spt);
  ret = NULL;
  for (doit = 0; doit < 2; ++doit)
    {
      cnt = 0;
      for (gid = 0; gid < sf->glyphcnt; ++gid)
        if ((sc = sf->glyphs[gid]) != NULL)
          {
            int matched;
            if (do_wildcards)
              {
                uint32_t *temp = utf82u_copy (sc->name);
                matched = GGadgetWildMatch ((uint32_t *) wild, temp, false);
                free (temp);
              }
            else
              matched =
                (u8_strncmp
                 (x_gc_u32_to_u8 (u32_force_valid (spt)),
                  u8_force_valid (sc->name), match_len) == 0);
            if (matched)
              {
                if (doit)
                  {
                    if (spt == basept)
                      {
                        ret[cnt] = x_u8_to_u32 (sc->name);
                      }
                    else
                      {
                        uint32_t *temp =
                          xmalloc ((spt - basept + strlen (sc->name) +
                                    4) * sizeof (uint32_t));
                        int len;
                        u32_strncpy (temp, basept, spt - basept);
                        utf82u_strcpy (temp + (spt - basept), sc->name);
                        len = u32_strlen (temp);
                        ret[cnt] = temp;
                      }
                  }
                ++cnt;
              }
          }
      if (doit)
        ret[cnt] = NULL;
      else if (cnt == 0)
        break;
      else
        ret = xmalloc ((cnt + 1) * sizeof (uint32_t *));
    }
  free (wild);
  return ret;
}

static uint32_t **
PSTKD_GlyphNameCompletion (GGadget *t, int from_tab)
{
  PSTKernDlg *pstkd =
    GDrawGetUserData (GDrawGetParentWindow (GGadgetGetWindow (t)));
  SplineFont *sf = pstkd->sf;

  return SFGlyphNameCompletion (sf, t, from_tab, false);
}

static uint32_t **
PSTKD_GlyphListCompletion (GGadget *t, int from_tab)
{
  PSTKernDlg *pstkd =
    GDrawGetUserData (GDrawGetParentWindow (GGadgetGetWindow (t)));
  SplineFont *sf = pstkd->sf;

  return SFGlyphNameCompletion (sf, t, from_tab, true);
}

static int
pstkd_e_h (GWindow gw, GEvent *event)
{
  PSTKernDlg *pstkd = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_close:
      PSTKD_DoCancel (pstkd);
      break;
    case et_char:
      if (event->u.chr.keysym == GK_F1 || event->u.chr.keysym == GK_Help)
        {
          int lookup_type = pstkd->sub->lookup->lookup_type;
          if (lookup_type == gpos_single)
            help ("lookups.html#Single-pos");
          else if (lookup_type == gpos_pair)
            help ("lookups.html#Pair");
          else
            help ("lookups.html#basic-subs");
          return true;
        }
      return false;
      break;
    case et_destroy:
      break;
    case et_mouseup:
    case et_mousemove:
    case et_mousedown:
      break;
    case et_expose:
      break;
    case et_resize:
      break;
    }
  return true;
}

static void
PSTKernD (SplineFont *sf, struct lookup_subtable *sub, int def_layer)
{
  PSTKernDlg pstkd;
  GRect pos;
  GWindowAttrs wattrs;
  char title[300];
  struct matrixinit mi;
  GGadgetCreateData gcd[23], buttongcd[6], box[6], hbox;
  GGadgetCreateData *h1array[8], *h2array[7], *h3array[7], *varray[20],
    *h4array[8], *h5array[4];
  GTextInfo label[23], buttonlabel[6];
  int i, k, mi_pos, mi_k;
  enum otlookup_type lookup_type = sub->lookup->lookup_type;
  char sepbuf[40], mkbuf[40];

  if (sub->separation == 0 && !sub->kerning_by_touch)
    {
      sub->separation = sf->width_separation;
      if (sf->width_separation == 0)
        sub->separation = 15 * (sf->ascent + sf->descent) / 100;
      sub->minkern = sub->separation / 10;
    }

  memset (&pstkd, 0, sizeof (pstkd));
  pstkd.sf = sf;
  pstkd.def_layer = def_layer;
  pstkd.sub = sub;

  memset (&wattrs, 0, sizeof (wattrs));
  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = true;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  snprintf (title, sizeof (title), _("Lookup Subtable, %s"),
            sub->subtable_name);
  wattrs.utf8_window_title = title;
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = GGadgetScale (GDrawPointsToPixels (NULL, 300));
  pos.height = GDrawPointsToPixels (NULL, 400);
  pstkd.gw = GDrawCreateTopWindow (NULL, &pos, pstkd_e_h, &pstkd, &wattrs);

  memset (&gcd, 0, sizeof (gcd));
  memset (&buttongcd, 0, sizeof (buttongcd));
  memset (&box, 0, sizeof (box));
  memset (&label, 0, sizeof (label));
  memset (&buttonlabel, 0, sizeof (buttonlabel));

  i = k = 0;
  label[i].text = (uint32_t *) _("_Alphabetic");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags =
    isalphabetic ? (gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup)
    : (gg_enabled | gg_visible | gg_utf8_popup);
  gcd[i].gd.popup_msg =
    (uint32_t *)
    _("Sort this display based on the alphabetic name of the glyph");
  gcd[i].gd.handle_controlevent = PSTKD_Sort;
  gcd[i].gd.cid = CID_Alpha;
  gcd[i].creator = GRadioCreate;
  h1array[0] = &gcd[i++];

  label[i].text = (uint32_t *) _("_Unicode");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags =
    !isalphabetic ? (gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup)
    : (gg_enabled | gg_visible | gg_utf8_popup);
  gcd[i].gd.popup_msg =
    (uint32_t *) _("Sort this display based on the unicode code of the glyph");
  gcd[i].gd.handle_controlevent = PSTKD_Sort;
  gcd[i].gd.cid = CID_Unicode;
  gcd[i].creator = GRadioCreate;
  h1array[1] = &gcd[i++];
  h1array[2] = GCD_HPad10;

  label[i].text = (uint32_t *) _("_By Base Char");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags =
    stemming ? (gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup)
    : (gg_enabled | gg_visible | gg_utf8_popup);
  gcd[i].gd.popup_msg =
    (uint32_t *)
    _
    ("Sort first using the base glyph (if any).\nThus Agrave would sort with A");
  gcd[i].gd.handle_controlevent = PSTKD_Sort;
  gcd[i].gd.cid = CID_BaseChar;
  gcd[i].creator = GCheckBoxCreate;
  h1array[3] = &gcd[i++];
  h1array[4] = GCD_HPad10;

  label[i].text = (uint32_t *) _("By _Scripts");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags =
    byscripts ? (gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup)
    : (gg_enabled | gg_visible | gg_utf8_popup);
  gcd[i].gd.popup_msg =
    (uint32_t *)
    _
    ("Sort first using the glyph's script.\nThus A and Z would sort together\nwhile Alpha would sort with Omega and not A");
  if (sub->lookup->features == NULL
      || (sub->lookup->features->next == NULL
          && (sub->lookup->features->scripts == NULL
              || sub->lookup->features->scripts->next == NULL)))
    gcd[i].gd.flags = gg_visible | gg_cb_on;    /* If there is only one script, we can't really sort by it */
  gcd[i].gd.handle_controlevent = PSTKD_Sort;
  gcd[i].gd.cid = CID_Scripts;
  gcd[i].creator = GCheckBoxCreate;
  h1array[5] = &gcd[i++];
  h1array[6] = GCD_Glue;
  h1array[7] = NULL;

  box[0].gd.flags = gg_enabled | gg_visible;
  box[0].gd.u.boxelements = h1array;
  box[0].creator = GHBoxCreate;
  varray[k++] = &box[0];
  varray[k++] = NULL;

  if (sub->lookup->lookup_type == gpos_pair
      || sub->lookup->lookup_type == gpos_single)
    {
      label[i].text = (uint32_t *) _("_Hide Unused Columns");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags =
        lookup_hideunused ? (gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup)
        : (gg_enabled | gg_visible | gg_utf8_popup);
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _
        ("Don't display columns of 0s.\nThe OpenType lookup allows for up to 8 kinds\nof data, but almost all lookups will use just one or two.\nOmitting the others makes the behavior clearer.");
      gcd[i].gd.handle_controlevent = PSTKD_HideUnused;
      gcd[i].creator = GCheckBoxCreate;
      varray[k++] = &gcd[i++];
      varray[k++] = NULL;
    }

  PSTMatrixInit (&mi, sf, sub, &pstkd);
  mi_pos = i;
  gcd[i].gd.pos.height = 200;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.cid = CID_PSTList;
  gcd[i].gd.u.matrix = &mi;
  gcd[i].data = &pstkd;
  gcd[i].creator = GMatrixEditCreate;
  mi_k = k;
  varray[k++] = &gcd[i++];
  varray[k++] = NULL;

  buttonlabel[0].text = (uint32_t *) _("_Populate");
  if (lookup_type == gpos_pair)
    buttonlabel[0].text = (uint32_t *) _("Auto_Kern");
  buttonlabel[0].text_is_1byte = true;
  buttonlabel[0].text_has_mnemonic = true;
  buttongcd[0].gd.label = &buttonlabel[0];
  buttongcd[0].gd.pos.x = 5;
  buttongcd[0].gd.pos.y = 5 + 4;
  buttongcd[0].gd.flags = sub->lookup->features == NULL
    || sub->vertical_kerning ? gg_visible | gg_utf8_popup : gg_enabled |
    gg_visible | gg_utf8_popup;
  if (lookup_type == gpos_pair)
    {
      buttongcd[0].gd.popup_msg =
        (uint32_t *)
        _("For each script to which this lookup applies, look at all pairs of\n"
          "glyphs in that script and try to guess a reasonable kerning value\n"
          "for that pair.");
      buttongcd[0].gd.handle_controlevent = PSTKD_AutoKern;
    }
  else
    {
      buttongcd[0].gd.popup_msg =
        (uint32_t *)
        _
        ("Add entries for all glyphs in the scripts to which this lookup applies.\nWhen FontForge can find a default value it will add that too.");
      buttongcd[0].gd.handle_controlevent = PSTKD_Populate;
    }
  buttongcd[0].creator = GButtonCreate;

  buttonlabel[1].text = (uint32_t *) _("_Add Selected");
  if (lookup_type == gpos_pair)
    buttonlabel[1].text = (uint32_t *) _("_AutoKern Selected");
  buttonlabel[1].text_is_1byte = true;
  buttonlabel[1].text_has_mnemonic = true;
  buttongcd[1].gd.label = &buttonlabel[1];
  buttongcd[1].gd.pos.x = 5;
  buttongcd[1].gd.pos.y = 5 + 4;
  buttongcd[1].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  if (lookup_type == gpos_pair)
    {
      if (sub->vertical_kerning)
        buttongcd[1].gd.flags = gg_visible | gg_utf8_popup;
      buttongcd[1].gd.popup_msg =
        (uint32_t *) _("Add kerning info between all pairs of selected glyphs");
      buttongcd[1].gd.handle_controlevent = PSTKD_AutoKernSelected;
    }
  else
    {
      buttongcd[1].gd.popup_msg =
        (uint32_t *) _("Add entries for all selected glyphs.");
      buttongcd[1].gd.handle_controlevent = PSTKD_PopulateSelected;
    }
  buttongcd[1].creator = GButtonCreate;

  buttonlabel[2].text = (uint32_t *) _("_Remove Empty");
  buttonlabel[2].text_is_1byte = true;
  buttonlabel[2].text_has_mnemonic = true;
  buttongcd[2].gd.label = &buttonlabel[2];
  buttongcd[2].gd.pos.x = 5;
  buttongcd[2].gd.pos.y = 5 + 4;
  buttongcd[2].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  buttongcd[2].gd.popup_msg =
    (uint32_t *) (sub->lookup->lookup_type ==
                  gpos_single ?
                  _
                  ("Remove all \"empty\" entries -- those where all fields are 0")
                  : sub->lookup->lookup_type ==
                  gpos_pair ?
                  _
                  ("Remove all \"empty\" entries -- entries with no second glyph")
                  : sub->lookup->lookup_type ==
                  gsub_ligature ?
                  _
                  ("Remove all \"empty\" entries -- those with no source glyphs")
                  :
                  _
                  ("Remove all \"empty\" entries -- those with no replacement glyphs"));
  buttongcd[2].gd.handle_controlevent = PSTKD_RemoveEmpty;
  buttongcd[2].creator = GButtonCreate;

  buttonlabel[3].text = (uint32_t *) _("Remove All");
  buttonlabel[3].text_is_1byte = true;
  buttonlabel[3].text_has_mnemonic = true;
  buttongcd[3].gd.label = &buttonlabel[3];
  buttongcd[3].gd.pos.x = 5;
  buttongcd[3].gd.pos.y = 5 + 4;
  buttongcd[3].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  buttongcd[3].gd.popup_msg = (uint32_t *) _("Remove all entries.");
  buttongcd[3].gd.handle_controlevent = PSTKD_RemoveAll;
  buttongcd[3].creator = GButtonCreate;

  if (sub->lookup->lookup_type == gsub_single)
    {
      label[i].text = (uint32_t *) _("_Default Using Suffix:");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _("Add entries to the lookup based on the following suffix.\n"
          "So if the suffix is set to \"superior\" and the font\n"
          "contains glyphs named \"A\" and \"A.superior\" (and the\n"
          "lookup applies to the latin script), then FontForge will\n"
          "add an entry mapping \"A\" -> \"A.superior\".");
      gcd[i].gd.handle_controlevent = PSTKD_PopulateWithSuffix;
      gcd[i].creator = GButtonCreate;
      h2array[0] = &gcd[i++];

      label[i].text = (uint32_t *) sub->suffix;
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = sub->suffix == NULL ? NULL : &label[i];
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
      gcd[i].gd.cid = CID_Suffix;
      gcd[i].creator = GTextFieldCreate;
      h2array[1] = &gcd[i++];
      h2array[2] = GCD_Glue;
      h2array[3] = NULL;

      box[1].gd.flags = gg_enabled | gg_visible;
      box[1].gd.u.boxelements = h2array;
      box[1].creator = GHBoxCreate;
      varray[k++] = &box[1];
      varray[k++] = NULL;
    }
  else if (sub->lookup->lookup_type == gpos_single)
    {
      label[i].text = (uint32_t *) _("_Default New Entries to First");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_cb_on | gg_utf8_popup;
      if (is_boundsFeat (sub) != 0)
        gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _
        ("When adding new entries, give them the same\ndelta values as those on the first line.");
      gcd[i].gd.cid = CID_AllSame;
      gcd[i].creator = GCheckBoxCreate;
      varray[k++] = &gcd[i++];
      varray[k++] = NULL;

    }
  else if (sub->lookup->lookup_type == gpos_pair)
    {
      label[i].text = (uint32_t *) _("_Default Separation:");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg =
        (uint32_t *) _("Add entries to the lookup trying to make the optical\n"
                       "separation between all pairs of glyphs equal to this\n"
                       "value.");
      gcd[i].creator = GLabelCreate;
      h4array[0] = &gcd[i++];

      sprintf (sepbuf, "%d", sub->separation);
      label[i].text = (uint32_t *) sepbuf;
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.width = 50;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
      gcd[i].gd.cid = CID_Separation;
      gcd[i].creator = GTextFieldCreate;
      h4array[1] = &gcd[i++];

      label[i].text = (uint32_t *) _("_Min Kern:");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _("Any computed kerning change whose absolute value is less\n"
          "that this will be ignored.\n");
      gcd[i].creator = GLabelCreate;
      h4array[2] = &gcd[i++];

      sprintf (mkbuf, "%d", sub->minkern);
      label[i].text = (uint32_t *) mkbuf;
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.width = 50;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
      gcd[i].gd.cid = CID_MinKern;
      gcd[i].creator = GTextFieldCreate;
      h4array[3] = &gcd[i++];

      label[i].text = (uint32_t *) _("_Touching");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 5;
      gcd[i].gd.pos.y = 5 + 4;
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      if (sub->kerning_by_touch)
        gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _("Normally kerning is based on achieving a constant (optical)\n"
          "separation between glyphs, but occasionally it is desirable\n"
          "to have a kerning table where the kerning is based on the\n"
          "closest approach between two glyphs (So if the desired separ-\n"
          "ation is 0 then the glyphs will actually be touching.");
      gcd[i].gd.cid = CID_Touched;
      gcd[i].creator = GCheckBoxCreate;
      h4array[4] = &gcd[i++];

      h4array[5] = GCD_Glue;
      h4array[6] = NULL;

      box[1].gd.flags = gg_enabled | gg_visible;
      box[1].gd.u.boxelements = h4array;
      box[1].creator = GHBoxCreate;
      varray[k++] = &box[1];
      varray[k++] = NULL;

      label[i].text = (uint32_t *) _("Only kern glyphs closer");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      if (sub->onlyCloser)
        gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _("When doing autokerning, only move glyphs closer together,\n"
          "so the kerning offset will be negative.");
      gcd[i].gd.cid = CID_OnlyCloser;
      gcd[i].creator = GCheckBoxCreate;
      h5array[0] = &gcd[i++];

      label[i].text = (uint32_t *) _("Autokern new entries");
      label[i].text_is_1byte = true;
      label[i].text_has_mnemonic = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
      if (!sub->dontautokern)
        gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
      gcd[i].gd.popup_msg =
        (uint32_t *)
        _("When adding new entries provide default kerning values.");
      gcd[i].gd.cid = CID_Autokern;
      gcd[i].creator = GCheckBoxCreate;
      h5array[1] = &gcd[i++];
      h5array[2] = NULL;

      memset (&hbox, 0, sizeof (hbox));
      hbox.gd.flags = gg_enabled | gg_visible;
      hbox.gd.u.boxelements = h5array;
      hbox.creator = GHBoxCreate;
      varray[k++] = &hbox;
      varray[k++] = NULL;

      label[i].text = (uint32_t *) _("Size:");
      label[i].text_is_1byte = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 30;
      gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 30;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      gcd[i++].creator = GLabelCreate;
      h2array[0] = &gcd[i - 1];

      pstkd.pixelsize = 150;
      label[i].text = (uint32_t *) "150";
      label[i].text_is_1byte = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 92;
      gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y - 4;
      gcd[i].gd.pos.width = 80;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      gcd[i].gd.cid = CID_PixelSize;
      gcd[i].gd.handle_controlevent = PSTKD_DisplaySizeChanged;
      gcd[i++].creator = GTextFieldCreate;
      h2array[1] = &gcd[i - 1];
      h2array[2] = GCD_HPad10;

/* TRANSLATORS: Short for "Magnification" */
      label[i].text = (uint32_t *) _("Mag:");
      label[i].text_is_1byte = true;
      gcd[i].gd.label = &label[i];
      gcd[i].gd.pos.x = 185;
      gcd[i].gd.pos.y = gcd[i - 2].gd.pos.y;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      gcd[i++].creator = GLabelCreate;
      h2array[3] = &gcd[i - 1];

      pstkd.mag = 1;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      gcd[i].gd.cid = CID_Magnification;
      gcd[i].gd.u.list = magnifications;
      gcd[i].gd.handle_controlevent = PSTKD_MagnificationChanged;
      gcd[i++].creator = GListButtonCreate;
      h2array[4] = &gcd[i - 1];
      h2array[5] = GCD_Glue;
      h2array[6] = NULL;

      box[2].gd.flags = gg_enabled | gg_visible;
      box[2].gd.u.boxelements = h2array;
      box[2].creator = GHBoxCreate;
      varray[k++] = &box[2];
      varray[k++] = NULL;

      gcd[i].gd.pos.width = 200;
      gcd[i].gd.pos.height = 200;
      gcd[i].gd.flags = gg_visible | gg_enabled;
      gcd[i].gd.u.drawable_e_h = pstkern_e_h;
      gcd[i].gd.cid = CID_KernDisplay;
      gcd[i].creator = GDrawableCreate;
      varray[k++] = &gcd[i++];
      varray[k++] = NULL;
    }

  gcd[i].gd.pos.x = 10;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 24 + 3;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[i].text = (uint32_t *) _("_OK");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = PSTKD_Ok;
  gcd[i++].creator = GButtonCreate;

  gcd[i].gd.pos.x = -10;
  gcd[i].gd.pos.y = gcd[i - 1].gd.pos.y + 3;
  gcd[i].gd.pos.width = -1;
  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[i].text = (uint32_t *) _("_Cancel");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = PSTKD_Cancel;
  gcd[i].gd.cid = CID_Cancel;
  gcd[i++].creator = GButtonCreate;

  h3array[0] = GCD_Glue;
  h3array[1] = &gcd[i - 2];
  h3array[2] = GCD_Glue;
  h3array[3] = GCD_Glue;
  h3array[4] = &gcd[i - 1];
  h3array[5] = GCD_Glue;
  h3array[6] = NULL;

  box[3].gd.flags = gg_enabled | gg_visible;
  box[3].gd.u.boxelements = h3array;
  box[3].creator = GHBoxCreate;
  varray[k++] = &box[3];
  varray[k++] = NULL;
  varray[k++] = NULL;

  box[4].gd.pos.x = box[4].gd.pos.y = 2;
  box[4].gd.flags = gg_enabled | gg_visible;
  box[4].gd.u.boxelements = varray;
  box[4].creator = GHVGroupCreate;

  GGadgetsCreate (pstkd.gw, box + 4);
  GHVBoxSetExpandableRow (box[4].ret, mi_k / 2);
  GHVBoxSetExpandableCol (box[3].ret, gb_expandgluesame);
  if (sub->lookup->lookup_type == gsub_single)
    GHVBoxSetExpandableCol (box[1].ret, gb_expandglue);
  else if (sub->lookup->lookup_type == gpos_pair)
    {
      GHVBoxSetExpandableCol (box[1].ret, gb_expandglue);
      GHVBoxSetExpandableCol (box[2].ret, gb_expandglue);
    }
  GHVBoxSetExpandableCol (box[0].ret, gb_expandglue);
  GMatrixEditAddButtons (gcd[mi_pos].ret, buttongcd);
  GMatrixEditSetColumnCompletion (gcd[mi_pos].ret, 0,
                                  PSTKD_GlyphNameCompletion);
  if (sub->lookup->lookup_type == gsub_single
      || sub->lookup->lookup_type == gpos_pair)
    GMatrixEditSetColumnCompletion (gcd[mi_pos].ret, 1,
                                    PSTKD_GlyphNameCompletion);
  else if (sub->lookup->lookup_type == gsub_multiple
           || sub->lookup->lookup_type == gsub_alternate
           || sub->lookup->lookup_type == gsub_ligature)
    GMatrixEditSetColumnCompletion (gcd[mi_pos].ret, 1,
                                    PSTKD_GlyphListCompletion);

  if (sub->lookup->lookup_type == gpos_pair)
    GMatrixEditSetTextChangeReporter (gcd[mi_pos].ret, PSTKD_METextChanged);
  else
    GMatrixEditSetMouseMoveReporter (gcd[mi_pos].ret, PST_PopupPrepare);
  if (sub->lookup->lookup_type == gpos_pair
      || sub->lookup->lookup_type == gpos_single)
    PSTKD_DoHideUnused (&pstkd);
  else
    GHVBoxFitWindow (box[4].ret);

  GDrawSetVisible (pstkd.gw, true);

  while (!pstkd.done)
    GDrawProcessOneEvent (NULL);
  GDrawDestroyWindow (pstkd.gw);
  if (pstkd.display != NULL)
    {
      BDFFontFree (pstkd.display);
      pstkd.display = NULL;
    }
}

/* ************************************************************************** */
/* *************************** Subtable Selection *************************** */
/* ************************************************************************** */

static int
SubtableNameInUse (char *subname, SplineFont *sf,
                   struct lookup_subtable *exclude)
{
  int isgpos, i, j;
  OTLookup *otl;
  struct lookup_subtable *sub;

  if (sf->fontinfo != NULL)
    {
      for (isgpos = 0; isgpos < 2; ++isgpos)
        {
          struct lkdata *lk = &sf->fontinfo->tables[isgpos];
          for (i = 0; i < lk->cnt; ++i)
            {
              if (lk->all[i].deleted)
                continue;
              for (j = 0; j < lk->all[i].subtable_cnt; ++j)
                {
                  if (lk->all[i].subtables[j].deleted
                      || lk->all[i].subtables[j].subtable == exclude)
                    continue;
                  if (strcmp
                      (lk->all[i].subtables[j].subtable->subtable_name,
                       subname) == 0)
                    return true;
                }
            }
        }
    }
  else
    {
      for (isgpos = 0; isgpos < 2; ++isgpos)
        {
          for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
               otl = otl->next)
            {
              for (sub = otl->subtables; sub != NULL; sub = sub->next)
                {
                  if (sub == exclude)
                    continue;
                  if (strcmp (sub->subtable_name, subname) == 0)
                    return true;
                }
            }
        }
    }
  return false;
}

int
EditSubtable (struct lookup_subtable *sub, int isgpos, SplineFont *sf,
              struct subtable_data *sd, int def_layer)
{
  char *def = sub->subtable_name;
  int new = def == NULL;
  char *freeme = NULL;
  int name_search;

  if (new)
    {
      def = freeme = xmalloc (strlen (sub->lookup->lookup_name) + 10);
      name_search = 1;
      do
        {
          sprintf (def, "%s-%d", sub->lookup->lookup_name, name_search++);
        }
      while (SubtableNameInUse (def, sf, sub));
    }
  while (true)
    {
      def =
        gwwv_ask_string (_("Please name this subtable"), def,
                         _("Please name this subtable"));
      free (freeme);
      if (def == NULL)
        return false;
      freeme = def;
      if (SubtableNameInUse (def, sf, sub))
        ff_post_notice (_("Duplicate name"),
                        _
                        ("There is already a subtable with that name, please pick another."));
      else
        break;
    }
  free (sub->subtable_name);
  sub->subtable_name = def;
  if (new && sub->lookup->lookup_type == gsub_single)
    sub->suffix = SuffixFromTags (sub->lookup->features);
  if (new && (sd == NULL || !(sd->flags & sdf_dontedit)))
    _LookupSubtableContents (sf, sub, sd, def_layer);
  return true;
}

static struct lookup_subtable *
NewSubtable (OTLookup *otl, int isgpos, SplineFont *sf,
             struct subtable_data *sd, int def_layer)
{
  struct lookup_subtable *sub, *last;
  int i, j;

  sub = xzalloc (sizeof (struct lookup_subtable));
  sub->lookup = otl;
  sub->separation = 15 * (sf->ascent + sf->descent) / 100;
  sub->minkern = sub->separation / 10;
  if (!EditSubtable (sub, isgpos, sf, sd, def_layer))
    {
      free (sub);
      return NULL;
    }
  if (otl->subtables == NULL)
    otl->subtables = sub;
  else
    {
      for (last = otl->subtables; last->next != NULL; last = last->next);
      last->next = sub;
    }
  if (sf->fontinfo != NULL)
    {
      struct lkdata *lk = &sf->fontinfo->tables[isgpos];
      for (i = 0; i < lk->cnt && lk->all[i].lookup != otl; ++i);
      if (i == lk->cnt)
        {
          IError ("Lookup missing from FontInfo lookup list");
        }
      else
        {
          if (lk->all[i].subtable_cnt >= lk->all[i].subtable_max)
            lk->all[i].subtables =
              xrealloc (lk->all[i].subtables,
                        (lk->all[i].subtable_max +=
                         10) * sizeof (struct lksubinfo));
          j = lk->all[i].subtable_cnt++;
          memset (&lk->all[i].subtables[j], 0, sizeof (struct lksubinfo));
          lk->all[i].subtables[j].subtable = sub;
          GFI_LookupScrollbars (sf->fontinfo, isgpos, true);
          GFI_LookupEnableButtons (sf->fontinfo, isgpos);
        }
    }
  return sub;
}

GTextInfo **
SFSubtablesOfType (SplineFont *sf, int lookup_type, int kernclass, int add_none)
{
  int isgpos = (lookup_type >= gpos_start);
  int k, cnt, lcnt, pos;
  OTLookup *otl;
  struct lookup_subtable *sub;
  GTextInfo **ti;

  if (sf->cidmaster != NULL)
    sf = sf->cidmaster;
  else if (sf->mm != NULL)
    sf = sf->mm->normal;

  for (k = 0; k < 2; ++k)
    {
      cnt = lcnt = pos = 0;
      if (k && add_none)
        {
          ti[pos] = xcalloc (1, sizeof (GTextInfo));
          ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
          ti[pos]->userdata = (void *) -1;
          ti[pos++]->text = utf82u_copy (_("No Subtable"));
          ti[pos] = xcalloc (1, sizeof (GTextInfo));
          ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
          ti[pos++]->line = true;
        }
      for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
           otl = otl->next)
        {
          if (otl->lookup_type == lookup_type && otl->subtables != NULL)
            {
              if (k)
                {
                  ti[pos] = xcalloc (1, sizeof (GTextInfo));

                  // FIXME: Why does this mix UTF-16 and UTF-32 quantities?
                  uint16_t *utf16 = x_gc_u8_to_u16 (otl->lookup_name);
                  ti[pos]->text =
                    xmalloc ((u16_mbsnlen (utf16, u16_strlen (utf16)) +
                              2) * sizeof (uint32_t));

                  ti[pos]->text[0] = ' ';
                  utf82u_strcpy (ti[pos]->text + 1, otl->lookup_name);
                  ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
                  ti[pos++]->disabled = true;
                }
              ++lcnt;
              for (sub = otl->subtables; sub != NULL; sub = sub->next)
                {
                  if (lookup_type != gpos_pair || kernclass == -1
                      || (kernclass && sub->kc != NULL)
                      || (!kernclass && sub->per_glyph_pst_or_kern))
                    {
                      if (k)
                        {
                          ti[pos] = xcalloc (1, sizeof (GTextInfo));
                          ti[pos]->text = utf82u_copy (sub->subtable_name);
                          ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
                          ti[pos++]->userdata = sub;
                        }
                      ++cnt;
                    }
                }
            }
        }
      if (!k)
        {
          ti = xcalloc (cnt + lcnt + 3 + 2 * add_none, sizeof (GTextInfo *));
        }
      else
        {
          ti[pos] = xcalloc (1, sizeof (GTextInfo));
          ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
          ti[pos++]->line = true;
          ti[pos] = xcalloc (1, sizeof (GTextInfo));
          ti[pos]->fg = ti[pos]->bg = COLOR_DEFAULT;
          ti[pos++]->text = utf82u_copy (_("New Lookup Subtable..."));
          ti[pos] = xcalloc (1, sizeof (GTextInfo));
          return ti;
        }
    }
  /* We'll never get here */
  return NULL;
}

GTextInfo *
SFSubtableListOfType (SplineFont *sf, int lookup_type, int kernclass,
                      int add_none)
{
  GTextInfo **temp, *ti;
  int cnt;

  temp = SFSubtablesOfType (sf, lookup_type, kernclass, add_none);
  if (temp == NULL)
    return NULL;
  for (cnt = 0; temp[cnt]->text != NULL || temp[cnt]->line; ++cnt);
  ti = xcalloc (cnt + 1, sizeof (GTextInfo));
  for (cnt = 0; temp[cnt]->text != NULL || temp[cnt]->line; ++cnt)
    {
      ti[cnt] = *temp[cnt];
      free (temp[cnt]);
    }
  free (temp);
  return ti;
}

struct lookup_subtable *
SFNewLookupSubtableOfType (SplineFont *sf, int lookup_type,
                           struct subtable_data *sd, int def_layer)
{
  int isgpos = (lookup_type >= gpos_start);
  OTLookup *otl, *found = NULL;
  int cnt, ans;
  struct lookup_subtable *sub;
  char **choices;

  if (sf->cidmaster)
    sf = sf->cidmaster;

  cnt = 0;
  for (otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
       otl = otl->next)
    if (otl->lookup_type == lookup_type)
      ++cnt;
  if (cnt == 0)
    {
      /* There are no lookups of this type, so there is nothing for them to */
      /*  pick from. So we must create a new lookup for them, and then add */
      /*  a subtable to it */
      found = CreateAndSortNewLookupOfType (sf, lookup_type);
      if (found == NULL)
        return NULL;
      sub = NewSubtable (found, isgpos, sf, sd, def_layer);
      /* even if they canceled the subtable creation they are now stuck */
      /*  with the lookup */
      return sub;
    }

  /* I thought briefly that if cnt were 1 I might want to automagically */
  /*  create a subtable in that lookup... but no. Still give them the */
  /*  option of creating a new lookup */

  choices = xmalloc ((cnt + 2) * sizeof (char *));
  for (cnt = 0, otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups; otl != NULL;
       otl = otl->next)
    if (otl->lookup_type == lookup_type)
      choices[cnt++] = otl->lookup_name;
  choices[cnt++] = _("Create a new lookup");
  choices[cnt] = NULL;
  ans =
    gwwv_choose (_("Add a subtable to which lookup?"), (const char **) choices,
                 cnt, cnt - 1, _("Add a subtable to which lookup?"));
  if (ans == -1)
    found = NULL;
  else if (ans == cnt)
    found = CreateAndSortNewLookupOfType (sf, lookup_type);
  else
    {
      found = NULL;
      for (cnt = 0, otl = isgpos ? sf->gpos_lookups : sf->gsub_lookups;
           otl != NULL; otl = otl->next)
        {
          if (otl->lookup_type == lookup_type)
            {
              if (cnt == ans)
                {
                  found = otl;
                  break;
                }
              else
                ++cnt;
            }
        }
    }
  free (choices);
  if (found == NULL)
    return NULL;

  return NewSubtable (found, isgpos, sf, sd, def_layer);
}

static void
kf_activateMe (struct fvcontainer *fvc, FontViewBase *fvb)
{
  struct kf_dlg *kf = (struct kf_dlg *) fvc;
  FontView *fv = (FontView *) fvb;

  if (!fv->notactive)
    return;

  kf->first_fv->notactive = true;
  kf->second_fv->notactive = true;
  fv->notactive = false;
  kf->active = fv;
  GDrawSetUserData (kf->dw, fv);
  GDrawRequestExpose (kf->dw, NULL, false);
}

static void
kf_charEvent (struct fvcontainer *fvc, void *event)
{
  struct kf_dlg *kf = (struct kf_dlg *) fvc;
  FVChar (kf->active, event);
}

static void
kf_doClose (struct fvcontainer *fvc)
{
  struct kf_dlg *kf = (struct kf_dlg *) fvc;
  kf->done = true;
}

static void
kf_doResize (struct fvcontainer *fvc, FontViewBase *fvb, int width, int height)
{
  struct kf_dlg *kf = (struct kf_dlg *) fvc;
  FontView *fv = (FontView *) fvb;
  FontView *otherfv = fv == kf->first_fv ? kf->second_fv : kf->first_fv;
  static int nested = 0;
  GRect size;

  if (fv->filled == NULL || otherfv->filled == NULL)
    return;                     /* Not initialized yet */
  if (nested)
    return;
  nested = 1;
  FVSetUIToMatch (otherfv, fv);
  nested = 0;

  memset (&size, 0, sizeof (size));
  size.height =
    fv->mbh + kf->infoh + kf->fh + 4 + 2 * (height - fv->mbh) + kf->fh + 2;
  size.width = width;
  GGadgetSetDesiredSize (kf->guts, NULL, &size);
  GHVBoxFitWindow (kf->topbox);
}

static struct fvcontainer_funcs kernformat_funcs = {
  fvc_kernformat,
  true,                         /* Modal dialog. No charviews, etc. */
  kf_activateMe,
  kf_charEvent,
  kf_doClose,
  kf_doResize
};

static void
kf_FVSetSize (struct kf_dlg *kf, FontView *fv, int width, int height, int y,
              int sbsize)
{
  int cc, rc, topchar;
  GRect subsize;

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
  GGadgetResize (fv->vsb, sbsize, subsize.height);

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

static void
kf_sizeSet (struct kf_dlg *kf, GWindow dw)
{
  GRect size, gsize;
  int width, height, y;

  GDrawGetSize (dw, &size);
  GGadgetGetSize (kf->first_fv->vsb, &gsize);
  width = size.width - gsize.width;
  height = size.height - kf->mbh - kf->first_fv->infoh - 2 * (kf->fh + 4);
  height /= 2;

  y = kf->mbh + kf->first_fv->infoh + (kf->fh + 4);
  kf_FVSetSize (kf, kf->first_fv, width, height, y, gsize.width);

  kf->label2_y = y + height + 2;
  y = kf->label2_y + kf->fh + 2;
  kf_FVSetSize (kf, kf->second_fv, width, height, y, gsize.width);
}

static int
kf_sub_e_h (GWindow pixmap, GEvent *event)
{
  FontView *active_fv;
  struct kf_dlg *kf;

  if (event->type == et_destroy)
    return true;

  active_fv = (FontView *) GDrawGetUserData (pixmap);
  kf = (struct kf_dlg *) (active_fv->b.container);

  if ((event->type == et_mouseup || event->type == et_mousedown)
      && (event->u.mouse.button >= 4 && event->u.mouse.button <= 7))
    {
      return GGadgetDispatchEvent (active_fv->vsb, event);
    }

  switch (event->type)
    {
    case et_expose:
      FVDrawInfo (active_fv, pixmap, event);
      GDrawSetFont (pixmap, kf->first_fv->notactive ? kf->plain : kf->bold);
      GDrawDrawText8 (pixmap, 10, kf->mbh + kf->first_fv->infoh + kf->as,
                      _("Select glyphs for the first part of the kern pair"),
                      -1, 0x000000);
      GDrawSetFont (pixmap, kf->second_fv->notactive ? kf->plain : kf->bold);
      GDrawDrawText8 (pixmap, 10, kf->label2_y + kf->as,
                      _("Select glyphs for the second part of the kern pair"),
                      -1, 0x000000);
      break;
    case et_char:
      kf_charEvent (&kf->base, event);
      break;
    case et_mousedown:
      if (event->u.mouse.y < kf->mbh)
        return false;
      kf_activateMe (&kf->base,
                     (struct fontviewbase *) ((event->u.mouse.y > kf->label2_y)
                                              ? kf->second_fv : kf->first_fv));
      return false;
      break;
    case et_mouseup:
    case et_mousemove:
      return false;
    case et_resize:
      kf_sizeSet (kf, pixmap);
      break;
    }
  return true;
}

#undef CID_Separation
#undef CID_MinKern
#undef CID_Touched
#undef CID_OnlyCloser
#undef CID_Autokern
#define CID_KPairs	1000
#define CID_KClasses	1001
#define CID_KCBuild	1002
#define CID_Separation	1003
#define CID_MinKern	1004
#define CID_Touched	1005
#define CID_ClassDistance	1006
#define CID_Guts	1008
#define CID_OnlyCloser	1009
#define CID_Autokern	1010

struct kf_results
{
  int asked;
  int autokern;
  int autobuild;
  real good_enough;
  SplineChar **firstglyphs;
  SplineChar **secondglyphs;
};

static int
KF_FormatChange (GGadget *g, GEvent *e)
{
  struct kf_dlg *kf;
  char mkbuf[10];

  if (e->type == et_controlevent && e->u.control.subtype == et_radiochanged)
    {
      kf = GDrawGetUserData (GGadgetGetWindow (g));
      if (GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_KPairs)))
        {
          GGadgetSetEnabled (GWidgetGetControl (kf->gw, CID_KCBuild), 0);
          sprintf (mkbuf, "%d", 15 * (kf->sf->ascent + kf->sf->descent) / 1000);
          GGadgetSetTitle8 (GWidgetGetControl (kf->gw, CID_MinKern), mkbuf);
        }
      else
        {
          GGadgetSetEnabled (GWidgetGetControl (kf->gw, CID_KCBuild), 1);
          GGadgetSetTitle8 (GWidgetGetControl (kf->gw, CID_MinKern), "0");
        }
    }
  return true;
}

static SplineChar **
SelectedGlyphs (FontView *_fv)
{
  FontViewBase *fv = (FontViewBase *) _fv;
  SplineFont *sf;
  EncMap *map;
  int selcnt;
  int enc, gid;
  SplineChar **glyphlist, *sc;

  map = fv->map;
  sf = fv->sf;
  selcnt = 0;
  for (enc = 0; enc < map->enc_limit; ++enc)
    {
      if (fv->selected[enc] && (gid = enc_to_gid (map, enc)) != -1
          && SCWorthOutputting (sf->glyphs[gid]))
        ++selcnt;
    }
  if (selcnt < 1)
    {
      ff_post_error (_("No selection"),
                     _
                     ("Please select some glyphs in the font views at the bottom of the dialog for FontForge to put into classes."));
      return NULL;
    }

  glyphlist = xmalloc ((selcnt + 1) * sizeof (SplineChar *));
  selcnt = 0;
  for (enc = 0; enc < map->enc_limit; ++enc)
    {
      if (fv->selected[enc] && (gid = enc_to_gid (map, enc)) != -1
          && SCWorthOutputting (sc = sf->glyphs[gid]))
        glyphlist[selcnt++] = sc;
    }
  glyphlist[selcnt] = NULL;
  return glyphlist;
}

static int
KF_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct kf_dlg *kf = GDrawGetUserData (GGadgetGetWindow (g));
      int touch, separation, minkern, err, onlyCloser, autokern;
      real good_enough = 0;
      int isclass, autobuild = 0;
      struct kf_results *results = kf->results;

      err = false;
      touch = GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_Touched));
      separation = GetInt8 (kf->gw, CID_Separation, _("Separation"), &err);
      minkern = GetInt8 (kf->gw, CID_MinKern, _("Min Kern"), &err);
      onlyCloser =
        GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_OnlyCloser));
      autokern = GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_Autokern));
      if (err)
        return true;

      isclass = GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_KClasses));
      if (isclass)
        {
          autobuild =
            GGadgetIsChecked (GWidgetGetControl (kf->gw, CID_KCBuild));
          good_enough =
            GetReal8 (kf->gw, CID_ClassDistance, _("Intra Class Distance"),
                      &err);
          if (err)
            return true;
        }
      if (autobuild || autokern)
        {
          results->firstglyphs = SelectedGlyphs (kf->first_fv);
          if (results->firstglyphs == NULL)
            return true;
          results->secondglyphs = SelectedGlyphs (kf->second_fv);
          if (results->secondglyphs == NULL)
            {
              free (results->firstglyphs);
              results->firstglyphs = NULL;
              return true;
            }
        }
      kf->sub->separation = separation;
      kf->sub->minkern = minkern;
      kf->sub->kerning_by_touch = touch;
      kf->sub->onlyCloser = onlyCloser;
      kf->sub->dontautokern = !autokern;
      results->good_enough = good_enough;
      if (!isclass)
        results->asked = 0;
      else
        results->asked = 1;
      results->autobuild = autobuild;
      results->autokern = autokern;
      kf->done = true;
    }
  return true;
}

static int
KF_Cancel (GGadget *g, GEvent *e)
{
  struct kf_dlg *kf;

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      kf = GDrawGetUserData (GGadgetGetWindow (g));
      kf->done = true;
    }
  return true;
}

static int
kf_e_h (GWindow gw, GEvent *event)
{
  struct kf_dlg *kf = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_close:
      kf->done = true;
      break;
    case et_char:
      return false;
      break;
    }
  return true;
}

static int
kern_format_dlg (SplineFont *sf, int def_layer, struct lookup_subtable *sub,
                 struct kf_results *results)
{
  GRect pos;
  GWindowAttrs wattrs;
  GGadgetCreateData gcd[16], boxes[7], hbox;
  GGadgetCreateData *varray[21], *h2array[6], *h3array[6], *h4array[8],
    *buttonarray[8], *h5array[4];
  GTextInfo label[15];
  char sepbuf[40], mkbuf[40], distancebuf[40];
  struct kf_dlg kf;
  int i, j, guts_row;
  /* Returns are 0=>Pairs, 1=>Classes, 2=>Cancel */
  int as, ds, ld;
  static GFont *plainfont = NULL, *boldfont = NULL;

  if (sub->separation == 0 && !sub->kerning_by_touch)
    {
      sub->separation = sf->width_separation;
      if (sf->width_separation == 0)
        sub->separation = 15 * (sf->ascent + sf->descent) / 100;
      sub->minkern = sub->separation / 10;
    }

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));
  memset (&kf, 0, sizeof (kf));

  kf.base.funcs = &kernformat_funcs;
  kf.sub = sub;
  kf.results = results;
  kf.sf = sf;
  kf.def_layer = def_layer;
  results->asked = 2;           /* Cancel */

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = false;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title = _("Kerning format");
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  kf.gw = GDrawCreateTopWindow (NULL, &pos, kf_e_h, &kf, &wattrs);

  if (plainfont == NULL)
    {
      plainfont = GDrawNewFont (NULL, "sans-serif", 12, 400, fs_none);
      plainfont = GResourceFindFont ("KernFormat.Font", plainfont);

      boldfont = GDrawNewFont (NULL, "sans-serif", 12, 700, fs_none);
      boldfont = GResourceFindFont ("KernFormat.BoldFont", boldfont);
    }
  kf.plain = plainfont;
  kf.bold = boldfont;
  GDrawGetFontMetrics (kf.gw, kf.plain, &as, &ds, &ld);
  kf.fh = as + ds;
  kf.as = as;

  i = j = 0;

  label[i].text = (uint32_t *) _("Use individual kerning pairs");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("In this format you specify every kerning pair in which\n"
                   "you are interested in.");
  gcd[i].gd.cid = CID_KPairs;
  gcd[i].gd.handle_controlevent = KF_FormatChange;
  gcd[i].creator = GRadioCreate;
  varray[j++] = &gcd[i++];
  varray[j++] = NULL;

  label[i].text = (uint32_t *) _("Use a matrix of kerning classes");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags =
    gg_enabled | gg_visible | gg_utf8_popup | gg_rad_continueold;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("In this format you define a series of glyph classes and\n"
                   "specify a matrix showing how each class interacts with all\n"
                   "the others.");
  gcd[i].gd.cid = CID_KClasses;
  gcd[i].gd.handle_controlevent = KF_FormatChange;
  gcd[i].creator = GRadioCreate;
  varray[j++] = &gcd[i++];
  varray[j++] = NULL;

  label[i].text =
    (uint32_t *) _("FontForge will guess kerning classes for selected glyphs");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags = gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[i].gd.popup_msg =
    (uint32_t *)
    _("FontForge will look at the glyphs selected in the font view\n"
      "and will try to find groups of glyphs which are most alike\n"
      "and generate kerning classes based on that information.");
  gcd[i].gd.cid = CID_KCBuild;
  gcd[i].creator = GCheckBoxCreate;
  h2array[0] = GCD_HPad10;
  h2array[1] = &gcd[i++];
  h2array[2] = GCD_Glue;
  h2array[3] = NULL;
  boxes[3].gd.flags = gg_enabled | gg_visible;
  boxes[3].gd.u.boxelements = h2array;
  boxes[3].creator = GHBoxCreate;
  varray[j++] = &boxes[3];
  varray[j++] = NULL;

  label[i].text = (uint32_t *) _("Intra Class Distance:");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("This is roughly (very roughly) the number off em-units\n"
                   "of error that two glyphs may have to belong in the same\n"
                   "class. This error is taken by comparing the two glyphs\n"
                   "to all other glyphs and summing the differences.\n"
                   "A small number here (like 2) means lots of small classes,\n"
                   "while a larger number (like 20) will mean fewer classes,\n"
                   "each with more glyphs.");
  gcd[i].creator = GLabelCreate;
  h3array[0] = GCD_HPad10;
  h3array[1] = &gcd[i++];

  sprintf (distancebuf, "%g", (sf->ascent + sf->descent) / 100.);
  label[i].text = (uint32_t *) distancebuf;
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.width = 50;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
  gcd[i].gd.cid = CID_ClassDistance;
  gcd[i].creator = GTextFieldCreate;
  h3array[2] = &gcd[i++];
  h3array[3] = GCD_Glue;
  h3array[4] = NULL;

  boxes[5].gd.flags = gg_enabled | gg_visible;
  boxes[5].gd.u.boxelements = h3array;
  boxes[5].creator = GHBoxCreate;
  varray[j++] = &boxes[5];
  varray[j++] = NULL;
  varray[j++] = GCD_Glue;
  varray[j++] = NULL;

  label[i].text = (uint32_t *) _("_Default Separation:");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("Add entries to the lookup trying to make the optical\n"
                   "separation between all pairs of glyphs equal to this\n"
                   "value.");
  gcd[i].creator = GLabelCreate;
  h4array[0] = &gcd[i++];

  sprintf (sepbuf, "%d", sub->separation);
  label[i].text = (uint32_t *) sepbuf;
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.width = 50;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
  gcd[i].gd.cid = CID_Separation;
  gcd[i].creator = GTextFieldCreate;
  h4array[1] = &gcd[i++];
  h4array[2] = GCD_Glue;

  label[i].text = (uint32_t *) _("_Min Kern:");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("Any computed kerning change whose absolute value is less\n"
                   "that this will be ignored.\n");
  gcd[i].creator = GLabelCreate;
  h4array[3] = &gcd[i++];

  sprintf (mkbuf, "%d", sub->minkern);
  label[i].text = (uint32_t *) mkbuf;
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.width = 50;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  gcd[i].gd.popup_msg = gcd[i - 1].gd.popup_msg;
  gcd[i].gd.cid = CID_MinKern;
  gcd[i].creator = GTextFieldCreate;
  h4array[4] = &gcd[i++];

  label[i].text = (uint32_t *) _("_Touching");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.pos.x = 5;
  gcd[i].gd.pos.y = 5 + 4;
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  if (sub->kerning_by_touch)
    gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[i].gd.popup_msg =
    (uint32_t *)
    _("Normally kerning is based on achieving a constant (optical)\n"
      "separation between glyphs, but occasionally it is desirable\n"
      "to have a kerning table where the kerning is based on the\n"
      "closest approach between two glyphs (So if the desired separ-\n"
      "ation is 0 then the glyphs will actually be touching.");
  gcd[i].gd.cid = CID_Touched;
  gcd[i].creator = GCheckBoxCreate;
  h4array[5] = &gcd[i++];

  h4array[6] = GCD_Glue;
  h4array[7] = NULL;

  boxes[4].gd.flags = gg_enabled | gg_visible;
  boxes[4].gd.u.boxelements = h4array;
  boxes[4].creator = GHBoxCreate;
  varray[j++] = &boxes[4];
  varray[j++] = NULL;

  label[i].text = (uint32_t *) _("Only kern glyphs closer");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  if (sub->onlyCloser)
    gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("When doing autokerning, only move glyphs closer together,\n"
                   "so the kerning offset will be negative.");
  gcd[i].gd.cid = CID_OnlyCloser;
  gcd[i].creator = GCheckBoxCreate;
  h5array[0] = &gcd[i++];

  label[i].text = (uint32_t *) _("Autokern new entries");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup;
  if (!sub->dontautokern)
    gcd[i].gd.flags = gg_enabled | gg_visible | gg_utf8_popup | gg_cb_on;
  gcd[i].gd.popup_msg =
    (uint32_t *) _("When adding new entries provide default kerning values.");
  gcd[i].gd.cid = CID_Autokern;
  gcd[i].creator = GCheckBoxCreate;
  h5array[1] = &gcd[i++];
  h5array[2] = NULL;

  memset (&hbox, 0, sizeof (hbox));
  hbox.gd.flags = gg_enabled | gg_visible;
  hbox.gd.u.boxelements = h5array;
  hbox.creator = GHBoxCreate;
  varray[j++] = &hbox;
  varray[j++] = NULL;

  guts_row = j / 2;
  gcd[i].gd.flags = gg_enabled | gg_visible;
  gcd[i].gd.cid = CID_Guts;
  gcd[i].gd.u.drawable_e_h = kf_sub_e_h;
  gcd[i].creator = GDrawableCreate;
  varray[j++] = &gcd[i++];
  varray[j++] = NULL;

  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_default;
  label[i].text = (uint32_t *) _("_OK");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = KF_OK;
  gcd[i++].creator = GButtonCreate;

  gcd[i].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  label[i].text = (uint32_t *) _("_Cancel");
  label[i].text_is_1byte = true;
  label[i].text_has_mnemonic = true;
  gcd[i].gd.label = &label[i];
  gcd[i].gd.handle_controlevent = KF_Cancel;
  gcd[i++].creator = GButtonCreate;

  buttonarray[0] = GCD_Glue;
  buttonarray[1] = &gcd[i - 2];
  buttonarray[2] = GCD_Glue;
  buttonarray[3] = GCD_Glue;
  buttonarray[4] = &gcd[i - 1];
  buttonarray[5] = GCD_Glue;
  buttonarray[6] = NULL;
  boxes[6].gd.flags = gg_enabled | gg_visible;
  boxes[6].gd.u.boxelements = buttonarray;
  boxes[6].creator = GHBoxCreate;
  varray[j++] = &boxes[6];
  varray[j++] = NULL;
  varray[j++] = NULL;

  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = varray;
  boxes[0].creator = GHVGroupCreate;

  GGadgetsCreate (kf.gw, boxes);

  GHVBoxSetExpandableRow (boxes[0].ret, guts_row);
  GHVBoxSetExpandableCol (boxes[3].ret, gb_expandglue);
  GHVBoxSetExpandableCol (boxes[4].ret, gb_expandglue);
  GHVBoxSetExpandableCol (boxes[5].ret, gb_expandglue);
  GHVBoxSetExpandableCol (boxes[6].ret, gb_expandgluesame);

  kf.topbox = boxes[0].ret;
  KFFontViewInits (&kf, GWidgetGetControl (kf.gw, CID_Guts));
  kf_activateMe ((struct fvcontainer *) &kf,
                 (struct fontviewbase *) kf.first_fv);

  GHVBoxFitWindow (boxes[0].ret);
  GDrawSetVisible (kf.gw, true);
  while (!kf.done)
    GDrawProcessOneEvent (NULL);
  FontViewFree (&kf.second_fv->b);
  FontViewFree (&kf.first_fv->b);
  GDrawSetUserData (kf.gw, NULL);
  GDrawDestroyWindow (kf.gw);
  return results->asked;
}

void
_LookupSubtableContents (SplineFont *sf, struct lookup_subtable *sub,
                         struct subtable_data *sd, int def_layer)
{
  int lookup_type = sub->lookup->lookup_type;
  static int nested = 0;
  extern int default_autokern_dlg;

  if ((lookup_type == gsub_context || lookup_type == gsub_contextchain
       || lookup_type == gsub_reversecchain || lookup_type == gpos_context
       || lookup_type == gpos_contextchain) && sub->fpst == NULL)
    {
      sub->fpst = xzalloc (sizeof (FPST));
      sub->fpst->type =
        lookup_type == gsub_context ? pst_contextsub : lookup_type ==
        gsub_contextchain ? pst_chainsub : lookup_type ==
        gsub_reversecchain ? pst_reversesub : lookup_type ==
        gpos_context ? pst_contextpos : pst_chainpos;
      if (lookup_type == gsub_reversecchain)
        sub->fpst->format = pst_reversecoverage;
      sub->fpst->subtable = sub;
      sub->fpst->next = sf->possub;
      sf->possub = sub->fpst;
    }
  else if (lookup_type == gpos_pair && sub->kc == NULL
           && !sub->per_glyph_pst_or_kern)
    {
      char *buts[5];
      struct kf_results results;

      memset (&results, 0, sizeof (results));
      if (sd != NULL && sd->flags & sdf_verticalkern)
        sub->vertical_kerning = true;
      else if (sd != NULL && sd->flags & sdf_horizontalkern)
        sub->vertical_kerning = false;
      else
        sub->vertical_kerning = VerticalKernFeature (sf, sub->lookup, true);

      if (sd != NULL && (sd->flags & sdf_kernclass))
        results.asked = 1;
      else if (sd != NULL && (sd->flags & sdf_kernpair))
        results.asked = 0;
      else
        {
          if (sub->vertical_kerning || nested || !default_autokern_dlg)
            {
              buts[0] = _("_Pairs");
              buts[1] = _("C_lasses");
              buts[2] = _("_Cancel");
              buts[3] = NULL;
              results.asked =
                gwwv_ask (_("Kerning format"), (const char **) buts, 0, 1,
                          _
                          ("Kerning may be specified either by classes of glyphs\nor by pairwise combinations of individual glyphs.\nWhich do you want for this subtable?"));
            }
          else
            {
              nested = 1;
              kern_format_dlg (sf, def_layer, sub, &results);
              nested = 0;
            }
          if (results.asked == 2)
            return;
        }
      if (results.asked == 0)
        {
          sub->per_glyph_pst_or_kern = true;
          if (results.autokern)
            {
              SplineChar **lefts, **rights;
              lefts = results.firstglyphs;
              rights = results.secondglyphs;
              AutoKern2 (sf, def_layer, lefts, rights, sub,
                         /* If separation==0 and !touch then use default values */
                         0, 0, 0, 0, 0, NULL, NULL);
            }
        }
      else
        {
          sub->kc = xzalloc (sizeof (KernClass));
          if (sub->vertical_kerning)
            {
              sub->kc->next = sf->vkerns;
              sf->vkerns = sub->kc;
            }
          else
            {
              sub->kc->next = sf->kerns;
              sf->kerns = sub->kc;
            }
          sub->kc->subtable = sub;
          sub->kc->first_cnt = sub->kc->second_cnt = 1;
          sub->kc->firsts = xcalloc (1, sizeof (char *));
          sub->kc->seconds = xcalloc (1, sizeof (char *));
          sub->kc->offsets = xcalloc (1, sizeof (int16_t));
          sub->kc->adjusts = xcalloc (1, sizeof (DeviceTable));
          /* Need to fix for Hebrew !!!! */
          if (results.autobuild)
            /* Specifying separation==0 and !touching means use default values */
            AutoKern2BuildClasses (sf, def_layer, results.firstglyphs,
                                   results.secondglyphs, sub, 0, 0, 0, 0, 0,
                                   results.good_enough);
        }
      free (results.firstglyphs);
      free (results.secondglyphs);
    }

  if (sub->fpst && sf->fontinfo != NULL)
    {
      ContextChainEdit (sf, sub->fpst, sf->fontinfo, NULL, def_layer);
    }
  else if (sub->kc != NULL)
    {
      KernClassD (sub->kc, sf, def_layer, sub->vertical_kerning);
    }
  else if (sub->lookup->lookup_type >= gpos_cursive
           && sub->lookup->lookup_type <= gpos_mark2mark)
    AnchorClassD (sf, sub, def_layer);
  else
    PSTKernD (sf, sub, def_layer);
}

/******************************************************************************/
/***************************   Add/Remove Language   **************************/
/******************************************************************************/
static uint32_t
StrNextLang (char **_pt)
{
  unsigned char *pt = (unsigned char *) *_pt;
  unsigned char tag[4];
  int i;

  memset (tag, ' ', 4);
  while (*pt == ' ' || *pt == ',')
    ++pt;
  if (*pt == '\0')
    return 0;

  for (i = 0; i < 4 && *pt != '\0' && *pt != ','; ++i)
    tag[i] = *pt++;
  while (*pt == ' ')
    ++pt;
  if (*pt != '\0' && *pt != ',')
    return 0xffffffff;
  *_pt = (char *) pt;
  return (tag[0] << 24) | (tag[1] << 16) | (tag[2] << 8) | tag[3];
}

static void
lk_AddRm (struct lkdata *lk, int add_lang, uint32_t script, char *langs)
{
  int i, l;
  OTLookup *otl;
  FeatureScriptLangList *fl;
  struct scriptlanglist *sl;
  uint32_t lang;
  char *pt;

  for (i = 0; i < lk->cnt; ++i)
    {
      if (lk->all[i].deleted || !lk->all[i].selected)
        continue;
      otl = lk->all[i].lookup;
      for (fl = otl->features; fl != NULL; fl = fl->next)
        {
          for (sl = fl->scripts; sl != NULL; sl = sl->next)
            if (sl->script == script)
              {
                pt = langs;
                while ((lang = StrNextLang (&pt)) != 0)
                  {
                    for (l = sl->lang_cnt - 1; l >= 0; --l)
                      {
                        if (((l >=
                              MAX_LANG) ? sl->morelangs[l -
                                                        MAX_LANG] :
                             sl->langs[l]) == lang)
                          break;
                      }
                    if (add_lang && l < 0)
                      {
                        if (sl->lang_cnt < MAX_LANG)
                          {
                            sl->langs[sl->lang_cnt++] = lang;
                          }
                        else
                          {
                            sl->morelangs =
                              xrealloc (sl->morelangs,
                                        (++sl->lang_cnt -
                                         MAX_LANG) * sizeof (uint32_t));
                            sl->morelangs[sl->lang_cnt - MAX_LANG - 1] = lang;
                          }
                      }
                    else if (!add_lang && l >= 0)
                      {
                        --sl->lang_cnt;
                        while (l < sl->lang_cnt)
                          {
                            uint32_t nlang =
                              l + 1 >=
                              MAX_LANG ? sl->morelangs[l + 1 -
                                                       MAX_LANG] : sl->langs[l +
                                                                             1];
                            if (l >= MAX_LANG)
                              sl->morelangs[l - MAX_LANG] = nlang;
                            else
                              sl->langs[l] = nlang;
                            ++l;
                          }
                        if (sl->lang_cnt == 0)
                          {
                            sl->langs[0] = DEFAULT_LANG;
                            sl->lang_cnt = 1;
                          }
                      }
                  }
              }
        }
    }
}

#define CID_ScriptTag	1001
#define CID_Langs	1002

struct addrmlang
{
  GWindow gw;
  int done;
  int add_lang;
  struct lkdata *lk;
};

static int
ARL_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct addrmlang *arl = GDrawGetUserData (GGadgetGetWindow (g));
      const uint32_t *spt;
      char *langs, *pt;
      int i;
      unsigned char tag[4];
      uint32_t script_tag, lang;

      memset (tag, ' ', 4);
      spt = _GGadgetGetTitle (GWidgetGetControl (arl->gw, CID_ScriptTag));
      while (*spt == ' ')
        ++spt;
      if (*spt == '\0')
        {
          ff_post_error (_("No Script Tag"),
                         _("Please specify a 4 letter opentype script tag"));
          return true;
        }
      for (i = 0; i < 4 && *spt != '\0'; ++i)
        tag[i] = *spt++;
      while (*spt == ' ')
        ++spt;
      if (*spt != '\0')
        {
          ff_post_error (_("Script Tag too long"),
                         _("Please specify a 4 letter opentype script tag"));
          return true;
        }
      script_tag = (tag[0] << 24) | (tag[1] << 16) | (tag[2] << 8) | tag[3];

      pt = langs = GGadgetGetTitle8 (GWidgetGetControl (arl->gw, CID_Langs));
      while ((lang = StrNextLang (&pt)) != 0)
        {
          if (lang == 0xffffffff)
            {
              ff_post_error (_("Invalid language"),
                             _
                             ("Please specify a comma separated list of 4 letter opentype language tags"));
              return true;
            }
        }

      lk_AddRm (arl->lk, arl->add_lang, script_tag, langs);
      free (langs);
      arl->done = true;
    }
  return true;
}

static int
ARL_Cancel (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      struct addrmlang *arl = GDrawGetUserData (GGadgetGetWindow (g));
      arl->done = true;
    }
  return true;
}

static int
ARL_TagChanged (GGadget *g, GEvent *e)
{
  if (e->type == et_controlevent && e->u.control.subtype == et_textchanged
      && e->u.control.u.tf_changed.from_pulldown != -1)
    {
      int which = e->u.control.u.tf_changed.from_pulldown;
      int len;
      GTextInfo **ti = GGadgetGetList (g, &len);
      char tag[8];
      uint32_t tagval = (intptr_t) (ti[which]->userdata);

      tag[0] = tagval >> 24;
      tag[1] = tagval >> 16;
      tag[2] = tagval >> 8;
      tag[3] = tagval;
      tag[4] = 0;
      GGadgetSetTitle8 (g, tag);
    }
  return true;
}

static int
arl_e_h (GWindow gw, GEvent *event)
{
  struct addrmlang *arl = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_char:
      return false;
    case et_close:
      arl->done = true;
      break;
    }
  return true;
}

static GTextInfo *
ScriptListOfFont (SplineFont *sf)
{
  uint32_t *ourscripts = SFScriptsInLookups (sf, -1);
  int i, j;
  GTextInfo *ti;
  char tag[8];

  if (ourscripts == NULL || ourscripts[0] == 0)
    {
      ourscripts = xmalloc (2 * sizeof (uint32_t));
      ourscripts[0] = DEFAULT_SCRIPT;
      ourscripts[1] = 0;
    }
  for (i = 0; ourscripts[i] != 0; ++i);
  ti = xcalloc (i + 1, sizeof (GTextInfo));
  for (i = 0; ourscripts[i] != 0; ++i)
    {
      ti[i].userdata = (void *) (intptr_t) ourscripts[i];
      for (j = 0; scripts[j].text != NULL; ++j)
        {
          if (scripts[j].userdata == (void *) (intptr_t) ourscripts[i])
            break;
        }
      if (scripts[j].text != NULL)
        ti[i].text = (uint32_t *) xstrdup_or_null ((char *) scripts[j].text);
      else
        {
          tag[0] = ourscripts[i] >> 24;
          tag[1] = ourscripts[i] >> 16;
          tag[2] = ourscripts[i] >> 8;
          tag[3] = ourscripts[i] & 0xff;
          tag[4] = 0;
          ti[i].text = (uint32_t *) xstrdup_or_null ((char *) tag);
        }
      ti[i].text_is_1byte = true;
    }
  ti[0].selected = true;
  free (ourscripts);
  return ti;
}

void
AddRmLang (SplineFont *sf, struct lkdata *lk, int add_lang)
{
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  struct addrmlang arl;
  GGadgetCreateData gcd[14], *hvarray[4][3], *barray[8], boxes[3];
  GTextInfo label[14];
  int k;
  GEvent dummy;

  LookupUIInit ();

  memset (&arl, 0, sizeof (arl));
  arl.lk = lk;
  arl.add_lang = add_lang;

  memset (&wattrs, 0, sizeof (wattrs));
  memset (&gcd, 0, sizeof (gcd));
  memset (&boxes, 0, sizeof (boxes));
  memset (&label, 0, sizeof (label));

  wattrs.mask =
    wam_events | wam_cursor | wam_utf8_wtitle | wam_undercursor | wam_isdlg |
    wam_restrict;
  wattrs.event_masks = ~(1 << et_charup);
  wattrs.restrict_input_to_me = false;
  wattrs.undercursor = 1;
  wattrs.cursor = ct_pointer;
  wattrs.utf8_window_title =
    add_lang ? _("Add Language(s) to Script") :
    _("Remove Language(s) from Script");
  wattrs.is_dlg = false;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  arl.gw = gw = GDrawCreateTopWindow (NULL, &pos, arl_e_h, &arl, &wattrs);

  k = 0;

  label[k].text = (uint32_t *) _("Script Tag:");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[0][0] = &gcd[k - 1];

  gcd[k].gd.u.list = ScriptListOfFont (sf);
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_list_alphabetic;
  gcd[k].gd.cid = CID_ScriptTag;
  gcd[k].gd.handle_controlevent = ARL_TagChanged;
  gcd[k++].creator = GListFieldCreate;
  hvarray[0][1] = &gcd[k - 1];
  hvarray[0][2] = NULL;

  label[k].text = (uint32_t *) _("Language Tag:");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[1][0] = &gcd[k - 1];

  gcd[k].gd.u.list = languages;
  gcd[k].gd.cid = CID_Langs;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_list_alphabetic;
  gcd[k].gd.handle_controlevent = ARL_TagChanged;
  gcd[k++].creator = GListFieldCreate;
  hvarray[1][1] = &gcd[k - 1];
  hvarray[1][2] = NULL;


  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  gcd[k].gd.handle_controlevent = ARL_OK;
  gcd[k++].creator = GButtonCreate;

  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  gcd[k].gd.handle_controlevent = ARL_Cancel;
  gcd[k++].creator = GButtonCreate;

  barray[0] = barray[2] = barray[3] = barray[4] = barray[6] = GCD_Glue;
  barray[7] = NULL;
  barray[1] = &gcd[k - 2];
  barray[5] = &gcd[k - 1];

  boxes[2].gd.flags = gg_enabled | gg_visible;
  boxes[2].gd.u.boxelements = barray;
  boxes[2].creator = GHBoxCreate;

  hvarray[2][0] = &boxes[2];
  hvarray[2][1] = GCD_ColSpan;
  hvarray[2][2] = NULL;
  hvarray[3][0] = NULL;

  boxes[0].gd.pos.x = boxes[0].gd.pos.y = 2;
  boxes[0].gd.flags = gg_enabled | gg_visible;
  boxes[0].gd.u.boxelements = hvarray[0];
  boxes[0].creator = GHVGroupCreate;

  GGadgetsCreate (gw, boxes);

  memset (&dummy, 0, sizeof (dummy));
  dummy.type = et_controlevent;
  dummy.u.control.subtype = et_textchanged;
  dummy.u.control.u.tf_changed.from_pulldown =
    GGadgetGetFirstListSelectedItem (gcd[1].ret);
  ARL_TagChanged (gcd[1].ret, &dummy);

  GTextInfoListFree (gcd[1].gd.u.list);

  GHVBoxSetExpandableCol (boxes[2].ret, gb_expandgluesame);

  GHVBoxFitWindow (boxes[0].ret);

  GDrawSetVisible (gw, true);
  while (!arl.done)
    GDrawProcessOneEvent (NULL);

  GDrawDestroyWindow (gw);
}

/******************************************************************************/
/****************************   Mass Glyph Rename   ***************************/
/******************************************************************************/
typedef struct massrenamedlg
{
  GWindow gw;
  int done;
  FontView *fv;
} MassRenameDlg;

#undef CID_Suffix
#define CID_SubTable		1001
#define CID_Suffix		1002
#define CID_StartName		1003
#define CID_ReplaceSuffix	1004
#define CID_Themselves		1005

static int
MRD_OK (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MassRenameDlg *mrd = GDrawGetUserData (GGadgetGetWindow (g));
      int sel_cnt, enc, enc_max = mrd->fv->b.map->enc_limit;
      char *start_name, *suffix, *pt;
      int enc_start;
      SplineChar *sc, *sourcesc;
      GTextInfo *subti;
      struct lookup_subtable *sub;
      PST *pst;
      int themselves =
        GGadgetIsChecked (GWidgetGetControl (mrd->gw, CID_Themselves));
      int rplsuffix =
        GGadgetIsChecked (GWidgetGetControl (mrd->gw, CID_ReplaceSuffix));

      for (enc = sel_cnt = 0; enc < enc_max; ++enc)
        if (mrd->fv->b.selected[enc])
          ++sel_cnt;
      if (!themselves)
        {
          start_name =
            GGadgetGetTitle8 (GWidgetGetControl (mrd->gw, CID_StartName));
          enc_start =
            SFFindSlot (mrd->fv->b.sf, mrd->fv->b.map, -1, start_name);
          if (enc_start == -1)
            {
              ff_post_error (_("No Start Glyph"),
                             _
                             ("The encoding does not contain something named %.40s"),
                             start_name);
              free (start_name);
              return true;
            }
          free (start_name);
          if (enc_start + sel_cnt >= enc_max)
            {
              ff_post_error (_("Not enough glyphs"),
                             _
                             ("There aren't enough glyphs in the encoding to name all the selected characters"));
              return true;
            }
          for (enc = enc_start; enc < enc_start + sel_cnt; ++enc)
            if (mrd->fv->b.selected[enc])
              {
                ff_post_error (_("Bad selection"),
                               _
                               ("You may not rename any of the base glyphs, but your selection overlaps the set of base glyphs."));
                return true;
              }
        }
      else
        enc_start = 0;

      sub = NULL;
      subti =
        GGadgetGetListItemSelected (GWidgetGetControl (mrd->gw, CID_SubTable));
      if (subti != NULL)
        sub = subti->userdata;
      if (sub == (struct lookup_subtable *) -1)
        sub = NULL;
      if (sub != NULL && themselves)
        {
          ff_post_error (_("Can't specify a subtable here"),
                         _
                         ("As the selected glyphs are also source glyphs, they will be renamed, so they can't act as source glyphs for a lookup."));
          return true;
        }

      suffix = GGadgetGetTitle8 (GWidgetGetControl (mrd->gw, CID_Suffix));
      if (*suffix == '\0' || (*suffix == '.' && suffix[1] == '\0'))
        {
          ff_post_error (_("Missing suffix"),
                         _
                         ("If you don't specify a suffix, the glyphs don't get renamed."));
          free (suffix);
          return true;
        }
      if (*suffix != '.')
        {
          char *old = suffix;
          suffix = xstrdup (x_gc_strjoin (".", suffix, NULL));
          free (old);
        }

      for (enc = sel_cnt = 0; enc < enc_max; ++enc)
        if (mrd->fv->b.selected[enc])
          {
            char *oldname;
            sourcesc = sc = SFMakeChar (mrd->fv->b.sf, mrd->fv->b.map, enc);
            if (!themselves)
              sourcesc =
                SFMakeChar (mrd->fv->b.sf, mrd->fv->b.map, enc_start + sel_cnt);
            oldname = sc->name;
            if (rplsuffix && (pt = strchr (sourcesc->name, '.')) != NULL)
              {
                char *name =
                  xmalloc (pt - sourcesc->name + strlen (suffix) + 2);
                strcpy (name, sourcesc->name);
                strcpy (name + (pt - sourcesc->name), suffix);
                sc->name = name;
              }
            else
              sc->name = xstrdup (x_gc_strjoin (sourcesc->name, suffix, NULL));
            free (oldname);
            sc->unicodeenc = -1;
            if (sub != NULL)
              {
                /* There can only be one single subs with this subtable */
                /*  attached to the source glyph */
                for (pst = sourcesc->possub;
                     pst != NULL && pst->subtable != sub; pst = pst->next);
                if (pst == NULL)
                  {
                    pst = xzalloc (sizeof (PST));
                    pst->next = sourcesc->possub;
                    sourcesc->possub = pst;
                    pst->subtable = sub;
                    pst->type = pst_substitution;
                  }
                free (pst->u.subs.variant);
                pst->u.subs.variant = xstrdup_or_null (sc->name);
              }
            ++sel_cnt;
          }
      free (suffix);
      mrd->done = true;
    }
  return true;
}

static int
MRD_Cancel (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_buttonactivate)
    {
      MassRenameDlg *mrd = GDrawGetUserData (GGadgetGetWindow (g));
      mrd->done = true;
    }
  return true;
}

static int
MRD_SuffixChange (GGadget *g, GEvent *e)
{

  if (e->type == et_controlevent && e->u.control.subtype == et_textchanged)
    {
      MassRenameDlg *mrd = GDrawGetUserData (GGadgetGetWindow (g));
      char *suffix = GGadgetGetTitle8 (g);
      int32_t len;
      int i;
      GTextInfo **ti =
        GGadgetGetList (GWidgetGetControl (mrd->gw, CID_SubTable), &len);
      struct lookup_subtable *sub;

      for (i = 0; i < len; ++i)
        {
          sub = ti[i]->userdata;
          if (sub == NULL || sub == (struct lookup_subtable *) -1)
            continue;
          if (sub->suffix == NULL)
            continue;
          if (strcmp (suffix, sub->suffix) == 0)
            {
              GGadgetSelectOneListItem (GWidgetGetControl
                                        (mrd->gw, CID_SubTable), i);
              return true;
            }
        }
    }
  return true;
}

static void
MRD_SelectSubtable (MassRenameDlg * mrd, struct lookup_subtable *sub)
{
  int32_t len;
  GTextInfo **ti =
    GGadgetGetList (GWidgetGetControl (mrd->gw, CID_SubTable), &len);
  int i, no_pos = -1;

  for (i = 0; i < len; ++i)
    if (!ti[i]->line)
      {
        if (ti[i]->userdata == sub)
          break;
        else if (ti[i]->userdata == (void *) -1)
          no_pos = i;
      }
  if (i == len)
    i = no_pos;
  if (i != -1)
    GGadgetSelectOneListItem (GWidgetGetControl (mrd->gw, CID_SubTable), i);
}

static int
MRD_Subtable (GGadget *g, GEvent *e)
{
  MassRenameDlg *mrd = GDrawGetUserData (GGadgetGetWindow (g));
  GTextInfo *ti;
  struct lookup_subtable *sub;

  if (e->type == et_controlevent && e->u.control.subtype == et_listselected)
    {
      ti = GGadgetGetListItemSelected (g);
      if (ti != NULL)
        {
          if (ti->userdata == NULL)
            {
              sub =
                SFNewLookupSubtableOfType (mrd->fv->b.sf, gsub_single, NULL,
                                           mrd->fv->b.active_layer);
              if (sub != NULL)
                GGadgetSetList (g,
                                SFSubtablesOfType (mrd->fv->b.sf, gsub_single,
                                                   false, true), false);
              MRD_SelectSubtable (mrd, sub);
            }
          else if ((sub = ti->userdata) != (struct lookup_subtable *) -1
                   && sub->suffix != NULL)
            GGadgetSetTitle8 (GWidgetGetControl (mrd->gw, CID_Suffix),
                              sub->suffix);
        }
    }
  return true;
}

static uint32_t **
MRD_GlyphNameCompletion (GGadget *t, int from_tab)
{
  MassRenameDlg *mrd = GDrawGetUserData (GGadgetGetWindow (t));
  SplineFont *sf = mrd->fv->b.sf;

  return SFGlyphNameCompletion (sf, t, from_tab, false);
}

static int
mrd_e_h (GWindow gw, GEvent *event)
{
  MassRenameDlg *mrd = GDrawGetUserData (gw);

  switch (event->type)
    {
    case et_char:
      return false;
    case et_close:
      mrd->done = true;
      break;
    }
  return true;
}

void
FVMassGlyphRename (FontView *fv)
{
  GRect pos;
  GWindow gw;
  GWindowAttrs wattrs;
  MassRenameDlg mrd;
  GGadgetCreateData gcd[14], *hvarray[11][3], *barray[8], boxes[3];
  GTextInfo label[14];
  int i, k, subtablek, startnamek;

  memset (&mrd, 0, sizeof (mrd));
  mrd.fv = fv;

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
  wattrs.utf8_window_title = _("Mass Glyph Rename");
  wattrs.is_dlg = false;
  pos.x = pos.y = 0;
  pos.width = 100;
  pos.height = 100;
  mrd.gw = gw = GDrawCreateTopWindow (NULL, &pos, mrd_e_h, &mrd, &wattrs);

  k = i = 0;

  label[k].text = (uint32_t *) _("Rename all glyphs in the selection");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i][1] = GCD_ColSpan;
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("By appending the suffix:");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[i][0] = &gcd[k - 1];

  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k].gd.cid = CID_Suffix;
  gcd[k].gd.handle_controlevent = MRD_SuffixChange;
  gcd[k++].creator = GTextFieldCreate;
  hvarray[i][1] = &gcd[k - 1];
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("To their own names");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.cid = CID_Themselves;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_cb_on;
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i][1] = GCD_ColSpan;
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("To the glyph names starting at:");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("So if you type \"A\" here the first selected glyph would be named \"A.suffix\".\nThe second \"B.suffix\", and so on.");
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];

  startnamek = k;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_utf8_popup;
  gcd[k].gd.cid = CID_StartName;
  gcd[k].gd.popup_msg =
    (uint32_t *)
    _
    ("So if you type \"A\" here the first selected glyph would be named \"A.suffix\".\nThe second \"B.suffix\", and so on.");
  gcd[k++].creator = GTextCompletionCreate;
  hvarray[i][1] = &gcd[k - 1];
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("If one of those glyphs already has a suffix");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i][1] = GCD_ColSpan;
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("Append to it");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GRadioCreate;
  hvarray[i][0] = &gcd[k - 1];

  label[k].text = (uint32_t *) _("Replace it");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_cb_on;
  gcd[k].gd.cid = CID_ReplaceSuffix;
  gcd[k++].creator = GRadioCreate;
  hvarray[i][1] = &gcd[k - 1];
  hvarray[i++][2] = NULL;

  label[k].text =
    (uint32_t *) _("Optionally, add this mapping to the lookup subtable:");
  label[k].text_is_1byte = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.pos.x = 10;
  gcd[k].gd.pos.y = 10;
  gcd[k].gd.flags = gg_visible | gg_enabled;
  gcd[k++].creator = GLabelCreate;
  hvarray[i][0] = &gcd[k - 1];
  hvarray[i][1] = GCD_ColSpan;
  hvarray[i++][2] = NULL;

  subtablek = k;
  gcd[k].gd.flags = gg_enabled | gg_visible;
  gcd[k].gd.cid = CID_SubTable;
  gcd[k].gd.handle_controlevent = MRD_Subtable;
  gcd[k++].creator = GListButtonCreate;
  hvarray[i][0] = GCD_Glue;
  hvarray[i][1] = &gcd[k - 1];
  hvarray[i++][2] = NULL;
  hvarray[i][0] = hvarray[i][1] = GCD_Glue;
  hvarray[i++][2] = NULL;

  label[k].text = (uint32_t *) _("_OK");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_default;
  gcd[k].gd.handle_controlevent = MRD_OK;
  gcd[k++].creator = GButtonCreate;

  label[k].text = (uint32_t *) _("_Cancel");
  label[k].text_is_1byte = true;
  label[k].text_has_mnemonic = true;
  gcd[k].gd.label = &label[k];
  gcd[k].gd.flags = gg_visible | gg_enabled | gg_but_cancel;
  gcd[k].gd.handle_controlevent = MRD_Cancel;
  gcd[k++].creator = GButtonCreate;

  barray[0] = barray[2] = barray[3] = barray[4] = barray[6] = GCD_Glue;
  barray[7] = NULL;
  barray[1] = &gcd[k - 2];
  barray[5] = &gcd[k - 1];
  hvarray[i][0] = &boxes[2];
  hvarray[i][1] = GCD_ColSpan;
  hvarray[i++][2] = NULL;
  hvarray[i][0] = NULL;

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
  GHVBoxSetExpandableCol (boxes[0].ret, 1);

  GGadgetSetList (gcd[subtablek].ret,
                  SFSubtablesOfType (fv->b.sf, gsub_single, false, true),
                  false);
  GGadgetSelectOneListItem (gcd[subtablek].ret, 0);
  GCompletionFieldSetCompletion (gcd[startnamek].ret, MRD_GlyphNameCompletion);
  GWidgetIndicateFocusGadget (GWidgetGetControl (gw, CID_Suffix));

  GHVBoxFitWindow (boxes[0].ret);

  GDrawSetVisible (gw, true);
  while (!mrd.done)
    GDrawProcessOneEvent (NULL);

  GDrawDestroyWindow (gw);
}

/* ************************************************************************** */
/* ******************** Interface to FontInfo Lookup list ******************* */
/* ************************************************************************** */

static void
FI_SortInsertLookup (SplineFont *sf, OTLookup *newotl)
{
  int isgpos = newotl->lookup_type >= gpos_start;
  int pos, i, k;

  if (sf->fontinfo)
    {
      struct lkdata *lk = &sf->fontinfo->tables[isgpos];
      pos = FeatureOrderId (isgpos, newotl->features);
      if (lk->cnt >= lk->max)
        lk->all = xrealloc (lk->all, (lk->max += 10) * sizeof (struct lkinfo));
      for (i = 0;
           i < lk->cnt
           && FeatureOrderId (isgpos, lk->all[i].lookup->features) < pos; ++i);
      for (k = lk->cnt; k > i + 1; --k)
        lk->all[k] = lk->all[k - 1];
      memset (&lk->all[k], 0, sizeof (struct lkinfo));
      lk->all[k].lookup = newotl;
      ++lk->cnt;
      GFI_LookupScrollbars (sf->fontinfo, isgpos, true);
      GFI_LookupEnableButtons (sf->fontinfo, isgpos);
    }
}

/* Before may be:
    * A lookup in into_sf, in which case insert new lookup before it
    * NULL               , in which case insert new lookup at end
    * -1                 , in which case insert new lookup at start
    * -2                 , try to guess a good position
*/
static void
FI_OrderNewLookup (SplineFont *into_sf, OTLookup *otl, OTLookup *before)
{
  int isgpos = otl->lookup_type >= gpos_start;
  OTLookup **head = isgpos ? &into_sf->gpos_lookups : &into_sf->gsub_lookups;
  int i, k;

  if (into_sf->fontinfo)
    {
      struct lkdata *lk = &into_sf->fontinfo->tables[isgpos];

      if (lk->cnt >= lk->max)
        lk->all = xrealloc (lk->all, (lk->max += 10) * sizeof (struct lkinfo));

      if (before == (OTLookup *) -2)
        FI_SortInsertLookup (into_sf, otl);
      else
        {
          if (before == (OTLookup *) -1 || *head == NULL || *head == before)
            {
              i = 0;
            }
          else
            {
              for (i = 0; i < lk->cnt && lk->all[i].lookup != before; ++i);
            }
          for (k = lk->cnt; k > i; --k)
            lk->all[k] = lk->all[k - 1];
          memset (&lk->all[i], 0, sizeof (lk->all[i]));
          lk->all[i].lookup = otl;
          ++lk->cnt;
          GFI_LookupScrollbars (into_sf->fontinfo, isgpos, true);
          GFI_LookupEnableButtons (into_sf->fontinfo, isgpos);
        }
    }
}

static void
FI_OTLookupCopyInto (SplineFont *into_sf, SplineFont *from_sf,
                     OTLookup *from_otl, OTLookup *to_otl, int scnt,
                     OTLookup *before)
{
  if (into_sf->fontinfo)
    {
      int isgpos = from_otl->lookup_type >= gpos_start;
      struct lkdata *lk = &into_sf->fontinfo->tables[isgpos];
      struct lookup_subtable *sub;
      int i;
      for (i = 0; i < lk->cnt; ++i)
        if (lk->all[i].lookup == to_otl)
          break;
      if (i == lk->cnt)
        {
          FI_OrderNewLookup (into_sf, to_otl, before);
          for (i = 0; i < lk->cnt; ++i)
            if (lk->all[i].lookup == to_otl)
              break;
        }
      lk->all[i].subtable_cnt = lk->all[i].subtable_max = scnt;
      lk->all[i].subtables = xcalloc (scnt, sizeof (struct lksubinfo));
      if (scnt > 0)
        for (sub = to_otl->subtables, scnt = 0; sub != NULL;
             sub = sub->next, ++scnt)
          lk->all[i].subtables[scnt].subtable = sub;
    }
}

struct fi_interface gdraw_fi_interface = {
  FI_SortInsertLookup,
  FI_OTLookupCopyInto,
  FontInfoDestroy
};
