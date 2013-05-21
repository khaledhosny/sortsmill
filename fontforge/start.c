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
#include "annotations.h"
#include <gfile.h>
#include <time.h>
#include <sys/time.h>
#include <locale.h>
#include <unistd.h>
#include <stdlib.h>

int32_t unicode_from_adobestd[256];
struct lconv localeinfo;
VISIBLE char *coord_sep = ",";

/* Unicode character names and annotations. */
uninm_names_db names_db;

static void
initadobeenc (void)
{
  int i, j;

  for (i = 0; i < 0x100; ++i)
    {
      if (strcmp (AdobeStandardEncoding[i], ".notdef") == 0)
        unicode_from_adobestd[i] = 0xfffd;
      else
        {
          j = UniFromName (AdobeStandardEncoding[i], ui_none, &custom);
          if (j == -1)
            j = 0xfffd;
          unicode_from_adobestd[i] = j;
        }
    }
}

static void
initrand (void)
{
  struct timeval tv;

  gettimeofday (&tv, NULL);
  srand (tv.tv_usec);
  srandom (tv.tv_usec);
}

void
InitSimpleStuff (void)
{
  char *names_db_file;

  initrand ();
  initadobeenc ();

  // setlocale (LC_ALL, ""); <-- This is to be done in the main
  //                             program now.

  localeinfo = *localeconv ();
  coord_sep = ",";
  if (*localeinfo.decimal_point == '.')
    coord_sep = ",";
  else if (*localeinfo.decimal_point != '.')
    coord_sep = " ";
  if (getenv ("FF_SCRIPT_IN_LATIN1"))
    use_utf8_in_script = false;

  /*
   * Load character names and annotations that come from the Unicode
   * NamesList.txt. This should not be done until after the locale
   * has been set.
   */
  names_db_file = uninm_find_names_db (NULL);
  names_db = (names_db_file == NULL) ?
    ((uninm_names_db) 0) : uninm_names_db_open (names_db_file);
  free (names_db_file);

  SetDefaults ();
}

void
doinitFontForgeMain (void)
{
  static int inited = false;

  if (inited)
    return;
  InitSimpleStuff ();
  if (default_encoding == NULL)
    default_encoding = FindOrMakeEncoding ("ISO8859-1");
  if (default_encoding == NULL)
    default_encoding = &custom; /* In case iconv is broken */
  inited = true;
}
