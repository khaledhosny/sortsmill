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

/* Copyright (C) 2000-2004 by George Williams */
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "ustring.h"
#include "gfile.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <stdarg.h>
#include <xalloc.h>
#include <sortsmill/xgc.h>
#include <xunistring.h>
#include <xunistring.h>
#include <filenamecat.h>
#include <dirname.h>

// Generate non-inline versions of these.
uint8_t *u8_GFileGetUserConfigDir (void);
uint8_t *u8_GFileGetUserCacheDir (void);
uint8_t *u8_GFileGetUserDataDir (void);
uint8_t *u8_GFileGetHomeDir (void);
uint8_t *u8_GFileBuildName (uint8_t *dir, uint8_t *file);
uint8_t *u8_GFileBaseName (const uint8_t *file);
uint8_t *u8_GFileAppendFile (const uint8_t *dir, const uint8_t *name,
                             bool isdir);

char *
GFileGetUserConfigDir (void)
{
  return x_gc_grabstr (g_build_filename (g_get_user_config_dir (),
                                         PACKAGE, NULL));
}

char *
GFileGetUserCacheDir (void)
{
  return x_gc_grabstr (g_build_filename (g_get_user_cache_dir (),
                                         PACKAGE, NULL));
}

char *
GFileGetUserDataDir (void)
{
  return x_gc_grabstr (g_build_filename (g_get_user_data_dir (),
                                         PACKAGE, NULL));
}

char *
GFileGetHomeDir (void)
{
  return x_gc_grabstr (g_build_filename (g_get_home_dir (), NULL));
}

char *
GFileBuildName (char *dir, char *file)
{
  return x_gc_grabstr (g_build_filename (dir, file, NULL));
}

char *
GFileBaseName (const char *file)
{
  return x_gc_grabstr (g_path_get_basename (file));
}

char *
GFileAppendFile (const char *dir, const char *name, bool isdir)
{
  char *filename = file_name_concat (dir, name, NULL);
  size_t length = strlen (filename);
  if (isdir && !ISSLASH (filename[length - 1]))
    {
      filename = xrealloc (filename, (length + 2) * sizeof (char));
      filename[length] = DIRECTORY_SEPARATOR;
      filename[length + 1] = '\0';
    }
  return filename;
}

bool
GFileIsAbsolute (const char *file)
{
  return g_path_is_absolute (file);
}

bool
GFileIsDir (const char *file)
{
  struct stat info;
  bool isdir;
  if (g_stat (file, &info) == -1)
    isdir = false;
  else
    isdir = (S_ISDIR (info.st_mode));

  return isdir;
}

bool
GFileExists (const char *file)
{
  return (g_access (file, 0) == 0);
}

bool
GFileReadable (const char *file)
{
  return (g_access (file, 04) == 0);
}

int
GFileMkDir (const char *name)
{
  return g_mkdir (name, 0755);
}

int
GFileUnlink (const char *name)
{
  return g_unlink (name);
}

uint32_t *
u32_GFileBaseName (const uint32_t *file)
{
  assert (u32_check (file, u32_strlen (file)) == NULL);
  return x_gc_u8_to_u32 (u8_GFileBaseName (x_gc_u32_to_u8 (file)));
}

uint32_t *
u32_GFileNormalize (uint32_t *name)
{
  uint32_t *pt, *base, *ppt;

  if ((pt = uc_strstr (name, "://")) != NULL)
    {
      base = u32_strchr (pt + 3, '/');
      if (base == NULL)
        return (name);
      ++base;
    }
  else if (*name == '/')
    base = name + 1;
  else
    base = name;
  for (pt = base; *pt != '\0';)
    {
      if (*pt == '/')
        u32_strcpy (pt, pt + 1);
      else if (u8_strncmp (x_gc_u32_to_u8 (u32_force_valid (pt)), "./", 2) == 0)
        u32_strcpy (pt, pt + 2);
      else if (u8_strncmp (x_gc_u32_to_u8 (u32_force_valid (pt)), "../", 2) == 0)
        {
          for (ppt = pt - 2; ppt >= base && *ppt != '/'; --ppt);
          ++ppt;
          if (ppt >= base)
            {
              u32_strcpy (ppt, pt + 3);
              pt = ppt;
            }
          else
            pt += 3;
        }
      else
        {
          while (*pt != '/' && *pt != '\0')
            ++pt;
          if (*pt == '/')
            ++pt;
        }
    }
  return (name);
}

uint32_t *
u32_GFileAppendFile (const uint32_t *dir, const uint32_t *name, bool isdir)
{
  return x_gc_u8_to_u32 (u8_GFileAppendFile (x_gc_u32_to_u8 (dir),
                                             x_gc_u32_to_u8 (name), isdir));
}
