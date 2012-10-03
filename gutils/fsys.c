#include <config.h>

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
#include <xgc.h>
#include <xuniconv.h>
#include <unistr.h>
#include <xgetcwd.h>
#include <filenamecat.h>
#include <dirname.h>

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
GFileAppendFile (char *dir, char *name, bool isdir)
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
GFileReadable (char *file)
{
  return (g_access (file, 04) == 0);
}

int
GFileMkDir (char *name)
{
  return g_mkdir (name, 0755);
}

int
GFileUnlink (char *name)
{
  return g_unlink (name);
}

unichar_t *
u32_GFileBaseName (const unichar_t *file)
{
  // FIXME: Convert to utf-8 instead of locale.
  char *locale_file = x_gc_u32_strconv_to_locale (file);
  char *locale_base = x_gc_grabstr (g_path_get_basename (locale_file));
  return x_gc_u32_strconv_from_locale (locale_base);
}

unichar_t *
u32_GFileNormalize (unichar_t *name)
{
  unichar_t *pt, *base, *ppt;

  if ((pt = uc_strstr (name, "://")) != NULL)
    {
      base = u_strchr (pt + 3, '/');
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
        u_strcpy (pt, pt + 1);
      else if (uc_strncmp (pt, "./", 2) == 0)
        u_strcpy (pt, pt + 2);
      else if (uc_strncmp (pt, "../", 2) == 0)
        {
          for (ppt = pt - 2; ppt >= base && *ppt != '/'; --ppt);
          ++ppt;
          if (ppt >= base)
            {
              u_strcpy (ppt, pt + 3);
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

// FIXME: Rewrite this, using UTF-8 and GFileAppendFile.
unichar_t *
u32_GFileAppendFile (unichar_t *dir, unichar_t *name, bool isdir)
{
  unichar_t *ret, *pt;

  ret =
    (unichar_t *) xmalloc ((u_strlen (dir) + u_strlen (name) + 3) *
                           sizeof (unichar_t));
  u_strcpy (ret, dir);
  pt = ret + u_strlen (ret);
  if (pt > ret && pt[-1] != '/')
    *pt++ = '/';
  u_strcpy (pt, name);
  if (isdir)
    {
      pt += u_strlen (pt);
      if (pt > ret && pt[-1] != '/')
        {
          *pt++ = '/';
          *pt = '\0';
        }
    }
  return (ret);
}
