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

static char dirname_[1024];
#if !defined(__MINGW32__)
#include <pwd.h>
#else
#include <Windows.h>
#endif

#if defined(__MINGW32__)
static void
_backslash_to_slash (char *c)
{
  for (; *c; c++)
    if (*c == '\\')
      *c = '/';
}

static void
_u_backslash_to_slash (unichar_t *c)
{
  for (; *c; c++)
    if (*c == '\\')
      *c = '/';
}
#endif

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

unichar_t *
u_GFileGetHomeDir (void)
{
  unichar_t *dir = NULL;
  char *tmp = GFileGetHomeDir ();
  if (tmp)
    {
      dir = uc_copy (tmp);
      free (tmp);
    }
  return dir;
}

char *
GFileBuildName (char *dir, char *file)
{
  return x_gc_grabstr (g_build_filename (dir, file, NULL));
}

/* Given a filename in a directory, pick the directory out of it, and */
/* create a new filename using that directory and the given nametail */
char *
GFileReplaceName (char *oldname, char *file)
{
  return
    x_gc_grabstr (g_build_filename
                  (x_gc_grabstr (g_path_get_dirname (oldname)), file, NULL));
}

char *
GFileNameTail (const char *file)
{
  return x_gc_grabstr (g_path_get_basename (file));
}

char *
GFileAppendFile (char *dir, char *name, int isdir)
{
  char *ret, *pt;

  ret = (char *) xmalloc1 ((strlen (dir) + strlen (name) + 3));
  strcpy (ret, dir);
  pt = ret + strlen (ret);
  if (pt > ret && pt[-1] != '/')
    *pt++ = '/';
  strcpy (pt, name);
  if (isdir)
    {
      pt += strlen (pt);
      if (pt > ret && pt[-1] != '/')
        {
          *pt++ = '/';
          *pt = '\0';
        }
    }
  return (ret);
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
GFileModifyable (const char *file)
{
  return (g_access (file, 02) == 0);
}

bool
GFileModifyableDir (const char *file)
{
  return GFileModifyable (g_path_get_dirname (file));
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
GFileRmDir (char *name)
{
  return g_rmdir (name);
}

int
GFileUnlink (char *name)
{
  return g_unlink (name);
}

// FIXME: Get rid of this.
unichar_t *
u_GFileGetAbsoluteName (unichar_t *name, unichar_t *result, int rsiz)
{
  /* result may be the same as name */
  unichar_t buffer[1000];

  if (!u_GFileIsAbsolute (name))
    {
      unichar_t *pt, *spt, *rpt, *bpt;

      if (dirname_[0] == '\0')
        {
          getcwd (dirname_, sizeof (dirname_));
        }
      uc_strcpy (buffer, dirname_);
      if (buffer[u_strlen (buffer) - 1] != '/')
        uc_strcat (buffer, "/");
      u_strcat (buffer, name);
#if defined(__MINGW32__)
      _u_backslash_to_slash (buffer);
#endif

      /* Normalize out any .. */
      spt = rpt = buffer;
      while (*spt != '\0')
        {
          if (*spt == '/')
            ++spt;
          for (pt = spt; *pt != '\0' && *pt != '/'; ++pt);
          if (pt == spt)        /* Found // in a path spec, reduce to / (we've */
            u_strcpy (spt, pt); /*  skipped past the :// of the machine name) */
          else if (pt == spt + 1 && spt[0] == '.' && *pt == '/')        /* Noop */
            u_strcpy (spt, spt + 2);
          else if (pt == spt + 2 && spt[0] == '.' && spt[1] == '.')
            {
              for (bpt = spt - 2; bpt > rpt && *bpt != '/'; --bpt);
              if (bpt >= rpt && *bpt == '/')
                {
                  u_strcpy (bpt, pt);
                  spt = bpt;
                }
              else
                {
                  rpt = pt;
                  spt = pt;
                }
            }
          else
            spt = pt;
        }
      name = buffer;
    }
  if (result != name)
    {
      u_strncpy (result, name, rsiz);
      result[rsiz - 1] = '\0';
#if defined(__MINGW32__)
      _u_backslash_to_slash (result);
#endif
    }
  return (result);
}

unichar_t *
u_GFileNameTail (const unichar_t *file)
{
  char *locale_file = x_gc_grabstr (x_u32_strconv_to_locale (file));
  char *locale_base = x_gc_grabstr (g_path_get_basename (locale_file));
  return x_gc_u32_grabstr (x_u32_strconv_from_locale (locale_base));
}

unichar_t *
u_GFileNormalize (unichar_t *name)
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

unichar_t *
u_GFileAppendFile (unichar_t *dir, unichar_t *name, int isdir)
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

bool
u_GFileIsAbsolute (const unichar_t *file)
{
  return GFileIsAbsolute (x_gc_grabstr (x_u32_strconv_to_locale (file)));
}

bool
u_GFileIsDir (const unichar_t *file)
{
  char *locale_file = x_gc_grabstr (x_u32_strconv_to_locale (file));
  char *dot_file = x_gc_malloc_atomic ((strlen (locale_file) + 10) * sizeof (char));
  strcpy (dot_file, locale_file);
  strcat (dot_file, "/.");
  return GFileIsDir (dot_file);
}

bool
u_GFileExists (const unichar_t *file)
{
  char buffer[1024];
  u2def_strncpy (buffer, file, sizeof (buffer));
  return GFileExists (buffer);
}

bool
u_GFileModifyable (const unichar_t *file)
{
  char buffer[1024];
  u2def_strncpy (buffer, file, sizeof (buffer));
  return GFileModifyable (buffer);
}

bool
u_GFileModifyableDir (const unichar_t *file)
{
  char buffer[1024];
  u2def_strncpy (buffer, file, sizeof (buffer));
  return GFileModifyableDir (buffer);
}

bool
u_GFileReadable (unichar_t *file)
{
  char buffer[1024];
  u2def_strncpy (buffer, file, sizeof (buffer));
  return GFileReadable (buffer);
}

int
u_GFileMkDir (unichar_t *name)
{
  char buffer[1024];
  u2def_strncpy (buffer, name, sizeof (buffer));
  return GFileMkDir (buffer);
}

int
u_GFileRmDir (unichar_t *name)
{
  char buffer[1024];
  u2def_strncpy (buffer, name, sizeof (buffer));
  return GFileRmDir (buffer);
}

int
u_GFileUnlink (unichar_t *name)
{
  char buffer[1024];
  u2def_strncpy (buffer, name, sizeof (buffer));
  return GFileUnlink (buffer);
}
