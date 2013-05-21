/*
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

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
#ifndef _GFILE_H
#define _GFILE_H

#include <config.h>

#include <assert.h>
#include <basics.h>
#include <stdbool.h>
#include <stdlib.h>
#include <xunistring.h>

VISIBLE extern char *GFileGetUserConfigDir (void);
VISIBLE extern char *GFileGetUserCacheDir (void);
VISIBLE extern char *GFileGetUserDataDir (void);
VISIBLE extern char *GFileGetHomeDir (void);
VISIBLE extern char *GFileBuildName (char *dir, char *file);
VISIBLE extern char *GFileBaseName (const char *file);
VISIBLE extern char *GFileAppendFile (const char *dir, const char *name,
                                      bool isdir);
VISIBLE extern bool GFileIsAbsolute (const char *file);
VISIBLE extern bool GFileIsDir (const char *file);
VISIBLE extern bool GFileExists (const char *file);
VISIBLE extern bool GFileReadable (char *file);
VISIBLE extern int GFileMkDir (char *name);
VISIBLE extern int GFileUnlink (char *name);

VISIBLE inline uint8_t *u8_GFileGetUserConfigDir (void);
VISIBLE inline uint8_t *u8_GFileGetUserCacheDir (void);
VISIBLE inline uint8_t *u8_GFileGetUserDataDir (void);
VISIBLE inline uint8_t *u8_GFileGetHomeDir (void);
VISIBLE inline uint8_t *u8_GFileBuildName (uint8_t *dir, uint8_t *file);
VISIBLE inline uint8_t *u8_GFileBaseName (const uint8_t *file);
VISIBLE inline uint8_t *u8_GFileAppendFile (const uint8_t *dir,
                                            const uint8_t *name, bool isdir);

VISIBLE extern uint32_t *u32_GFileBaseName (const uint32_t *oldname);
VISIBLE extern uint32_t *u32_GFileNormalize (uint32_t *name);
VISIBLE extern uint32_t *u32_GFileAppendFile (const uint32_t *dir,
                                              const uint32_t *name,
                                              bool isdir);

inline uint8_t *
u8_GFileGetUserConfigDir (void)
{
  return (uint8_t *) GFileGetUserConfigDir ();
}

inline uint8_t *
u8_GFileGetUserCacheDir (void)
{
  return (uint8_t *) GFileGetUserCacheDir ();
}

inline uint8_t *
u8_GFileGetUserDataDir (void)
{
  return (uint8_t *) GFileGetUserDataDir ();
}

inline uint8_t *
u8_GFileGetHomeDir (void)
{
  return (uint8_t *) GFileGetHomeDir ();
}

inline uint8_t *
u8_GFileBuildName (uint8_t *dir, uint8_t *file)
{
  assert (u8_valid (dir));
  assert (u8_valid (file));
  return (uint8_t *) GFileBuildName ((char *) dir, (char *) file);
}

inline uint8_t *
u8_GFileBaseName (const uint8_t *oldname)
{
  assert (u8_valid (oldname));
  return (uint8_t *) GFileBaseName ((const char *) oldname);
}

inline uint8_t *
u8_GFileAppendFile (const uint8_t *dir, const uint8_t *name, bool isdir)
{
  assert (u8_valid (dir));
  assert (u8_valid (name));
  return (uint8_t *) GFileAppendFile ((const char *) dir, (const char *) name,
                                      isdir);
}

#endif // _GFILE_H
