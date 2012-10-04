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
#include <xunistr.h>

extern char *GFileGetUserConfigDir (void);
extern char *GFileGetUserCacheDir (void);
extern char *GFileGetUserDataDir (void);
extern char *GFileGetHomeDir (void);
extern char *GFileBuildName (char *dir, char *file);
extern char *GFileBaseName (const char *file);
extern char *GFileAppendFile (const char *dir, const char *name, bool isdir);
extern bool GFileIsAbsolute (const char *file);
extern bool GFileIsDir (const char *file);
extern bool GFileExists (const char *file);
extern bool GFileReadable (char *file);
extern int GFileMkDir (char *name);
extern int GFileUnlink (char *name);

inline uint8_t *u8_GFileGetUserConfigDir (void);
inline uint8_t *u8_GFileGetUserCacheDir (void);
inline uint8_t *u8_GFileGetUserDataDir (void);
inline uint8_t *u8_GFileGetHomeDir (void);
inline uint8_t *u8_GFileBuildName (uint8_t *dir, uint8_t *file);
inline uint8_t *u8_GFileBaseName (const uint8_t *file);
inline uint8_t *u8_GFileAppendFile (const uint8_t *dir, const uint8_t *name,
                                    bool isdir);

extern uint32_t *u32_GFileBaseName (const uint32_t *oldname);
extern uint32_t *u32_GFileNormalize (uint32_t *name);
extern uint32_t *u32_GFileAppendFile (const uint32_t *dir,
                                      const uint32_t *name, bool isdir);

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
