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

#include <basics.h>
#include <stdbool.h>

extern char *GFileGetUserConfigDir (void);
extern char *GFileGetUserCacheDir (void);
extern char *GFileGetUserDataDir (void);
extern char *GFileGetHomeDir (void);
extern char *GFileBuildName (char *dir, char *file);
extern char *GFileBaseName (const char *file);
extern char *GFileAppendFile (char *dir, char *name, bool isdir);
extern bool GFileIsAbsolute (const char *file);
extern bool GFileIsDir (const char *file);
extern bool GFileExists (const char *file);
extern bool GFileReadable (char *file);
extern int GFileMkDir (char *name);
extern int GFileUnlink (char *name);
extern unichar_t *u_GFileBaseName (const unichar_t *oldname);
extern unichar_t *u_GFileNormalize (unichar_t *name);
extern unichar_t *u_GFileAppendFile (unichar_t *dir, unichar_t *name, bool isdir);
extern bool u_GFileIsAbsolute (const unichar_t *file);

#endif
