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
#ifndef _GIO_H
#define _GIO_H

#include <config.h>

#include <basics.h>
#include <time.h>

enum giofuncs { gf_dir, gf_statfile, gf_mkdir };

typedef struct giocontrol {
    uint32_t *path;
    uint32_t *origpath;		/* what the user asked for (before any redirects), NULL if path doesn't change */
    uint32_t *topath;			/* for renames and copies */
    void *userdata;
    struct gio_connectiondata *connectiondata;
    struct gio_threaddata *threaddata;
    void *iodata;
    void (*receivedata)(struct giocontrol *);
    void (*receiveintermediate)(struct giocontrol *);
    void (*receiveerror)(struct giocontrol *);
    bool done;
    bool direntrydata;
    bool abort;
    enum giofuncs gf;
    int protocol_index;
    struct giocontrol *next;
    int return_code;
    uint32_t *error;
    uint32_t status[80];
} GIOControl;
    
typedef struct gdirentry {
    uint32_t *name;
    char *mimetype;
    bool isdir;
    bool isexe;
    bool islnk;
    bool hasdir;
    bool hasexe;
    bool haslnk;
    bool hasmode;
    bool hassize;
    bool hastime;
    bool timezoneknown;	/* We got a time, but we don't know the timezone. might be off by 24 hours either way */
    unsigned int fcdata: 2;
    short mode;
    uint32_t size;
    time_t modtime;
    struct gdirentry *next;
} GDirEntry;

VISIBLE extern void GIOdir(GIOControl *gc);
VISIBLE extern void GIOfileExists(GIOControl *gc);
VISIBLE extern void GIOmkDir(GIOControl *gc);
VISIBLE extern GDirEntry *GIOgetDirData(GIOControl *gc);
VISIBLE extern void GIOcancel(GIOControl *gc);
VISIBLE extern void GIOclose(GIOControl *gc);
VISIBLE extern GIOControl *GIOCreate(uint32_t *path,void *userdata,
	void (*receivedata)(struct giocontrol *),
	void (*receiveerror)(struct giocontrol *));

VISIBLE extern char *GIOGetMimeType(const char *path);

VISIBLE extern char *GIO_PasswordCache(char *proto,char *host,char *username,char *password);
VISIBLE extern char *GIODecomposeURL(const uint32_t *url,char **host, int *port, char **username,
				       char **password);
VISIBLE extern void GIO_SetThreadCallback(void (*callback)(void *,void *,void *));

#endif
