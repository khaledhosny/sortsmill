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

enum giofuncs { gf_dir, gf_statfile, gf_getfile, gf_putfile,
	gf_mkdir, gf_delfile, gf_deldir, gf_renamefile,
	/*gf_lockfile, gf_unlockfile,*/
	gf_max };

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
    unsigned int done: 1;
    unsigned int direntrydata: 1;
    unsigned int abort: 1;
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
    unsigned int isdir: 1;
    unsigned int isexe: 1;
    unsigned int islnk: 1;
    unsigned int hasdir: 1;
    unsigned int hasexe: 1;
    unsigned int haslnk: 1;
    unsigned int hasmode: 1;
    unsigned int hassize: 1;
    unsigned int hastime: 1;
    unsigned int timezoneknown: 1;	/* We got a time, but we don't know the timezone. might be off by 24 hours either way */
    unsigned int fcdata: 2;
    short mode;
    uint32_t size;
    time_t modtime;
    struct gdirentry *next;
} GDirEntry;

VISIBLE extern void GIOdir(GIOControl *gc);
extern void GIOstatFile(GIOControl *gc);
VISIBLE extern void GIOfileExists(GIOControl *gc);
extern void GIOgetFile(GIOControl *gc);
extern void GIOputFile(GIOControl *gc);
VISIBLE extern void GIOmkDir(GIOControl *gc);
extern void GIOdelFile(GIOControl *gc);
extern void GIOdelDir(GIOControl *gc);
extern void GIOrenameFile(GIOControl *gc);
VISIBLE extern GDirEntry *GIOgetDirData(GIOControl *gc);
extern int32_t GIOread(GIOControl *gc,void *buffer,int32_t len);
extern int32_t GIOwrite(GIOControl *gc,void *buffer,int32_t len);
extern void GIOFreeDirEntries(GDirEntry *lst);
VISIBLE extern void GIOcancel(GIOControl *gc);
VISIBLE extern void GIOclose(GIOControl *gc);
VISIBLE extern GIOControl *GIOCreate(uint32_t *path,void *userdata,
	void (*receivedata)(struct giocontrol *),
	void (*receiveerror)(struct giocontrol *));
extern void GIOSetDefAuthorizer(int32_t (*getauth)(struct giocontrol *));
extern void GIOSetUserAgent(uint32_t *agent);

VISIBLE extern char *GIOGetMimeType(const char *path);

VISIBLE extern char *GIO_PasswordCache(char *proto,char *host,char *username,char *password);
VISIBLE extern char *_GIO_decomposeURL(const uint32_t *url,char **host, int *port, char **username,
				       char **password);

VISIBLE extern void GIO_SetThreadCallback(void (*callback)(void *,void *,void *));

#endif
