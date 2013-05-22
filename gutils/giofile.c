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

#include "basics.h"
#include "giofuncP.h"
#include "gfile.h"
#include "string.h"
#include "ustring.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <errno.h>
#include <xunistring.h>

/* the initial space is so that these guys will come first in ordered error */
/*  lists in the file chooser */
static uint32_t err401[] = { ' ','U','n','a','u','t','h','o','r','i','z','e','d', '\0' };
static uint32_t err403[] = { ' ','F','o','r','b','i','d','d','e','n', '\0' };
static uint32_t err404[] = { ' ','N','o','t',' ','F','o','u','n','d', '\0' };
static uint32_t err405[] = { ' ','M','e','t','h','o','d',' ','N','o','t',' ','A','l','l','o','w','e','d', '\0' };
static uint32_t err406[] = { ' ','N','o','t',' ','A','c','c','e','p','t','a','b','l','e', '\0' };
static uint32_t err409[] = { ' ','C','o','n','f','l','i','c','t', '\0' };
static uint32_t err412[] = { ' ','P','r','e','c','o','n','d','i','t','i','o','n',' ','F','a','i','l','e','d', '\0' };
static uint32_t err414[] = { ' ','R','e','q','u','e','s','t','-','U','R','I',' ','T','o','o',' ','L','o','n','g', '\0' };
static uint32_t err500[] = { ' ','I','n','t','e','r','n','a','l',' ','S','e','r','v','e','r',' ','E','r','r','o','r', '\0' };

static void GIOReporterror(GIOControl *gc, int errn) {

#if 1
    uc_strncpy(gc->status,strerror(errn),sizeof(gc->status)/sizeof(uint32_t));
#else
    if ( errn<sys_nerr )
	uc_strncpy(gc->status,sys_errlist[errn],sizeof(gc->status)/sizeof(uint32_t));
    else
	gc->status[0] = '\0';
#endif

    if ( errn==ENOENT || (gc->gf!=gf_dir && errn==ENOTDIR) ) {
	gc->return_code = 404;
	gc->error = err404;
    } else if ( errn==EACCES || errn==EPERM ) {
	gc->return_code = 401;
	gc->error = err401;
    } else if ( errn==EROFS || errn==ENOTEMPTY || errn==EBUSY ) {
	gc->return_code = 403;
	gc->error = err403;
    } else if ( errn==ENOTDIR || errn==EISDIR ) {
	gc->return_code = 405;
	gc->error = err405;
    } else if ( errn==EINVAL ) {
	gc->return_code = 406;
	gc->error = err406;
    } else if ( errn==EEXIST ) {
	gc->return_code = 409;
	gc->error = err409;
    } else if ( errn==ENOSPC || errn==EXDEV || errn==EMLINK) {
	gc->return_code = 412;
	gc->error = err412;
    } else if ( errn==ENAMETOOLONG ) {
	gc->return_code = 414;
	gc->error = err414;
    } else {
	gc->return_code = 500;
	gc->error = err500;
    }
    gc->done = true;
    (gc->receiveerror)(gc);
}

static void _gio_file_dir(GIOControl *gc,char *path) {
    DIR *dir;
    struct dirent *ent;
    GDirEntry *head=NULL, *last=NULL, *cur;
    char *buffer, *ept;
    struct stat statb;

    dir = opendir(path);
    if ( dir==NULL ) {
	GIOReporterror(gc,errno);
return;
    }

    buffer = (char *) xmalloc(strlen(path)+FILENAME_MAX+3);
    strcpy(buffer,path);
    ept = buffer+strlen(buffer);
    if ( ept[-1]!='/' )
	*ept++ = '/';

    while (( ent = readdir(dir))!=NULL ) {
	cur = (GDirEntry *) xcalloc(1,sizeof(GDirEntry));
	cur->name = x_u32_strconv_from_locale (ent->d_name);
	strcpy(ept,ent->d_name);
	stat(buffer,&statb);
	cur->hasdir = cur->hasexe = cur->hasmode = cur->hassize = cur->hastime = true;
	cur->size    = statb.st_size;
	cur->mode    = statb.st_mode;
	cur->modtime = statb.st_mtime;
	cur->isdir   = S_ISDIR(cur->mode);
	cur->isexe   = !cur->isdir && (cur->mode & 0100);
	cur->mimetype= x_u8_strdup_or_null (GIOGetMimeType (buffer));
	if ( last==NULL )
	    head = last = cur;
	else {
	    last->next = cur;
	    last = cur;
	}
    }
#if __CygWin
    /* Under cygwin we should give the user access to /cygdrive, even though */
    /*  a diropen("/") will not find it */
    if ( strcmp(path,"/")==0 ) {
	cur = (GDirEntry *) xcalloc(1,sizeof(GDirEntry));
	cur->name = x_u32_strconv_from_locale("cygdrive");
	strcpy(ept,"cygdrive");
	stat(buffer,&statb);
	cur->hasdir = cur->hasexe = cur->hasmode = cur->hassize = cur->hastime = true;
	cur->size    = statb.st_size;
	cur->mode    = statb.st_mode;
	cur->modtime = statb.st_mtime;
	cur->isdir   = S_ISDIR(cur->mode);
	cur->isexe   = !cur->isdir && (cur->mode & 0100);
	if ( last==NULL )
	    head = last = cur;
	else {
	    last->next = cur;
	    last = cur;
	}
    }
#endif
    closedir(dir);
    free(buffer);
    gc->iodata = head;
    gc->direntrydata = true;
    gc->return_code = 200;
    gc->done = true;
    (gc->receivedata)(gc);
}

static void _gio_file_statfile(GIOControl *gc,char *path)
{
  GDirEntry *cur;
  struct stat statb;

  if ( stat(path,&statb)==-1 )
    GIOReporterror(gc,errno);
  else
    {
      cur = (GDirEntry *) xcalloc(1,sizeof(GDirEntry));
      errno = 0;

      // FIXME: This is likely wrong, but we do _not_ want to allow
      // invalid UTF-8/32. In what encoding is 'path' supposed to be?
      // If UTF-8, why is it passed directly to 'stat', above? Is it
      // provably valid?
      if (u8_valid (path))
	{
	  cur->name = x_u8_to_u32 (u8_GFileBaseName(path));
	  cur->hasdir = cur->hasexe = cur->hasmode = cur->hassize = cur->hastime = true;
	  cur->size    = statb.st_size;
	  cur->mode    = statb.st_mode;
	  cur->modtime = statb.st_mtime;
	  cur->isdir   = S_ISDIR(cur->mode);
	  cur->isexe   = !cur->isdir && (cur->mode & 0100);
	  gc->iodata = cur;
	  gc->direntrydata = true;
	  gc->return_code = 200;
	  gc->done = true;
	  (gc->receivedata)(gc);
	}
      else
	GIOReporterror(gc,errno);
    }
}

static void _gio_file_mkdir(GIOControl *gc,char *path) {
    if ( GFileMkDir(path)==-1 ) {
	GIOReporterror(gc,errno);
    } else {
	gc->return_code = 201;
	gc->done = true;
	(gc->receivedata)(gc);
    }
}

void _GIO_localDispatch(GIOControl *gc) {
    char *path = u2def_copy(gc->path);
    char *topath;

    switch ( gc->gf ) {
      case gf_dir:
	_gio_file_dir(gc,path);
      break;
      case gf_statfile:
	_gio_file_statfile(gc,path);
      break;
      case gf_mkdir:
	_gio_file_mkdir(gc,path);
      break;
	free(topath);
      break;
    }
    free(path);
}

/* pathname preceded by "file://" just strip off the "file://" and treat as a */
/*  filename */
void *_GIO_fileDispatch(GIOControl *gc) {
    char *username, *password, *host, *path;
    int port;

    path = GIODecomposeURL(gc->path,&host,&port,&username,&password);
    free(host); free(username); free(password);
    switch ( gc->gf ) {
      case gf_dir:
	_gio_file_dir(gc,path);
      break;
      case gf_statfile:
	_gio_file_statfile(gc,path);
      break;
      case gf_mkdir:
	_gio_file_mkdir(gc,path);
      break;
    }
    free(path);
return( NULL );
}
