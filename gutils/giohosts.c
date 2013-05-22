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

#include "giofuncP.h"
#include "gfile.h"
#include "ustring.h"
#include "utype.h"

#if !defined(__MINGW32__)
#include <netdb.h>
#endif

char *
GIODecomposeURL(const uint32_t *url,char **host, int *port, char **username,
		  char **password)
{
  uint32_t *pt, *pt2, *upt, *ppt;
  char *path;
  char proto[40];
  /* ftp://[user[:password]@]ftpserver[:port]/url-path */

  *username = NULL;
  *password = NULL;
  *port = -1;
  pt = uc_strstr(url,"://");
  if ( pt==NULL )
    {
      *host = NULL;
      return( x_u32_to_u8 (u32_force_valid (url)));
    }
  cu_strncpy(proto,url,pt-url<sizeof(proto)?pt-url:sizeof(proto));
  pt += 3;

  pt2 = u32_strchr(pt,'/');
  if ( pt2==NULL )
    {
      pt2 = pt+u32_strlen(pt);
      path = xstrdup("/");
    }
  else
    {
      path = x_u32_to_u8 (u32_force_valid (pt2));
    }

  upt = u32_strchr(pt,'@');
  if ( upt!=NULL && upt<pt2 )
    {
      ppt = u32_strchr(pt,':');
      if ( ppt==NULL )
	*username = x_u32_to_u8 (x_gc_u32_strmbndup (pt,upt-pt));
      else
	{
	  *username = x_u32_to_u8 (x_gc_u32_strmbndup (pt,ppt-pt));
	  *password = x_u32_to_u8 (x_gc_u32_strmbndup (ppt+1,upt-ppt-1));
	}
      pt = upt+1;
    }

  ppt = u32_strchr(pt,':');
  if ( ppt!=NULL && ppt<pt2 )
    {
      char *temp = x_u32_to_u8 (x_gc_u32_strmbndup (ppt+1,pt2-ppt-1));
      char *end;
      *port = strtol(temp,&end,10);
      if ( *end!='\0' )
	*port = -2;
      free(temp);
      pt2 = ppt;
    }
  *host = x_u32_to_u8 (x_gc_u32_strmbndup (pt,pt2-pt));
  if ( *username )
    *password = GIO_PasswordCache(proto,*host,*username,*password);
  return( path );
}

struct passwd_cache {
    char *proto;
    char *host;
    char *username;
    char *password;
};
static int pc_cnt = 0, pc_max=0;
struct passwd_cache *pc = NULL;

char *GIO_PasswordCache(char *proto,char *host,char *username,char *password) {
    int i;
#ifdef HAVE_PTHREAD_H
    static pthread_mutex_t mymutex = PTHREAD_MUTEX_INITIALIZER;
#endif

    if ( proto==NULL || host==NULL || username==NULL )
return( password );

#ifdef HAVE_PTHREAD_H
	pthread_mutex_lock(&mymutex);
#endif

    for ( i=0; i<pc_cnt; ++i ) {
	if ( strcasecmp(proto,pc[i].proto)==0 &&
		strcasecmp(host,pc[i].host)==0 &&
		strcmp(username,pc[i].username)==0 ) {
	    if ( password==NULL ) {
		password = xstrdup_or_null( pc[i].password );
 goto leave;
	    }
	    if ( strcmp(password,pc[i].password)!=0 ) {
		free( pc[i].password );
		pc[i].password = xstrdup_or_null( password );
	    }
 goto leave;
	}
    }

    if ( password==NULL )
 goto leave;

    if ( pc_cnt>=pc_max )
	pc = xrealloc(pc,(pc_max+=10)*sizeof(struct passwd_cache));
    pc[pc_cnt].proto = xstrdup_or_null( proto );
    pc[pc_cnt].host  = xstrdup_or_null( host  );
    pc[pc_cnt].username = xstrdup_or_null( username );
    pc[pc_cnt].password = xstrdup_or_null( password );
    ++pc_cnt;
 leave:
#ifdef HAVE_PTHREAD_H
    pthread_mutex_unlock(&mymutex);
#endif

return( password );
}
