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
#include "gdraw.h"
#include "ggadgetP.h"
#include "gwidgetP.h"
#include "../gdraw/gdrawP.h"
#include "gio.h"
#include "gfile.h"
#include "ustring.h"
#include "utype.h"
#include <invoke_funcs.h>

#include <stdlib.h>
#include <xgetcwd.h>
#include <sortsmill/core.h>
#include <xunistring.h>

/* This isn't really a gadget, it's just a collection of gadgets with some glue*/
/*  to make them work together. Therefore there are no expose routines here, */
/*  nor mouse, nor key. That's all handled by the individual gadgets themselves*/
/* Instead we've got a bunch of routines that make them work as a whole */
static GBox gfilechooser_box = GBOX_EMPTY; /* no box */
static uint32_t *lastdir;
static int showhidden = false;
static enum { dirs_mixed, dirs_first, dirs_separate } dir_placement = dirs_mixed;
static uint32_t **bookmarks = NULL;
static void *prefs_changed_data = NULL;
static void (*prefs_changed)(void *) = NULL;

static uint32_t *SubMatch(uint32_t *pattern, uint32_t *eop, uint32_t *name,int ignorecase) {
    uint32_t ch, *ppt, *npt, *ept, *eon;

    while ( pattern<eop && ( ch = *pattern)!='\0' ) {
	if ( ch=='*' ) {
	    if ( pattern[1]=='\0' )
return( name+u32_strlen(name));
	    for ( npt=name; ; ++npt ) {
		if ( (eon = SubMatch(pattern+1,eop,npt,ignorecase))!= NULL )
return( eon );
		if ( *npt=='\0' )
return( NULL );
	    }
	} else if ( ch=='?' ) {
	    if ( *name=='\0' )
return( NULL );
	    ++name;
	} else if ( ch=='[' ) {
	    /* [<char>...] matches the chars
	     * [<char>-<char>...] matches any char within the range (inclusive)
	     * the above may be concattenated and the resultant pattern matches
	     *		anything thing which matches any of them.
	     * [^<char>...] matches any char which does not match the rest of
	     *		the pattern
	     * []...] as a special case a ']' immediately after the '[' matches
	     *		itself and does not end the pattern
	     */
	    int found = 0, not=0;
	    ++pattern;
	    if ( pattern[0]=='^' ) { not = 1; ++pattern; }
	    for ( ppt = pattern; (ppt!=pattern || *ppt!=']') && *ppt!='\0' ; ++ppt ) {
		ch = *ppt;
		if ( ppt[1]=='-' && ppt[2]!=']' && ppt[2]!='\0' ) {
		    uint32_t ch2 = ppt[2];
		    if ( (*name>=ch && *name<=ch2) ||
			    (ignorecase && islower(ch) && islower(ch2) &&
				    *name>=toupper(ch) && *name<=toupper(ch2)) ||
			    (ignorecase && isupper(ch) && isupper(ch2) &&
				    *name>=tolower(ch) && *name<=tolower(ch2))) {
			if ( !not ) {
			    found = 1;
	    break;
			}
		    } else {
			if ( not ) {
			    found = 1;
	    break;
			}
		    }
		    ppt += 2;
		} else if ( ch==*name || (ignorecase && tolower(ch)==tolower(*name)) ) {
		    if ( !not ) {
			found = 1;
	    break;
		    }
		} else {
		    if ( not ) {
			found = 1;
	    break;
		    }
		}
	    }
	    if ( !found )
return( NULL );
	    while ( *ppt!=']' && *ppt!='\0' ) ++ppt;
	    pattern = ppt;
	    ++name;
	} else if ( ch=='{' ) {
	    /* matches any of a comma separated list of substrings */
	    for ( ppt = pattern+1; *ppt!='\0' ; ppt = ept ) {
		for ( ept=ppt; *ept!='}' && *ept!=',' && *ept!='\0'; ++ept );
		npt = SubMatch(ppt,ept,name,ignorecase);
		if ( npt!=NULL ) {
		    uint32_t *ecurly = ept;
		    while ( *ecurly!='}' && ecurly<eop && *ecurly!='\0' ) ++ecurly;
		    if ( (eon=SubMatch(ecurly+1,eop,npt,ignorecase))!=NULL )
return( eon );
		}
		if ( *ept=='}' )
return( NULL );
		if ( *ept==',' ) ++ept;
	    }
	} else if ( ch==*name ) {
	    ++name;
	} else if ( ignorecase && tolower(ch)==tolower(*name)) {
	    ++name;
	} else
return( NULL );
	++pattern;
    }
return( name );
}

/* Handles *?{}[] wildcards */
int GGadgetWildMatch(uint32_t *pattern, uint32_t *name,int ignorecase) {
    uint32_t *eop = pattern + u32_strlen(pattern);

    if ( pattern==NULL )
return( true );

    name = SubMatch(pattern,eop,name,ignorecase);
    if ( name==NULL )
return( false );
    if ( *name=='\0' )
return( true );

return( false );
}

enum fchooserret
GFileChooserDefFilter(GGadget *g,GDirEntry *ent,const uint32_t *dir)
{
  GFileChooser *gfc = (GFileChooser *) g;
  int i;
  char *mime;

  uint8_t *utf8_ent_name = x_gc_u32_to_u8 (u32_force_valid (ent->name));

  if ( u8_strcmp(utf8_ent_name,".")==0 )	/* Don't show the current directory entry */
    return( fc_hide );
  if ( gfc->wildcard!=NULL && *gfc->wildcard=='.' )
    /* If they asked for hidden files, show them */;
  else if ( !showhidden && ent->name[0]=='.' && u8_strcmp(utf8_ent_name,"..")!=0 )
    return( fc_hide );
  if ( ent->isdir )			/* Show all other directories */
    return( fc_show );
  if ( gfc->wildcard==NULL && gfc->mimetypes==NULL )
    return( fc_show );
  /* If we've got a wildcard, and it matches, then show */
  if ( gfc->wildcard!=NULL && GGadgetWildMatch(gfc->wildcard,ent->name,true))
    return( fc_show );
  /* If no mimetypes then we're done */
  if ( gfc->mimetypes==NULL )
    return( fc_hide );
  /* match the mimetypes */
  mime = ent->mimetype==NULL?GIOGetMimeType(utf8_ent_name):ent->mimetype;
  for ( i=0; gfc->mimetypes[i]!=NULL; ++i )
    if (strcasecmp(gfc->mimetypes[i], mime) == 0)
      return( fc_show );

  return( fc_hide );
}

static char *
GFileChooserPickIcon(GDirEntry *e)
{
  char *mime = e->mimetype;
  uint8_t *utf8_ent_name = x_gc_u32_to_u8 (u32_force_valid (e->name));

  if (e->isdir) {
    if (u8_strcmp(utf8_ent_name,"..") == 0)
      return "chooserupdir.png";
    return "chooserdir.png";
  }

  if (mime == NULL)
    mime = GIOGetMimeType(utf8_ent_name);

  if (strncasecmp("text/", mime, 5) == 0) {
    if (strcasecmp("text/html", mime) == 0)
      return "choosertexthtml.png";
    if (strcasecmp("text/xml", mime) == 0)
      return "choosertextxml.png";
    if (strcasecmp("text/css", mime) == 0)
      return "choosertextcss.png";
    if (strcasecmp("text/c", mime) == 0)
      return "choosertextc.png";
    if (strcasecmp("text/java", mime) == 0)
      return "choosertextjava.png";
    if (strcasecmp("text/x-makefile", mime) == 0)
      return "choosertextmake.png";
    if (strcasecmp("text/fontps", mime) == 0)
      return "choosertextfontps.png";
    if (strcasecmp("text/font", mime) == 0)
      return "choosertextfontbdf.png";
    if (strcasecmp("text/ps", mime) == 0)
      return "choosertextps.png";

    return "choosertextplain.png";
  }

  if (strncasecmp("image/", mime, 6) == 0)
    return "chooserimage.png";
  if (strncasecmp("video/", mime, 6) == 0)
    return "chooservideo.png";
  if (strncasecmp("audio/", mime, 6) == 0)
    return "chooseraudio.png";
  if (strcasecmp("application/x-navidir", mime) == 0 ||
      strcasecmp("inode/directory", mime) == 0)
    return "chooserdir.png";
  if (strcasecmp("application/x-object", mime) == 0)
    return "chooserobject.png";
  if (strcasecmp("application/x-core", mime) == 0)
    return "choosercore.png";
  if (strcasecmp("application/x-tar", mime) == 0)
    return "choosertar.png";
  if (strcasecmp("application/x-compressed", mime) == 0)
    return "choosercompressed.png";
  if (strcasecmp("application/pdf", mime) == 0)
    return "choosertexthtml.png";
  if (strcasecmp("application/vnd.font-fontforge-sfd", mime) == 0)
    return "choosertextfontsfd.png";
  if (strcasecmp("application/x-font-type1", mime) == 0)
    return "choosertextfontps.png";
  if (strcasecmp("application/x-font-ttf", mime) == 0 ||
      strcasecmp("application/x-font-otf", mime) == 0)
    return "chooserttf.png";
  if (strcasecmp("application/x-font-cid", mime) == 0 )
    return "choosercid.png";
  if (strcasecmp("application/x-macbinary", mime) == 0 ||
      strcasecmp("application/x-mac-binhex40", mime) == 0 )
    return "choosermac.png";
  if (strcasecmp("application/x-mac-dfont", mime) == 0 ||
      strcasecmp("application/x-mac-suit", mime) == 0 )
    return "choosermacttf.png";
  if (strcasecmp("application/x-font-pcf", mime) == 0 ||
      strcasecmp("application/x-font-snf", mime) == 0)
    return "choosertextfontbdf.png";

  return "chooserunknown.png";
}

static void GFileChooserFillList(GFileChooser *gfc,GDirEntry *first,
	const uint32_t *dir) {
    GDirEntry *e;
    int len, dlen;
    GTextInfo **ti, **dti;

    len = dlen = 0;
    for ( e=first; e!=NULL; e=e->next ) {
	e->fcdata = (gfc->filter)(&gfc->g,e,dir);
	if ( e->fcdata!=fc_hide ) {
	    if ( e->isdir )
		++dlen;
	    else
		++len;
	}
    }

    if ( dir_placement == dirs_separate ) {
	ti = xmalloc((len+1)*sizeof(GTextInfo *));
	dti = xmalloc((dlen+1)*sizeof(GTextInfo *));
	len = dlen = 0;
	for ( e=first; e!=NULL; e=e->next ) {
	    if ( e->fcdata!=fc_hide ) {
		GTextInfo **me;
		if ( e->isdir )
		    me = &dti[dlen++];
		else
		    me = &ti[len++];

		*me = xcalloc(1,sizeof(GTextInfo));
		(*me)->text = x_u32_strdup_or_null(e->name);
		(*me)->image = (GImage *) GFileChooserPickIcon(e);
		(*me)->fg = COLOR_DEFAULT;
		(*me)->bg = COLOR_DEFAULT;
		(*me)->font = NULL;
		(*me)->disabled = e->fcdata==fc_showdisabled;
		(*me)->image_precedes = true;
		(*me)->checked = e->isdir;
	    }
	}
	ti[len] = xcalloc(1,sizeof(GTextInfo));
	dti[dlen] = xcalloc(1,sizeof(GTextInfo));
	GGadgetSetList(&gfc->files->g,ti,false);
	GGadgetSetList(&gfc->subdirs->g,dti,false);
    } else {
	ti = xmalloc((len+dlen+1)*sizeof(GTextInfo *));
	len = 0;
	for ( e=first; e!=NULL; e=e->next ) {
	    if ( e->fcdata!=fc_hide ) {
		ti[len] = xcalloc(1,sizeof(GTextInfo));
		ti[len]->text = x_u32_strdup_or_null(e->name);
		ti[len]->image = (GImage *) GFileChooserPickIcon(e);
		ti[len]->fg = COLOR_DEFAULT;
		ti[len]->bg = COLOR_DEFAULT;
		ti[len]->font = NULL;
		ti[len]->disabled = e->fcdata==fc_showdisabled;
		ti[len]->image_precedes = true;
		ti[len]->checked = e->isdir;
		ti[len]->sort_me_first_in_list =
		  (dir_placement == dirs_first && e->isdir);
		++len;
	    }
	}
	ti[len] = xcalloc(1,sizeof(GTextInfo));
	GGadgetSetList(&gfc->files->g,ti,false);
    }

    GGadgetScrollListToText(&gfc->files->g,u32_GFileBaseName(_GGadgetGetTitle(&gfc->name->g)),true);
}

static void GFileChooserIntermediateDir(GIOControl *gc) {
    GFileChooser *gfc = (GFileChooser *) (gc->userdata);

    GFileChooserFillList(gfc,GIOgetDirData(gc),gc->path);
}

static void GFileChooserReceiveDir(GIOControl *gc) {
    GFileChooser *gfc = (GFileChooser *) (gc->userdata);

    GGadgetSetEnabled(&gfc->files->g,true);
    GGadgetSetEnabled(&gfc->subdirs->g,true);
    if ( gfc->lastname!=NULL ) {
	GGadgetSetTitle(&gfc->name->g,gfc->lastname);
	free(gfc->lastname);
	gfc->lastname=NULL;
    }
    GFileChooserFillList(gfc,GIOgetDirData(gc),gc->path);
    GIOclose(gc);
    gfc->outstanding = NULL;
    GDrawSetCursor(gfc->g.base,gfc->old_cursor);
}

static void GFileChooserErrorDir(GIOControl *gc) {
    GFileChooser *gfc = (GFileChooser *) (gc->userdata);
    GTextInfo *ti[3], _ti[3];
    static uint32_t nullstr[] = { 0 };

    memset(_ti,'\0',sizeof(_ti));
    _ti[0].text = gc->error;
    if ( gc->status[0]!='\0' )
	_ti[1].text = gc->status;
    _ti[0].fg = _ti[0].bg = _ti[1].fg = _ti[1].bg = COLOR_DEFAULT;
    ti[0] = _ti; ti[1] = _ti+1; ti[2] = _ti+2;
    GGadgetSetEnabled(&gfc->files->g,false);
    GGadgetSetList(&gfc->files->g,ti,true);
    GGadgetSetEnabled(&gfc->subdirs->g,false);
    GGadgetSetList(&gfc->subdirs->g,ti,true);
    if ( gfc->lastname!=NULL ) {
	GGadgetSetTitle(&gfc->name->g,gfc->lastname);
	free(gfc->lastname);
	gfc->lastname=NULL;
    } else
	GGadgetSetTitle(&gfc->name->g,nullstr);
    if ( gfc->filterb!=NULL && gfc->ok!=NULL ) 
	_GWidget_MakeDefaultButton(&gfc->ok->g);
    GIOcancel(gc);
    gfc->outstanding = NULL;
    GDrawSetCursor(gfc->g.base,gfc->old_cursor);
}

static void GFileChooserScanDir(GFileChooser *gfc,uint32_t *dir) {
    GTextInfo **ti=NULL;
    int cnt, tot=0;
    uint32_t *pt, *ept, *freeme;

    dir = u32_GFileNormalize(dir);
    while ( 1 ) {
	ept = dir;
	cnt = 0;
	if ( (pt=uc_strstr(dir,"://"))!=NULL ) {
	    ept = u32_strchr(pt+3,'/');
	    if ( ept==NULL )
		ept = pt+u32_strlen(pt);
	    else
		++ept;
	} else if ( *dir=='/' )
	    ept = dir+1;
	if ( ept!=dir ) {
	    if ( ti!=NULL ) {
		ti[tot-cnt] = xcalloc(1,sizeof(GTextInfo));
		ti[tot-cnt]->text = x_u32_strmbndup (dir,ept-dir);
		ti[tot-cnt]->fg = ti[tot-cnt]->bg = COLOR_DEFAULT;
	    }
	    cnt = 1;
	}
	for ( pt = ept; *pt!='\0'; pt = ept ) {
	    for ( ept = pt; *ept!='/' && *ept!='\0'; ++ept );
	    if ( ti!=NULL ) {
		ti[tot-cnt] = xcalloc(1,sizeof(GTextInfo));
		ti[tot-cnt]->text = x_u32_strmbndup (pt,ept-pt);
		ti[tot-cnt]->fg = ti[tot-cnt]->bg = COLOR_DEFAULT;
	    }
	    ++cnt;
	    if ( *ept=='/' ) ++ept;
	}
	if ( ti!=NULL )
    break;
	ti = xmalloc((cnt+1)*sizeof(GTextInfo *));
	tot = cnt-1;
    }
    ti[cnt] = xcalloc(1,sizeof(GTextInfo));

    GGadgetSetList(&gfc->directories->g,ti,false);
    GGadgetSelectOneListItem(&gfc->directories->g,0);

    /* Password management for URLs */
    if ( (pt = uc_strstr(dir,"://"))!=NULL ) {
	int port;
	char proto[40];
	char *host, *username, *password;
	free( GIODecomposeURL(dir,&host,&port,&username,&password));
	if ( username!=NULL && password==NULL ) {
	    password = gwwv_ask_password(_("Password?"),"",_("Enter password for %s@%s"), username, host );
	    cu_strncpy(proto,dir,pt-dir<sizeof(proto)?pt-dir:sizeof(proto));
	    password = GIO_PasswordCache(proto,host,username,password);
	}
	free(host); free(username); free(password);
    }

    if ( gfc->outstanding!=NULL ) {
	GIOcancel(gfc->outstanding);
	gfc->outstanding = NULL;
    } else {
	gfc->old_cursor = GDrawGetCursor(gfc->g.base);
	GDrawSetCursor(gfc->g.base,ct_watch);
    }

    gfc->outstanding = GIOCreate(dir,gfc,GFileChooserReceiveDir,
	    GFileChooserErrorDir);
    gfc->outstanding->receiveintermediate = GFileChooserIntermediateDir;
    GIOdir(gfc->outstanding);

    freeme = NULL;
    if ( dir[u32_strlen(dir)-1]!='/' ) {
	freeme = xmalloc((u32_strlen(dir)+3)*sizeof(uint32_t));
	u32_strcpy(freeme,dir);
	u32_strcat (freeme, x_gc_u8_to_u32 ("/"));
	dir = freeme;
    }
    if ( gfc->hpos>=gfc->hmax )
	gfc->history = xrealloc(gfc->history,(gfc->hmax+20)*sizeof(uint32_t *));
    if ( gfc->hcnt==0 ) {
	gfc->history[gfc->hcnt++] = x_u32_strdup_or_null(dir);
    } else if ( u32_strcmp(gfc->history[gfc->hpos],dir)==0 ) // FIXME: Should this be a normalized comparison?
	/* Just a refresh */;
    else {
	gfc->history[++gfc->hpos] = x_u32_strdup_or_null(dir);
	gfc->hcnt = gfc->hpos+1;
    }
    free(freeme);
}

/* Handle events from the text field */
static int GFileChooserTextChanged(GGadget *t,GEvent *e) {
    GFileChooser *gfc;
    const uint32_t *pt, *spt;

    if ( e->type!=et_controlevent || e->u.control.subtype!=et_textchanged )
return( true );
    pt = spt = _GGadgetGetTitle(t);
    if ( pt==NULL )
return( true );
    while ( *pt && *pt!='*' && *pt!='?' && *pt!='[' && *pt!='{' )
	++pt;
    if ( *spt!='\0' && spt[u32_strlen(spt)-1]=='/' )
	pt = spt+u32_strlen(spt)-1;
    gfc = (GFileChooser *) GGadgetGetUserData(t);

    /* if there are no wildcards and no directories in the filename */
    /*  then as it gets changed the list box should show the closest */
    /*  approximation to it */
    if ( *pt=='\0' ) {
	GGadgetScrollListToText(&gfc->files->g,spt,true);
	if ( gfc->filterb!=NULL && gfc->ok!=NULL ) 
	    _GWidget_MakeDefaultButton(&gfc->ok->g);
    } else if ( *pt=='/' && e->u.control.u.tf_changed.from_pulldown!=-1 ) {
	GEvent e;
	e.type = et_controlevent;
	e.u.control.subtype = et_buttonactivate;
	e.u.control.g = (gfc->ok!=NULL?&gfc->ok->g:&gfc->g);
	e.u.control.u.button.clicks = 0;
	e.w = e.u.control.g->base;
	if ( e.u.control.g->handle_controlevent != NULL )
	    (e.u.control.g->handle_controlevent)(e.u.control.g,&e);
	else
	    GDrawPostEvent(&e);
    } else {
	if ( gfc->filterb!=NULL && gfc->ok!=NULL ) 
	    _GWidget_MakeDefaultButton(&gfc->filterb->g);
    }
    free(gfc->lastname); gfc->lastname = NULL;
return( true );
}

static uint32_t **GFileChooserCompletion(GGadget *t,int from_tab) {
    GFileChooser *gfc;
    const uint32_t *pt, *spt; uint32_t **ret;
    GTextInfo **ti;
    int32_t len;
    int i, cnt, doit, match_len;

    pt = spt = _GGadgetGetTitle(t);
    if ( pt==NULL )
return( NULL );
    while ( *pt && *pt!='/' && *pt!='*' && *pt!='?' && *pt!='[' && *pt!='{' )
	++pt;
    if ( *pt!='\0' )
return( NULL );		/* Can't complete if not in cur directory or has wildcards */

    match_len = u32_strlen(spt);
    gfc = (GFileChooser *) GGadgetGetUserData(t);
    ti = GGadgetGetList(&gfc->files->g,&len);
    ret = NULL;
    for ( doit=0; doit<2; ++doit ) {
	cnt=0;
	for ( i=0; i<len; ++i ) {
	  // FIXME: Should this u32_strncmp be a normalized comparison?
	  if ( u32_strncmp(ti[i]->text,spt,match_len)==0 ) {
		if ( doit ) {
		    if ( ti[i]->checked /* isdirectory */ ) {
			int len = u32_strlen(ti[i]->text);
			ret[cnt] = xmalloc((len+2)*sizeof(uint32_t));
			u32_strcpy(ret[cnt],ti[i]->text);
			ret[cnt][len] = '/';
			ret[cnt][len+1] = '\0';
		    } else 
			ret[cnt] = x_u32_strdup_or_null(ti[i]->text);
		}
		++cnt;
	    }
	}
	if ( doit )
	    ret[cnt] = NULL;
	else if ( cnt==0 )
return( NULL );
	else
	    ret = xmalloc((cnt+1)*sizeof(uint32_t *));
    }
return( ret );
}

static uint32_t *GFileChooserGetCurDir(GFileChooser *gfc,int dirindex) {
    GTextInfo **ti;
    int32_t len; int j, cnt;
    uint32_t *dir, *pt;

    ti = GGadgetGetList(&gfc->directories->g,&len);
    if ( dirindex==-1 )
	dirindex = 0;
    dirindex = dirindex;

    for ( j=len-1, cnt=0; j>=dirindex; --j )
	cnt += u32_strlen(ti[j]->text)+1;
    pt = dir = xmalloc((cnt+1)*sizeof(uint32_t));
    for ( j=len-1; j>=dirindex; --j ) {
	u32_strcpy(pt,ti[j]->text);
	pt += u32_strlen(pt);
	if ( pt[-1]!='/' )
	    *pt++ = '/';
    }
    *pt = '\0';
return( dir );
}

/* Handle events from the directory dropdown list */
static int GFileChooserDListChanged(GGadget *pl,GEvent *e) {
    GFileChooser *gfc;
    int i; /*int32_t len;*/
    uint32_t *dir;
    /*GTextInfo **ti;*/

    if ( e->type!=et_controlevent || e->u.control.subtype!=et_listselected )
return( true );
    i = GGadgetGetFirstListSelectedItem(pl);
    if ( i==-1 )
return(true);
    /*ti = GGadgetGetList(pl,&len);*/
    if ( i==0 )		/* Nothing changed */
return( true );

    gfc = (GFileChooser *) GGadgetGetUserData(pl);
    dir = GFileChooserGetCurDir(gfc,i);
    GFileChooserScanDir(gfc,dir);
    free(dir);
return( true );
}

/* Handle events from the file list list */
static int GFileChooserFListSelected(GGadget *gl,GEvent *e) {
    GFileChooser *gfc;
    int i;
    int32_t listlen; int len, cnt, dirpos, apos;
    uint32_t *dir, *newdir;
    GTextInfo *ti, **all;

    if ( e->type!=et_controlevent ||
	    ( e->u.control.subtype!=et_listselected &&
	      e->u.control.subtype!=et_listdoubleclick ))
return( true );
    if ( ((GDList *) gl)->multiple_sel ) {
	all = GGadgetGetList(gl,&listlen);
	len = cnt = 0;
	dirpos = apos = -1;
	for ( i=0; i<listlen; ++i ) {
	    if ( !all[i]->selected )
		/* Not selected? ignore it */;
	    else if ( all[i]->checked )		/* Directory */
		dirpos = i;
	    else {
		len += u32_strlen( all[i]->text ) + 2;
		++cnt;
		apos = i;
	    }
	}
	if ( dirpos!=-1 && cnt>0 ) {
	    /* If a directory was selected, clear everthing else */
	    for ( i=0; i<listlen; ++i ) if ( i!=dirpos )
		all[i]->selected = false;
	    _ggadget_redraw(gl);
	}
	if ( dirpos!=-1 ) { cnt = 1; apos = dirpos; }
    } else {
	cnt = 1;
	apos = GGadgetGetFirstListSelectedItem(gl);
    }
    if ( apos==-1 )
return(true);
    gfc = (GFileChooser *) GGadgetGetUserData(gl);
    ti = GGadgetGetListItem(gl,apos);
    if ( e->u.control.subtype==et_listselected && cnt==1 ) {
	/* Nope, quite doesn't work. Goal is to remember first filename. But */
	/*  if user types into the list box we'll (probably) get several diff*/
	/*  filenames before we hit the directory. So we just ignore the typ-*/
	/*  ing case for now. */
	if ( ti->checked /* it's a directory */ && e->u.control.u.list.from_mouse &&
		gfc->lastname==NULL )
	    gfc->lastname = GGadgetGetTitle(&gfc->name->g);
	if ( ti->checked ) {
	    uint32_t *val = xmalloc((u32_strlen(ti->text)+2)*sizeof(uint32_t));
	    u32_strcpy(val,ti->text);
	    u32_strcat (val, x_gc_u8_to_u32 ("/"));
	    GGadgetSetTitle(&gfc->name->g,val);
	    free(val);
	    if ( gfc->filterb!=NULL && gfc->ok!=NULL )
		_GWidget_MakeDefaultButton(&gfc->filterb->g);
	} else {
	    GGadgetSetTitle(&gfc->name->g,ti->text);
	    if ( gfc->filterb!=NULL && gfc->ok!=NULL )
		_GWidget_MakeDefaultButton(&gfc->ok->g);
	    free(gfc->lastname);
	    gfc->lastname = NULL;
	}
    } else if ( e->u.control.subtype==et_listselected ) {
	uint32_t *val, *upt = xmalloc((len+1)*sizeof(uint32_t));
	val = upt;
	for ( i=0; i<listlen; ++i ) {
	    if ( all[i]->selected ) {
		u32_strcpy(upt,all[i]->text);
		upt += u32_strlen(upt);
		if ( --cnt>0 ) {
		    u32_strcpy(upt, x_gc_u8_to_u32 ("; "));
		    upt += 2;
		}
	    }
	}
	GGadgetSetTitle(&gfc->name->g,val);
	free(val);
    } else if ( ti->checked /* it's a directory */ ) {
	dir = GFileChooserGetCurDir(gfc,-1);
	newdir = u32_GFileAppendFile(dir,ti->text,true);
	GFileChooserScanDir(gfc,newdir);
	free(dir);
    } else {
	/* Post the double click (on a file) to the parent */
	/*  if we know what the ok button is then pretend it got pressed */
	/*  otherwise just send a list double click from ourselves */
	if ( gfc->ok!=NULL ) {
	    e->u.control.subtype = et_buttonactivate;
	    e->u.control.g = &gfc->ok->g;
	    e->u.control.u.button.clicks = 0;
	    e->w = e->u.control.g->base;
	} else
	    e->u.control.g = &gfc->g;
	if ( e->u.control.g->handle_controlevent != NULL )
	    (e->u.control.g->handle_controlevent)(e->u.control.g,e);
	else
	    GDrawPostEvent(e);
    }
return( true );
}

VISIBLE void GFCHideToggle(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;

    showhidden = !showhidden;

    dir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,dir);
    free(dir);

    if ( prefs_changed!=NULL )
	(prefs_changed)(prefs_changed_data);
}

static void GFCRemetric(GFileChooser *gfc) {
    GRect size;

    GGadgetGetSize(&gfc->topbox->g,&size);
    GGadgetResize(&gfc->topbox->g,size.width,size.height);
}

VISIBLE void GFCDirsAmidToggle(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;

    if ( dir_placement==dirs_separate ) {
	GGadgetSetVisible(&gfc->subdirs->g,false);
	GFCRemetric(gfc);
    }
    dir_placement = dirs_mixed;

    dir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,dir);
    free(dir);

    if ( prefs_changed!=NULL )
	(prefs_changed)(prefs_changed_data);
}

VISIBLE void GFCDirsFirstToggle(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;

    if ( dir_placement==dirs_separate ) {
	GGadgetSetVisible(&gfc->subdirs->g,false);
	GFCRemetric(gfc);
    }
    dir_placement = dirs_first;

    dir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,dir);
    free(dir);

    if ( prefs_changed!=NULL )
	(prefs_changed)(prefs_changed_data);
}

VISIBLE void GFCDirsSeparateToggle(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;

    if ( dir_placement!=dirs_separate ) {
	GGadgetSetVisible(&gfc->subdirs->g,true);
	GFCRemetric(gfc);
    }
    dir_placement = dirs_separate;

    dir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,dir);
    free(dir);

    if ( prefs_changed!=NULL )
	(prefs_changed)(prefs_changed_data);
}

VISIBLE void GFCRefresh(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;

    dir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,dir);
    free(dir);
}

static int GFileChooserHome(GGadget *g, GEvent *e)
{
  if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate )
    {
      uint32_t* homedir = x_u32_strconv_from_locale (GFileGetHomeDir());
      if ( homedir==NULL )
	GGadgetSetEnabled(g,false);
      else
	{
	  GFileChooser *gfc = (GFileChooser *) GGadgetGetUserData(g);
	  GFileChooserScanDir(gfc,homedir);
	  free(homedir);
	}
    }
  return( true );
}

static int GFileChooserUpDirButton(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate ) {
	GFileChooser *gfc = (GFileChooser *) GGadgetGetUserData(g);
	uint32_t *dir, *newdir;
	static uint32_t dotdot[] = { '.', '.',  0 };
	dir = GFileChooserGetCurDir(gfc,-1);
	newdir = u32_GFileAppendFile(dir,dotdot,true);
	GFileChooserScanDir(gfc,newdir);
	free(dir);
    }
return( true );
}

// *INDENT-OFF*

static GMenuItem gfcpopupmenu[] = {
    {
      .ti = {
	.text = (uint32_t *) N_("Show Hidden Files"),
	.image = NULL,
	.fg = COLOR_DEFAULT,
	.bg = COLOR_DEFAULT,
	.userdata = NULL,
	.font = NULL,
	.disabled = 0,
	.image_precedes = 0,
	.checkable = 1,
	.checked = 0,
	.selected = 0,
	.line = 0,
	.text_is_1byte = 1,
	.text_has_mnemonic = 0,
	.changed = 0,
	.mnemonic = 'H'
      },
      .shortcut_char = '\0',
      .short_mask = ksm_control,
      .sub = NULL,
      .moveto = NULL,
      .invoke = GFCHideToggle,
      .mid = 0
    },
    {
      .ti = {
	.text = (uint32_t *) N_("Directories Amid Files"),
	.image = NULL,
	.fg = COLOR_DEFAULT,
	.bg = COLOR_DEFAULT,
	.userdata = NULL,
	.font = NULL,
	.disabled = 0,
	.image_precedes = 0,
	.checkable = 1,
	.checked = 0,
	.selected = 0,
	.line = 0,
	.text_is_1byte = 1,
	.text_has_mnemonic = 0,
	.changed = 0,
	.mnemonic = 'H'
      },
      .shortcut_char = '\0',
      .short_mask = ksm_control,
      .sub = NULL,
      .moveto = NULL,
      .invoke = GFCDirsAmidToggle,
      .mid = 0
    },
    {
      .ti = {
	.text = (uint32_t *) N_("Directories First"),
	.image = NULL,
	.fg = COLOR_DEFAULT,
	.bg = COLOR_DEFAULT,
	.userdata = NULL,
	.font = NULL,
	.disabled = 0,
	.image_precedes = 0,
	.checkable = 1,
	.checked = 0,
	.selected = 0,
	.line = 0,
	.text_is_1byte = 1,
	.text_has_mnemonic = 0,
	.changed = 0,
	.mnemonic = 'H'
      },
      .shortcut_char = '\0',
      .short_mask = ksm_control,
      .sub = NULL,
      .moveto = NULL,
      .invoke = GFCDirsFirstToggle,
      .mid = 0
    },
    {
      .ti = {
	.text = (uint32_t *) N_("Directories Separate"),
	.image = NULL,
	.fg = COLOR_DEFAULT,
	.bg = COLOR_DEFAULT,
	.userdata = NULL,
	.font = NULL,
	.disabled = 0,
	.image_precedes = 0,
	.checkable = 1,
	.checked = 0,
	.selected = 0,
	.line = 0,
	.text_is_1byte = 1,
	.text_has_mnemonic = 0,
	.changed = 0,
	.mnemonic = 'H'
      },
      .shortcut_char = '\0',
      .short_mask = ksm_control,
      .sub = NULL,
      .moveto = NULL,
      .invoke = GFCDirsSeparateToggle,
      .mid = 0
    },
    {
      .ti = {
	.text = (uint32_t *) N_("Refresh File List"),
	.image = NULL,
	.fg = COLOR_DEFAULT,
	.bg = COLOR_DEFAULT,
	.userdata = NULL,
	.font = NULL,
	.disabled = 0,
	.image_precedes = 0,
	.checkable = 0,
	.checked = 0,
	.selected = 0,
	.line = 0,
	.text_is_1byte = 1,
	.text_has_mnemonic = 0,
	.changed = 0,
	.mnemonic = 'H'
      },
      .shortcut_char = '\0',
      .short_mask = ksm_control,
      .sub = NULL,
      .moveto = NULL,
      .invoke = GFCRefresh,
      .mid = 0
    },
    GMENUITEM_EMPTY
};

// *INDENT-ON*

static int gotten=false;

static void GFCPopupMenu(GGadget *g, GEvent *e) {
    int i;
    GFileChooser *gfc = (GFileChooser *) GGadgetGetUserData(g);

    for ( i=0; gfcpopupmenu[i].ti.text!=NULL || gfcpopupmenu[i].ti.line; ++i )
	gfcpopupmenu[i].ti.userdata = gfc;
    gfcpopupmenu[0].ti.checked = showhidden;
    gfcpopupmenu[1].ti.checked = dir_placement == dirs_mixed;
    gfcpopupmenu[2].ti.checked = dir_placement == dirs_first;
    gfcpopupmenu[3].ti.checked = dir_placement == dirs_separate;
    if ( !gotten ) {
	gotten = true;
	for ( i=0; gfcpopupmenu[i].ti.text!=NULL || gfcpopupmenu[i].ti.line; ++i )
	    gfcpopupmenu[i].ti.text = (uint32_t *) _( (char *) gfcpopupmenu[i].ti.text);
    }
    GMenuCreatePopupMenu(g->base,e, gfcpopupmenu);
}

static int GFileChooserConfigure(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonpress ) {
	GEvent fake;
	GRect pos;
	GGadgetGetSize(g,&pos);
	memset(&fake,0,sizeof(fake));
	fake.type = et_mousedown;
	fake.w = g->base;
	fake.u.mouse.x = pos.x;
	fake.u.mouse.y = pos.y+pos.height;
	GFCPopupMenu(g,&fake);
    }
return( true );
}

VISIBLE void GFCBack(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);

    if ( gfc->hpos<=0 )
return;
    --gfc->hpos;
    GFileChooserScanDir(gfc,gfc->history[gfc->hpos]);
}

VISIBLE void GFCForward(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);

    if ( gfc->hpos+1>=gfc->hcnt )
return;
    ++gfc->hpos;
    GFileChooserScanDir(gfc,gfc->history[gfc->hpos]);
}

VISIBLE void GFCAddCur(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    uint32_t *dir;
    int bcnt;

    dir = GFileChooserGetCurDir(gfc,-1);
    bcnt = 0;
    if ( bookmarks!=NULL )
	for ( ; bookmarks[bcnt]!=NULL; ++bcnt );
    bookmarks = xrealloc(bookmarks,(bcnt+2)*sizeof(uint32_t *));
    bookmarks[bcnt] = dir;
    bookmarks[bcnt+1] = NULL;

    if ( prefs_changed!=NULL )
	(prefs_changed)(prefs_changed_data);
}

VISIBLE void
GFCRemoveBook(GWindow gw,struct gmenuitem *mi,GEvent *e)
{
  int i, bcnt;
  char **books;
  char *sel;
  char *buts[2];

  if ( bookmarks != NULL && bookmarks[0] != NULL )
    {
      for ( bcnt=0; bookmarks[bcnt]!=NULL; ++bcnt )
	;
      sel = xcalloc(bcnt,1);
      books = xcalloc(bcnt+1,sizeof(char *));
      for ( bcnt=0; bookmarks[bcnt]!=NULL; ++bcnt )
	books[bcnt] = x_u32_to_u8 (bookmarks[bcnt]);
      books[bcnt] = NULL;
      buts[0] = _("_Remove");
      buts[1] = _("_Cancel");
      if ( GWidgetChoicesBM8( _("Remove bookmarks"),(const char **) books,sel,bcnt,buts,
			      _("Remove selected bookmarks"))==0 )
	{
	  for ( i=bcnt=0; bookmarks[bcnt]!=NULL; ++bcnt )
	    {
	      if ( sel[bcnt] )
		free(bookmarks[bcnt]);
	      else
		bookmarks[i++] = bookmarks[bcnt];
	    }
	  bookmarks[i] = NULL;

	  if ( prefs_changed!=NULL )
	    (prefs_changed)(prefs_changed_data);
	}
      for ( i=0; books[i]!=NULL; ++i )
	free(books[i]);
      free(books);
      free(sel);
    }
}

static void GFCBookmark(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    char *home;

    if ( *bookmarks[mi->mid]=='~' && bookmarks[mi->mid][1]=='/' ) {
	home = GFileGetHomeDir();
	uint32_t *space;
	space = xmalloc((strlen(home)+u32_strlen(bookmarks[mi->mid])+2)*sizeof(uint32_t));
	u32_strcpy(space, x_gc_u8_to_u32 (home));
	u32_strcat(space,bookmarks[mi->mid]+1);
	GFileChooserScanDir(gfc,space);
	free(space);
    } else
	GFileChooserScanDir(gfc,bookmarks[mi->mid]);
}

static void GFCPath(GWindow gw,struct gmenuitem *mi,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) (mi->ti.userdata);
    char *home;

    if ( *gfc->paths[mi->mid]=='~' && gfc->paths[mi->mid][1]=='/' ) {
	home = GFileGetHomeDir();
	uint32_t *space;
	space = xmalloc((strlen(home)+u32_strlen(bookmarks[mi->mid])+2)*sizeof(uint32_t));
	u32_strcpy(space, x_gc_u8_to_u32 (home));
	u32_strcat(space,gfc->paths[mi->mid]+1);
	GFileChooserScanDir(gfc,space);
	free(space);
    } else
	GFileChooserScanDir(gfc,gfc->paths[mi->mid]);
}

// *INDENT-OFF*

static GMenuItem gfcbookmarkmenu[] = {
  {
    .ti = {
      .text = (uint32_t *) N_("Directory|Back"),
      .image = (GImage *) "chooserback.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true
    },
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = GFCBack
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Directory|Forward"),
      .image = (GImage *) "chooserforward.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true
    },
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = GFCForward
  },

  GMENUITEM_LINE,

  {
    .ti = {
      .text = (uint32_t *) N_("Bookmark Current Dir"),
      .image = (GImage *) "chooserbookmark.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true
    },
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = GFCAddCur
  },

  {
    .ti = {
      .text = (uint32_t *) N_("Remove Bookmark..."),
      .image = (GImage *) "choosernobookmark.png",
      .fg = COLOR_DEFAULT,
      .bg = COLOR_DEFAULT,
      .image_precedes = true,
      .text_is_1byte = true
    },
    .shortcut_char = '\0',
    .short_mask = ksm_control,
    .invoke = GFCRemoveBook
  },

  GMENUITEM_LINE,
  GMENUITEM_EMPTY
};

// *INDENT-ON*

static int bgotten=false;

static int GFileChooserBookmarks(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonpress ) {
	GFileChooser *gfc = (GFileChooser *) GGadgetGetUserData(g);
	GMenuItem *mi;
	int i, bcnt, mcnt, pcnt;
	GEvent fake;
	GRect pos;

	if ( !bgotten ) {
	    bgotten = true;
	    for ( i=0; gfcbookmarkmenu[i].ti.text!=NULL || gfcbookmarkmenu[i].ti.line; ++i )
		if ( gfcbookmarkmenu[i].ti.text!=NULL )
		    gfcbookmarkmenu[i].ti.text = (uint32_t *) S_( (char *) gfcbookmarkmenu[i].ti.text);
	}
	for ( mcnt=0; gfcbookmarkmenu[mcnt].ti.text!=NULL || gfcbookmarkmenu[mcnt].ti.line; ++mcnt );
	bcnt = 0;
	if ( bookmarks!=NULL )
	    for ( ; bookmarks[bcnt]!=NULL; ++bcnt );
	if ( gfc->paths!=NULL ) {
	    for ( pcnt=0; gfc->paths[pcnt]!=NULL; ++pcnt );
	    if ( pcnt!=0 && bcnt!=0 ) ++pcnt;
	    bcnt += pcnt;
	}
	mi = xcalloc((mcnt+bcnt+1),sizeof(GMenuItem));
	for ( mcnt=0; gfcbookmarkmenu[mcnt].ti.text!=NULL || gfcbookmarkmenu[mcnt].ti.line; ++mcnt ) {
	    mi[mcnt] = gfcbookmarkmenu[mcnt];
	    mi[mcnt].ti.text = (uint32_t *) xstrdup_or_null( (char *) mi[mcnt].ti.text);
	    mi[mcnt].ti.userdata = gfc;
	}
	if ( gfc->hpos==0 )
	    mi[0].ti.disabled = true;		/* can't go further back */
	if ( gfc->hpos+1>=gfc->hcnt )
	    mi[1].ti.disabled = true;		/* can't go further forward */
	if ( bookmarks==NULL )
	    mi[4].ti.disabled = true;		/* can't remove bookmarks, already none */
	else {
	    if ( gfc->paths!=NULL ) {
		for ( bcnt=0; gfc->paths[bcnt]!=NULL; ++bcnt ) {
		    mi[mcnt+bcnt].ti.text = x_u32_strdup_or_null(gfc->paths[bcnt]);
		    mi[mcnt+bcnt].ti.fg = mi[mcnt+bcnt].ti.bg = COLOR_DEFAULT;
		    mi[mcnt+bcnt].ti.userdata = gfc;
		    mi[mcnt+bcnt].mid = bcnt;
		    mi[mcnt+bcnt].invoke = GFCPath;
		}
		mcnt+=bcnt;
		if ( bookmarks!=NULL && bookmarks[0]!=NULL ) {
		    mi[mcnt].ti.line = true;
		    mi[mcnt].ti.fg = mi[mcnt].ti.bg = COLOR_DEFAULT;
		    ++mcnt;
		}
	    }
	    for ( bcnt=0; bookmarks[bcnt]!=NULL; ++bcnt ) {
		mi[mcnt+bcnt].ti.text = x_u32_strdup_or_null(bookmarks[bcnt]);
		mi[mcnt+bcnt].ti.fg = mi[mcnt+bcnt].ti.bg = COLOR_DEFAULT;
		mi[mcnt+bcnt].ti.userdata = gfc;
		mi[mcnt+bcnt].mid = bcnt;
		mi[mcnt+bcnt].invoke = GFCBookmark;
	    }
	}
	GGadgetGetSize(g,&pos);
	memset(&fake,0,sizeof(fake));
	fake.type = et_mousedown;
	fake.w = g->base;
	fake.u.mouse.x = pos.x;
	fake.u.mouse.y = pos.y+pos.height;
	GMenuCreatePopupMenu(gfc->g.base,&fake, mi);
	GMenuItemArrayFree(mi);
    }
return( true );
}

/* Routine to be called as the mouse moves across the dlg */
void GFileChooserPopupCheck(GGadget *g,GEvent *e) {
    GFileChooser *gfc = (GFileChooser *) g;
    int inside=false;

    if ( e->type == et_mousemove && (e->u.mouse.state&ksm_buttons)==0 ) {
	GGadgetEndPopup();
	for ( g=((GContainerD *) (gfc->g.base->widget_data))->gadgets; g!=NULL; g=g->prev ) {
	    if ( g!=(GGadget *) gfc && g!=(GGadget *) (gfc->filterb) &&
		     g!=(GGadget *) (gfc->files) &&
		     g->takes_input &&
		     e->u.mouse.x >= g->r.x &&
		     e->u.mouse.x<g->r.x+g->r.width &&
		     e->u.mouse.y >= g->r.y &&
		     e->u.mouse.y<g->r.y+g->r.height ) {
		inside = true;
	break;
	    }
	}
	if ( !inside )
	    GGadgetPreparePopup(gfc->g.base,gfc->wildcard);
    } else if ( e->type == et_mousedown && e->u.mouse.button==3 ) {
	GFCPopupMenu(g,e);
    }
}

/* Routine to be called by the filter button */
void GFileChooserFilterIt(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
    uint32_t *pt, *spt, *slashpt, *temp;
    int wasdir;

    wasdir = gfc->lastname!=NULL;

    spt = (uint32_t *) _GGadgetGetTitle(&gfc->name->g);
    if ( *spt=='\0' ) {		/* Werner tells me that pressing the Filter button with nothing should show the default filter mask */
	if ( gfc->wildcard!=NULL )
	    GGadgetSetTitle(&gfc->name->g,gfc->wildcard);
return;
    }

    if (( slashpt = u32_strrchr(spt,'/'))==NULL )
	slashpt = spt;
    else
	++slashpt;
    pt = slashpt;
    while ( *pt && *pt!='*' && *pt!='?' && *pt!='[' && *pt!='{' )
	++pt;
    if ( *pt!='\0' ) {
	free(gfc->wildcard);
	gfc->wildcard = x_u32_strdup_or_null(slashpt);
    } else if ( gfc->lastname==NULL )
	gfc->lastname = x_u32_strdup_or_null(slashpt);

    uint32_t *dir;

    if( GFileIsAbsolute (x_gc_u32_strconv_to_locale(spt)) )
      dir = x_gc_u32_grabstr (x_u32_strmbndup (spt,slashpt-spt));
    else
      {
	uint32_t *curdir = GFileChooserGetCurDir(gfc,-1);

	if ( slashpt!=spt )
	  {
	    temp = x_u32_strmbndup (spt,slashpt-spt);
	    dir = u32_GFileAppendFile(curdir,temp,true);
	    free(temp);
	  }
	else if ( wasdir && *pt=='\0' )
	  dir = u32_GFileAppendFile(curdir,spt,true);
	else
	  dir = x_gc_u32_grabstr (curdir);

	//free(curdir); // FIXME: why is this resulting in a double free?
      }
    GFileChooserScanDir(gfc,dir);
}

/* A function that may be connected to a filter button as its handle_controlevent */
int GFileChooserFilterEh(GGadget *g, GEvent *e) {
    if ( e->type==et_controlevent && e->u.control.subtype == et_buttonactivate )
	GFileChooserFilterIt(GGadgetGetUserData(g));
return( true );
}

void GFileChooserConnectButtons(GGadget *g,GGadget *ok, GGadget *filter) {
    GFileChooser *gfc = (GFileChooser *) g;
    gfc->ok = (GButton *) ok;
    gfc->filterb = (GButton *) filter;
}

void GFileChooserSetFilterText(GGadget *g,const uint32_t *wildcard) {
    GFileChooser *gfc = (GFileChooser *) g;
    free(gfc->wildcard);
    gfc->wildcard = x_u32_strdup_or_null(wildcard);
}

uint32_t *GFileChooserGetFilterText(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
return( gfc->wildcard );
}

void GFileChooserSetFilterFunc(GGadget *g,GFileChooserFilterType filter) {
    GFileChooser *gfc = (GFileChooser *) g;
    if ( filter==NULL )
	filter = GFileChooserDefFilter;
    gfc->filter = filter;
}

GFileChooserFilterType GFileChooserGetFilterFunc(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
return( gfc->filter );
}

void GFileChooserSetMimetypes(GGadget *g, char **mimetypes) {
    GFileChooser *gfc = (GFileChooser *) g;
    int i;

    if ( gfc->mimetypes ) {
	for ( i=0; gfc->mimetypes[i]!=NULL; ++i )
	    free( gfc->mimetypes[i]);
	free(gfc->mimetypes);
    }
    if ( mimetypes ) {
	for ( i=0; mimetypes[i]!=NULL; ++i );
	gfc->mimetypes = xmalloc((i+1)*sizeof(char *));
	for ( i=0; mimetypes[i]!=NULL; ++i )
	    gfc->mimetypes[i] = x_u8_strdup_or_null(mimetypes[i]);
	gfc->mimetypes[i] = NULL;
    } else
	gfc->mimetypes = NULL;
}

char **GFileChooserGetMimetypes(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
return( gfc->mimetypes );
}

/* Change the current file, or the current directory/file */
static void GFileChooserSetTitle(GGadget *g,const uint32_t *tit) {
    GFileChooser *gfc = (GFileChooser *) g;
    uint32_t *pt, *curdir, *temp, *dir, *base;

    if ( tit==NULL ) {
	curdir = GFileChooserGetCurDir(gfc,-1);
	GFileChooserScanDir(gfc,curdir);
	free(curdir);
return;
    }

    pt = u32_strrchr(tit,'/');
    free(gfc->lastname);
    gfc->lastname = NULL;

    if ( GFileIsAbsolute( x_gc_u32_strconv_to_locale(tit)) ){
	base = uc_strstr(tit, "://");
	if(!base) base = (uint32_t*) tit;
	if(pt > base && pt[1] && (pt[1]!='.' || pt[2]!='\0')){
	    gfc->lastname = x_u32_strdup_or_null(pt+1);
	    dir = x_u32_strmbndup (tit, pt-tit);
	}
	else{
	    dir = x_u32_strdup_or_null(tit);
	}
	GFileChooserScanDir(gfc,dir);
	free(dir);
    } else if ( pt==NULL ) {
	GGadgetSetTitle(&gfc->name->g,tit);
	curdir = GFileChooserGetCurDir(gfc,-1);
	GFileChooserScanDir(gfc,curdir);
	free(curdir);
    } else {
	curdir = GFileChooserGetCurDir(gfc,-1);
	temp = x_u32_strmbndup (tit,pt-tit);
	dir = u32_GFileAppendFile(curdir,temp,true);
	free(temp); free(curdir);
	free(gfc->lastname);
	if ( pt[1]!='\0' )
	    gfc->lastname = x_u32_strdup_or_null(pt+1);
	GFileChooserScanDir(gfc,dir);
    }
}

void GFileChooserRefreshList(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
    uint32_t *curdir;

    curdir = GFileChooserGetCurDir(gfc,-1);
    GFileChooserScanDir(gfc,curdir);
    free(curdir);
}

/* Get the current directory/file */
static uint32_t *GFileChooserGetTitle(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
    uint32_t *spt, *curdir, *file;

    spt = (uint32_t *) _GGadgetGetTitle(&gfc->name->g);
    if ( GFileIsAbsolute(x_gc_u32_strconv_to_locale (spt)) )
	file = x_u32_strdup_or_null(spt);
    else {
	curdir = GFileChooserGetCurDir(gfc,-1);
	file = x_u32_strdup_or_null (u32_GFileAppendFile(curdir,spt,gfc->lastname!=NULL));
	free(curdir);
    }
return( file );
}

static void GFileChooser_destroy(GGadget *g) {
    GFileChooser *gfc = (GFileChooser *) g;
    int i;

    free(lastdir);
    lastdir = GFileChooserGetCurDir(gfc,-1);

    if ( gfc->outstanding )
	GIOcancel(gfc->outstanding);
    GGadgetDestroy(&gfc->topbox->g);	/* destroys everything */
    if ( gfc->paths!=NULL ) {
	for ( i=0; gfc->paths[i]!=NULL; ++i )
	    free(gfc->paths[i]);
	free(gfc->paths);
    }
    free(gfc->wildcard);
    free(gfc->lastname);
    if ( gfc->mimetypes ) {
	for ( i=0; gfc->mimetypes[i]!=NULL; ++i )
	    free( gfc->mimetypes[i]);
	free(gfc->mimetypes);
    }
    for ( i=0; i<gfc->hcnt; ++i )
	free(gfc->history[i]);
    free(gfc->history);
    _ggadget_destroy(&gfc->g);
}

static int gfilechooser_expose(GWindow pixmap, GGadget *g, GEvent *event) {
return( true );
}

static int gfilechooser_mouse(GGadget *g, GEvent *event) {
    GFileChooser *gfc = (GFileChooser *) g;

    if (( event->type==et_mouseup || event->type==et_mousedown ) &&
	    (event->u.mouse.button>=4 && event->u.mouse.button<=7) ) {
	if ( gfc->files->vsb!=NULL )
return( GGadgetDispatchEvent(&gfc->files->vsb->g,event));
	else
return( true );
    }

return( false );
}

static int gfilechooser_noop(GGadget *g, GEvent *event) {
return( false );
}

static int gfilechooser_timer(GGadget *g, GEvent *event) {
return( false );
}

static void gfilechooser_move(GGadget *g, int32_t x, int32_t y ) {
    GFileChooser *gfc = (GFileChooser *) g;

    GGadgetMove(&gfc->topbox->g,x,y);
    _ggadget_move(g,x,y);
}

static void gfilechooser_resize(GGadget *g, int32_t width, int32_t height ) {
    GFileChooser *gfc = (GFileChooser *) g;

    GGadgetResize(&gfc->topbox->g,width,height);
    _ggadget_resize(g,width,height);
}

static void gfilechooser_setvisible(GGadget *g, int visible ) {
    GFileChooser *gfc = (GFileChooser *) g;
    GGadgetSetVisible(&gfc->files->g,visible);
    GGadgetSetVisible(&gfc->directories->g,visible);
    GGadgetSetVisible(&gfc->name->g,visible);
    GGadgetSetVisible(&gfc->up->g,visible);
    GGadgetSetVisible(&gfc->home->g,visible);
    GGadgetSetVisible(&gfc->bookmarks->g,visible);
    GGadgetSetVisible(&gfc->config->g,visible);
    GGadgetSetVisible(&gfc->topbox->g,visible);
    _ggadget_setvisible(g,visible);
}

static void gfilechooser_setenabled(GGadget *g, int enabled ) {
    GFileChooser *gfc = (GFileChooser *) g;
    GGadgetSetEnabled(&gfc->files->g,enabled);
    GGadgetSetEnabled(&gfc->subdirs->g,enabled);
    GGadgetSetEnabled(&gfc->directories->g,enabled);
    GGadgetSetEnabled(&gfc->name->g,enabled);
    GGadgetSetEnabled(&gfc->up->g,enabled);
    GGadgetSetEnabled(&gfc->home->g,enabled);
    GGadgetSetEnabled(&gfc->bookmarks->g,enabled);
    GGadgetSetEnabled(&gfc->config->g,enabled);
    GGadgetSetEnabled(&gfc->topbox->g,enabled);
    _ggadget_setenabled(g,enabled);
}

static void GFileChooserGetDesiredSize(GGadget *g,GRect *outer,GRect *inner) {
    if ( inner!=NULL ) {
	int bp = GBoxBorderWidth(g->base,g->box);
	inner->x = inner->y = 0;
	inner->width = g->desired_width - 2*bp;
	inner->height = g->desired_height - 2*bp;
    }
    if ( outer!=NULL ) {
	outer->x = outer->y = 0;
	outer->width = g->desired_width;
	outer->height = g->desired_height;
    }
}

static int gfilechooser_FillsWindow(GGadget *g) {
return( g->prev==NULL &&
	(_GWidgetGetGadgets(g->base)==g ||
	 _GWidgetGetGadgets(g->base)==(GGadget *) ((GFileChooser *) g)->up ));
}

struct gfuncs GFileChooser_funcs = {
    0,
    sizeof(struct gfuncs),

    gfilechooser_expose,
    gfilechooser_mouse,
    gfilechooser_noop,
    NULL,
    NULL,
    gfilechooser_timer,
    NULL,

    _ggadget_redraw,
    gfilechooser_move,
    gfilechooser_resize,
    gfilechooser_setvisible,
    gfilechooser_setenabled,
    _ggadget_getsize,
    _ggadget_getinnersize,

    GFileChooser_destroy,

    GFileChooserSetTitle,
    NULL,
    GFileChooserGetTitle,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    GFileChooserGetDesiredSize,
    _ggadget_setDesiredSize,
    gfilechooser_FillsWindow,
    NULL
};

static void GFileChooserCreateChildren(GFileChooser *gfc, int flags) {
    GGadgetCreateData gcd[9], boxes[4], *varray[9], *harray[12], *harray2[4];
    GTextInfo label[9];
    int k=0, l=0, homek, upk, bookk, confk, dirsk, subdirsk, filesk, textk;

    memset(&gcd,'\0',sizeof(gcd));
    memset(&boxes,'\0',sizeof(boxes));
    memset(&label,'\0',sizeof(label));

    gcd[k].gd.flags = gg_visible|gg_enabled|gg_utf8_popup;
    gcd[k].gd.popup_msg = (uint32_t *) _("Home Folder");
    label[k].image = (GImage *) "chooserhomefolder.png";
    gcd[k].gd.label = &label[k];
    gcd[k].gd.handle_controlevent = GFileChooserHome;
    homek = k;
    gcd[k++].creator = GButtonCreate;
    harray[l++] = &gcd[k-1]; harray[l++] = GCD_Glue;

    gcd[k].gd.flags = gg_visible|gg_enabled|gg_utf8_popup;
    gcd[k].gd.popup_msg = (uint32_t *) _("Bookmarks");
    label[k].image = (GImage *) "chooserbookmark.png";
    gcd[k].gd.label = &label[k];
    gcd[k].gd.handle_controlevent = GFileChooserBookmarks;
    bookk = k;
    gcd[k++].creator = GButtonCreate;
    harray[l++] = &gcd[k-1]; harray[l++] = GCD_Glue;

    gcd[k].gd.flags = gg_visible|gg_enabled|gg_list_exactlyone;
    gcd[k].gd.handle_controlevent = GFileChooserDListChanged;
    dirsk = k;
    gcd[k++].creator = GListButtonCreate;
    harray[l++] = &gcd[k-1]; harray[l++] = GCD_Glue;

    gcd[k].gd.flags = gg_visible|gg_enabled|gg_utf8_popup;
    gcd[k].gd.popup_msg = (uint32_t *) _("Parent Folder");
    label[k].image = (GImage *) "chooserupdir.png";
    gcd[k].gd.label = &label[k];
    gcd[k].gd.handle_controlevent = GFileChooserUpDirButton;
    upk = k;
    gcd[k++].creator = GButtonCreate;
    harray[l++] = &gcd[k-1]; harray[l++] = GCD_Glue;

    gcd[k].gd.flags = gg_visible|gg_enabled|gg_utf8_popup;
    gcd[k].gd.popup_msg = (uint32_t *) _("Configure");
    label[k].image = (GImage *) "chooserconfigtool.png";
    gcd[k].gd.label = &label[k];
    gcd[k].gd.handle_controlevent = GFileChooserConfigure;
    confk = k;
    gcd[k++].creator = GButtonCreate;
    harray[l++] = &gcd[k-1]; harray[l++] = NULL;

    boxes[2].gd.flags = gg_enabled|gg_visible;
    boxes[2].gd.u.boxelements = harray;
    boxes[2].creator = GHBoxCreate;

    l=0;
    varray[l++] = &boxes[2];

    if ( dir_placement==dirs_separate )
	gcd[k].gd.flags = gg_visible|gg_enabled|gg_list_alphabetic|gg_list_exactlyone;
    else
	gcd[k].gd.flags =            gg_enabled|gg_list_alphabetic|gg_list_exactlyone;
    gcd[k].gd.handle_controlevent = GFileChooserFListSelected;
    subdirsk = k;
    gcd[k++].creator = GListCreate;
    harray2[0] = &gcd[k-1];

    if ( flags & gg_file_multiple )
	gcd[k].gd.flags = gg_visible|gg_enabled|gg_list_alphabetic|gg_list_multiplesel;
    else
	gcd[k].gd.flags = gg_visible|gg_enabled|gg_list_alphabetic|gg_list_exactlyone;
    gcd[k].gd.handle_controlevent = GFileChooserFListSelected;
    filesk = k;
    gcd[k++].creator = GListCreate;
    harray2[1] = &gcd[k-1]; harray2[2] = NULL;

    boxes[3].gd.flags = gg_enabled|gg_visible;
    boxes[3].gd.u.boxelements = harray2;
    boxes[3].creator = GHBoxCreate;
    varray[l++] = &boxes[3];

    gcd[k].gd.flags = gg_visible|gg_enabled;
    gcd[k].gd.handle_controlevent = GFileChooserTextChanged;
    textk = k;
    if ( flags&gg_file_pulldown )
	gcd[k++].creator = GListFieldCreate;
    else
	gcd[k++].creator = GTextCompletionCreate;
    varray[l++] = &gcd[k-1]; varray[l] = NULL;

    boxes[0].gd.pos.x = gfc->g.r.x;
    boxes[0].gd.pos.y = gfc->g.r.y;
    boxes[0].gd.pos.width  = gfc->g.r.width;
    boxes[0].gd.pos.height = gfc->g.r.height;
    boxes[0].gd.flags = gg_enabled|gg_visible;
    boxes[0].gd.u.boxelements = varray;
    boxes[0].creator = GVBoxCreate;

    for ( l=0; l<k; ++l )
	gcd[l].data = gfc;

    GGadgetsCreate(gfc->g.base,boxes);

    gfc->topbox      = (GHVBox *)      boxes[0].ret;
    gfc->home        = (GButton *)     gcd[homek].ret;
    gfc->bookmarks   = (GButton *)     gcd[bookk].ret;
    gfc->directories = (GListButton *) gcd[dirsk].ret;
    gfc->up          = (GButton *)     gcd[upk  ].ret;
    gfc->config      = (GButton *)     gcd[confk].ret;
    gfc->subdirs     = (GDList *)      gcd[subdirsk].ret;
    gfc->files       = (GDList *)      gcd[filesk].ret;
    gfc->name        = (GTextField *)  gcd[textk].ret;

    gfc->home->g.contained = true;
    gfc->bookmarks->g.contained = true;
    gfc->directories->g.contained = true;
    gfc->up->g.contained = true;
    gfc->config->g.contained = true;
    gfc->subdirs->g.contained = true;
    gfc->files->g.contained = true;
    gfc->name->g.contained = true;
    gfc->topbox->g.contained = true;

    GCompletionFieldSetCompletion(&gfc->name->g,GFileChooserCompletion);
    GCompletionFieldSetCompletionMode(&gfc->name->g,true);

    GHVBoxSetExpandableRow(boxes[0].ret,1);
    GHVBoxSetExpandableCol(boxes[2].ret,4);
    if ( boxes[0].gd.pos.width!=0 && boxes[0].gd.pos.height!=0 )
	GGadgetResize(boxes[0].ret,boxes[0].gd.pos.width,boxes[0].gd.pos.height);
}

GGadget *GFileChooserCreate(struct gwindow *base, GGadgetData *gd,void *data) {
    GFileChooser *gfc = xcalloc(1,sizeof(GFileChooser));

    gfc->g.funcs = &GFileChooser_funcs;
    _GGadget_Create(&gfc->g,base,gd,data,&gfilechooser_box);
    gfc->g.takes_input = gfc->g.takes_keyboard = false; gfc->g.focusable = false;
    if ( gfc->g.r.width == 0 )
	gfc->g.r.width = GGadgetScale(GDrawPointsToPixels(base,140));
    if ( gfc->g.r.height == 0 )
	gfc->g.r.height = GDrawPointsToPixels(base,100);
    gfc->g.desired_width = gfc->g.r.width;
    gfc->g.desired_height = gfc->g.r.height;
    gfc->g.inner = gfc->g.r;
    _GGadget_FinalPosition(&gfc->g,base,gd);

    GFileChooserCreateChildren(gfc, gd->flags);
    gfc->filter = GFileChooserDefFilter;
    
    if ( gd->flags & gg_group_end )
	_GGadgetCloseGroup(&gfc->g);

    if ( lastdir==NULL ) {
      char *dir = x_gc_grabstr (xgetcwd ());
      strcat(dir, "/.");
      lastdir = x_u32_strconv_from_locale (dir);
    }
    if ( gd->label==NULL || gd->label->text==NULL )
	GFileChooserSetTitle(&gfc->g,lastdir);
    else if ( GFileIsAbsolute(x_gc_u32_strconv_to_locale(gd->label->text)) )
	GFileChooserSetTitle(&gfc->g,gd->label->text);
    else {
	uint32_t *temp = u32_GFileAppendFile(lastdir,gd->label->text,false);
	temp = u32_GFileNormalize(temp);
	GFileChooserSetTitle(&gfc->g,temp);
    }

return( &gfc->g );
}

void GFileChooserSetDir(GGadget *g,uint32_t *dir) {
    GFileChooserScanDir((GFileChooser *) g,dir);
}

uint32_t *GFileChooserGetDir(GGadget *g) {
return( GFileChooserGetCurDir((GFileChooser *) g,-1));
}

GIOControl *GFileChooserReplaceIO(GGadget *g,GIOControl *gc) {
    GFileChooser *gfc = (GFileChooser *)g;

    if ( gfc->outstanding!=NULL ) {
	GIOclose(gfc->outstanding);
	gfc->outstanding = NULL;
	GDrawSetCursor(gfc->g.base,gfc->old_cursor);
    }
    if ( gc!=NULL ) {
	gfc->old_cursor = GDrawGetCursor(gfc->g.base);
	GDrawSetCursor(gfc->g.base,ct_watch);
	gfc->outstanding = gc;
    }
return( gc );
}

void GFileChooserGetChildren(GGadget *g,GGadget **pulldown, GGadget **list, GGadget **tf) {
    GFileChooser *gfc = (GFileChooser *)g;

    if ( pulldown!=NULL ) *pulldown = &gfc->directories->g;
    if ( tf!=NULL )	  *tf = &gfc->name->g;
    if ( list!=NULL )	  *list = &gfc->files->g;
}

int GFileChooserPosIsDir(GGadget *g, int pos) {
    GFileChooser *gfc = (GFileChooser *)g;
    GGadget *list = &gfc->files->g;
    GTextInfo *ti;

    ti = GGadgetGetListItem(list,pos);
    if ( ti==NULL )
return( false );

return( ti->checked );
}

uint32_t *GFileChooserFileNameOfPos(GGadget *g, int pos) {
  GFileChooser *gfc = (GFileChooser *)g;
  GGadget *list = &gfc->files->g;
  GTextInfo *ti;
  uint32_t *curdir, *file;

  ti = GGadgetGetListItem(list,pos);
  if ( ti==NULL )
    return( NULL );

  curdir = GFileChooserGetCurDir(gfc,-1);
  file = x_u32_strdup_or_null (u32_GFileAppendFile(curdir,ti->text,false));
  free(curdir);

  return( file );
}

void GFileChooserSetShowHidden(int sh) {
    showhidden = sh;
}

int GFileChooserGetShowHidden(void) {
return( showhidden );
}

void GFileChooserSetDirectoryPlacement(int dp) {
    dir_placement = dp;
}

int GFileChooserGetDirectoryPlacement(void) {
return( dir_placement );
}

void GFileChooserSetBookmarks(uint32_t **b) {
    if ( bookmarks!=NULL && bookmarks!=b ) {
	int i;

	for ( i=0; bookmarks[i]!=NULL; ++i )
	    free(bookmarks[i]);
	free(bookmarks);
    }
    bookmarks = b;
}

uint32_t **GFileChooserGetBookmarks(void) {
return( bookmarks );
}

void GFileChooserSetPrefsChangedCallback(void *data, void (*p_c)(void *)) {
    prefs_changed = p_c;
    prefs_changed_data = data;
}

void GFileChooserSetPaths(GGadget *g,char **path) {
    uint32_t **dirs = NULL;
    int dcnt;
    GFileChooser *gfc = (GFileChooser *) g;

    if ( gfc->paths!=NULL ) {
	for ( dcnt=0; gfc->paths[dcnt]!=NULL; ++dcnt )
	    free( gfc->paths[dcnt] );
	free(gfc->paths);
	gfc->paths = NULL;
    }
    if ( path==NULL || path[0]==NULL )
return;

    for ( dcnt=0; path[dcnt]!=NULL; ++dcnt );
    gfc->paths = dirs = xmalloc((dcnt+1)*sizeof(uint32_t *));
    for ( dcnt=0; path[dcnt]!=NULL; ++dcnt )
	dirs[dcnt] = utf82u_copy(path[dcnt]);
    dirs[dcnt] = NULL;
}
