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

/* Copyright (C) 2005-2012 by George Williams */
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
#include "fontforgevw.h"
#include "groups.h"
#include <unistd.h>
#include <ustring.h>
#include <utype.h>

Group *group_root = NULL;

void GroupFree(Group *g) {
    int i;

    if ( g==NULL )
return;

    free(g->name);
    free(g->glyphs);
    for ( i=0; i<g->kid_cnt; ++i )
	GroupFree(g->kids[i]);
    free(g->kids);
    free(g);
}

Group *GroupCopy(Group *g) {
    int i;
    Group *gp;

    if ( g==NULL )
return( NULL );

    gp = xzalloc(sizeof (Group));
    gp->name = xstrdup_or_null(g->name);
    gp->glyphs = xstrdup_or_null(g->glyphs);
    if ( g->kid_cnt!=0 ) {
	gp->kids = xmalloc((gp->kid_cnt=g->kid_cnt)*sizeof(Group *));
	for ( i=0; i<g->kid_cnt; ++i ) {
	    gp->kids[i] = GroupCopy(g->kids[i]);
	    gp->kids[i]->parent = gp;
	}
    }
return( gp );
}

/******************************************************************************/
/***************************** File IO for Groups *****************************/
/******************************************************************************/

static char *getPfaEditGroups(void) {
    static char *groupname=NULL;
    char buffer[1025];

    if ( groupname!=NULL )
return( groupname );
    if ( getUserDataDir()==NULL )
return( NULL );
    sprintf(buffer,"%s/groups", getUserDataDir());
    groupname = xstrdup_or_null(buffer);
return( groupname );
}

static void _SaveGroupList(FILE *file, Group *g, int indent) {
    int i;

    for ( i=0; i<indent; ++i )
	putc(' ',file);
    fprintf(file,"\"%s\": %d", g->name, g->unique );
    if ( g->glyphs!=NULL && g->kid_cnt==0 )
	fprintf(file, " \"%s\"\n", g->glyphs );
    else {
	putc('\n',file);
	for ( i=0; i<g->kid_cnt; ++i )
	    _SaveGroupList(file,g->kids[i], indent+1);
    }
}

void SaveGroupList(void) {
    char *groupfilename;
    FILE *groups;

    groupfilename = getPfaEditGroups();
    if ( groupfilename==NULL )
return;
    if ( group_root==NULL || (group_root->kid_cnt==0 && group_root->glyphs==NULL )) {
	unlink(groupfilename);
return;
    }
    groups = fopen(groupfilename,"w");
    if ( groups==NULL )
return;
    _SaveGroupList(groups,group_root,0);
    fclose(groups);
}

struct gcontext {
    int found_indent;
    int bmax;
    char *buffer;
    int lineno;
};

static int countIndent(FILE *file) {
    int ch, cnt=0;

    while ( (ch=getc(file))==' ' )
	++cnt;
    if ( cnt==0 && ch==EOF )
return( -1 );
    ungetc(ch,file);
return( cnt );
}

static int lineCountIndent(FILE *file, struct gcontext *gc) {
    int ch;

    while ( (ch=getc(file))!=EOF && ch!='\n' && ch!='\r' );
    if ( ch!=EOF )
	++gc->lineno;
    if ( ch=='\r' ) {
	ch = getc(file);
	if ( ch!='\n' )
	    ungetc(ch,file);
    }
return( gc->found_indent = countIndent(file));
}

static char *loadString(FILE *file, struct gcontext *gc) {
    int i, ch;

    ch = getc(file);
    if ( ch!='"' ) {
	ungetc(ch,file);
return( NULL );
    }
    for ( i=0 ; (ch=getc(file))!=EOF && ch!='"' ; ++i ) {
	if ( i+1>=gc->bmax ) {
	    gc->bmax += 100;
	    gc->buffer = xrealloc(gc->buffer,gc->bmax);
	}
	gc->buffer[i] = ch;
    }
    if ( ch==EOF )
return( NULL );

    if ( i==0 )
return( xstrdup(""));
    gc->buffer[i] = '\0';
return( xstrdup_or_null( gc->buffer ));
}

static Group *_LoadGroupList(FILE *file, Group *parent, int expected_indent,
	struct gcontext *gc) {
    Group *g;
    char *n;
    int i, ch;
    Group **glist=NULL;
    int gmax = 0;

    if ( expected_indent!=gc->found_indent )
return( NULL );

    n = loadString(file,gc);
    if ( n==NULL )
return( NULL );
    g = xzalloc(sizeof (Group));
    g->parent = parent;
    g->name = n;
    if ( (ch = getc(file))==':' )
	ch = getc(file);
    while ( ch==' ' )
	ch = getc(file);
    if ( ch=='1' )
	g->unique = true;
    else if ( ch!='0' ) {
	GroupFree(g);
return( NULL );
    }
    while ( (ch = getc(file))==' ' );
    if ( ch=='"' ) {
	ungetc(ch,file);
	g->glyphs = loadString(file,gc);
	if ( g->glyphs==NULL ) {
	    GroupFree(g);
return( NULL );
	}
	lineCountIndent(file,gc);
    } else if ( ch=='\n' || ch=='\r' ) {
	ungetc(ch,file);
	lineCountIndent(file,gc);
	for ( i=0 ;; ++i ) {
	    if ( i>=gmax ) {
		gmax += 10;
		glist = xrealloc(glist,gmax*sizeof(Group *));
	    }
	    glist[i] = _LoadGroupList(file, g, expected_indent+1, gc);
	    if ( glist[i]==NULL )
	break;
	}
	g->kid_cnt = i;
	if ( i!=0 ) {
	    g->kids = xmalloc(i*sizeof(Group *));
	    memcpy(g->kids,glist,i*sizeof(Group *));
	    free(glist);
	}
    }
return( g );
}
    
void LoadGroupList(void) {
    char *groupfilename;
    FILE *groups;
    struct gcontext gc;

    groupfilename = getPfaEditGroups();
    if ( groupfilename==NULL )
return;
    groups = fopen(groupfilename,"r");
    if ( groups==NULL )
return;
    GroupFree(group_root);
    memset(&gc,0,sizeof(gc));
    gc.found_indent = countIndent(groups);
    group_root = _LoadGroupList(groups,NULL,0,&gc);
    if ( !feof(groups))
	LogError( _("Unparsed characters found after end of groups file (last line parsed was %d).\n"), gc.lineno );
    fclose(groups);

    free(gc.buffer);
}
