#include <config.h>

/* -*- coding: utf-8 -*- */
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

#include <config.h>

#include <stdbool.h>
#include "fontforgevw.h"
#include "ustring.h"
#include <utype.h>
#include <gfile.h>
#include "namehash.h"
#include "namelist_data.h"

int recognizePUA = false;
NameList *force_names_when_opening=NULL;
NameList *force_names_when_saving=NULL;

NameList agl_sans;
NameList agl;
NameList agl_nf;
NameList adobepua;
NameList greeksc;
NameList tex;
NameList ams;

NameList *namelist_for_new_fonts = &agl_nf;

/* Adobe's standard names are wrong for: */
/* 0x2206 is named Delta, 0x394 should be */
/* 0x2126 is named Omega, 0x3A9 should be */
/* 0x00b5 is named mu, 0x3BC should be */
/* The following have been corrected removed from AGL For New Fonts: */
/* 0x0162 is named Tcommaaccent, 0x21A should be */
/* 0x0163 is named tcommaaccent, 0x21B should be */
/* 0xf6be is named dotlessj, 0x237 should be */

static int psnamesinited=false;
#define HASH_SIZE	257
struct psbucket { const char *name; int uni; struct psbucket *prev; } *psbuckets[HASH_SIZE];

static void psaddbucket(const char *name, int uni) {
    int hash = hashname(name);
    struct psbucket *buck = xcalloc(1,sizeof(struct psbucket));

    buck->name = name;
    buck->uni = uni;
    buck->prev = psbuckets[hash];
    psbuckets[hash] = buck;
}

static void NameListHash(NameList *nl) {
    int i,j,k;
    for ( i=0; i<17; ++i ) if ( nl->unicode[i]!=NULL ) {
	for ( j=0; j<256; ++j ) if ( nl->unicode[i][j]!=NULL ) {
	    for ( k=0; k<256; ++k ) if ( nl->unicode[i][j][k]!=NULL )
		psaddbucket(nl->unicode[i][j][k],(i<<16)|(j<<8)|k );
	}
    }
}

static void psinitnames(void) {
    int i;
    NameList *nl;

    agl.next = &agl_nf;
    agl_nf.next = &agl_sans;
    agl_sans.next = &adobepua;
    adobepua.next = &greeksc;
    greeksc.next = &tex;
    tex.next = &ams;

    for ( i=0; psaltnames[i].name!=NULL ; ++i )
	psaddbucket(psaltnames[i].name,psaltnames[i].unicode);
    for ( nl=&agl; nl!=NULL; nl=nl->next )
	NameListHash(nl);
    psnamesinited = true;
}

static void psreinitnames(void) {
    /* If we reread a (loaded) namelist file, then we must remove the old defn*/
    /*  which means we must remove all the old hash entries before we can put */
    /*  in the new ones */
    int i;
    struct psbucket *cur, *prev;
    NameList *nl;

    for ( i=0; i<HASH_SIZE; ++i ) {
	for ( cur = psbuckets[i]; cur!=NULL; cur=prev ) {
	    prev = cur->prev;
	    free(cur);
	}
	psbuckets[i] = NULL;
    }

    for ( i=0; psaltnames[i].name!=NULL ; ++i )
	psaddbucket(psaltnames[i].name,psaltnames[i].unicode);
    for ( nl=&agl; nl!=NULL; nl=nl->next )
	NameListHash(nl);
}

int UniFromName(const char *name,enum uni_interp interp,Encoding *encname) {
    int i = -1;
    char *end;
    struct psbucket *buck;
    int _recognizePUA = recognizePUA;

    if ( strncmp(name,"uni",3)==0 ) {
	i = strtol(name+3,&end,16);
	if ( *end || end-name!=7 )	/* uniXXXXXXXX means a ligature of uniXXXX and uniXXXX */
	    i = -1;
	_recognizePUA = true;
    } else if ( (name[0]=='U' || name[0]=='u') && name[1]=='+' &&
	    (strlen(name)==6 || strlen(name)==7)) {
	/* Unifont uses this convention */
	i = strtol(name+2,&end,16);
	if ( *end )
	    i = -1;
	_recognizePUA = true;
    } else if ( name[0]=='u' && strlen(name)>=5 ) {
	i = strtol(name+1,&end,16);
	if ( *end )
	    i = -1;
	else if ( encname!=NULL && !encname->is_unicodefull &&
		(interp==ui_ams || interp==ui_trad_chinese)) {
	    int j;
	    extern const int cns14pua[], amspua[];
	    const int *pua = interp==ui_ams ? amspua : cns14pua;
	    for ( j=0xf8ff-0xe000; j>=0; --j )
		if ( pua[j]==i ) {
		    i = j+0xe000;
	    break;
		}
	}
	if ( i!=-1 )
	    _recognizePUA = true;
    } else if ( name[0]!='\0' && name[1]=='\0' )
	i = ((unsigned char *) name)[0];
    if ( i==-1 ) {
	if ( !psnamesinited )
	    psinitnames();
	for ( buck = psbuckets[hashname(name)]; buck!=NULL; buck=buck->prev )
	    if ( strcmp(buck->name,name)==0 )
	break;
	if ( buck!=NULL )
	    i = buck->uni;
    }
    if ( !_recognizePUA && i>=0xe000 && i<=0xf8ff )
	i = -1;
return( i );
}

const char *StdGlyphName(char *buffer, int uni,enum uni_interp interp,NameList *for_this_font) {
    const char *name = NULL;
    NameList *nl;
    int up, ub, uc;

    if ( for_this_font==NULL )
	for_this_font = namelist_for_new_fonts;
    else if ( for_this_font==(NameList *) -1 )
	for_this_font = &agl;
    if ( (uni>=0 && uni<' ') ||
	    (uni>=0x7f && uni<0xa0) )
	/* standard controls */;
    else if ( uni!=-1  ) {
	if ( uni>=0xe000 && uni<=0xf8ff &&
		(interp==ui_trad_chinese || for_this_font==&ams)) {
	    extern const int cns14pua[], amspua[];
	    const int *pua = interp==ui_trad_chinese ? cns14pua : amspua;
	    if ( pua[uni-0xe000]!=0 )
		uni = pua[uni-0xe000];
	}
	up = uni>>16;
	ub = (uni&0xff00)>>8;
	uc = (uni&0xff);
	if ( up<17 )
	    for ( nl=for_this_font; nl!=NULL; nl=nl->basedon ) {
		if ( nl->unicode[up]!=NULL && nl->unicode[up][ub]!=NULL &&
			(name = nl->unicode[up][ub][uc])!=NULL )
	    break;
	}
    }
    if ( name==NULL ) {
	if ( uni>=0x10000 )
	    sprintf( buffer, "u%04X", uni);
	else
	    sprintf( buffer, "uni%04X", uni);
	name = buffer;
    }
return( name );
}

#define RefMax	40

static int transcmp(RefChar *r1, RefChar *r2) {
    double d1, d2;

    if ( r1->transform[4]<r2->transform[4] )
return( -1 );
    else if ( r1->transform[4]>r2->transform[4] )
return(  1 );
    if ( (d1 = r1->transform[5])<0 ) d1 = -d1;
    if ( (d2 = r2->transform[5])<0 ) d2 = -d2;
    if ( d1<d2 )
return( -1 );
    else if ( d1==d2 )
return( 0 );
    else
return( 1 );
}

static int FindAllRefs(SplineChar *sc,SplineChar *rsc[RefMax], int *au) {
    RefChar *refs[RefMax], *alp[RefMax], *out[RefMax];
    RefChar *ref;
    int layer, last, rcnt, acnt, ocnt, i,j;
    int alluni;
    /* We also order the reference. The order stored in the splinechar doesn't*/
    /*  mean anything, so try and guess what is intended semantically. */

    if ( sc==NULL )
return( 0 );
    last = ly_fore;
    if ( sc->parent->multilayer )
	last = sc->layer_cnt-1;
    for ( layer=ly_fore; layer<=last; ++layer )
	if ( sc->layers[layer].splines!=NULL || sc->layers[layer].images!=NULL )
return( 0 );
    rcnt = 0;
    for ( layer=ly_fore; layer<=last; ++layer ) {
	for ( ref = sc->layers[layer].refs; ref!=NULL; ref = ref->next ) {
	    if ( rcnt>=RefMax )
return( 0 );
	    refs[rcnt++] = ref;
	}
    }
    alluni = true;
    for ( i=0; i<rcnt; ++i ) {
	if ( refs[i]->sc->unicodeenc==-1 ) {
	    alluni = false;
    break;
	}
    }
    if ( !alluni ) {
	/* If not all unicode we can't make any guesses about meaning, so */
	/*  order by transformation */
	for ( i=0; i<rcnt; ++i ) for ( j=i+1; j<rcnt; ++j ) {
	    if ( transcmp(refs[i],refs[j])>0 ) {
		ref = refs[i];
		refs[i] = refs[j];
		refs[j] = ref;
	    }
	}
    } else {
	acnt = 0;
	for ( i=0; i<rcnt; ++i ) {
	    if ( isalpha(refs[i]->sc->unicodeenc )) {
		alp[acnt++] = refs[i];
		--rcnt;
		for ( j=i; j<rcnt; ++j )
		    refs[j] = refs[j+1];
		--i;
	    }
	}
	for ( i=0; i<acnt; ++i ) for ( j=i+1; j<acnt; ++j ) {
	    if ( transcmp(alp[i],alp[j])>0 ) {
		ref = alp[i];
		alp[i] = alp[j];
		alp[j] = ref;
	    }
	}
	for ( i=0; i<rcnt; ++i ) for ( j=i+1; j<rcnt; ++j ) {
	    if ( transcmp(refs[i],refs[j])>0 ) {
		ref = refs[i];
		refs[i] = refs[j];
		refs[j] = ref;
	    }
	}
	if ( acnt!=0 ) {
	    int a=0, r=0;
	    real cutpoint;
	    ocnt = 0;
	    out[ocnt++] = alp[a++];
	    while (true) {
		if ( a<acnt ) cutpoint = (alp[a]->transform[4]+3*alp[a-1]->transform[4])/4;
		else		cutpoint = 1e30;
		while ( r<rcnt && refs[r]->transform[4]<cutpoint )
		    out[ocnt++] = refs[r++];
		if ( a>=acnt )
	    break;
		out[ocnt++] = alp[a++];
	    }
	    memcpy(refs,out,ocnt*sizeof(RefChar *));
	    rcnt = ocnt;
	}
    }
    for ( i=0; i<rcnt; ++i )
	rsc[i] = refs[i]->sc;
    /* alluni now means can be written as uniXXXX.XXXX.XXXX... */
    for ( i=0; i<rcnt; ++i ) {
	if ( refs[i]->sc->unicodeenc>0x10000 ) {
	    alluni = false;
    break;
	}
    }
    *au = alluni;
return( rcnt );
}

/* Return a list of all alternate or standard glyph names for this encoding */
char **AllGlyphNames(int uni, NameList *for_this_font, SplineChar *sc) {
    int cnt, k, j, i, len;
    NameList *nl, *nl2, *nl3;
    char **names = NULL;
    const char *name;
    int up, ub, uc;
    char buffer[40], *pt;
    SplineChar *refs[RefMax];
    int rcnt, alluni;

    rcnt = FindAllRefs(sc,refs,&alluni);

    up = uni>>16;
    ub = (uni&0xff00)>>8;
    uc = (uni&0xff);

    for ( k=0; k<2; ++k ) {
	cnt = 0;
	/* try the default namelist first to put that at the head of the list */
	name = NULL;
	nl = nl3 = NULL;
	if ( uni>=0 && up<17 ) {
	    if ( for_this_font!=NULL ) {
		for ( nl3=for_this_font; nl3!=NULL; nl3=nl3->basedon ) {
		    if ( nl3->unicode[up]!=NULL && nl3->unicode[up][ub]!=NULL &&
			    (name = nl3->unicode[up][ub][uc])!=NULL )
		break;
		}
		if ( name!=NULL ) {
		    if ( names )
			names[cnt] = copy(name);
		    ++cnt;
		}
	    }
	    if ( for_this_font!=namelist_for_new_fonts ) {
		for ( nl=namelist_for_new_fonts; nl!=NULL; nl=nl->basedon ) if ( nl!=nl3 ) {
		    if ( nl->unicode[up]!=NULL && nl->unicode[up][ub]!=NULL &&
			    (name = nl->unicode[up][ub][uc])!=NULL )
		break;
		}
		if ( name!=NULL ) {
		    if ( names )
			names[cnt] = copy(name);
		    ++cnt;
		}
	    }
	    for ( nl2 = &agl; nl2!=NULL; nl2=nl2->next ) if ( nl2!=nl && nl2!=nl3) {
		if ( nl2->unicode[up]!=NULL && nl2->unicode[up][ub]!=NULL &&
			(name = nl2->unicode[up][ub][uc])!=NULL ) {
		    if ( names )
			names[cnt] = copy(name);
		    ++cnt;
		}
	    }
	    for ( i=0; psaltnames[i].name!=NULL ; ++i ) {
		if ( psaltnames[i].unicode==uni ) {
		    if ( names )
			names[cnt] = copy(psaltnames[i].name);
		    ++cnt;
		}
	    }
	    if ( uni<0x10000 ) {
		if ( names ) {
		    sprintf( buffer, "uni%04X", uni);
		    names[cnt] = copy(buffer);
		}
		++cnt;
	    }
	    if ( names ) {
		sprintf( buffer, "u%04X", uni);
		names[cnt] = copy(buffer);
	    }
	    ++cnt;
	}
	if ( rcnt>1 && alluni && (uni<0 || (uni>=0xe000 && uni<0xf900) || uni>=0xf0000 ) ) {
	    if ( names ) {
		names[cnt] = xmalloc1(4+4*rcnt);
		strcpy(names[cnt],"uni");
		pt = names[cnt]+3;
		for ( i=0; i<rcnt; ++i ) {
		    if ( refs[i]->unicodeenc==0x131 || refs[i]->unicodeenc==0x237 ||
			    refs[i]->unicodeenc==0xf6be )
			sprintf( pt,"%04X", refs[i]->unicodeenc==0x131?'i':'j' );
		    else
			sprintf( pt,"%04X", CanonicalCombiner(refs[i]->unicodeenc));
		    pt += 4;
		}
	    }
	    ++cnt;
	}
	if ( rcnt>1 ) {
	    if ( names ) {
		for ( i=len=0; i<rcnt; ++i )
		    len += strlen( refs[i]->name )+1;
		names[cnt] = pt = xmalloc1(len);
		for ( i=len=0; i<rcnt; ++i ) {
		    strcpy(pt,refs[i]->name);
		    pt += strlen(pt);
		    *pt++ = '_';
		}
		pt[-1] = '\0';
	    }
	    ++cnt;
	}
	if ( uni<0 || up>=17 ) {
	    if ( names )
		names[cnt] = copy(".notdef");
	    ++cnt;
	}
	if ( k==0 ) {
	    names = xmalloc1((cnt+1)*sizeof(char *));
	    names[cnt] = NULL;
	}
    }
    /* Remove any names from multiiple namelists */
    for ( i=0; i<cnt; ++i ) for ( j=i+1; j<cnt; ++j ) {
	if ( strcmp(names[i],names[j])==0 ) {
	    for ( k=j+1; k<cnt; ++k )
		names[k-1] = names[k];
	    names[--cnt] = NULL;
	    --j;
	}
    }
return( names );
}

char **AllNamelistNames(void) {
    NameList *nl;
    int cnt;
    char **names;

    for ( nl = &agl, cnt=0; nl!=NULL; nl=nl->next, ++cnt );
    names = xmalloc1((cnt+1) *sizeof(char *));
    for ( nl = &agl, cnt=0; nl!=NULL; nl=nl->next, ++cnt )
	names[cnt] = copy(_(nl->title));
    names[cnt] = NULL;
return( names );
}

#if 0
uint8 *AllNamelistUnicodes(void) {
    NameList *nl;
    int cnt;
    uint8 *uses;

    for ( nl = &agl, cnt=0; nl!=NULL; nl=nl->next, ++cnt );
    uses = xmalloc1((cnt+1) *sizeof(uint8));
    for ( nl = &agl, cnt=0; nl!=NULL; nl=nl->next, ++cnt )
	uses[cnt] = nl->uses_unicode;
    uses[cnt] = 0xff;
return( uses );
}
#endif

NameList *DefaultNameListForNewFonts(void) {
return( namelist_for_new_fonts );
}

NameList *NameListByName(char *name) {
    NameList *nl;

    /* ΤεΧ is hard tp type e.g. from scripting, so accept TeX as alias */
    if (strcmp(name,"TeX Names")==0)
	name = copy("ΤεΧ Names");

    for ( nl = &agl; nl!=NULL; nl=nl->next ) {
	if ( strcmp(_(nl->title),name)==0 || strcmp(nl->title,name)==0 )
return( nl );
    }
return( NULL );
}

static void NameListFreeContents(NameList *nl) {
    int np, nb, nc, i;

    for ( np=0; np<17; ++np ) if ( nl->unicode[np]!=NULL ) {
	for ( nb=0; nb<256; ++nb ) if ( nl->unicode[np][nb]!=NULL ) {
	    for ( nc=0; nc<256; ++nc ) if ( nl->unicode[np][nb][nc]!=NULL )
		free((char *)nl->unicode[np][nb][nc] );
	    free( (char **) nl->unicode[np][nb]);
	}
	free( (char ***) nl->unicode[np]);
    }
    free( nl->title );
    if ( nl->renames!=NULL ) {
	for ( i=0; nl->renames[i].from!=NULL; ++i ) {
	    free(nl->renames[i].from);
	    free(nl->renames[i].to);
	}
	free(nl->renames);
    }
    free(nl->a_utf8_name);
}

static void NameListFree(NameList *nl) {
    NameListFreeContents(nl);
    free(nl);
}
/* ************************************************************************** */

#include <sys/types.h>
#include <dirent.h>

NameList *LoadNamelist(char *filename) {
    FILE *file = fopen(filename,"r");
    NameList *nl, *nl2;
    char buffer[400];
    char *pt, *end, *test;
    int uni;
    int len;
    int up, ub, uc;
    int rn_cnt=0, rn_max = 0;
    int uses_unicode = false;

    if ( file==NULL )
return( NULL );

    if ( !psnamesinited )
	psinitnames();

    nl = (NameList *) xzalloc(sizeof (NameList));
    pt = strrchr(filename,'/');
    if ( pt==NULL ) pt = filename; else ++pt;
    nl->title = def2utf8_copy(pt);
    pt = strrchr(nl->title,'.');
    if ( pt!=NULL ) *pt = '\0';

    while ( fgets(buffer,sizeof(buffer),file)!=NULL ) {
	if ( buffer[0]=='#' || buffer[0]=='\n' || buffer[0]=='\r' )
    continue;
	len = strlen( buffer );
	if ( buffer[len-1]=='\n' ) buffer[--len] = '\0';
	if ( buffer[len-1]=='\r' ) buffer[--len] = '\0';
	if ( strncmp(buffer,"Based:",6)==0 ) {
	    for ( pt=buffer+6; *pt==' ' || *pt=='\t'; ++pt);
	    for ( nl2 = &agl; nl2!=NULL; nl2 = nl2->next )
		if ( strcmp( nl2->title,pt )==0 )
	    break;
	    if ( nl2==NULL ) {
		ff_post_error(_("NameList base missing"),_("NameList %s based on %s which could not be found"), nl->title, pt );
		NameListFree(nl);
return( NULL );
	    } else if ( nl->basedon!=NULL ) {
		ff_post_error(_("NameList based twice"),_("NameList %s based on two NameLists"), nl->title );
		NameListFree(nl);
return( NULL );
	    }
	    nl->basedon = nl2;
	} else if ( strncmp(buffer,"Rename:",7)==0 ) {
	    for ( pt=buffer+7; *pt==' ' || *pt=='\t'; ++pt);
	    for ( test=pt; *test!=' ' && *test!='\t' && *test!='\0'; ++test );
	    if ( *test=='\0' ) {
		ff_post_error(_("NameList parsing error"),_("Missing rename \"to\" name %s\n%s"), nl->title, buffer );
		NameListFree(nl);
return( NULL );
	    }
	    *test='\0';
	    for ( ++test; *test==' ' || *test=='\t'; ++test);
	    if ( (test[0]=='-' || test[0]=='=') && test[1]=='>' )
		for ( test+=2; *test==' ' || *test=='\t'; ++test);
	    if ( *test=='\0' ) {
		ff_post_error(_("NameList parsing error"),_("Missing rename \"to\" name %s\n%s"), nl->title, buffer );
		NameListFree(nl);
return( NULL );
	    }
	    if ( rn_cnt>=rn_max-1 )
		nl->renames = xrealloc(nl->renames,(rn_max+=20)*sizeof(struct renames));
	    nl->renames[rn_cnt].from   = copy(pt);
	    nl->renames[rn_cnt].to     = copy(test);
	    nl->renames[++rn_cnt].from = NULL;		/* End mark */
	} else {
	    pt = buffer;
	    if ( *pt=='0' && (pt[1]=='x' || pt[1]=='X'))
		pt += 2;
	    else if (( *pt=='u' || *pt=='U') && pt[1]=='+' )
		pt += 2;
	    uni = strtol(pt,&end,16);
	    if ( end==pt || uni<0 || uni>=unicode4_size ) {
		ff_post_error(_("NameList parsing error"),_("Bad unicode value when parsing %s\n%s"), nl->title, buffer );
		NameListFree(nl);
return( NULL );
	    }
	    pt = end;
	    while ( *pt==' ' || *pt==';' || *pt=='\t' ) ++pt;
	    if ( *pt=='\0' ) {
		ff_post_error(_("NameList parsing error"),_("Missing name when parsing %s for unicode %x"), nl->title, uni );
		NameListFree(nl);
return( NULL );
	    }
	    for ( test=pt; *test; ++test ) {
		if ( (*test<=' ' && *test>=0) ||
		    *test=='(' || *test=='[' || *test=='{' || *test=='<' ||
		    *test==')' || *test==']' || *test=='}' || *test=='>' ||
		    *test=='%' || *test=='/' ) {
		    ff_post_error(_("NameList parsing error"),_("Bad name when parsing %s for unicode %x"), nl->title, uni );
		    NameListFree(nl);
return( NULL );
		}
		if ( *test&0x80 ) {
		    uses_unicode = true;
		    if ( nl->a_utf8_name==NULL )
			nl->a_utf8_name = copy(pt);
		}
	    }
	    up = uni>>16;
	    ub = (uni&0xff00)>>8;
	    uc = uni&0xff;
	    if ( nl->unicode[up]==NULL )
		nl->unicode[up] = xcalloc(256,sizeof(char **));
	    if ( nl->unicode[up][ub]==NULL )
		nl->unicode[up][ub] = xcalloc(256,sizeof(char *));
	    if ( nl->unicode[up][ub][uc]==NULL )
		nl->unicode[up][ub][uc]=copy(pt);
	    else {
		ff_post_error(_("NameList parsing error"),_("Multiple names when parsing %s for unicode %x"), nl->title, uni );
		NameListFree(nl);
return( NULL );
	    }
	}
    }

    nl->uses_unicode = uses_unicode;
    if ( nl->basedon!=NULL && nl->basedon->uses_unicode )
	nl->uses_unicode = true;
    fclose(file);
    for ( nl2 = &agl; nl2->next!=NULL; nl2=nl2->next ) {
	if ( strcmp(nl2->title,nl->title)==0 ) {	/* Replace old version */
	    NameList *next = nl2->next;
	    NameListFreeContents(nl2);
	    *nl2 = *nl;
	    nl2->next = next;
	    free(nl);
	    psreinitnames();
return( nl2 );
	}
    }
    NameListHash(nl);
    nl2->next = nl;
return( nl );
}

static int isnamelist(char *filename) {
    char *pt;

    pt = strrchr(filename,'.');
    if ( pt==NULL )
return( false );
    if ( strcmp(pt,".nam")==0 )
return( true );

return( false );
}

void LoadNamelistDir(char *dir) {
    DIR *diro;
    struct dirent *ent;
    char buffer[1025];

    if ( dir == NULL )
	dir = getUserDataDir();
    if ( dir == NULL )
return;

    diro = opendir(dir);
    if ( diro==NULL )		/* It's ok not to have any */
return;
    
    while ( (ent = readdir(diro))!=NULL ) {
	if ( isnamelist(ent->d_name) ) {
	    sprintf( buffer, "%s/%s", dir, ent->d_name );
	    LoadNamelist(buffer);
	}
    }
    closedir(diro);
}
/* ************************************************************************** */
const char *RenameGlyphToNamelist(char *buffer, SplineChar *sc,NameList *old,
	NameList *new_, char **sofar) {
    int i, up, ub, uc, ch, gid;
    char space[80];		/* glyph names are supposed to be less<=31 chars */
    char tempbuf[32];
    char *pt, *start, *opt, *oend; const char *newsubname;
    SplineChar *tempsc;
    NameList *nl;

    if ( sc->unicodeenc!=-1 ) {
	up = sc->unicodeenc>>16;
	ub = (sc->unicodeenc>>8)&0xff;
	uc = (sc->unicodeenc&0xff);
	for ( nl=new_; nl!=NULL; nl=nl->basedon )
	    if ( nl->unicode[up]!=NULL && nl->unicode[up][ub]!=NULL && nl->unicode[up][ub][uc]!=NULL )
return( nl->unicode[up][ub][uc] );
	if ( up==0 )
	    sprintf( buffer, "uni%04X", sc->unicodeenc );
	else
	    sprintf( buffer, "u%04X", sc->unicodeenc );
return( buffer );
    } else {
	if ( old!=NULL && old->renames!=NULL ) {
	    for ( i=0; old->renames[i].from!=NULL; ++i )
		if ( strcmp(sc->name,old->renames[i].from)==0 )
return( old->renames[i].to );
	}
	if ( new_->renames!=NULL ) {
	    for ( i=0; new_->renames[i].from!=NULL; ++i )
		if ( strcmp(sc->name,new_->renames[i].to)==0 )
return( new_->renames[i].from );
	}
	if ( strlen(sc->name)>sizeof(space)-1 )
return( sc->name );
	strcpy(space,sc->name);
	opt = buffer; oend = buffer+31;
	start = space;
	/* Check for composite names f_i, A.small */
	while ( *start ) {
	    for ( pt=start; *pt!='\0' && *pt!='.' && *pt!='_'; ++pt );
	    if ( *pt=='\0' && start==space )
return( sc->name );
	    ch = *pt;
	    *pt = '\0';
	    tempsc = SFGetChar(sc->parent,-1,start);
	    newsubname = NULL;
	    if ( tempsc!=NULL )
		newsubname = RenameGlyphToNamelist(tempbuf,tempsc,old,new_,sofar);
	    else if ( sofar!=NULL ) {
		for ( gid=sc->parent->glyphcnt-1; gid>=0; --gid ) if ( sofar[gid]!=NULL ) {
		    if ( strcmp(sofar[gid],start)==0 )
		break;
		}
		if ( gid!=-1 )
		    newsubname = sc->parent->glyphs[gid]->name;
	    }
	    if ( newsubname==NULL )
return( sc->name );
	    while ( opt<oend && *newsubname )
		*opt++ = *newsubname++;
	    if ( *newsubname )
return( sc->name );
	    if ( ch=='\0' ) {
		*opt = '\0';
return( buffer );
	    } else if ( ch=='.' ) {
		/* don't attempt to translate anything after a '.' just copy that litterally */
		*pt = ch;
		while ( opt<oend && *pt )
		    *opt++ = *pt++;
		if ( *pt )
return( sc->name );
		*opt = '\0';
return( buffer );
	    } else {		/* _ */
		*opt++ = '_';
		start = pt+1;
	    }
	}
	*opt = '\0';
return( buffer );
    }
}

static void BuildHash(struct glyphnamehash *hash,SplineFont *sf,char **oldnames) {
    int gid, hv;
    SplineChar *sc;
    struct glyphnamebucket *new_;

    memset(hash,0,sizeof(*hash));
    for ( gid = 0; gid<sf->glyphcnt; ++gid ) {
	if ( (sc=sf->glyphs[gid])!=NULL && oldnames[gid]!=NULL ) {
	    new_ = (struct glyphnamebucket *) xzalloc(sizeof (struct glyphnamebucket));
	    new_->sc = sf->glyphs[gid];
	    hv = hashname(oldnames[gid]);
	    new_->next = hash->table[hv];
	    new_->name = oldnames[gid];
	    hash->table[hv] = new_;
	}
    }
}

static SplineChar *HashFind(struct glyphnamehash *hash,const char *name) {
    struct glyphnamebucket *test;

    for ( test=hash->table[hashname(name)]; test!=NULL; test = test->next )
	if ( strcmp(test->name,name)==0 )
return( test->sc );

return( NULL );
}

struct bits {
    char *start, *end;
    SplineChar *rpl;
};

static void safestrcpy(char *to, const char *from) {
    int ch;

    while ( (ch=*from++)!='\0' )
	*to++ = ch;
    *to = '\0';
}

static char *DoReplacements(struct bits *bits,int bc,char **_src,char *start) {
    int offset = start - *_src;
    int diff, i, off, allsmall=1, len;
    char *ret, *last, *last_orig;

    for ( diff=i=0; i<bc; ++i ) {
	off = strlen(bits[i].rpl->name) - (bits[i].end-bits[i].start);
	diff += off;
	if ( diff>0 )
	    allsmall = 0;
    }
    if ( allsmall ) {
	diff = 0;
	for ( i=0; i<bc; ++i ) {
	    len = strlen(bits[i].rpl->name);
	    memcpy(bits[i].start+diff,bits[i].rpl->name,len);
	    if ( len<(bits[i].end-bits[i].start) )
		safestrcpy(bits[i].start+len+diff,bits[i].end+diff);
	    diff += len - (bits[i].end-bits[i].start);
	}
    } else {
	int totlen = strlen(*_src);
	last = ret = xmalloc1(totlen + diff + 1);
	last_orig = *_src;
	for ( i=0; i<bc; ++i ) {
	    if ( last_orig<bits[i].start ) {
		memcpy( last,last_orig,bits[i].start-last_orig);
		last += bits[i].start-last_orig;
	    }
	    strcpy(last,bits[i].rpl->name);
	    last += strlen(bits[i].rpl->name);
	    last_orig = bits[i].end;
	}
	strcpy(last,last_orig);
	free(*_src);
	*_src = ret;
    }

return( *_src + offset + diff );
}

static void ReplaceByHash(char **_src,struct glyphnamehash *hash) {
    struct bits bits[40];
    int bc,ch;
    char *start, *end;

    start = *_src;
    if ( start==NULL )
return;
    while ( *start==' ' ) ++start;

    bc = 0;
    for ( ; *start; start=end ) {
	if ( bc>=40 ) {
	    start = DoReplacements(bits,bc,_src,start);
	    bc=0;
	}
	for ( end=start; *end!='\0' && *end!=' '; ++end );
	ch = *end; *end='\0';
	bits[bc].start = start;
	bits[bc].end   = end;
	bits[bc].rpl   = HashFind(hash,start);
	if ( bits[bc].rpl!=NULL )
	    ++bc;
	*end = ch;
	while ( *end==' ' ) ++end;
    }
    if ( bc!=0 )
	(void) DoReplacements(bits,bc,_src,start);
}

static void SFRenameLookupsByHash(SplineFont *sf,struct glyphnamehash *hash) {
    int gid, i,j,h;
    SplineChar *sc, *rpl;
    PST *pst;
    FPST *fpst;
    ASM *sm;
    struct glyphvariants *gv;
    KernClass *kc;

    for ( gid = 0; gid<sf->glyphcnt; ++gid ) if ( (sc=sf->glyphs[gid])!=NULL ) {
	for ( pst=sc->possub; pst!=NULL; pst=pst->next ) {
	    switch ( pst->type ) {
	      case pst_pair: case pst_substitution:
		rpl = HashFind(hash,pst->u.subs.variant);	/* variant is at same location as paired */
		if ( rpl!=NULL ) {
		    free( pst->u.subs.variant );
		    pst->u.subs.variant = copy(rpl->name);
		}
	      break;
	      case pst_alternate: case pst_multiple: case pst_ligature:
		ReplaceByHash(&pst->u.mult.components,hash);
	      break;
	      default:
		/* position and lcaret don't need anything */
	      break;
	    }
	}
	for ( h=0; h<2; ++h ) {
	    gv = h ? sc->horiz_variants:sc->vert_variants;
	    if ( gv==NULL )
	continue;
	    ReplaceByHash(&gv->variants,hash);
	    for ( i=0; i<gv->part_cnt; ++i ) {
		struct gv_part *part = &gv->parts[i];
		ReplaceByHash(&part->component,hash);
	    }
	}
    }

    for ( fpst=sf->possub; fpst!=NULL; fpst=fpst->next ) {
	switch ( fpst->format ) {
	  case pst_glyphs:
	    for ( i=0; i<fpst->rule_cnt; ++i ) {
		struct fpst_rule *r= &fpst->rules[i];
		ReplaceByHash(&r->u.glyph.names,hash);
		ReplaceByHash(&r->u.glyph.back,hash);
		ReplaceByHash(&r->u.glyph.fore,hash);
	    }
	  break;
	  case pst_class:
	    for ( i=0; i<fpst->nccnt; ++i )
		ReplaceByHash(&fpst->nclass[i],hash);
	    for ( i=0; i<fpst->bccnt; ++i )
		ReplaceByHash(&fpst->bclass[i],hash);
	    for ( i=0; i<fpst->fccnt; ++i )
		ReplaceByHash(&fpst->fclass[i],hash);
	  break;
	  case pst_coverage:
	  case pst_reversecoverage:
	    for ( i=0; i<fpst->rule_cnt; ++i ) {
		struct fpst_rule *r= &fpst->rules[i];
		for ( j=0; j<r->u.coverage.ncnt; ++j )
		    ReplaceByHash(&r->u.coverage.ncovers[j],hash);
		for ( j=0; j<r->u.coverage.bcnt; ++j )
		    ReplaceByHash(&r->u.coverage.bcovers[j],hash);
		for ( j=0; j<r->u.coverage.fcnt; ++j )
		    ReplaceByHash(&r->u.coverage.fcovers[j],hash);
		if ( fpst->format == pst_reversecoverage )
		    ReplaceByHash(&r->u.rcoverage.replacements,hash);
	    }
	  break;
	}
    }

    for ( sm = sf->sm; sm!=NULL; sm=sm->next ) {
	for ( i=0; i<sm->class_cnt; ++i )
	    ReplaceByHash(&sm->classes[i],hash);
    }

    for ( h=0; h<2; ++h ) {
	for ( kc = h ? sf->kerns:sf->vkerns; kc!=NULL; kc=kc->next ) {
	    for ( i=0; i<kc->first_cnt; ++i )
		ReplaceByHash(&kc->firsts[i],hash);
	    for ( i=0; i<kc->second_cnt; ++i )
		ReplaceByHash(&kc->seconds[i],hash);
	}
    }
}

char **SFTemporaryRenameGlyphsToNamelist(SplineFont *sf,NameList *new_) {
    int gid;
    char buffer[40]; const char *name;
    SplineChar *sc;
    char **ret;
    struct glyphnamehash hash;

    if ( new_==NULL )
return( NULL );

    ret = xcalloc(sf->glyphcnt,sizeof(char *));
    for ( gid = 0; gid<sf->glyphcnt; ++gid ) if ( (sc=sf->glyphs[gid])!=NULL ) {
	name = RenameGlyphToNamelist(buffer,sc,sf->for_new_glyphs,new_,ret);
	if ( name!=sc->name ) {
	    ret[gid] = sc->name;
	    sc->name = copy(name);
	}
    }

    BuildHash(&hash,sf,ret);
    SFRenameLookupsByHash(sf,&hash);
    __GlyphHashFree(&hash);
    GlyphHashFree(sf);
return( ret );
}

void SFTemporaryRestoreGlyphNames(SplineFont *sf,char **former) {
    int gid;
    SplineChar *sc;
    struct glyphnamehash hash;

    for ( gid = 0; gid<sf->glyphcnt; ++gid ) if ( (sc=sf->glyphs[gid])!=NULL ) {
	if ( former[gid]!=NULL ) {
	    char *old = sc->name;
	    sc->name = former[gid];
	    former[gid] = old;
	}
    }
    BuildHash(&hash,sf,former);
    SFRenameLookupsByHash(sf,&hash);
    __GlyphHashFree(&hash);
    GlyphHashFree(sf);
    for ( gid = 0; gid<sf->glyphcnt; ++gid )
	free(former[gid]);
    free(former);
}

void SFRenameGlyphsToNamelist(SplineFont *sf,NameList *new_) {
    char **ret;
    int gid;

    if ( new_==NULL )
return;

    ret = SFTemporaryRenameGlyphsToNamelist(sf,new_);
    for ( gid = 0; gid<sf->glyphcnt; ++gid )
	free(ret[gid]);
    free(ret);

    sf->for_new_glyphs = new_;
}
/* ************************************************************************** */
