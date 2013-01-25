#include <config.h>

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
#include "fontforge.h"
#include "ttf.h"
#include <math.h>
#include <ustring.h>

static int PtNumbersAreSet(SplineChar *sc) {
    struct splinecharlist *dep;

    if ( sc==NULL )
return( false );

    if ( sc->ttf_instrs!=NULL )
return( true );
    for ( dep= sc->dependents; dep!=NULL ; dep=dep->next )
	if ( dep->sc->ttf_instrs!=NULL )
return( true );

return( false );
}

static int AssignPtNumbers(MMSet *mm,int gid) {
    /* None of the instances has fixed point numbers. Make them match */
    int cnt=0;
    SplineSet **ss;
    SplinePoint **sp;
    int i;
    int allavg, alllines, stillmore, ret=true;

    ss = xmalloc((mm->instance_count+1)*sizeof(SplineSet *));
    sp = xmalloc((mm->instance_count+1)*sizeof(SplinePoint *));
    for ( i=0; i<mm->instance_count; ++i )
	ss[i] = mm->instances[i]->glyphs[gid]->layers[ly_fore].splines;
    ss[i] = mm->normal->glyphs[gid]->layers[ly_fore].splines;

    if ( ss[0]==NULL ) {
	stillmore = false;
	for ( i=0; i<=mm->instance_count; ++i )
	    if ( ss[i]!=NULL ) stillmore = true;
	if ( stillmore )
return( false );
return( true );
    } else {
	stillmore = true;
	for ( i=0; i<=mm->instance_count; ++i )
	    if ( ss[i]==NULL ) stillmore = false;
	if ( !stillmore )
return( false );
    }
	    
    while (true) {
	for ( i=0; i<=mm->instance_count; ++i )
	    sp[i] = ss[i]->first;
	while (true) {
	    allavg = alllines = true;
	    for ( i=0; i<=mm->instance_count; ++i ) {
		if ( !RealNear(sp[i]->me.x,(sp[i]->nextcp.x+sp[i]->prevcp.x)/2) ||
			!RealNear(sp[i]->me.y,(sp[i]->nextcp.y+sp[i]->prevcp.y)/2) )
		    allavg = false;
		if ( !sp[i]->nonextcp )
		    alllines = false;
	    }
	    if ( sp[0] == ss[0]->first )
		allavg = false;
	    for ( i=0; i<=mm->instance_count; ++i ) {
		if ( allavg )
		    sp[i]->ttfindex = 0xffff;
		else
		    sp[i]->ttfindex = cnt;
	    }
	    if ( !allavg )
		++cnt;
	    for ( i=0; i<=mm->instance_count; ++i ) {
		if ( alllines )
		    sp[i]->nextcpindex = 0xffff;
		else
		    sp[i]->nextcpindex = cnt;
	    }
	    if ( !alllines )
		++cnt;

	    if ( sp[0]->next==NULL ) {
		stillmore = false;
		for ( i=1; i<=mm->instance_count; ++i )
		    if ( sp[i]->next!=NULL )
			stillmore = true;
		if ( stillmore )
		    ret = false;
	break;
	    }
	    for ( i=1; i<=mm->instance_count; ++i )
		if ( sp[i]->next==NULL )
		    stillmore = false;
	    if ( !stillmore ) {
		ret = false;
	break;
	    }
	    sp[0] = sp[0]->next->to;
	    for ( i=1; i<=mm->instance_count; ++i )
		sp[i] = sp[i]->next->to;
	    if ( sp[0]==ss[0]->first ) {
		stillmore = false;
		for ( i=1; i<=mm->instance_count; ++i )
		    if ( sp[i]!=ss[i]->first )
			stillmore = true;
		if ( stillmore )
		    ret = false;
	break;
	    }
	    for ( i=1; i<=mm->instance_count; ++i ) {
		if ( sp[i]==ss[i]->first )
		    stillmore = false;
	    }
	    if ( !stillmore ) {
		ret = false;
	break;
	    }
	}
	if ( !ret )
    break;
	stillmore = true;
	for ( i=0; i<=mm->instance_count; ++i )
	    ss[i] = ss[i]->next;
	if ( ss[0]==NULL ) {
	    stillmore=false;
	    for ( i=1; i<=mm->instance_count; ++i )
		if ( ss[i]!=NULL )
		    stillmore = true;
	    if ( stillmore )
		ret = true;
    break;
	}
	for ( i=1; i<=mm->instance_count; ++i )
	    if ( ss[i]==NULL )
		stillmore = false;
	if ( !stillmore ) {
	    ret = true;
    break;
	}
    }
    free(ss);
    free(sp);
return( ret );
}

static int MatchPoints(SplineFont *sffixed, SplineFont *sfother, int gid) {
    SplineChar *fixed, *other;
    SplineSet *ss1, *ss2;
    SplinePoint *sp1, *sp2;

    fixed = sffixed->glyphs[gid]; other = sfother->glyphs[gid];

    if ( PtNumbersAreSet(other)) {
	/* Point numbers must match exactly, both are fixed */
	for ( ss1=fixed->layers[ly_fore].splines,
		  ss2=other->layers[ly_fore].splines;
		ss1!=NULL && ss2!=NULL ;
		ss1 = ss1->next, ss2=ss2->next ) {
	    for ( sp1=ss1->first, sp2=ss2->first; ; ) {
		if ( sp1->ttfindex!=sp2->ttfindex ||
			sp1->nextcpindex!=sp2->nextcpindex )
return( false );
		if ( sp1->next==NULL || sp2->next==NULL ) {
		    if ( sp1->next!=NULL || sp2->next!=NULL )
return( false );
	    break;
		}
		sp1 = sp1->next->to; sp2=sp2->next->to;
		if ( sp1==ss1->first || sp2==ss2->first ) {
		    if ( sp1!=ss1->first || sp2!=ss2->first )
return( false );
	    break;
		}
	    }
	}
return( ss1==NULL && ss2==NULL );
    } else {
	for ( ss1=fixed->layers[ly_fore].splines,
		  ss2=other->layers[ly_fore].splines;
		ss1!=NULL && ss2!=NULL ;
		ss1 = ss1->next, ss2=ss2->next ) {
	    for ( sp1=ss1->first, sp2=ss2->first; ; ) {
		if ( sp1->ttfindex!=0xffff )
		    sp2->ttfindex = sp1->ttfindex;
		else if ( !RealNear(sp2->me.x,(sp2->nextcp.x+sp2->prevcp.x)/2) ||
			!RealNear(sp2->me.y,(sp2->nextcp.y+sp2->prevcp.y)/2) )
return( false );
		else
		    sp2->ttfindex = 0xffff;
		if ( sp1->nextcpindex!=0xffff )
		    sp2->nextcpindex = sp1->nextcpindex;
		else if ( !sp2->nonextcp )
return( false );
		else
		    sp2->nextcpindex = 0xffff;
		if ( sp1->next==NULL || sp2->next==NULL ) {
		    if ( sp1->next!=NULL || sp2->next!=NULL )
return( false );
	    break;
		}
		sp1 = sp1->next->to; sp2=sp2->next->to;
		if ( sp1==ss1->first || sp2==ss2->first ) {
		    if ( sp1!=ss1->first || sp2!=ss2->first )
return( false );
	    break;
		}
	    }
	}
return( ss1==NULL && ss2==NULL );
    }
}

int ContourPtNumMatch(MMSet *mm, int gid) {
    SplineFont *sf;
    int i;

    if ( !mm->apple )
return( false );

    if ( gid>=mm->normal->glyphcnt )
return( false );
    if ( !SCWorthOutputting(mm->normal->glyphs[gid] ) ) {
	for ( i=0; i<mm->instance_count; ++i ) {
	    if ( gid>=mm->instances[i]->glyphcnt )
return( false );
	    if ( SCWorthOutputting(mm->instances[i]->glyphs[gid]))
return( false );
	}
return( true );		/* None is not worth outputting, and that's ok, they match */
    } else {
	for ( i=0; i<mm->instance_count; ++i ) {
	    if ( gid>=mm->instances[i]->glyphcnt )
return( false );
	    if ( !SCWorthOutputting(mm->instances[i]->glyphs[gid]))
return( false );
	}
	    /* All are worth outputting */
    }

    if ( mm->normal->glyphs[gid]->layers[ly_fore].refs!=NULL && mm->normal->glyphs[gid]->layers[ly_fore].splines!=NULL )
return( false );
    for ( i=0; i<mm->instance_count; ++i ) {
	if ( mm->instances[i]->glyphs[gid]->layers[ly_fore].refs!=NULL && mm->instances[i]->glyphs[gid]->layers[ly_fore].splines!=NULL )
return( false );
    }
    if ( mm->normal->glyphs[gid]->layers[ly_fore].refs!=NULL ) {
	RefChar *r;
	int cnt, c;
	for ( r=mm->normal->glyphs[gid]->layers[ly_fore].refs, cnt=0; r!=NULL; r=r->next )
	    ++cnt;
	for ( i=0; i<mm->instance_count; ++i ) {
	    for ( r=mm->instances[i]->glyphs[gid]->layers[ly_fore].refs, c=0; r!=NULL; r=r->next )
		++c;
	    if ( c!=cnt )
return( false );
	}
    }

    sf = NULL;
    if ( PtNumbersAreSet(mm->normal->glyphs[gid]) )
	sf = mm->normal;
    else {
	for ( i=0; i<mm->instance_count; ++i ) {
	    if ( PtNumbersAreSet(mm->instances[i]->glyphs[gid])) {
		sf = mm->instances[i];
	break;
	    }
	}
    }
    if ( sf==NULL )
	/* No instance has fixed points. Make sure all fonts are consistent */
return( AssignPtNumbers(mm,gid));

    if ( sf!=mm->normal && !MatchPoints(sf,mm->normal,gid))
return( false );
    for ( i=0; i<mm->instance_count; ++i ) if ( sf!=mm->instances[i] ) {
	if ( !MatchPoints(sf, mm->instances[i],gid) )
return( false );
    }
return( true );
}

static int SCPointCount(SplineChar *sc) {
    int ptcnt=0;
    RefChar *r;

    ptcnt = SSTtfNumberPoints(sc->layers[ly_fore].splines);
    for ( r=sc->layers[ly_fore].refs; r!=NULL ; r=r->next )
	++ptcnt;
return( ptcnt );
}

int16_t **SCFindDeltas(MMSet *mm, int gid, int *_ptcnt) {
    /* When figuring out the deltas the first thing we must do is figure */
    /*  out each point's number */
    int i, j, k, l, cnt, ptcnt;
    int16_t **deltas;
    SplineSet *ss1, *ss2;
    SplinePoint *sp1, *sp2;
    RefChar *r1, *r2;

    if ( !ContourPtNumMatch(mm,gid))
return( NULL );
    if ( !SCWorthOutputting(mm->normal->glyphs[gid]))
return( NULL );

    *_ptcnt = ptcnt = SCPointCount(mm->normal->glyphs[gid])+4;
    deltas = xmalloc(2*mm->instance_count*sizeof(int16_t *));
    for ( i=0; i<2*mm->instance_count; ++i )
	deltas[i] = xcalloc(ptcnt,sizeof(int16_t));
    for ( i=0; i<mm->instance_count; ++i ) {
	for ( ss1=mm->normal->glyphs[gid]->layers[ly_fore].splines,
		  ss2=mm->instances[i]->glyphs[gid]->layers[ly_fore].splines;
		ss1!=NULL && ss2!=NULL ;
		ss1 = ss1->next, ss2=ss2->next ) {
	    for ( sp1=ss1->first, sp2=ss2->first; ; ) {
		if ( sp1->ttfindex!=0xffff ) {
		    deltas[2*i][sp1->ttfindex] = rint(sp2->me.x)-rint(sp1->me.x);
		    deltas[2*i+1][sp1->ttfindex] = rint(sp2->me.y)-rint(sp1->me.y);
		}
		if ( sp1->nextcpindex != 0xffff ) {
		    deltas[2*i][sp1->nextcpindex] = rint(sp2->nextcp.x)-rint(sp1->nextcp.x);
		    deltas[2*i+1][sp1->nextcpindex] = rint(sp2->nextcp.y)-rint(sp1->nextcp.y);
		}
		if ( sp1->next==NULL )
	    break;
		sp1 = sp1->next->to; sp2 = sp2->next->to;
		if ( sp1==ss1->first )
	    break;
	    }
	}
	for ( cnt=0,
		r1=mm->normal->glyphs[gid]->layers[ly_fore].refs,
		r2=mm->instances[i]->glyphs[gid]->layers[ly_fore].refs;
		r1!=NULL && r2!=NULL;
		r1=r1->next, r2=r2->next, ++cnt ) {
	    deltas[2*i][cnt] = r2->transform[4]-r1->transform[4];
	    deltas[2*i+1][cnt] = r2->transform[5]-r1->transform[5];
	}
	/* Phantom points */
	deltas[2*i][ptcnt-4] = 0; deltas[2*i+1][ptcnt-4] = 0;	/* lbearing */
	deltas[2*i][ptcnt-3] = mm->instances[i]->glyphs[gid]->width -mm->normal->glyphs[gid]->width;
		deltas[2*i+1][ptcnt-3] = 0;			/* horizontal advance */
	deltas[2*i][ptcnt-2] = 0; deltas[2*i+1][ptcnt-2] = 0;	/* top bearing */
	deltas[2*i][ptcnt-1] = 0;				/* vertical advance */
		deltas[2*i+1][ptcnt-1] = mm->instances[i]->glyphs[gid]->vwidth -mm->normal->glyphs[gid]->vwidth;	/* horizontal advance */
    }

    /* Ok, each delta now contains the difference between the instance[i] points */
    /*  and the base points. But that isn't good enough. We must subtract */
    /*  [0,1] and [1,0] from [1,1], and then subtract [1,1,0] [1,0,1] [0,1,1] */
    /*  from [1,1,1] and so on (also [-1,0] from [-1,1], etc.) */
    for ( j=1; j<mm->axis_count; ++j ) {
	for ( i=0; i<mm->instance_count; ++i ) {
	    for ( k=cnt=0; k<mm->axis_count; ++k )
		if ( mm->positions[i*mm->axis_count+k]!=0 )
		    ++cnt;
	    if ( cnt==j ) {
		for ( l = 0; l<mm->instance_count; ++l ) if ( l!=i ) {
		    for ( k=0; k<mm->axis_count; ++k )
			if ( mm->positions[i*mm->axis_count+k]!=0 &&
				mm->positions[l*mm->axis_count+k]!=mm->positions[i*mm->axis_count+k])
		    break;
		    if ( k==mm->axis_count ) {
			for ( k=0; k<ptcnt; ++k ) {
			    deltas[2*l][k] -= deltas[2*i][k];
			    deltas[2*l+1][k] -= deltas[2*i+1][k];
			}
		    }
		}
	    }
	}
    }

    /* If all variants of the glyph are the same, no point in having a gvar */
    /*  entry for it */
    for ( i=0 ; i<mm->instance_count; ++i ) {
	for ( j=0; j<ptcnt; ++j )
	    if ( deltas[i][j]!=0 )
	break;
	if ( j!=ptcnt )
    break;
    }
    if ( i==mm->instance_count ) {
	/* All zeros */
	for ( i=0 ; i<mm->instance_count; ++i )
	    free(deltas[i]);
	free(deltas);
return( NULL );
    }

return( deltas );
}

int16_t **CvtFindDeltas(MMSet *mm, int *_ptcnt) {
    int i, j, k, l, cnt, ptcnt;
    int16_t **deltas;
    struct ttf_table *cvt, *icvt;
    for ( cvt = mm->normal->ttf_tables; cvt!=NULL && cvt->tag!=CHR('c','v','t',' '); cvt=cvt->next );

    if ( cvt==NULL )
return( NULL );

    icvt = NULL;
    for ( i=0; i<mm->instance_count; ++i )
	if ( (icvt=mm->instances[i]->ttf_tables)!=NULL )
    break;
    if ( icvt==NULL )		/* No other cvt tables => no variation */
return( NULL );

    *_ptcnt = ptcnt = cvt->len/2;
    deltas = xcalloc(mm->instance_count,sizeof(int16_t *));
    for ( i=0; i<mm->instance_count; ++i ) if ( (icvt=mm->instances[i]->ttf_tables)!=NULL ) {
	deltas[i] = xcalloc(ptcnt,sizeof(int16_t));
	for ( j=0; j<ptcnt; ++j )
	    deltas[i][j] = memushort(icvt->data,icvt->len, sizeof(uint16_t)*j)-
		    memushort(cvt->data,cvt->len, sizeof(uint16_t)*j);
    }

    /* Ok, each delta now contains the difference between the instance[i] points */
    /*  and the base points. But that isn't good enough. We must subtract */
    /*  [0,1] and [1,0] from [1,1], and then subtract [1,1,0] [1,0,1] [0,1,1] */
    /*  from [1,1,1] and so on (also [-1,0] from [-1,1], etc.) */
    for ( j=1; j<mm->axis_count; ++j ) {
	for ( i=0; i<mm->instance_count; ++i ) if ( deltas[i]!=NULL ) {
	    for ( k=cnt=0; k<mm->axis_count; ++k )
		if ( mm->positions[i*mm->axis_count+k]!=0 )
		    ++cnt;
	    if ( cnt==j ) {
		for ( l = 0; l<mm->instance_count; ++l ) if ( l!=i && deltas[l]!=NULL ) {
		    for ( k=0; k<mm->axis_count; ++k )
			if ( mm->positions[i*mm->axis_count+k]!=0 &&
				mm->positions[l*mm->axis_count+k]!=mm->positions[i*mm->axis_count+k])
		    break;
		    if ( k==mm->axis_count ) {
			for ( k=0; k<ptcnt; ++k )
			    deltas[l][k] -= deltas[i][k];
		    }
		}
	    }
	}
    }

    /* If all variants of the cvt are the same, no point in having a gvar */
    /*  entry for it */
    for ( i=0 ; i<mm->instance_count; ++i ) if ( deltas[i]!=NULL ) {
	for ( j=0; j<ptcnt; ++j )
	    if ( deltas[i][j]!=0 )
	break;
	if ( j==ptcnt ) {
	    free(deltas[i]);
	    deltas[i] = NULL;
	}
    }
    for ( i=0 ; i<mm->instance_count; ++i )
	if ( deltas[i]!=NULL )
    break;
    if ( i==mm->instance_count ) {
	/* All zeros */
	free(deltas);
return( NULL );
    }

return( deltas );
}
