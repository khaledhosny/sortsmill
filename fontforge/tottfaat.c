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
#include "fontforge.h"
#include <utype.h>

#include "ttf.h"

/* This file contains routines to create some of the Apple Advanced Typography Tables */
/*  (or GX fonts)  */

/* ************************************************************************** */
/* *************************    The 'kern' table    ************************* */
/* ************************************************************************** */


/* Apple's docs imply that kerning info is always provided left to right, even*/
/*  for right to left scripts. If that be so then we need code in here to reverse */
/*  the order of the characters for right to left since pfaedit's convention */
/*  is to follow writing order rather than to go left to right */


static void DumpKernClass(FILE *file, uint16_t *class,int cnt,int add,int mul) {
    int i, first=-1, last=-1;

    for ( i=0; i<cnt; ++i ) {
	if ( class[i] ) last = i;
	if ( class[i] && first==-1 ) first = i;
    }
    putshort(file,first);
    putshort(file,last-first+1);
    for ( i=first; i<=last; ++i )
	putshort(file,class[i]*mul+add);
}

static int morx_dumpASM(FILE *temp,ASM *sm, struct alltabs *at, SplineFont *sf );

struct kerncounts {
    int cnt;
    int vcnt;
    int mh, mv;
    int kccnt;
    int vkccnt;
    int ksm;
    int hsubs;
    int *hbreaks;
    int vsubs;
    int *vbreaks;
};

static int CountKerns(struct alltabs *at, SplineFont *sf, struct kerncounts *kcnt) {
    int i, cnt, vcnt, j, kccnt=0, vkccnt=0, ksm=0, mh, mv;
    KernPair *kp;
    KernClass *kc;
    ASM *sm;

    cnt = mh = vcnt = mv = 0;
    for ( i=0; i<at->gi.gcnt; ++i ) if ( at->gi.bygid[i]!=-1 ) {
	j = 0;
	for ( kp = sf->glyphs[at->gi.bygid[i]]->kerns; kp!=NULL; kp=kp->next )
	    if ( kp->off!=0 && kp->sc->ttf_glyph!=-1 &&
		    LookupHasDefault(kp->subtable->lookup ))
		++cnt, ++j;
	if ( j>mh ) mh=j;
	j=0;
	for ( kp = sf->glyphs[at->gi.bygid[i]]->vkerns; kp!=NULL; kp=kp->next )
	    if ( kp->off!=0 && kp->sc->ttf_glyph!=-1 &&
		    LookupHasDefault(kp->subtable->lookup ))
		++vcnt, ++j;
	if ( j>mv ) mv=j;
    }
    kcnt->cnt = cnt;
    kcnt->vcnt = vcnt;
    kcnt->mh = mh;
    kcnt->mv = mv;
    kcnt->hbreaks = kcnt->vbreaks = NULL;
    if ( cnt>=10000 ) {
	/* the sub-table size is 6*cnt+14 or so and needs to be less 65535 */
	/*  so break it up into little bits */
	/* We might not need this when applemode is set because the subtable */
	/*  length is a long. BUT... there's a damn binsearch header with */
	/*  shorts in it still */
	int b=0;
	kcnt->hbreaks = xmalloc((at->gi.gcnt+1)*sizeof(int));
	cnt = 0;
	for ( i=0; i<at->gi.gcnt; ++i ) if ( at->gi.bygid[i]!=-1 ) {
	    j = 0;
	    for ( kp = sf->glyphs[at->gi.bygid[i]]->kerns; kp!=NULL; kp=kp->next )
		if ( kp->off!=0 && LookupHasDefault(kp->subtable->lookup ))
		    ++j;
	    if ( (cnt+j)*6>64000L && cnt!=0 ) {
		kcnt->hbreaks[b++] = cnt;
		cnt = 0;
	    }
	    cnt += j;
	}
	kcnt->hbreaks[b++] = cnt;
	kcnt->hsubs = b;
    } else if ( cnt!=0 )
	kcnt->hsubs = 1;
    else
	kcnt->hsubs = 0;
    if ( vcnt>=10000 ) {
	int b=0;
	kcnt->vbreaks = xmalloc((at->gi.gcnt+1)*sizeof(int));
	vcnt = 0;
	for ( i=0; i<at->gi.gcnt; ++i ) if ( at->gi.bygid[i]!=-1 ) {
	    j = 0;
	    for ( kp = sf->glyphs[at->gi.bygid[i]]->vkerns; kp!=NULL; kp=kp->next )
		if ( kp->off!=0 && LookupHasDefault(kp->subtable->lookup))
		    ++j;
	    if ( (vcnt+j)*6>64000L && vcnt!=0 ) {
		kcnt->vbreaks[b++] = vcnt;
		vcnt = 0;
	    }
	    vcnt += j;
	}
	kcnt->vbreaks[b++] = vcnt;
	kcnt->vsubs = b;
    } else if ( vcnt!=0 )
	kcnt->vsubs = 1;
    else
	kcnt->vsubs = 0;

    if ( at->applemode ) {	/* if we aren't outputting Apple's extensions to kerning (by classes, and by state machine) then don't check for those extensions */
	for ( kc=sf->kerns; kc!=NULL; kc = kc->next ) if ( LookupHasDefault(kc->subtable->lookup) )
	    ++kccnt;
	for ( kc=sf->vkerns; kc!=NULL; kc = kc->next ) if ( LookupHasDefault(kc->subtable->lookup) )
	    ++vkccnt;
	for ( sm=sf->sm; sm!=NULL; sm=sm->next )
	    if ( sm->type == asm_kern )
		++ksm;
    }
    kcnt->kccnt = kccnt;
    kcnt->vkccnt = vkccnt;
    kcnt->ksm = ksm;
return( kcnt->hsubs + kcnt->vsubs + kccnt + vkccnt + ksm );
}

static void ttf_dumpsfkerns(struct alltabs *at, SplineFont *sf, int tupleIndex, int version) {
    struct kerncounts kcnt;
    int i, j, k, m, c, gid, tot, km;
    KernPair *kp;
    KernClass *kc;
    ASM *sm;
    uint16_t *glnum, *offsets;
    int isv;
    int tupleMask = tupleIndex==-1 ? 0 : 0x2000;
    int b, bmax;
    int *breaks;
    int winfail=0;

    if ( CountKerns(at,sf,&kcnt)==0 )
return;

    if ( tupleIndex==-1 ) tupleIndex = 0;
    
    for ( isv=0; isv<2; ++isv ) {
	c = isv ? kcnt.vcnt : kcnt.cnt;
	bmax = isv ? kcnt.vsubs : kcnt.hsubs;
	breaks = isv ? kcnt.vbreaks : kcnt.hbreaks;
	if ( c!=0 ) {
	    km = isv ? kcnt.mv : kcnt.mh;
	    glnum = xmalloc(km*sizeof(uint16_t));
	    offsets = xmalloc(km*sizeof(uint16_t));
	    gid = 0;
	    for ( b=0; b<bmax; ++b ) {
		c = bmax==1 ? c : breaks[b];
		if ( version==0 ) {
		    putshort(at->kern,0);		/* subtable version */
		    if ( c>10920 )
			ff_post_error(_("Too many kern pairs"),_("The 'kern' table supports at most 10920 kern pairs in a subtable"));
		    putshort(at->kern,(7+3*c)*sizeof(uint16_t)); /* subtable length */
		    putshort(at->kern,!isv);	/* coverage, flags=hor/vert&format=0 */
		} else {
		    putlong(at->kern,(8+3*c)*sizeof(uint16_t)); /* subtable length */
		    /* Apple's new format has a completely different coverage format */
		    putshort(at->kern,(isv?0x8000:0)| /* format 0, horizontal/vertical flags (coverage) */
				    tupleMask);
		    putshort(at->kern,tupleIndex);
		}
		putshort(at->kern,c);
		for ( i=1,j=0; i<=c; i<<=1, ++j );
		i>>=1; --j;
		putshort(at->kern,i*6);		/* binary search headers */
		putshort(at->kern,j);
		putshort(at->kern,6*(c-i));

		for ( tot = 0; gid<at->gi.gcnt && tot<c; ++gid ) if ( at->gi.bygid[gid]!=-1 ) {
		    SplineChar *sc = sf->glyphs[at->gi.bygid[gid]];
		    m = 0;
		    for ( kp = isv ? sc->vkerns : sc->kerns; kp!=NULL; kp=kp->next ) {
			if ( kp->off!=0 && kp->sc->ttf_glyph!=-1 &&
				LookupHasDefault(kp->subtable->lookup)) {
			    /* order the pairs */
			    for ( j=0; j<m; ++j )
				if ( kp->sc->ttf_glyph<glnum[j] )
			    break;
			    for ( k=m; k>j; --k ) {
				glnum[k] = glnum[k-1];
				offsets[k] = offsets[k-1];
			    }
			    glnum[j] = kp->sc->ttf_glyph;
			    offsets[j] = kp->off;
			    ++m;
			    /* check if a pair will cause problems on Windows */
			    /* If the glyph is outside BMP, so either unicode >0xffff */
			    /*  or -1. Cast to unsigned catches both */
			    if( (unsigned)(sf->glyphs[at->gi.bygid[gid]]->unicodeenc)>0xFFFF ||
				    (unsigned)(sf->glyphs[at->gi.bygid[glnum[j]]]->unicodeenc)>0xFFFF )
				winfail++;
			}
		    }
		    for ( j=0; j<m; ++j ) {
			putshort(at->kern,gid);
			putshort(at->kern,glnum[j]);
			putshort(at->kern,offsets[j]);
		    }
		    tot += m;
		}
	    }
	    free(offsets);
	    free(glnum);
	}
    }
    free(kcnt.hbreaks); free(kcnt.vbreaks);

    if( winfail > 0 )
	ff_post_error(_("Kerning is likely to fail on Windows"),_(
		"On Windows many apps will have problems with this font's "
		"kerning, because %d of its glyph kern pairs cannot "
		"be mapped to unicode-BMP kern pairs"),
	    winfail);

    if ( at->applemode ) for ( isv=0; isv<2; ++isv ) {
	for ( kc=isv ? sf->vkerns : sf->kerns; kc!=NULL; kc=kc->next ) if ( LookupHasDefault(kc->subtable->lookup) ) {
	    /* If we are here, we must be using version 1 */
	    uint32_t len_pos = ftell(at->kern), pos;
	    uint16_t *class1, *class2;
	    int first_cnt = kc->first_cnt;

	    /* OpenType fonts can actually have a set of glyphs in class[0] of*/
	    /*  the first class. This happens when there are glyphs in the */
	    /*  coverage table which are not in any of the classes. Otherwise */
	    /*  class 0 is sort of useless in opentype */
	    if ( kc->firsts[0]!=NULL )
		++first_cnt;

	    putlong(at->kern,0); /* subtable length */
	    putshort(at->kern,(isv?0x8002:2)|	/* format 2, horizontal/vertical flags (coverage) */
			    tupleMask);
	    putshort(at->kern,tupleIndex);

	    putshort(at->kern,sizeof(uint16_t)*kc->second_cnt);
	    putshort(at->kern,0);		/* left classes */
	    putshort(at->kern,0);		/* right classes */
	    putshort(at->kern,16);		/* Offset to array, next byte */

	    if ( kc->firsts[0]!=NULL ) {
		/* Create a dummy class to correspond to the mac's class 0 */
		/*  all entries will be 0 */
		for ( i=0 ; i<kc->second_cnt; ++i )
		    putshort(at->kern,0);
	    }
	    for ( i=0; i<kc->first_cnt*kc->second_cnt; ++i )
		putshort(at->kern,kc->offsets[i]);

	    pos = ftell(at->kern);
	    fseek(at->kern,len_pos+10,SEEK_SET);
	    putshort(at->kern,pos-len_pos);
	    fseek(at->kern,pos,SEEK_SET);
	    class1 = ClassesFromNames(sf,kc->firsts,kc->first_cnt,at->maxp.numGlyphs,NULL,true);
	    DumpKernClass(at->kern,class1,at->maxp.numGlyphs,16,sizeof(uint16_t)*kc->second_cnt);
	    free(class1);

	    pos = ftell(at->kern);
	    fseek(at->kern,len_pos+12,SEEK_SET);
	    putshort(at->kern,pos-len_pos);
	    fseek(at->kern,pos,SEEK_SET);
	    class2 = ClassesFromNames(sf,kc->seconds,kc->second_cnt,at->maxp.numGlyphs,NULL,true);
	    DumpKernClass(at->kern,class2,at->maxp.numGlyphs,0,sizeof(uint16_t));
	    free(class2);

	    pos = ftell(at->kern);
	    fseek(at->kern,len_pos,SEEK_SET);
	    putlong(at->kern,pos-len_pos);
	    fseek(at->kern,pos,SEEK_SET);
	}
    }

    if ( at->applemode ) if ( kcnt.ksm!=0 ) {
	for ( sm=sf->sm; sm!=NULL; sm=sm->next ) if ( sm->type == asm_kern ) {
	    uint32_t len_pos = ftell(at->kern), pos;

	    putlong(at->kern,0); 		/* subtable length */
	    putshort(at->kern,((sm->flags&0x8000)?0x8001:1)|	/* format 1, horizontal/vertical flags (coverage) */
			    tupleMask);
	    putshort(at->kern,tupleIndex);
	    morx_dumpASM(at->kern,sm,at,sf);

	    pos = ftell(at->kern);
	    fseek(at->kern,len_pos,SEEK_SET);
	    putlong(at->kern,pos-len_pos);
	    fseek(at->kern,pos,SEEK_SET);
	}
    }
}

void ttf_dumpkerns(struct alltabs *at, SplineFont *sf) {
    int i, mmcnt=0, sum;
    int version;
    MMSet *mm = at->dovariations ? sf->mm : NULL;
    struct kerncounts kcnt;
    int must_use_old_style = 0;

    if ( !at->applemode && (!at->opentypemode || (at->gi.flags&ttf_flag_oldkern)) ) {
	must_use_old_style = true;
	SFKernClassTempDecompose(sf,false);
	mm = NULL;
    } else {
	if ( mm!=NULL ) {
	    for ( i=0; i<mm->instance_count; ++i ) {
		mmcnt += CountKerns(at,mm->instances[i],&kcnt);
		free(kcnt.hbreaks); free(kcnt.vbreaks);
	    }
	    sf = mm->normal;
	}
    }

    sum = CountKerns(at,sf,&kcnt);
    free(kcnt.hbreaks); free(kcnt.vbreaks);
    if ( sum==0 && mmcnt==0 ) {
	if ( must_use_old_style )
	    SFKernCleanup(sf,false);
return;
    }

    /* Old kerning format (version 0) uses 16 bit quantities */
    /* Apple's new format (version 0x00010000) uses 32 bit quantities */
    at->kern = tmpfile();
    if ( must_use_old_style  ||
	    ( kcnt.kccnt==0 && kcnt.vkccnt==0 && kcnt.ksm==0 && mmcnt==0 )) {
	/* MS does not support format 1,2,3 kern sub-tables so if we have them */
	/*  we might as well admit that this table is for apple only and use */
	/*  the new format apple recommends. Otherwise, use the old format */
	/* If we might need to store tuple data, use the new format */
	putshort(at->kern,0);			/* version */
	putshort(at->kern,sum);			/* number of subtables */
	version = 0;
    } else {
	putlong(at->kern,0x00010000);		/* version */
	putlong(at->kern,sum+mmcnt);		/* number of subtables */
	version = 1;
    }

    ttf_dumpsfkerns(at, sf, -1, version);
    if ( mm!=NULL ) {
	for ( i=0; i<mm->instance_count; ++i )
	    ttf_dumpsfkerns(at, mm->instances[i], i, version);
    }
    if ( must_use_old_style )
	SFKernCleanup(sf,false);

    at->kernlen = ftell(at->kern);
    if ( at->kernlen&2 )
	putshort(at->kern,0);		/* pad it */
}

/* ************************************************************************** */
/* *************************    The 'morx' table    ************************* */
/* *************************      (and 'feat')      ************************* */
/* ************************************************************************** */

/* Each lookup gets its own subtable, so there may be multiple subtables */
/*  with the same feature/setting. The subtables will be ordered the same */
/*  way the lookups are, which might lead to awkwardness if there are many */
/*  chains and the same feature occurs in several of them */
/* (only the default language will be used) */
struct feature {
    int16_t featureType, featureSetting;
    MacFeat *mf, *smf;
    struct macsetting *ms, *sms;
    bool vertOnly;
    bool r2l;	/* I think this is the "descending" flag */
    bool needsOff;
    bool singleMutex;
    bool dummyOff;
    uint8_t subtable_type;
    int chain;
    int32_t flag, offFlags;
    uint32_t feature_start;
    uint32_t feature_len;		/* Does not include header yet */
    struct feature *next;	/* features in output order */
    struct feature *nexttype;	/* features in feature/setting order */
    struct feature *nextsame;	/* all features with the same feature/setting */
    int setting_cnt, setting_index, real_index;
};

static int PSTHasTag(PST *pst, uint32_t tag);

static void mort_classes(FILE *temp,SplineFont *sf,struct glyphinfo *gi) {
    int first, last, i, cnt;
    /* Mort tables just have a trimmed byte array for the classes */

    for ( first=0; first<gi->gcnt; ++first )
	if ( gi->bygid[first]!=-1 && sf->glyphs[gi->bygid[first]]->lsidebearing!=1 )
    break;
    for ( last=gi->gcnt-1; last>first; --last )
	if ( gi->bygid[last]!=-1 && sf->glyphs[gi->bygid[last]]->lsidebearing!=1 )
    break;
    cnt = last-first+1;

    putshort(temp,first);
    putshort(temp,cnt);
    for ( i=first; i<=last; ++i )
	if ( gi->bygid[i]==-1 )
	    putc(1,temp);
	else
	    putc(sf->glyphs[gi->bygid[i]]->lsidebearing,temp);
    if ( cnt&1 )
	putc(1,temp);			/* Pad to a word boundary */
}

static void morx_lookupmap(FILE *temp,SplineChar **glyphs,uint16_t *maps,int gcnt) {
    int i, j, k, l, seg_cnt, tot, last, offset;
    /* We do four passes. The first just calculates how much space we will need (if any) */
    /*  the second provides the top-level lookup table structure */
    /*  the third provides the arrays of offsets needed for type 4 lookup tables */

    for ( k=0; k<3; ++k ) {
	for ( i=seg_cnt=tot=0; i<gcnt; ++i ) {
	    if ( glyphs[i]==NULL )
	continue;
	    if ( k==1 )
		tot = 0;
	    else if ( k==2 ) {
		putshort(temp,maps[i]);
	    }
	    last = i;
	    for ( j=i+1, ++tot; j<gcnt && glyphs[j]!=NULL && glyphs[j]->ttf_glyph==glyphs[i]->ttf_glyph+j-i; ++j ) {
		++tot;
		last = j;
		if ( k==2 ) {
		    putshort(temp,maps[j]);
		}
	    }
	    if ( k==1 ) {
		putshort(temp,glyphs[last]->ttf_glyph);
		putshort(temp,glyphs[i]->ttf_glyph);
		putshort(temp,offset);
		offset += 2*tot;
	    }
	    ++seg_cnt;
	    i = j-1;
	}
	if ( k==0 ) {
	    putshort(temp,4);		/* Lookup table format 4 */
		/* Binary search header */
	    putshort(temp,6);		/* Entry size */
	    putshort(temp,seg_cnt);	/* Number of segments */
	    for ( j=0,l=1; l<=seg_cnt; l<<=1, ++j );
	    --j; l>>=1;
	    putshort(temp,6*l);
	    putshort(temp,j);
	    putshort(temp,6*(seg_cnt-l));
	    if ( seg_cnt==0 )
return;
	    offset = 6*2 + seg_cnt*6 + 6;
	} else if ( k==1 ) {		/* flag entry */
	    putshort(temp,0xffff);
	    putshort(temp,0xffff);
	    putshort(temp,0);
	}
    }
}

struct transition { uint16_t next_state, dontconsume, ismark, trans_ent; LigList *l; };
struct trans_entries { uint16_t next_state, flags, act_index; LigList *l; };

static void morx_dumpnestedsubs(FILE *temp,SplineFont *sf,OTLookup *otl,struct glyphinfo *gi) {
    int i, j, gcnt;
    PST *pst;
    SplineChar **glyphs, *sc;
    uint16_t *map;
    struct lookup_subtable *sub = otl->subtables;	/* Mac can't have more than one subtable/lookup */

    for ( j=0; j<2; ++j ) {
	gcnt = 0;
	for ( i = 0; i<gi->gcnt; ++i ) if ( gi->bygid[i]!=-1 ) {
	    for ( pst=sf->glyphs[gi->bygid[i]]->possub;
		    pst!=NULL && pst->subtable!=sub;  pst=pst->next );
	    if ( pst!=NULL && pst->type==pst_substitution &&
		    (sc=SFGetChar(sf,-1,pst->u.subs.variant))!=NULL &&
		    sc->ttf_glyph!=-1 ) {
		if ( j ) {
		    glyphs[gcnt] = sf->glyphs[gi->bygid[i]];
		    map[gcnt] = sc->ttf_glyph;
		}
		++gcnt;
	    }
	}
	if ( !j ) {
	    glyphs = xmalloc((gcnt+1)*sizeof(SplineChar *));
	    map = xmalloc(gcnt*sizeof(uint16_t));
	    glyphs[gcnt] = NULL;
	}
    }
    morx_lookupmap(temp,glyphs,map,gcnt);
    free(glyphs);
    free(map);
}

static uint16_t *NamesToGlyphs(SplineFont *sf,char *names,uint16_t *cnt) {
    char *pt, *start;
    int c, ch;
    uint16_t *ret;
    SplineChar *sc;

    for ( c=0, pt=names; *pt; ++pt )
	if ( *pt==' ' ) ++c;
    ret = xmalloc((c+1)*sizeof(uint16_t));

    for ( c=0, pt=names; *pt; ) {
	while ( *pt==' ' ) ++pt;
	if ( *pt=='\0' )
    break;
	start = pt;
	while ( *pt!=' ' && *pt!='\0' ) ++pt;
	ch = *pt; *pt='\0';
	sc = SFGetChar(sf,-1,start);
	*pt = ch;
	if ( sc!=NULL && sc->ttf_glyph!=-1 )
	    ret[c++] = sc->ttf_glyph;
    }
    *cnt = c;
return( ret );
}

static int morx_dumpASM(FILE *temp,ASM *sm, struct alltabs *at, SplineFont *sf ) {
    int i, j, k, gcnt, ch;
    char *pt, *end;
    uint16_t *map;
    SplineChar **glyphs, *sc;
    int stcnt, tcnt;
    struct ins { char *names; uint16_t len,pos; uint16_t *glyphs; } *subsins=NULL;
    OTLookup **subslookups=NULL;
    uint32_t start, here, substable_pos, state_offset;
    struct transdata { uint16_t transition, mark_index, cur_index; } *transdata;
    struct trans { uint16_t ns, flags, mi, ci; } *trans;
    int ismort = sm->type == asm_kern;
    FILE *kernvalues;

    for ( i=0; i<sf->glyphcnt; ++i ) if ( sf->glyphs[i]!=NULL )
	sf->glyphs[i]->lsidebearing = 1;

    gcnt = 0;
    for ( i=4; i<sm->class_cnt; ++i ) {
	for ( pt = sm->classes[i]; ; pt=end ) {
	    while ( *pt==' ' ) ++pt;
	    if ( *pt=='\0' )
	break;
	    for ( end=pt; *end!='\0' && *end!=' '; ++end );
	    ch = *end; *end = '\0';
	    sc = SFGetChar(sf,-1,pt);
	    *end = ch;
	    if ( sc!=NULL ) {
		sc->lsidebearing = i;
		++gcnt;
	    }
	}
    }
    glyphs = xmalloc((gcnt+1)*sizeof(SplineChar *));
    map = xmalloc((gcnt+1)*sizeof(uint16_t));
    gcnt = 0;
    for ( i=0; i<at->gi.gcnt; ++i ) if ( at->gi.bygid[i]!=-1 && sf->glyphs[at->gi.bygid[i]]->lsidebearing!=1 ) {
	glyphs[gcnt] = sf->glyphs[at->gi.bygid[i]];
	map[gcnt++] = sf->glyphs[at->gi.bygid[i]]->lsidebearing;
    }
    glyphs[gcnt] = NULL;

    /* Give each subs tab an index into the mac's substitution lookups */
    transdata = xcalloc(sm->state_cnt*sm->class_cnt,sizeof(struct transdata));
    stcnt = 0;
    subslookups = NULL; subsins = NULL;
    if ( sm->type==asm_context ) {
	subslookups = xmalloc(2*sm->state_cnt*sm->class_cnt*sizeof(OTLookup));
	for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j ) {
	    struct asm_state *this = &sm->state[j];
	    transdata[j].mark_index = transdata[j].cur_index = 0xffff;
	    if ( this->u.context.mark_lookup!=NULL ) {
		for ( i=0; i<stcnt; ++i )
		    if ( subslookups[i]==this->u.context.mark_lookup )
		break;
		if ( i==stcnt )
		    subslookups[stcnt++] = this->u.context.mark_lookup;
		transdata[j].mark_index = i;
	    }
	    if ( this->u.context.cur_lookup!=NULL ) {
		for ( i=0; i<stcnt; ++i )
		    if ( subslookups[i]==this->u.context.cur_lookup )
		break;
		if ( i==stcnt )
		    subslookups[stcnt++] = this->u.context.cur_lookup;
		transdata[j].cur_index = i;
	    }
	}
    } else if ( sm->type==asm_insert ) {
	subsins = xmalloc(2*sm->state_cnt*sm->class_cnt*sizeof(struct ins));
	for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j ) {
	    struct asm_state *this = &sm->state[j];
	    transdata[j].mark_index = transdata[j].cur_index = 0xffff;
	    if ( this->u.insert.mark_ins!=0 ) {
		for ( i=0; i<stcnt; ++i )
		    if ( strcmp(subsins[i].names,this->u.insert.mark_ins)==0 )
		break;
		if ( i==stcnt ) {
		    subsins[stcnt].pos = stcnt==0 ? 0 : subsins[stcnt-1].pos +
							subsins[stcnt-1].len;
		    subsins[stcnt].names = this->u.insert.mark_ins;
		    subsins[stcnt].glyphs = NamesToGlyphs(sf,subsins[stcnt].names,&subsins[stcnt].len);
		    ++stcnt;
		}
		transdata[j].mark_index = subsins[i].pos;
	    }
	    if ( this->u.insert.cur_ins!=0 ) {
		for ( i=0; i<stcnt; ++i )
		    if ( strcmp(subsins[i].names,this->u.insert.cur_ins)==0 )
		break;
		if ( i==stcnt ) {
		    subsins[stcnt].pos = stcnt==0 ? 0 : subsins[stcnt-1].pos +
							subsins[stcnt-1].len;
		    subsins[stcnt].names = this->u.insert.cur_ins;
		    subsins[stcnt].glyphs = NamesToGlyphs(sf,subsins[stcnt].names,&subsins[stcnt].len);
		    ++stcnt;
		}
		transdata[j].cur_index = subsins[i].pos;
	    }
	}
    } else if ( sm->type==asm_kern ) {
	int off=0;
	kernvalues = tmpfile();
	for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j ) {
	    struct asm_state *this = &sm->state[j];
	    transdata[j].mark_index = 0xffff;
	    if ( this->u.kern.kcnt!=0 ) {
		for ( k=0; k<j; ++k )
		    if ( sm->state[k].u.kern.kcnt==this->u.kern.kcnt &&
			    memcmp(sm->state[k].u.kern.kerns,this->u.kern.kerns,
				    this->u.kern.kcnt*sizeof(int16_t))==0 )
		break;
		if ( k!=j )
		    transdata[j].mark_index = transdata[k].mark_index;
		else {
		    transdata[j].mark_index = off;
		    off += this->u.kern.kcnt*sizeof(int16_t);
		    /* kerning values must be output backwards */
		    for ( k=this->u.kern.kcnt-1; k>=1; --k )
			putshort(kernvalues,this->u.kern.kerns[k]&~1);
		    /* And the last one must be odd */
		    putshort(kernvalues,this->u.kern.kerns[0]|1);
		}
	    }
	}
    }

    trans = xmalloc(sm->state_cnt*sm->class_cnt*sizeof(struct trans));
    tcnt = 0;
    for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j ) {
	struct asm_state *this = &sm->state[j];
	for ( i=0; i<tcnt; ++i )
	    if ( trans[i].ns==this->next_state && trans[i].flags==this->flags &&
		    trans[i].mi==transdata[j].mark_index &&
		    trans[i].ci==transdata[j].cur_index )
	break;
	if ( i==tcnt ) {
	    trans[tcnt].ns = this->next_state;
	    trans[tcnt].flags = this->flags;
	    trans[tcnt].mi = transdata[j].mark_index;
	    trans[tcnt++].ci = transdata[j].cur_index;
	}
	transdata[j].transition = i;
    }


    /* Output the header */
    start = ftell(temp);
    if ( ismort /* old format still used for kerning */ ) {
	putshort(temp,sm->class_cnt);
	putshort(temp,5*sizeof(uint16_t));	/* class offset */
	putshort(temp,0);			/* state offset */
	putshort(temp,0);			/* transition entry offset */
	putshort(temp,0);			/* kerning values offset */
	mort_classes(temp,sf,&at->gi);			/* dump the class table */
    } else {
	putlong(temp,sm->class_cnt);
	if ( sm->type==asm_indic ) {
	    putlong(temp,4*sizeof(uint32_t));	/* class offset */
	    putlong(temp,0);			/* state offset */
	    putlong(temp,0);			/* transition entry offset */
	} else {
	    putlong(temp,5*sizeof(uint32_t));	/* class offset */
	    putlong(temp,0);			/* state offset */
	    putlong(temp,0);			/* transition entry offset */
	    putlong(temp,0);			/* substitution/insertion table offset */
	}
	morx_lookupmap(temp,glyphs,map,gcnt);/* dump the class lookup table */
    }
    free(glyphs); free(map);


    state_offset = ftell(temp)-start;
    if ( ismort ) {
	fseek(temp,start+2*sizeof(uint16_t),SEEK_SET);
	putshort(temp,state_offset);		/* Point to start of state arrays */
    } else {
	fseek(temp,start+2*sizeof(uint32_t),SEEK_SET);
	putlong(temp,state_offset);		/* Point to start of state arrays */
    }
    fseek(temp,0,SEEK_END);

    if ( ismort ) {
	for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j )
	    putc(transdata[j].transition,temp);
	if ( ftell(temp)&1 )
	    putc(0,temp);			/* Pad to a word boundry */
    } else {
	for ( j=0; j<sm->state_cnt*sm->class_cnt; ++j )
	    putshort(temp,transdata[j].transition);
    }
    free(transdata);

    here = ftell(temp);
    if ( ismort ) {
	fseek(temp,start+3*sizeof(uint16_t),SEEK_SET);
	putshort(temp,here-start);		/* Point to start of transition arrays */
    } else {
	fseek(temp,start+3*sizeof(uint32_t),SEEK_SET);
	putlong(temp,here-start);		/* Point to start of transition arrays */
    }
    fseek(temp,0,SEEK_END);

    /* Now the transitions */
    if ( sm->type==asm_kern ) {
	substable_pos = here+tcnt*2*sizeof(int16_t);
	for ( i=0; i<tcnt; ++i ) {
	    /* mort tables use an offset rather than the state number */
	    putshort(temp,trans[i].ns*sm->class_cnt+state_offset);
	    if ( trans[i].mi!=0xffff )
		trans[i].flags |= substable_pos-start+trans[i].mi;
	    putshort(temp,trans[i].flags);
	}
    } else {
	for ( i=0; i<tcnt; ++i ) {
	    putshort(temp,trans[i].ns);
	    putshort(temp,trans[i].flags);
	    if ( sm->type!=asm_indic && sm->type!=asm_kern ) {
		putshort(temp,trans[i].mi );
		putshort(temp,trans[i].ci );
	    }
	}
    }
    free(trans);

    if ( sm->type==asm_context ) {
	substable_pos = ftell(temp);
	fseek(temp,start+4*sizeof(uint32_t),SEEK_SET);
	putlong(temp,substable_pos-start);		/* Point to start of substitution lookup offsets */
	fseek(temp,0,SEEK_END);

	/* And finally the substitutions */
	for ( i=0; i<stcnt; ++i )
	    putlong(temp,0);	/* offsets to the substitutions */
	for ( i=0; i<stcnt; ++i ) {
	    here = ftell(temp);
	    fseek(temp,substable_pos+i*sizeof(uint32_t),SEEK_SET);
	    putlong(temp,here-substable_pos);
	    fseek(temp,0,SEEK_END);
	    morx_dumpnestedsubs(temp,sf,subslookups[i],&at->gi);
	}
	free(subslookups);
    } else if ( sm->type==asm_insert ) {
	substable_pos = ftell(temp);
	fseek(temp,start+4*sizeof(uint32_t),SEEK_SET);
	putlong(temp,substable_pos-start);		/* Point to start of insertions */
	fseek(temp,0,SEEK_END);

	for ( i=0; i<stcnt; ++i ) {
	    for ( j=0; j<subsins[i].len; ++j )
		putshort(temp,subsins[i].glyphs[j]);
	    free(subsins[i].glyphs);
	}
	free(subsins);
    } else if ( sm->type==asm_kern ) {
	if ( substable_pos!=ftell(temp) )
	    IError( "Kern Values table in wrong place.\n" );
	fseek(temp,start+4*sizeof(uint16_t),SEEK_SET);
	putshort(temp,substable_pos-start);		/* Point to start of insertions */
	fseek(temp,0,SEEK_END);
	if ( !ttfcopyfile(temp,kernvalues,substable_pos,"kern-subtable")) at->error = true;
    }
return( true );
}

int Macable(SplineFont *sf, OTLookup *otl) {
    int ft, fs;
    FeatureScriptLangList *features;

    switch ( otl->lookup_type ) {
    /* These lookup types are mac only */
      case kern_statemachine: case morx_indic: case morx_context: case morx_insert:
return( true );
    /* These lookup types or OpenType only */
      case gsub_multiple: case gsub_alternate:
      case gpos_single: case gpos_cursive: case gpos_mark2base:
      case gpos_mark2ligature: case gpos_mark2mark:
return( false );
    /* These are OpenType only, but they might be convertable to a state */
    /*  machine */
      case gsub_context:
      case gsub_contextchain: case gsub_reversecchain:
      case gpos_context: case gpos_contextchain:
	if ( sf==NULL || sf->sm!=NULL )
return( false );
	/* Else fall through into the test on the feature tag */;
    /* These two can be expressed in both, and might be either */
      case gsub_single: case gsub_ligature: case gpos_pair:
	for ( features = otl->features; features!=NULL; features = features->next ) {
	    if ( features->ismac || OTTagToMacFeature(features->featuretag,&ft,&fs))
return( true );
	}
    }
return( false );
}

/* ************************************************************************** */
/* *************************    The 'opbd' table    ************************* */
/* ************************************************************************** */

int haslrbounds(SplineChar *sc, PST **left, PST **right) {
    PST *pst;

    *left = *right = NULL;
    for ( pst=sc->possub; pst!=NULL ; pst=pst->next ) {
	if ( pst->type == pst_position ) {
	    if ( PSTHasTag(pst,CHR('l','f','b','d')) ) {
		*left = pst;
		if ( *right )
return( true );
	    } else if ( PSTHasTag(pst,CHR('r','t','b','d')) ) {
		*right = pst;
		if ( *left )
return( true );
	    }
	}
    }
return( *left!=NULL || *right!=NULL );
}

/* ************************************************************************** */
/* *************************    The 'prop' table    ************************* */
/* ************************************************************************** */

uint16_t *props_array(SplineFont *sf,struct glyphinfo *gi) {
    uint16_t *props;
    int i;
    SplineChar *sc, *bsc;
    int dir, isfloat, isbracket, offset, doit=false;
    AnchorPoint *ap;
    PST *pst;
    int p;

    props = xcalloc(gi->gcnt+1,sizeof(uint16_t));
    props[gi->gcnt] = -1;

    for ( i=0; i<gi->gcnt; ++i ) if ( (p = gi->bygid==NULL ? i : gi->bygid[i])!=-1 ) {
	sc = sf->glyphs[p];
	if ( sc!=NULL && (gi->bygid==NULL || sc->ttf_glyph!=-1 )) {
	    dir = 0;
	    if ( sc->unicodeenc>=0x10300 && sc->unicodeenc<=0x103ff )
		dir = 0;
	    else if ( sc->unicodeenc>=0x10800 && sc->unicodeenc<=0x103ff )
		dir = 1;
	    else if ( sc->unicodeenc!=-1 && sc->unicodeenc<0x10fff ) {
		if ( iseuronumeric(sc->unicodeenc) )
		    dir = 3;
		else if ( iseuronumsep(sc->unicodeenc))
		    dir = 4;
		else if ( iseuronumterm(sc->unicodeenc))
		    dir = 5;
		else if ( isarabnumeric(sc->unicodeenc))
		    dir = 6;
		else if ( iscommonsep(sc->unicodeenc))
		    dir = 7;
		else if ( isspace(sc->unicodeenc))
		    dir = 10;
		else if ( islefttoright(sc->unicodeenc) )
		    dir = 0;
		else if ( isrighttoleft(sc->unicodeenc) )
		    dir = 1;
		else if ( SCScriptFromUnicode(sc)==CHR('a','r','a','b') )
		    dir = 2;
		else if ( SCScriptFromUnicode(sc)==CHR('h','e','b','r') )
		    dir = 1;
		else
		    dir = 11;		/* Other neutrals */
		/* Not dealing with unicode 3 classes */
		/* nor block seperator/ segment seperator */
	    } else if ( SCScriptFromUnicode(sc)==CHR('a','r','a','b') )
		dir = 2;
	    else if ( SCScriptFromUnicode(sc)==CHR('h','e','b','r') )
		dir = 1;

	    if ( dir==1 || dir==2 ) doit = true;
	    isfloat = false;
	    if ( sc->width==0 &&
		    ((sc->anchor!=NULL && sc->anchor->type==at_mark) ||
		     (sc->unicodeenc!=-1 && sc->unicodeenc<0x10000 && iscombining(sc->unicodeenc))))
		isfloat = doit = true;
	    isbracket = offset = 0;
	    if ( sc->unicodeenc!=-1 && sc->unicodeenc<0x10000 && tomirror(sc->unicodeenc)!=0 ) {
		bsc = SFGetChar(sf,tomirror(sc->unicodeenc),NULL);
		if ( bsc!=NULL && bsc->ttf_glyph-sc->ttf_glyph>-8 && bsc->ttf_glyph-sc->ttf_glyph<8 ) {
		    isbracket = true;
		    offset = bsc->ttf_glyph-sc->ttf_glyph;
		}
	    }
	    if ( !isbracket ) {
		for ( pst=sc->possub; pst!=NULL && PSTHasTag(pst,CHR('r','t','l','a')); pst=pst->next );
		if ( pst!=NULL && pst->type==pst_substitution &&
			(bsc=SFGetChar(sf,-1,pst->u.subs.variant))!=NULL &&
			bsc->ttf_glyph!=-1 && bsc->ttf_glyph-sc->ttf_glyph>-8 && bsc->ttf_glyph-sc->ttf_glyph<8 ) {
		    isbracket = true;
		    offset = bsc->ttf_glyph-sc->ttf_glyph;
		    doit = true;
		}
	    }
	    if ( SCRightToLeft(sc) ) {
		/* Apple docs say attached right. So for r2l scripts we look for */
		/*  a cursive entry, and for l2r a cursive exit */
		for ( ap=sc->anchor; ap!=NULL && ap->type!=at_centry; ap=ap->next );
	    } else {
		for ( ap=sc->anchor; ap!=NULL && ap->type!=at_cexit; ap=ap->next );
	    }
	    props[sc->ttf_glyph] = dir |
		    (isfloat ? 0x8000 : 0 ) |
		    (isbracket ? 0x1000 : 0 ) |
		    (ap!=NULL ? 0x80 : 0 ) |
		    ((offset&0xf)<<8);
	    /* not dealing with */
	    /*	hang left 0x4000 */
	    /*	hang right 0x2000 */
	}
    }

    if ( !doit ) {
	free(props);
return( NULL );
    }

return( props );
}

/* ************************************************************************** */
/* *************************    The 'bsln' table    ************************* */
/* ************************************************************************** */

static int BslnFromTag(uint32_t tag) {
    switch ( tag ) {
      case CHR('r','o','m','n'):
return( 0 );
    /* Apple has a centered ideographic baseline, while OT has a top ideo bsln*/
    /* no way to get Apple's baseline #1 */
      case CHR('i','d','e','o'):
return( 2 );
      case CHR('h','a','n','g'):
return( 3 );
      case CHR('m','a','t','h'):
return( 4 );
      default:
return( 0xffff );
    }
}

int16_t *PerGlyphDefBaseline(SplineFont *sf,int *def_baseline) {
    int16_t *baselines = xmalloc(sf->glyphcnt*sizeof(int16_t));
    int gid, bsln, i, any;
    SplineChar *sc;
    int counts[32];		/* Apple supports a max of 32 baselines, but only 5 are defined */
    struct Base *base = sf->horiz_base;
    struct basescript *bs;
    int bestbsln, bestcnt;

    memset(counts,0,sizeof(counts));

    for ( gid = 0; gid<sf->glyphcnt; ++gid ) if ( (sc = sf->glyphs[gid])!=NULL ) {
	uint32_t script = SCScriptFromUnicode(sc);
	for ( bs= base->scripts; bs!=NULL; bs=bs->next )
	    if ( bs->script==script )
	break;
	if ( bs==NULL )
	    bsln = 0xffff;
	else
	    bsln = BslnFromTag( base->baseline_tags[bs->def_baseline] );
/* This if is duplicated (almost) in basedlg.c:Base_FinishEdit */
	if ( bsln==0xffff ) {
	    if ( script==CHR('k','a','n','a') || script==CHR('h','a','n','g') ||
		    script==CHR('h','a','n','i') || script==CHR('b','o','p','o') ||
		    script==CHR('j','a','m','o') || script==CHR('y','i',' ',' '))
		bsln = 2;
	    else if ( script==CHR('t','i','b','t' ) ||
		    script == CHR('b','e','n','g' ) || script == CHR('b','n','g','2') ||
		    script == CHR('d','e','v','a' ) || script == CHR('d','e','v','2') ||
		    script == CHR('g','u','j','r' ) || script == CHR('g','j','r','2') ||
		    script == CHR('g','u','r','u' ) || script == CHR('g','u','r','2') ||
		    script == CHR('k','n','d','a' ) || script == CHR('k','n','d','2') ||
		    script == CHR('m','l','y','m' ) || script == CHR('m','l','y','2') ||
		    script == CHR('o','r','y','a' ) || script == CHR('o','r','y','2') ||
		    script == CHR('t','a','m','l' ) || script == CHR('t','m','l','2') ||
		    script == CHR('t','e','l','u' ) || script == CHR('t','e','l','2'))
		bsln = 3;
	    else if ( script==CHR('m','a','t','h') )
		bsln = 4;
	    else
		bsln = 0;
	}
	baselines[gid] = bsln;
	if ( bsln!=0xffff )
	    ++counts[bsln];
    }
    
    bestbsln = 0;
    bestcnt = 0;
    any = 0;
    for ( i=0; i<32 ; ++i ) {
	if ( counts[i]>bestcnt ) {
	    bestbsln = i;
	    bestcnt = counts[i];
	    ++any;
	}
    }
    *def_baseline = bestbsln | (any<=1 ? 0x100 : 0 );
return( baselines );
}

void FigureBaseOffsets(SplineFont *sf,int def_bsln,int offsets[32]) {
    struct Base *base = sf->horiz_base;
    struct basescript *bs = base->scripts;
    int i;

    memset( offsets,0xff,32*sizeof(int));
    for ( i=0; i<base->baseline_cnt; ++i ) {
	int bsln = BslnFromTag(base->baseline_tags[i]);
	if ( bsln!=0xffff )
	    offsets[bsln] = bs->baseline_pos[i];
    }
    if ( offsets[def_bsln]!=-1 ) {
	for ( i=0; i<32; ++i ) {
	    if ( offsets[i]!=-1 )
		offsets[i] -= offsets[def_bsln];
	}
    }
    /* I suspect baseline 1 is the standard baseline for CJK glyphs on the mac*/
    /*  (because baseline 2 is often the same as baseline 1, which is wrong for 2) */
    /* OT doesn't have a centered ideographic baseline, so guestimate */
    /* And I don't want to base it on the actual ideo baseline (go up half an em?) */
    /*  because in my small sample of 'bsln' tables baseline 2 has been wrong */
    /*  most of the time, and it is wrong in the example in the docs. */
    /* (I know it is wrong because it has the same value as baseline 1, but */
    /*  is supposed to be below baseline 1 ) */
    if ( offsets[1]==-1 ) {
	if ( offsets[2]!=-1 )
	    offsets[1] = offsets[2]+(sf->ascent+sf->descent)/2;
	else
	    offsets[1] = (sf->ascent+sf->descent)/2 - sf->descent;
    }
    for ( i=0; i<32; ++i )
	if ( offsets[i]==-1 )
	    offsets[i] = 0;
}

/* ************************************************************************** */
/* *************************    utility routines    ************************* */
/* ************************************************************************** */

uint32_t MacFeatureToOTTag(int featureType,int featureSetting) {
    int i;
    struct macsettingname *msn = user_macfeat_otftag ? user_macfeat_otftag : macfeat_otftag;

    for ( i=0; msn[i].otf_tag!=0; ++i )
	if ( msn[i].mac_feature_type == featureType &&
		msn[i].mac_feature_setting == featureSetting )
return( msn[i].otf_tag );

return( 0 );
}

int OTTagToMacFeature(uint32_t tag, int *featureType,int *featureSetting) {
    int i;
    struct macsettingname *msn = user_macfeat_otftag ? user_macfeat_otftag : macfeat_otftag;

    for ( i=0; msn[i].otf_tag!=0; ++i )
	if ( msn[i].otf_tag == tag ) {
	    *featureType = msn[i].mac_feature_type;
	    *featureSetting = msn[i].mac_feature_setting;
return( true );
	}
    *featureType = (tag >> 16);
    *featureSetting = (tag & 0xFFFF);
	/* Ranges taken from Apple Font Registry. An OT tag without a 
    corresponding mac feature should fail this test.*/
    if (*featureType >= 0 && *featureType < 105 && *featureSetting < 16)
        return ( true );

    *featureType = 0;
    *featureSetting = 0;
return( false );
}

static int PSTHasTag(PST *pst, uint32_t tag) {
    FeatureScriptLangList *fl;

    if ( pst->subtable==NULL )
return( false );
    for ( fl=pst->subtable->lookup->features; fl!=NULL; fl=fl->next )
	if ( fl->featuretag == tag )
return( true );

return( false );
}

int scriptsHaveDefault(struct scriptlanglist *sl) {
    int i;

    for ( ; sl!=NULL; sl=sl->next ) {
	for ( i=0; i<sl->lang_cnt; ++i ) {
	    if ( (i<MAX_LANG && sl->langs[i]==DEFAULT_LANG) ||
		    (i>=MAX_LANG && sl->morelangs[i-MAX_LANG]==DEFAULT_LANG)) {
return( true );
	    }
	}
    }
return( false );
}

int LookupHasDefault(OTLookup *otl) {
    FeatureScriptLangList *feats;

    if ( otl->def_lang_checked )
return( otl->def_lang_found );

    otl->def_lang_checked = true;
    for ( feats=otl->features; feats!=NULL; feats = feats->next ) {
	if ( scriptsHaveDefault(feats->scripts) ) {
	    otl->def_lang_found = true;
return( true );
	}
    }
    otl->def_lang_found = false;
return( false );
}
