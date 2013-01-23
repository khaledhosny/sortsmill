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


struct kerncounts {
    int cnt;
    int vcnt;
    int mh, mv;
    int hsubs;
    int *hbreaks;
    int vsubs;
    int *vbreaks;
};

static int CountKerns(struct alltabs *at, SplineFont *sf, struct kerncounts *kcnt) {
    int i, cnt, vcnt, j, mh, mv;
    KernPair *kp;

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

return( kcnt->hsubs + kcnt->vsubs);
}

static void ttf_dumpsfkerns(struct alltabs *at, SplineFont *sf, int version) {
    struct kerncounts kcnt;
    int i, j, k, m, c, gid, tot, km;
    KernPair *kp;
    uint16_t *glnum, *offsets;
    int isv;
    int b, bmax;
    int *breaks;
    int winfail=0;

    if ( CountKerns(at,sf,&kcnt)==0 )
return;

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
		putshort(at->kern,version);		/* subtable version */
		if ( c>10920 )
		    ff_post_error(_("Too many kern pairs"),_("The 'kern' table supports at most 10920 kern pairs in a subtable"));
		putshort(at->kern,(7+3*c)*sizeof(uint16_t)); /* subtable length */
		putshort(at->kern,!isv);	/* coverage, flags=hor/vert&format=0 */
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
}

void ttf_dumpkerns(struct alltabs *at, SplineFont *sf) {
    int sum;
    int version;
    struct kerncounts kcnt;

    SFKernClassTempDecompose(sf,false);

    sum = CountKerns(at,sf,&kcnt);
    free(kcnt.hbreaks); free(kcnt.vbreaks);
    if ( sum==0 ) {
	SFKernCleanup(sf,false);
return;
    }

    /* Old kerning format (version 0) uses 16 bit quantities */
    /* Apple's new format (version 0x00010000) uses 32 bit quantities */
    version = 0;
    at->kern = tmpfile();
    putshort(at->kern,version);			/* version */
    putshort(at->kern,sum);			/* number of subtables */

    ttf_dumpsfkerns(at, sf, version);
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

struct transition { uint16_t next_state, dontconsume, ismark, trans_ent; LigList *l; };
struct trans_entries { uint16_t next_state, flags, act_index; LigList *l; };

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
