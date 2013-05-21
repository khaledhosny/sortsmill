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

/* Copyright (C) 2007-2012 by George Williams */
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

#ifndef _SFLAYOUTP_H
# define _SFLAYOUTP_H

enum sftf_fonttype { sftf_pfb, sftf_ttf, sftf_otf, sftf_nohints, sftf_bitmap, sftf_pfaedit };

typedef struct fontdata {
    SplineFont *sf;
    enum sftf_fonttype fonttype;
    int pointsize;
    int antialias;
    int layer;
    BDFFont *bdf;
    struct fontdata *next;
    struct fontdata *depends_on;	/* We use much of the ftc allocated for depends_on */
					/* Can't free depends_on until after we get freeed */
    struct _GImage base;
    GImage gi;
    GClut clut;
    struct sfmaps *sfmap;
} FontData;

struct lineheights {
    int32_t y;
    int16_t as, fh;
    uint16_t p, linelen;
    uint32_t start_pos;
};

struct fontlist {
    int start, end;		/* starting and ending characters [start,end) */
				/*  always break at newline & will omit it between fontlists */
    uint32_t *feats;		/* Ends with a 0 entry */
    uint32_t script, lang;
    FontData *fd;
    SplineChar **sctext;
    int scmax;
    struct opentype_str *ottext;
    struct fontlist *next;
};

struct sfmaps {
    SplineFont *sf;
    EncMap *map;
    int16_t sfbit_id;
    int16_t notdef_gid;
    SplineChar *fake_notdef;
    struct sfmaps *next;
};

struct paras {
    struct opentype_str **para;	/* An array of pointers to ottext entries */
    int start_pos;
};

typedef struct layoutinfo {
    uint32_t *text, *oldtext;	/* Input glyphs (in unicode) */
    int16_t lcnt, lmax;
    struct opentype_str ***lines;	/* pointers into the paras array */
    int16_t xmax;
    struct lineheights *lineheights;
    struct fontlist *fontlist, *oldfontlist;
    struct sfmaps *sfmaps;
    struct paras *paras;
    int pcnt, pmax;
    int ps, pe, ls, le;
    struct fontlist *oldstart, *oldend;
    FontData *generated;
    float dpi;
    bool wrap;
} LayoutInfo;

extern void GImageDrawRect(GImage *img,GRect *r,Color col);
extern void GImageDrawImage(GImage *dest,GImage *src,GRect *junk,int x, int y);
VISIBLE extern int LI_FDDrawChar(void *data,
	void (*drawImage)(void *,GImage *,GRect *,int x, int y),
	void (*drawRect)(void *,GRect *,Color col),
	struct opentype_str *osc,int x,int y,Color col);
VISIBLE extern uint32_t *LI_TagsCopy(uint32_t *tags);
extern struct fontlist *LI_fontlistcopy(struct fontlist *fl );
VISIBLE extern void LI_fontlistmergecheck(LayoutInfo *li);
VISIBLE extern void LayoutInfoRefigureLines(LayoutInfo *li, int start_of_change,
	int end_of_change, int width);
VISIBLE extern int LayoutInfoReplace(LayoutInfo *li, const uint32_t *str,
	int sel_start, int sel_end,int width);
VISIBLE extern void LayoutInfo_Destroy(LayoutInfo *li);
VISIBLE extern void SFMapFill(struct sfmaps *sfmaps,SplineFont *sf);
extern struct sfmaps *SFMapOfSF(LayoutInfo *li,SplineFont *sf);
VISIBLE extern FontData *LI_FindFontData(LayoutInfo *li, SplineFont *sf,
	int layer, enum sftf_fonttype fonttype, int size, int antialias);
VISIBLE extern FontData *LI_RegenFontData(LayoutInfo *li, FontData *ret);
VISIBLE extern void LayoutInfoInitLangSys(LayoutInfo *li, int end, uint32_t script, uint32_t lang);
VISIBLE extern LayoutInfo *LIConvertToPrint(LayoutInfo *li, int width, int height, int dpi);
VISIBLE extern SplineSet *LIConvertToSplines(LayoutInfo *li,double dpi,int order2);
extern void LayoutInfoSetTitle(LayoutInfo *li,const uint32_t *tit,int width);
VISIBLE extern struct fontlist *LI_BreakFontList(LayoutInfo *li,int start,int end);
VISIBLE extern int LI_SetFontData(LayoutInfo *li, int start, int end, SplineFont *sf,
	int layer, enum sftf_fonttype fonttype, int size, int antialias,int width);
#endif
