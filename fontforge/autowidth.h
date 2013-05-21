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

struct charone {
    real lbearing, rmax;
    real newl, newr;
    int baseserif, lefttops, righttops;		/* serif zones which affect this character */
    SplineChar *sc;
    int base, top;		/* bottom of character, number of decimation zones we've got */
    short *ledge;
    short *redge;
    struct charpair *asleft;
    struct charpair *asright;
};

struct charpair {
    struct charone *left, *right;
    struct charpair *nextasleft, *nextasright;
    int base, top;
    short *distances;
    short visual;
};

typedef struct widthinfo {
    real spacing;		/* desired spacing between letters */
    real decimation;
    real serifsize;
    real seriflength;
    real caph;
    real descent;
    real xheight;
    real n_stem_exterior_width, n_stem_interior_width;
    real current_I_spacing;
    int serifs[4][2];		/* Four serif zones: descent, baseline, xheight, cap */
    int lcnt, rcnt;		/* count of left and right chars respectively */
    int real_lcnt, real_rcnt;	/* what the user asked for. We might add I */
    int tcnt;			/* sum of r+l cnt */
    int pcnt;			/* pair count, often r*l cnt */
    int l_Ipos, r_Ipos;
    struct charone **left, **right;
    struct charpair **pairs;
    int space_guess;
    int threshold;
    SplineFont *sf;
    FontViewBase *fv;
    int layer;
    bool done;
    bool autokern;
    bool onlynegkerns;
    struct lookup_subtable *subtable;
} WidthInfo;

#define NOTREACHED	-9999.0

extern struct charone *AW_MakeCharOne(SplineChar *sc);
extern void AW_InitCharPairs(WidthInfo *wi);
extern void AW_FreeCharList(struct charone **list);
extern void AW_FreeCharPairs(struct charpair **list, int cnt);
extern void AW_ScriptSerifChecker(WidthInfo *wi);
extern int AW_ReadKernPairFile(char *fn,WidthInfo *wi);
extern void AW_BuildCharPairs(WidthInfo *wi);
extern void AW_AutoKern(WidthInfo *wi);
extern void AW_AutoWidth(WidthInfo *wi);
extern void AW_FindFontParameters(WidthInfo *wi);
extern void AW_KernRemoveBelowThreshold(SplineFont *sf,int threshold);

extern SplineFont *aw_old_sf;
extern int aw_old_spaceguess;
