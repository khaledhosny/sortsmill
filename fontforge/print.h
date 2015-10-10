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
#include "baseviews.h"

typedef struct printinfo {
    FontViewBase *fv;
    struct metricsview *mv;
    SplineChar *sc;
    SplineFont *mainsf;
    EncMap *mainmap;
    int pointsize;
    int32_t *pointsizes;
    int extrahspace, extravspace;
    FILE *out;
    bool showvm;
    bool overflow;
    bool done;
    bool hadsize;
    int ypos;
    int max;		/* max chars per line */
    int chline;		/* High order bits of characters we're outputting */
    int page;
    int lastbase;
    real xoff, yoff, scale;
    char *printer;
    int copies;
  /* data for pdf files */
    int *object_offsets;
    int *page_objects;
    int next_object, max_object;
    int next_page, max_page;
    /* In most print styles sfcnt==1 and we only print one font, but with */
    /*  sample text there may be many logical fonts. And each one may need to */
    /*  be represented by many actual fonts to encode all our glyphs */
    int sfcnt, sfmax, sfid;
    long start_cur_page;
    int lastfont, intext;
    struct layoutinfo *sample;
    int wassfid, wasfn, wasps;
    int lastx, lasty;
} PI, DI;

extern int PdfDumpGlyphResources(PI *pi,SplineChar *sc);
extern void makePatName(char *buffer,
	RefChar *ref,SplineChar *sc,int layer,int isstroke,int isgrad);
