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
#ifndef _GDRAWP_H
#define _GDRAWP_H

#include "gdraw.h"

typedef struct gcol {
    int16_t red, green, blue;
    uint32_t pixel;
} GCol;

struct revcol /* : GCol */ {
    int16_t red, green, blue;
    uint32_t index;
    uint8_t dist;
    struct revcol *next;
};

struct revitem {
    struct revcol *cols[2];	/* cols[0] => colours in this subcube, cols[1] => those near */
    int16_t cnt;
    struct revcmap *sub;
};

struct revcmap {
    int16_t range;		/* red_max-red_min+1, total number of colors */
				/*  in the cube along any linear dimension */
    int16_t side_cnt;		/* Number of sub-cubes along each linear side side of the cube */
				/* ie. we decimate by a factor of side_cnt, there */
			        /*  will be side_cnt levels of red, and side_cnt^3*/
			        /*  subcubes */
    int16_t side_shift;		/* if side_cnt==2^n then this is n */
    int16_t div_mul, div_shift, div_add;
				/* tricks for dividing by range/side_cnt */
			        /* We can do (small) integer division by */
			        /*  multiplying by an integer reciprical and */
			        /*  left shifting */
    bool is_grey;
    Color mask;			/* masks off the high order bits that this revclut handles, leaving us with those bits of interest to the subclut */
    struct revitem *cube;
    GCol *greys;		/* 256 entries, if set */
};

typedef struct {		/* normal 16 bit characters are two bytes */
    unsigned char byte1;
    unsigned char byte2;
} GChar2b;

struct gchr_transform {
    uint32_t oldstate;
    uint32_t newstate;
    uint32_t resch;
};

struct gchr_lookup {
    int cnt;
    struct gchr_transform *transtab;
};

struct gchr_accents {
    uint32_t accent;
    uint32_t mask;
};

struct gwindow {
    GGC *ggc;
    GDisplay *display;
    int (*eh)(GWindow,GEvent *);
    GRect pos;
    GWindow parent;
    void *user_data;
    struct gwidgetdata *widget_data;
    void *native_window;
    bool is_visible;
    bool is_pixmap;
    bool is_toplevel;
    bool visible_request;
    bool is_dying;
    bool is_popup;
    bool disable_expose_requests;
};

struct ginput_context {
    GWindow w;
    enum gic_style style;
    void *ic;
    struct ginput_context *next;
};

struct gtimer {
    long time_sec;				/* longs not int32s to match timeval */
    long time_usec;
    int32_t repeat_time;				/* 0 => one shot */
    GWindow owner;
    void *userdata;
    struct gtimer *next;
    bool active;
};

struct gdisplay {
    void *semaphore;				/* To lock the display against multiple threads */
    int16_t res;
    int16_t scale_screen_by;			/* When converting screen pixels to printer pixels */
    GWindow groot;
    Color def_background, def_foreground;
    /* display specific data */
};
#define PointToPixel(points,res)		(((points)*(res)+36)/72)
#define PointTenthsToPixel(pointtenths,res)	((((pointtenths)*(res)+36)/72)/10)
#define PixelToPoint(pixels,res)		(((pixels)*72+(res)/2)/(res))
#define PixelToPointTenths(pixels,res)		(((pixels)*720+(res)/2)/(res))

extern void _GDraw_InitError(GDisplay *);
extern void _GDraw_ComposeChars(GDisplay *gdisp,GEvent *gevent);

extern void _GDraw_getimageclut(struct _GImage *base, struct gcol *clut);
extern const GCol *_GImage_GetIndexedPixel(Color col,RevCMap *rev);
extern const GCol *_GImage_GetIndexedPixelPrecise(Color col,RevCMap *rev);
#endif
