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
    int16 red, green, blue;
    uint32 pixel;
} GCol;

struct revcol /* : GCol */ {
    int16 red, green, blue;
    uint32 index;
    uint8 dist;
    struct revcol *next;
};

struct revitem {
    struct revcol *cols[2];	/* cols[0] => colours in this subcube, cols[1] => those near */
    int16 cnt;
    struct revcmap *sub;
};

struct revcmap {
    int16 range;		/* red_max-red_min+1, total number of colors */
				/*  in the cube along any linear dimension */
    int16 side_cnt;		/* Number of sub-cubes along each linear side side of the cube */
				/* ie. we decimate by a factor of side_cnt, there */
			        /*  will be side_cnt levels of red, and side_cnt^3*/
			        /*  subcubes */
    int16 side_shift;		/* if side_cnt==2^n then this is n */
    int16 div_mul, div_shift, div_add;
				/* tricks for dividing by range/side_cnt */
			        /* We can do (small) integer division by */
			        /*  multiplying by an integer reciprical and */
			        /*  left shifting */
    unsigned int is_grey: 1;
    Color mask;			/* masks off the high order bits that this revclut handles, leaving us with those bits of interest to the subclut */
    struct revitem *cube;
    GCol *greys;		/* 256 entries, if set */
};

typedef struct {		/* normal 16 bit characters are two bytes */
    unsigned char byte1;
    unsigned char byte2;
} GChar2b;

struct gchr_transform {
    uint32 oldstate;
    uint32 newstate;
    unichar_t resch;
};

struct gchr_lookup {
    int cnt;
    struct gchr_transform *transtab;
};

struct gchr_accents {
    unichar_t accent;
    uint32 mask;
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
    unsigned int is_visible: 1;
    unsigned int is_pixmap: 1;
    unsigned int is_toplevel: 1;
    unsigned int visible_request: 1;
    unsigned int is_dying: 1;
    unsigned int is_popup: 1;
    unsigned int disable_expose_requests: 1;
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
    int32 repeat_time;				/* 0 => one shot */
    GWindow owner;
    void *userdata;
    struct gtimer *next;
    unsigned int active: 1;
};

struct gdisplay {
    void *semaphore;				/* To lock the display against multiple threads */
    int16 res;
    int16 scale_screen_by;			/* When converting screen pixels to printer pixels */
    GWindow groot;
    Color def_background, def_foreground;
    uint16 mykey_state;
    uint16 mykey_keysym;
    uint16 mykey_mask;
    unsigned int mykeybuild: 1;
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
