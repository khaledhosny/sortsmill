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
#ifndef X_DISPLAY_MISSING
#include "gxdrawP.h"
#include "gxcdrawP.h"
#include <math.h>
#include <string.h>

void _GXDraw_Image( GWindow w, GImage *image, GRect *src, int32 x, int32 y) {
    GXWindow gw = (GXWindow) w;

    _GXCDraw_Image(gw,image,src,x,y);
}

void _GXDraw_TileImage( GWindow w, GImage *image, GRect *src, int32 x, int32 y) {
    GXWindow gw = (GXWindow) w;

    _GXCDraw_TileImage(gw,image,src,x,y);
}

/* When drawing an anti-aliased glyph, I've been pretending that it's an image*/
/*  with colors running from foreground to background and with background be- */
/*  ing transparent. That works reasonably well -- on a blank background, but */
/*  when two glyphs overlap (as in a script font, for instance) we get a faint*/
/*  light halo around the edge of the second glyph. */
/* What we really want to do is use the grey levels as an alpha channel with */
/*  the foreground color as the color. But alpha channels haven't been avail- */
/*  able on most X-displays. An alternative is to do the composing ourselves  */
/*  in an image that's as big as the window, and then transfer that when done */
/*  That sounds slow. */
/* What should the composing look like? I'm not entirely but it should be */
/* somewhere between a "max" and a "clipped add" applied component by component*/
/*  of the color. X does not support either of those as primitives -- but X */
/*  does support bitwise boolean operators, and an "or" will always produce */
/*  a value somewhere between those extremes. */
/* Actually since the color values (black==foreground, white==background)   */
/*  generally run in the oposite direction from the alpha channel (100%=fore, */
/*  0%=back) we will need to reverse the "or" to be an "and", but the idea    */
/*  is the same */
void _GXDraw_Glyph( GWindow w, GImage *image, GRect *src, int32 x, int32 y) {
    GXWindow gw = (GXWindow) w;

    _GXCDraw_Glyph(gw,image,src,x,y);
}

/* ******************************** Magnified ******************************* */

GImage *_GImageExtract(struct _GImage *base,GRect *src,GRect *size,
	double xscale, double yscale) {
    static GImage temp;
    static struct _GImage tbase;
    static uint8 *data;
    static int dlen;
    int r,c;

    memset(&temp,0,sizeof(temp));
    tbase = *base;
    temp.u.image = &tbase;
    tbase.width = size->width; tbase.height = size->height;
    if ( base->image_type==it_mono )
	tbase.bytes_per_line = (size->width+7)/8;
    else if ( base->image_type==it_index )
	tbase.bytes_per_line = size->width;
    else
	tbase.bytes_per_line = 4*size->width;
    if ( tbase.bytes_per_line*size->height>dlen )
	data = grealloc(data,dlen = tbase.bytes_per_line*size->height );
    tbase.data = data;

    /* I used to use rint(x). Now I use floor(x). For normal images rint */
    /*  might be better, but for text we need floor */

    if ( base->image_type==it_mono ) {
	memset(data,0,tbase.height*tbase.bytes_per_line);
	for ( r=0; r<size->height; ++r ) {
	    int or = ((int) floor( (r+size->y)/yscale ));
	    uint8 *pt = data+r*tbase.bytes_per_line;
	    uint8 *opt = base->data+or*base->bytes_per_line;
	    for ( c=0; c<size->width; ++c ) {
		int oc = ((int) floor( (c+size->x)/xscale));
		if ( opt[oc>>3] & (0x80>>(oc&7)) )
		    pt[c>>3] |= (0x80>>(c&7));
	    }
	}
    } else if ( base->image_type==it_index ) {
	for ( r=0; r<size->height; ++r ) {
	    int or = ((int) floor( (r+size->y)/yscale ));
	    uint8 *pt = data+r*tbase.bytes_per_line;
	    uint8 *opt = base->data+or*base->bytes_per_line;
	    for ( c=0; c<size->width; ++c ) {
		int oc = ((int) floor( (c+size->x)/xscale));
		*pt++ = opt[oc];
	    }
	}
    } else {
	for ( r=0; r<size->height; ++r ) {
	    int or = ((int) floor( (r+size->y)/yscale ));
	    uint32 *pt = (uint32 *) (data+r*tbase.bytes_per_line);
	    uint32 *opt = (uint32 *) (base->data+or*base->bytes_per_line);
	    for ( c=0; c<size->width; ++c ) {
		int oc = ((int) floor( (c+size->x)/xscale));
		*pt++ = opt[oc];
	    }
	}
    }
return( &temp );
}

/* Given an image, magnify it so that its width/height are as specified */
/*  then extract the given given rectangle (in magnified coords) and */
/*  place it on the screen at x,y */
void _GXDraw_ImageMagnified(GWindow w, GImage *image, GRect *magsrc,
	int32 x, int32 y, int32 width, int32 height) {
    GXWindow gw = (GXWindow) w;

    _GXCDraw_ImageMagnified(gw,image,magsrc,x,y,width,height);
}

#else	/* NO X */
int gimagexdraw_a_file_must_define_something=3;
#endif
