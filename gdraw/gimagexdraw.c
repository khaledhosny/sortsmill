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

static GImage *xi1_to_gi1(GXDisplay *gdisp,XImage *xi) {
    GImage *gi;
    struct _GImage *base;

    gi = gcalloc(1,sizeof(GImage));
    base = galloc(sizeof(struct _GImage));
    if ( gi==NULL || base==NULL )
return( NULL );
    gi->u.image = base;
    base->image_type = it_mono;
    base->width = xi->width;
    base->height = xi->height;
    base->bytes_per_line = xi->bytes_per_line;
    base->data = (uint8 *) (xi->data);
    base->clut = NULL;
    base->trans = COLOR_UNKNOWN;

    if ( xi->bitmap_bit_order==LSBFirst ) {
	/* sigh. The server doesn't use our convention. invert all bytes */
	int len = base->height*base->bytes_per_line;
	uint8 *newdata = galloc(len), *pt, *ipt, *end;
	int m1,m2,val;

	for ( ipt = (uint8 *) xi->data, pt=newdata, end=pt+len; pt<end; ++pt, ++ipt ) {
	    val = 0;
	    for ( m1=1, m2=0x80; m2!=0; m1<<=1, m2>>=1 )
		if ( *ipt&m1 ) val|=m2;
	    *pt = val;
	}
	base->data = newdata;
    } else
	xi->data = NULL;
return( gi );
}

static GImage *xi8_to_gi8(GXDisplay *gdisp,XImage *xi) {
    GImage *gi;
    struct _GImage *base;
    GClut *clut;
    int i;
    XColor cols[256];

    gi = gcalloc(1,sizeof(GImage));
    base = galloc(sizeof(struct _GImage));
    clut = galloc(sizeof(GClut));
    if ( gi==NULL || base==NULL )
return( NULL );
    gi->u.image = base;
    base->image_type = it_index;
    base->width = xi->width;
    base->height = xi->height;
    base->bytes_per_line = xi->bytes_per_line;
    base->data = (uint8 *) xi->data;
    base->clut = clut;
    base->trans = COLOR_UNKNOWN;

    clut->clut_len = 256;
    for ( i=0; i<(1<<gdisp->pixel_size); ++i )
	cols[i].pixel = i;
    XQueryColors(gdisp->display,gdisp->cmap,cols,1<<gdisp->pixel_size);
    for ( i=0; i<(1<<gdisp->pixel_size); ++i )
	clut->clut[i] = COLOR_CREATE(cols[i].red>>8, cols[i].green>>8, cols[i].blue>>8);
    clut->is_grey = ( gdisp->visual->class==StaticGray || gdisp->visual->class==GrayScale );
return( gi );
}

static GImage *xi16_to_gi32(GXDisplay *gdisp,XImage *xi) {
    GImage *gi;
    struct _GImage *base;
    uint16 *pt; uint32 *ipt, val;
    int i,j,rs,gs,bs;
    int rs2,gs2=0,bs2;
    int rm, gm, bm;

    if (( gi = GImageCreate(it_true,xi->width,xi->height))==NULL )
return( NULL );
    base = gi->u.image;

    rs = gdisp->cs.red_shift; gs = gdisp->cs.green_shift; bs = gdisp->cs.blue_shift;
    rm = gdisp->visual->red_mask; gm = gdisp->visual->green_mask; bm = gdisp->visual->blue_mask;
    if ( rs>gs && rs>bs ) {
	rs2 = 8-(16-rs);
	if ( gs>bs ) {
	    bs2 = 8-gs2;
	    gs2 = 8-(rs-gs);
	} else {
	    gs2 = 8-bs;
	    bs2 = 8-(rs-bs);
	}
    } else if ( gs>rs && gs>bs ) {
	gs2 = 8-(16-gs);
	if ( rs>bs ) {
	    bs2 = 8-rs;
	    rs2 = 8-(gs-rs);
	} else {
	    rs2 = 8-bs;
	    bs2 = 8-(gs-bs);
	}
    } else {
	bs2 = 8-(16-bs);
	if ( rs>gs ) {
	    gs2 = 8-rs;
	    rs2 = 8-(bs-rs);
	} else {
	    rs2 = 8-gs;
	    gs2 = 8-(bs-gs);
	}
    }

    for ( i=0; i<base->height; ++i ) {
	pt = (uint16 *) (xi->data + i*xi->bytes_per_line);
	ipt = (uint32 *) (base->data + i*base->bytes_per_line);
	for ( j=0; j<base->width; ++j ) {
	    val = *pt++;
	    if ( val!=0 )
		val = pt[-1];
	    *ipt++ = COLOR_CREATE(((val&rm)>>rs)<<rs2,((val&gm)>>gs)<<gs2,((val&bm)>>bs)<<bs2);
	}
    }
return( gi );
}

static GImage *xi24_to_gi32(GXDisplay *gdisp,XImage *xi) {
    GImage *gi;
    struct _GImage *base;
    uint8 *pt; uint32 *ipt, val;
    int i,j,rs,gs,bs;

    if (( gi = GImageCreate(it_true,xi->width,xi->height))==NULL )
return( NULL );
    base = gi->u.image;

    rs = gdisp->cs.red_shift; gs = gdisp->cs.green_shift; bs = gdisp->cs.blue_shift;
    for ( i=0; i<base->height; ++i ) {
	pt = (uint8 *) xi->data + i*xi->bytes_per_line;
	ipt = (uint32 *) (base->data + i*base->bytes_per_line);
	for ( j=0; j<base->width; ++j ) {
	    if ( xi->byte_order==MSBFirst ) {
		val = *pt++;
		val = (val<<8) + *pt++;
		val = (val<<8) + *pt++;
	    } else {
		val = *pt++;
		val |= (*pt++<<8);
		val |= (*pt++<<16);
	    }
	    *ipt++ = COLOR_CREATE((val>>rs)&0xff,(val>>gs)&0xff,(val>>bs)&0xff);
	}
    }
return( gi );
}

static GImage *xi32_to_gi32(GXDisplay *gdisp,XImage *xi) {
    GImage *gi;
    struct _GImage *base;
    uint32 *pt; uint32 *ipt, val;
    int i,j,rs,gs,bs;

    if (( gi = GImageCreate(it_true,xi->width,xi->height))==NULL )
return( NULL );
    base = gi->u.image;

    rs = gdisp->cs.red_shift; gs = gdisp->cs.green_shift; bs = gdisp->cs.blue_shift;
    for ( i=0; i<base->height; ++i ) {
	pt = (uint32 *) (xi->data + i*xi->bytes_per_line);
	ipt = (uint32 *) (base->data + i*base->bytes_per_line);
	for ( j=0; j<base->width; ++j ) {
	    val = *pt++;
	    *ipt++ = COLOR_CREATE((val>>rs)&0xff,(val>>gs)&0xff,(val>>bs)&0xff);
	}
    }
return( gi );
}

GImage *_GXDraw_CopyScreenToImage(GWindow _w, GRect *rect) {
    GXWindow gw = (GXWindow) _w;
    GXDisplay *gdisp = gw->display;
    Display *display=gdisp->display;
    Window w = gw->w;
    int depth;
    XImage *xi;
    GImage *gi=NULL;

    depth = gdisp->pixel_size;
    if ( gw->ggc->bitmap_col ) depth = 1;

    if ( depth!=1 && depth!=8 && depth!=16 && depth!=24 && depth!=32 )
return( NULL );
    xi = XGetImage(display,w,rect->x,rect->y, rect->width, rect->height,
	    -1,ZPixmap);
    if ( xi==NULL )
return( NULL );
    switch ( xi->bits_per_pixel ) {
      case 1:
	gi = xi1_to_gi1(gdisp,xi);
      break;
      case 8:
	gi = xi8_to_gi8(gdisp,xi);
      break;
      case 16:
	gi = xi16_to_gi32(gdisp,xi);
      break;
      case 24:
	gi = xi24_to_gi32(gdisp,xi);
      break;
      case 32:
	gi = xi32_to_gi32(gdisp,xi);
      break;
    }
    XDestroyImage(xi);
return( gi );
}
#else	/* NO X */
int gimagexdraw_a_file_must_define_something=3;
#endif
