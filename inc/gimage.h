/*
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

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
#ifndef _GIMAGE_H
#define _GIMAGE_H

#include <config.h>

#include "basics.h"

typedef uint32_t Color;

#define COLOR_UNKNOWN		((Color) 0xffffffff)
#define COLOR_TRANSPARENT	((Color) 0xffffffff)
#define COLOR_DEFAULT		((Color) 0xfffffffe)
#define COLOR_CREATE(r,g,b)	(((r)<<16) | ((g)<<8) | (b))
#define COLOR_ALPHA(col)	(((col)>>24))
#define COLOR_RED(col)		(((col)>>16) & 0xff)
#define COLOR_GREEN(col)	(((col)>>8) & 0xff)
#define COLOR_BLUE(col)		((col)&0xff)

struct hslrgb {
    double h,s,l,v;
    double r,g,b;
    uint8_t rgb, hsl, hsv;
};

struct hslrgba {
    double h,s,l,v;
    double r,g,b;
    uint8_t rgb, hsl, hsv, has_alpha;
    double alpha;
};

typedef struct clut {
    int16_t clut_len;
    bool is_grey;
    uint32_t trans_index; /* will be ignored for cluts in images, use base->trans instead */
    Color clut[256];
} GClut;

#define GCLUT_CLUT_EMPTY \
{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 \
}


typedef struct revcmap RevCMap;

enum image_type { it_mono, it_bitmap=it_mono, it_index, it_true, it_rgba };

struct _GImage {
/* Format: bitmaps are stored with the most significant bit first in byte units
	    indexed    images are stored in byte units
	    true color images are stored in 4 byte units, 0,red,green,blue
	    rgba       images are stored in 4 byte units, alpha,red,green blue
*/
    enum image_type image_type: 2;
    int16_t delay;		/* for animated GIFs, delay to next frame */
    int32_t width, height;
    int32_t bytes_per_line;
    uint8_t *data;
    GClut *clut;
    Color trans;		/* PNG supports more than one transparent color, we don't */
				/* for non-true color images this is the index, not a color */
};

/* We deal with 1 bit, 8 bit and 32 bit images internal. 1 bit images may have*/
/*  a clut (if they don't assume bw, 0==black, 1==white), 8 bit must have a */
/*  clut, 32bit are actually 24 bit RGB images, but we pad them for easy */
/*  accessing. it_screen means that we've got an image that can be drawn */
/*  directly on the screen */
typedef struct gimage {
    short list_len;		/* length of list */
    union {			/* depends on whether has_list is set */
	struct _GImage *image;
    	struct _GImage **images;
    } u;
    void *userdata;
    const char *filename;	/* PNG file name to be used by cairo */
} GImage;

enum pastetrans_type { ptt_paste_trans_to_trans, ptt_old_shines_through};

typedef struct grect {
    int32_t x,y,width,height;
} GRect;

#define GRECT_EMPTY { 0, 0, 0, 0 }


typedef struct gpoint {
    int16_t x,y;
} GPoint;

#define GPOINT_EMPTY { 0, 0 }


VISIBLE extern GImage *GImageCreate(enum image_type type, int32_t width, int32_t height);
extern GImage *_GImage_Create(enum image_type type, int32_t width, int32_t height);
VISIBLE extern void GImageDestroy(GImage *gi);
extern GImage *GImageCreateAnimation(GImage **images, int n);
extern GImage *GImageAddImageBefore(GImage *dest, GImage *src, int pos);

extern GImage *GImageBaseGetSub(struct _GImage *base, enum image_type it, GRect *src, GClut *nclut, RevCMap *rev);
extern GImage *GImageGetSub(GImage *image,enum image_type it, GRect *src, GClut *nclut, RevCMap *rev);
extern int GImageInsertToBase(struct _GImage *tobase, GImage *from, GRect *src, RevCMap *rev,
	int to_x, int to_y, enum pastetrans_type ptt );
extern int GImageInsert(GImage *to, GImage *from, GRect *src, RevCMap *rev,
	int to_x, int to_y, enum pastetrans_type ptt );
VISIBLE extern Color _GImageGetPixelColor(struct _GImage *base,int x, int y);	/* Obsolete */
extern Color GImageGetPixelColor(GImage *base,int x, int y);		/* Obsolete */
VISIBLE extern Color GImageGetPixelRGBA(GImage *base,int x, int y);
VISIBLE extern int GImageGetWidth(GImage *);
VISIBLE extern int GImageGetHeight(GImage *);
extern void *GImageGetUserData(GImage *img);
extern void GImageSetUserData(GImage *img,void *userdata);
extern void GImageResize(struct _GImage *tobase, struct _GImage *fbase,
	GRect *src, RevCMap *rev);
extern GImage *GImageResize32(GImage *from, GRect *src, int width, int height, Color trans);
extern GImage *GImageResizeSame(GImage *from, GRect *src, int width, int height, RevCMap *rev);
extern RevCMap *GClutReverse(GClut *clut,int side_size);
void GClut_RevCMapFree(RevCMap *rev);
extern GClut *GImageFindCLUT(GImage *image,GClut *clut,int clutmax);
extern int GImageSameClut(GClut *clut,GClut *nclut);
extern int GImageGreyClut(GClut *clut);
extern Color GImageColourFName(uint32_t *name);
extern Color _GImage_ColourFName(char *name);
extern char *GImageNameFColour(Color col);
extern Color GDrawColorDarken(Color col, int by);
extern Color GDrawColorBrighten(Color col, int by);

extern int GImageWriteGImage(GImage *gi, char *filename);
VISIBLE extern int GImageWrite_Bmp(GImage *gi, FILE *fp);
VISIBLE extern int GImageWriteBmp(GImage *gi, char *filename);
VISIBLE extern GImage *GImageRead_Bmp(FILE *file);
extern GImage *GImageReadBmp(char *filename);
VISIBLE extern int GImageWriteXbm(GImage *gi, char *filename);
extern GImage *GImageReadXbm(char *filename);
extern int GImageWriteXpm(GImage *gi, char *filename);
extern GImage *GImageReadXpm(char *filename);
extern int GImageWriteEps(GImage *gi, char *filename);
extern GImage *GImageReadTiff(char *filename);
extern GImage *GImageReadJpeg(char *filename);
VISIBLE extern GImage *GImageRead_Jpeg(FILE *fp);
VISIBLE extern int GImageWrite_Jpeg(GImage *gi, FILE *outfile, int quality, int progressive);
extern int GImageWriteJpeg(GImage *gi, char *filename, int quality, int progressive);
VISIBLE extern GImage *GImageRead_Png(FILE *fp);
extern GImage *GImageReadPng(char *filename);
VISIBLE extern int GImageWrite_Png(GImage *gi, FILE *fp, int progressive);
VISIBLE extern int GImageWritePng(GImage *gi, char *filename, int progressive);
extern GImage *GImageReadGif(char *filename);
extern int GImageWriteGif(GImage *gi,char *filename,int progressive);
extern GImage *GImageReadRas(char *filename);		/* Sun Raster */
extern GImage *GImageReadRgb(char *filename);		/* SGI */
VISIBLE extern GImage *GImageRead(char *filename);

VISIBLE extern void GImageDrawRect(GImage *img,GRect *r,Color col);
VISIBLE extern void GImageDrawImage(GImage *dest,GImage *src,GRect *junk,int x, int y);

extern void gRGB2HSL(struct hslrgb *col);
VISIBLE extern void gHSL2RGB(struct hslrgb *col);
VISIBLE extern void gRGB2HSV(struct hslrgb *col);
VISIBLE extern void gHSV2RGB(struct hslrgb *col);
VISIBLE extern void gColor2Hslrgb(struct hslrgb *col,Color from);
VISIBLE extern void gColor2Hslrgba(struct hslrgba *col,Color from);
VISIBLE extern Color gHslrgb2Color(struct hslrgb *col);
VISIBLE extern Color gHslrgba2Color(struct hslrgba *col);

#endif
