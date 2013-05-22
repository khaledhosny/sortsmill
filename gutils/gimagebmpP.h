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
#ifndef _GIMAGEBMPP_H
#define _GIMAGEBMPP_H

#include <config.h>

struct bmpheader {
    char b;		/* should contain 'B' */
    char m;		/* should contain 'M' */
    long size;		/* lowbyte first, size of file in bytes */
    short mbz1;		/* reserved */
    short mbz2;		/* reserved */
    long offset;	/* lowbyte first, offset to bitmap */
    long headersize;	/* who cares */
    long width;
    long height;
    short planes;	/* must be 1 */
    short bitsperpixel;	/* 1,4,8,24 (later 16,32 also) */
    long compression;	/* 0 => none, 1=>8bit rlc, 2=>4bit rlc, 3=>16/32 */
    long imagesize;
    long ignore1;	/* suggested x pixels per meter */
    long ignore2;	/* suggested y pixels per meter */
    long colorsused;	/* size of color table */
    long colorsimportant;
    uint32_t clut[256];
    long red_mask, green_mask, blue_mask;
    int red_shift, green_shift, blue_shift;
    bool invert;
    unsigned char *byte_pixels;
    uint32_t *int32_pixels;
};
#endif
