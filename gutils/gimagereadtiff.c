#include <config.h>

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

#ifdef _NO_LIBTIFF

static int a_file_must_define_something = 0;    /* ANSI says so */

#else

#include <tiffio.h>

//#define int32_t _int32
//#define uint32_t _uint32
//#define int16_t _int16
//#define uint16_t _uint16
//#define int8_t _int8
//#define uint8_t _uint8

#include "gimage.h"

#undef uint32

GImage *
GImageReadTiff (char *filename)
{
  TIFF *tif;
  uint32_t w, h, i, j;
  uint32_t *ipt, *fpt;
  size_t npixels;
  uint32 *raster;
  GImage *ret = NULL;
  struct _GImage *base;

  tif = TIFFOpen (filename, "r");

  if (tif == NULL)
    return (ret);

  TIFFGetField (tif, TIFFTAG_IMAGEWIDTH, &w);
  TIFFGetField (tif, TIFFTAG_IMAGELENGTH, &h);
  npixels = w * h;
  raster = (uint32_t *) xmalloc (szmax (1, npixels * sizeof (uint32_t)));
  if (raster != NULL)
    {
      if (TIFFReadRGBAImage (tif, w, h, raster, 0))
        {
          ret = GImageCreate (it_true, w, h);
          if (ret != NULL)
            {
              base = ret->u.image;
              for (i = 0; i < h; ++i)
                {
                  ipt = (uint32_t *) (base->data + i * base->bytes_per_line);
                  fpt = raster + (h - 1 - i) * w;
                  for (j = 0; j < w; ++j)
                    *ipt++ =
                      COLOR_CREATE (TIFFGetR (fpt[j]), TIFFGetG (fpt[j]),
                                    TIFFGetB (fpt[j]));
                }
            }
        }
      free (raster);
    }
  TIFFClose (tif);
  return (ret);
}

#endif
