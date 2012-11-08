#include <config.h>

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
#include "fontforgeui.h"

GCursor ct_magplus, ct_magminus, ct_circle, ct_square, ct_triangle,
	ct_ruler, ct_pen, ct_knife, ct_rotate, ct_skew, ct_scale, ct_flip,
	ct_3drotate, ct_perspective, ct_hvcircle, ct_g2circle;
GCursor ct_rect, ct_elipse, ct_poly, ct_star, ct_pencil, ct_shift, ct_line,
	ct_myhand, ct_filledrect, ct_filledelipse, ct_setwidth, ct_eyedropper;
GCursor ct_nesw, ct_nwse;
GCursor ct_rbearing, ct_kerning, ct_lbearing;
GCursor ct_prohibition, ct_ddcursor, ct_features;
GCursor ct_spiroleft, ct_spiroright;
GWindow logo_icon;

#define logo_width 32
#define logo_height 32
static unsigned char logo_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3c, 0xf0, 0x00, 0x00,
   0x6c, 0xb0, 0x01, 0x00, 0xe0, 0x86, 0xfb, 0x01, 0xc0, 0x28, 0x53, 0x03,
   0xd0, 0x55, 0xa7, 0x02, 0x80, 0x01, 0x06, 0x03, 0xf0, 0xdf, 0x7f, 0x02,
   0xf0, 0xdf, 0x7f, 0x03, 0x00, 0x03, 0x0c, 0x02, 0x30, 0x57, 0x5d, 0x03,
   0x50, 0xa6, 0x98, 0x02, 0xb0, 0x4e, 0x39, 0x03, 0x50, 0xae, 0x3a, 0x02,
   0xb0, 0x4c, 0x31, 0x03, 0x50, 0x9d, 0x72, 0x02, 0xb0, 0x18, 0x65, 0x02,
   0x50, 0x39, 0xe0, 0x00, 0xb0, 0x72, 0xc6, 0x19, 0x50, 0xe5, 0x97, 0x1f,
   0xb0, 0x8a, 0x23, 0x0e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf0, 0x3f, 0xff, 0x03, 0xe0, 0x3f, 0xff, 0x01, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f, 0xff, 0x03, 0xf0, 0x3f, 0xff, 0x03,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };

void InitCursors(void) {
    ct_magplus = GDrawCreateCursor("magplus");
    ct_magminus = GDrawCreateCursor("magminus");

    ct_circle = GDrawCreateCursor("pointercic");
    ct_hvcircle = GDrawCreateCursor("pointerhvcic");
    ct_triangle = GDrawCreateCursor("pointertri");
    ct_square = GDrawCreateCursor("pointersqr");
    ct_pen = GDrawCreateCursor("pencur");
    ct_setwidth = GDrawCreateCursor("setwidthcur");
    ct_g2circle = GDrawCreateCursor("pointerg2cic");

    ct_spiroleft = GDrawCreateCursor("pointerleft");
    ct_spiroright = GDrawCreateCursor("pointerright");

    ct_ruler = GDrawCreateCursor("rulercur");
    ct_knife = GDrawCreateCursor("knifecur");
    ct_flip = GDrawCreateCursor("flipcur");
    ct_rotate = GDrawCreateCursor("rotatecur");
    ct_scale = GDrawCreateCursor("scalecur");
    ct_skew = GDrawCreateCursor("skewcur");
    ct_3drotate = GDrawCreateCursor("rotate3dcur");
    ct_perspective = GDrawCreateCursor("perspectivecur");

    ct_rect = GDrawCreateCursor("rectcur");
    ct_elipse = GDrawCreateCursor("elipsecur");
    ct_poly = GDrawCreateCursor("polycur");
    ct_star = GDrawCreateCursor("starcur");

    ct_nwse = GDrawCreateCursor("nwse");
    ct_nesw = GDrawCreateCursor("nesw");

    ct_pencil = GDrawCreateCursor("pencil");
    ct_eyedropper = GDrawCreateCursor("eyedropper");
    ct_shift = GDrawCreateCursor("shift");
    ct_line = GDrawCreateCursor("linecur");
    ct_myhand = GDrawCreateCursor("hand");
    ct_filledrect = GDrawCreateCursor("filledrectcur");
    ct_filledelipse = GDrawCreateCursor("filledelipsecur");

    ct_kerning = GDrawCreateCursor("kerncur");
    ct_rbearing = GDrawCreateCursor("rbearcur");
    ct_lbearing = GDrawCreateCursor("lbearcur");

    ct_prohibition = GDrawCreateCursor("prohibition");
    ct_ddcursor = GDrawCreateCursor("ddcursor");
    ct_features = GDrawCreateCursor("featurescursor");

    logo_icon = GDrawCreateBitmap(NULL,logo_width,logo_height,logo_bits);
    GDrawSetDefaultIcon(logo_icon);
}
