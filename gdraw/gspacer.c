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
#include <stdlib.h>
#include "gdraw.h"
#include "ggadgetP.h"
#include "ustring.h"
#include "gkeysym.h"
#include "gresource.h"

static GBox spacer_box = GBOX_EMPTY;
static int gspacer_inited = false;

static int gspacer_expose(GWindow pixmap, GGadget *g, GEvent *event) {
return( true );
}

static int gspacer_mouse(GGadget *g, GEvent *event) {
return( false );
}

static int gspacer_key(GGadget *g, GEvent *event) {
return(false);
}

static int gspacer_focus(GGadget *g, GEvent *event) {
return(false);
}

static void gspacer_destroy(GGadget *g) {

    if ( g==NULL )
return;
    _ggadget_destroy(g);
}

static void GSpacerSetTitle(GGadget *g,const uint32_t *tit) {
}

static const uint32_t *_GSpacerGetTitle(GGadget *g) {
return( NULL );
}

static void _gspacer_resize(GGadget *g, int32_t width, int32_t height ) {

    g->inner.height = g->r.height = height;
    g->inner.width = g->r.width = width;
}

struct gfuncs gspacer_funcs = {
    0,
    sizeof(struct gfuncs),

    gspacer_expose,
    gspacer_mouse,
    gspacer_key,
    NULL,
    gspacer_focus,
    NULL,
    NULL,

    _ggadget_redraw,
    _ggadget_move,
    _gspacer_resize,
    _ggadget_setvisible,
    _ggadget_setenabled,
    _ggadget_getsize,
    _ggadget_getinnersize,

    gspacer_destroy,

    GSpacerSetTitle,
    _GSpacerGetTitle,
    NULL,
    NULL,
    NULL,

    NULL,
    NULL,

    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,

    _ggadget_getDesiredSize,
    _ggadget_setDesiredSize,
    NULL,
    NULL
};

static void GSpacerInit() {
    spacer_box.border_type = bt_none;
    spacer_box.border_width = spacer_box.padding = spacer_box.flags = 0;
    gspacer_inited = true;
}

GGadget *GSpacerCreate(struct gwindow *base, GGadgetData *gd,void *data) {
    GSpacer *gs = xcalloc(1,sizeof(GSpacer));

    if ( !gspacer_inited )
	GSpacerInit();
    gs->funcs = &gspacer_funcs;
    _GGadget_Create(gs,base,gd,data,&spacer_box);

return( gs );
}
