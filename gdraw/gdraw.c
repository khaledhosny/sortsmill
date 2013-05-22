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

#include "gdrawP.h"
#include <gkeysym.h>
#include <ustring.h>
#include <gio.h>

/* Functions for font metrics:
    rectangle of text (left side bearing of first char, right of last char)
*/

GDisplay *screen_display = NULL;

int GDrawGetRes(GWindow gw) {
    if ( gw==NULL ) {
	if ( screen_display==NULL )
return( 100 );
	gw = screen_display->groot;
    }
return( gw->display->res );
}

int GDrawPointsToPixels(GWindow gw,int points) {
    if ( gw==NULL ) {
	if ( screen_display==NULL )
return( PointToPixel(points,100));
	gw = screen_display->groot;
    }
return( PointToPixel(points,gw->display->res));
}

int GDrawPixelsToPoints(GWindow gw,int pixels) {
    if ( gw==NULL ) {
	if ( screen_display==NULL )
return( PixelToPoint(pixels,100));
	gw = screen_display->groot;
    }
return( PixelToPoint(pixels,gw->display->res));
}

int GDrawIsVisible(GWindow w) {
    if ( w==NULL )
return( false );
    while ( w!=NULL && ( w->is_visible || w->is_pixmap ))
	w = w->parent;
return( w==NULL );
}

GWindow GDrawGetRoot(GDisplay *gdisp) {
    if ( gdisp==NULL ) gdisp = screen_display;
return(gdisp->groot);
}

Color GDrawGetDefaultBackground(GDisplay *gdisp) {
    if ( gdisp==NULL ) gdisp = screen_display;
return(gdisp->def_background);
}

Color GDrawGetDefaultForeground(GDisplay *gdisp) {
    if ( gdisp==NULL ) gdisp = screen_display;
return(gdisp->def_foreground);
}

GRect *GDrawGetSize(GWindow w, GRect *ret) {
    *ret = w->pos;
return(ret);
}

GDrawEH GDrawGetEH(GWindow w) {
return( w->eh );
}

void GDrawSetEH(GWindow w,GDrawEH eh) {
    w->eh = eh;
}

int32_t GDrawEventInWindow(GWindow inme,GEvent *event) {
    GPoint pt;
    if ( event->type<et_char || event->type>et_crossing )
return( false );
    pt.x = event->u.mouse.x; pt.y = event->u.mouse.y;
    GDrawTranslateCoordinates(event->w,inme,&pt);
    if ( pt.x<0 || pt.y<0 || pt.x >= inme->pos.width || pt.y >= inme->pos.height )
return( false );

return( true );
}

GWindow GDrawGetParentWindow(GWindow gw) {
return( gw->parent );
}

int GDrawWindowIsAncestor(GWindow ancester, GWindow descendent) {
    while ( descendent!=NULL && descendent!=ancester )
	descendent = descendent->parent;
return( descendent==ancester );
}

void GDrawSetUserData(GWindow gw, void *ud) {
    gw->user_data = ud;
}

void *GDrawGetUserData(GWindow gw) {
return( gw->user_data );
}

GDisplay *GDrawGetDisplayOfWindow(GWindow gw) {
    if ( gw==NULL )
return( screen_display );

return( gw->display );
}

void GDrawGetClip(GWindow w, GRect *ret) {
    *ret = w->ggc->clip;
}

void GDrawSetClip(GWindow w, GRect *rct) {
    if ( rct==NULL ) {
	w->ggc->clip.x = w->ggc->clip.y = 0;
	w->ggc->clip.width = w->ggc->clip.height = 0x7fff;
    } else
	w->ggc->clip = *rct;
}

GGC *GDrawGetWindowGGC(GWindow w) {
return( w->ggc );
}

void GDrawSetXORBase(GWindow w,Color col) {
    w->ggc->xor_base = col;
}

void GDrawSetXORMode(GWindow w) {
    w->ggc->func = df_xor;
}

void GDrawSetCopyMode(GWindow w) {
    w->ggc->func = df_copy;
}

void GDrawSetCopyThroughSubWindows(GWindow w,int16_t through) {
    w->ggc->copy_through_sub_windows = through;
}

void GDrawSetDashedLine(GWindow w,int16_t dash_len, int16_t skip_len, int16_t off) {
    w->ggc->dash_offset = off;
    w->ggc->dash_len = dash_len;
    w->ggc->skip_len = skip_len;
}

void GDrawSetStippled(GWindow w,int16_t ts, int32_t yoff,int32_t xoff) {
    w->ggc->ts = ts;
    w->ggc->ts_xoff = xoff; w->ggc->ts_yoff = yoff;
}

void GDrawSetLineWidth(GWindow w,int16_t width) {
    w->ggc->line_width = width;
}

void GDrawSetForeground(GWindow w,Color col) {
    w->ggc->fg = col;
}

void GDrawSetBackground(GWindow w,Color col) {
    w->ggc->bg = col;
}

int GDrawEnableExposeRequests(GWindow w,int enabled) {
    int old = w->disable_expose_requests;
    w->disable_expose_requests = enabled;
return( old );
}
