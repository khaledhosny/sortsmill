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
#include "gwidget.h"

struct wfuncs {
    int placeholder;
};

/* this is the thing that hangs off of the GWindow that contains all the widget*/
/*  info */
typedef struct gwidgetdata {
    struct wfuncs *funcs;
    GWindow w;
    struct gwidgetdata *next;			/* siblings */
    int (*e_h)(GWindow, GEvent *);		/* User's event function for window, our eh will call it */
    GIC *gic;
    bool has_focus;
    /*bool visible;*/		/* this is in the window structure */
    bool enabled;
    bool iscontainer;
    bool istoplevel;
} GWidgetD;

typedef struct gwidgetcontainerdata /* : GWidgetD */{
    struct wfuncs *funcs;
    GWindow w;
    struct gwidgetdata *next;			/* siblings */
    int (*e_h)(GWindow, GEvent *);		/* User's event function for window, our eh will call it */
    GIC *gic;
    bool has_focus;
    /*bool visible;*/		/* this is in the window structure */
    bool enabled;
    bool iscontainer;
    bool istoplevel;
    /* ******************* */
    struct ggadget *gadgets;
    struct gwidgetdata *widgets;		/* children */
    /*struct ggadget *gdef, *gcancel;*/
    struct ggadget *grabgadget;
    struct ggadget *lastddgadget;
    struct ggadget *lastwiggle;
} GContainerD;

typedef struct gtopleveldata /* : GContainerD */{
    struct wfuncs *funcs;
    GWindow w;
    struct gwidgetdata *next;			/* siblings */
    int (*e_h)(GWindow, GEvent *);		/* User's event function for window, our eh will call it */
    GIC *gic;
    bool has_focus;
    /*bool visible;*/		/* this is in the window structure */
    bool enabled;
    bool iscontainer;
    bool istoplevel;
    unsigned int programmove: 10;
    struct ggadget *gadgets;
    struct gwidgetdata *widgets;		/* children */
    struct ggadget *grabgadget;
    struct ggadget *lastddgadget;
    struct ggadget *lastwiggle;
    /* ******************* */
    struct ggadget *popupowner;			/* Our gadget which owns the */
		/*  current popup. It needs all events until popup vanishes */
    struct ggadget *gdef, *gcancel, *gmenubar;
    struct ggadget *gfocus;
    GWindow wfocus;
    int (*handle_key)(GWindow top, GWindow ew, GEvent *);	/* All key events are handled by top level window */
} GTopLevelD;

GWidgetD *_GWidget_ChangeInternalFocus(GWidget gw,GWidgetD *to,struct ggadget *gto);
void _GWidget_RestorePixmap(GWindow gw, GWindow ours, GRect *rect);
GWindow _GWidget_GetPixmap(GWindow gw,GRect *rect);
