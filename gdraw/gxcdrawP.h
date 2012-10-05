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

#ifndef _GXCDRAWP_H
# define _GXCDRAWP_H

#include "fontP.h"

extern void _GXCDraw_NewWindow(GXWindow nw);
extern void _GXCDraw_ResizeWindow(GXWindow gw,GRect *rect);
extern void _GXCDraw_DestroyWindow(GXWindow gw);

extern void _GXCDraw_PushClip(GXWindow gw);
extern void _GXCDraw_PopClip(GXWindow gw);

extern void _GXCDraw_DrawLine(GWindow w, int32 x,int32 y, int32 xend,int32 yend, Color col);
extern void _GXCDraw_DrawRect(GWindow w, GRect *rect, Color col);

extern void _GXCDraw_CopyArea( GXWindow from, GXWindow into, GRect *src, int32 x, int32 y);

extern void _GXCDraw_Flush(GXWindow gw);
extern void _GXCDraw_DirtyRect(GXWindow gw,double x, double y, double width, double height);

extern void _GXPDraw_NewWindow(GXWindow nw);
extern void _GXPDraw_DestroyWindow(GXWindow nw);

#endif /* _GXCDRAWP_H */
