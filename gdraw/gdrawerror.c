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

#include <stdio.h>
#include <stdarg.h>
#include <xunistring.h>
#include <unistr.h>

#include "gdrawP.h"
#include "ustring.h"

/* Preallocate an error dialog so that we can pop it up if things go really bad*/
/*  ie. if memory gets munched somehow */

#define ERR_LINE_MAX	20
static GWindow error;
enum err_type { et_info, et_warn, et_error, et_fatal };
static struct errinfo {
    uint32_t *lines[ERR_LINE_MAX];
    bool dismissed;
    int width;
    enum err_type err_type;
} errinfo;

static int e_h(GWindow gw, GEvent *event) {
    int line;
    int x,len, max_len;
    GRect r;
    static uint32_t ok[] = { 'O', 'K', '\0' };

    if ( event->type == et_expose ) {
	max_len = 0;
	for ( line = 0; line<ERR_LINE_MAX && errinfo.lines[line]!=NULL; ++line ) {
	    len = GDrawGetTextWidth(gw,errinfo.lines[line],-1);
	    if ( len>max_len ) max_len = len;
	}
	x = (errinfo.width-max_len)/2;
	for ( line = 0; line<ERR_LINE_MAX && errinfo.lines[line]!=NULL; ++line )
	    GDrawDrawText(gw,x, 10+10+15*line, errinfo.lines[line],-1,0x000000);

	x = (errinfo.width-(len = GDrawGetTextWidth(gw,ok,2)))/2;
	r.x = x-10; r.y = 25+15*line; r.width = len+20; r.height = 18;
	GDrawFillRect(gw,&r,0xffffff);
	GDrawDrawRect(gw,&r,0x000000);
	GDrawDrawText(gw,x,r.y+13,ok,2,0x000000);
    } else if ( event->type==et_char ) {
	if ( event->u.chr.chars[0]=='\r' || event->u.chr.chars[0]=='\33' )
	    errinfo.dismissed = true;
    } else if ( event->type==et_mouseup ) {
	errinfo.dismissed = true;
    } else if ( event->type==et_close ) {
	errinfo.dismissed = true;
    }
return( 1 );
}

static void RunError() {
    errinfo.dismissed = false;
    GDrawSetVisible(error,true);
    while ( !errinfo.dismissed )
	GDrawProcessOneEvent(NULL);
    GDrawSetVisible(error,false);
    GDrawSync(NULL);
    GDrawProcessPendingEvents(NULL);
}

static void ProcessText(uint32_t *ubuf,char *buf, enum err_type et)
{
  int max_len = 60, len;
  char *pt, *ept, *last_space;
  uint32_t *ue = ubuf;
  int line=0;

  pt = buf;
  for ( line=0; line<ERR_LINE_MAX && *pt; ++line )
    {
      last_space = NULL;
      for ( ept = pt; *ept!='\n' && *ept!='\0' && ept-pt<max_len; ++ept )
	if ( *ept==' ' )
	  last_space = ept;
      if ( *ept!='\n' && *ept!='\0' && last_space!=NULL )
	ept = last_space;
      uint32_t *utext = x_u32_strconv_from_locale (pt);
      errinfo.lines[line] = u32_strncpy (ue, utext, ept - pt);
      free (utext);
      ue[ept-pt] = 0;
      ue += (ept+1-pt);
      if ( *ept=='\n' || *ept==' ' )
	++ept;
      pt = ept;
    }
  for ( ; line<ERR_LINE_MAX ; ++line )
    errinfo.lines[line] = NULL;
  errinfo.err_type = et;

  max_len = 0;
  for ( line = 0; line<ERR_LINE_MAX && errinfo.lines[line]!=NULL; ++line )
    {
      len = GDrawGetTextWidth(error,errinfo.lines[line],-1);
      if ( len>max_len ) max_len = len;
    }
  errinfo.width = max_len+30;
  GDrawResize(error,max_len+30,15*line+50);
}

void _GDraw_InitError(GDisplay *gd) {
    GRect screen, pos;
    static uint32_t title[]= { 'E', 'r', 'r', 'o', 'r', '\0' };
    static GDisplay *static_gd;
    GWindowAttrs wattrs;
    GFont *font;

    if ( gd!=NULL )
	static_gd = gd;
    else
	screen_display = gd = static_gd;

    if ( gd==NULL )
return;

    if ( error != NULL )
return;
    GDrawGetSize(GDrawGetRoot(gd),&screen);

    memset(&wattrs,0,sizeof(wattrs));
    wattrs.mask = wam_events|wam_positioned|wam_cursor|wam_wtitle|wam_backcol|
	    wam_restrict|wam_redirect|wam_isdlg;
    wattrs.event_masks = -1;
    wattrs.positioned = 1;
    wattrs.cursor = ct_pointer;
    wattrs.window_title = title;
    wattrs.background_color = 0xbbbbbb;
    wattrs.restrict_input_to_me = true;
    wattrs.redirect_chars_to_me = true;
    wattrs.is_dlg = true;
    pos.width = 300; pos.height = 180;
    pos.x = (screen.width-pos.width)/2;
    pos.y = (screen.width-pos.width)/3;
    errinfo.width = pos.width;

    error = GDrawCreateTopWindow(gd,&pos,e_h,NULL,&wattrs);

    font = GDrawNewFont(error, "courier", -12, 400, fs_none);
    GDrawSetFont(error, font);
}

void GDrawIError(const char *fmt,...) {
    char buf[1025]; uint32_t ubuf[1025];
    va_list ap;

    strcpy(buf,"Internal Error:\n");
    va_start(ap, fmt);
    vsprintf(buf+strlen(buf), fmt, ap);
    va_end(ap);
    fprintf( stderr, "%s\n", buf );
    _GDraw_InitError(NULL);
    if ( error!=NULL ) {
	ProcessText(ubuf,buf,et_error);
	RunError();
    }
}

void GDrawFatalError(const char *fmt,...) {
    char buf[1025]; uint32_t ubuf[1025];
    va_list ap;

    strcpy(buf,"Fatal Error:\n");
    va_start(ap, fmt);
    vsprintf(buf+strlen(buf), fmt, ap);
    va_end(ap);
	fprintf( stderr, "%s\n", buf );
    if ( error!=NULL ) {
	ProcessText(ubuf,buf,et_fatal);
	RunError();
    }
    exit(1);
}
