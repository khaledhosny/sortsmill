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

#include <basics.h>
#include <stdio.h>
#include <stdlib.h>
#include <ggadget.h>
#include "gwidget.h"
#include "ustring.h"

void GGadgetProtest8(char *label) {
    char buf[80];

    snprintf( buf, sizeof(buf),_("Bad Number in %s"), label);
    if ( buf[strlen(buf)-1]==' ' )
	buf[strlen(buf)-1]='\0';
    if ( buf[strlen(buf)-1]==':' )
	buf[strlen(buf)-1]='\0';
    gwwv_post_notice(buf,buf);
}

double GetCalmReal8(GWindow gw,int cid,char *name,int *err) {
    char *txt, *end;
    double val;

    txt = GGadgetGetTitle8(GWidgetGetControl(gw,cid));
    val = strtod(txt,&end);
    if ( (*txt=='-' || *txt=='.') && end==txt && txt[1]=='\0' )
	end = txt+1;
    else if ( *txt=='-' && txt[1]=='.' && txt[2]=='\0' )
	end = txt+2;
    if ( *end!='\0' ) {
	GDrawBeep(NULL);
	*err = true;
    }
    free(txt);
return( val );
}

double GetReal8(GWindow gw,int cid,char *name,int *err) {
    char *txt, *end;
    double val;

    txt = GGadgetGetTitle8(GWidgetGetControl(gw,cid));
    val = strtod(txt,&end);
    if ( *end!='\0' ) {
	GTextFieldSelect(GWidgetGetControl(gw,cid),0,-1);
	GGadgetProtest8(name);
	*err = true;
    }
    free(txt);
return( val );
}

int GetCalmInt8(GWindow gw,int cid,char *name,int *err) {
    char *txt, *end;
    int val;

    txt = GGadgetGetTitle8(GWidgetGetControl(gw,cid));
    val = strtol(txt,&end,10);
    if ( *txt=='-' && end==txt && txt[1]=='\0' )
	end = txt+1;
    if ( *end!='\0' ) {
	GDrawBeep(NULL);
	*err = true;
    }
    free(txt);
return( val );
}

int GetInt8(GWindow gw,int cid,char *name,int *err) {
    char *txt, *end;
    int val;

    txt = GGadgetGetTitle8(GWidgetGetControl(gw,cid));
    val = strtol(txt,&end,10);
    if ( *end!='\0' ) {
	GTextFieldSelect(GWidgetGetControl(gw,cid),0,-1);
	GGadgetProtest8(name);
	*err = true;
    }
    free(txt);
return( val );
}

int GetUnicodeChar8(GWindow gw,int cid,char *name,int *err) {
    char *txt, *end, *pt;
    int val;
    const uint32_t *utxt;

    utxt = _GGadgetGetTitle(GWidgetGetControl(gw,cid));
    if ( u32_strlen(utxt)==1 )
return( utxt[0] );

    txt = GGadgetGetTitle8(GWidgetGetControl(gw,cid));
    val = strtol(txt,&end,16);
    if ( *end!='\0' ) {
	for ( pt=txt; *pt==' '; ++pt );
	if ( (*pt=='U' || *pt=='u') && pt[1]=='+' ) {	/* Unicode notation */
	    pt += 2;
	    val = strtol(pt,&end,16);
	    if ( *end!='\0' ) {
		GTextFieldSelect(GWidgetGetControl(gw,cid),0,-1);
		GGadgetProtest8(name);
		*err = true;
	    }
	}
    }
    free(txt);
return( val );
}
