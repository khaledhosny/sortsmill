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
#ifndef _PFAEDIT_H_
#define _PFAEDIT_H_

#include <config.h>

#include <basics.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <setjmp.h>
#include <intl.h>
#include "splinefont.h"
#include "uiinterface.h"


static const int unicode4_size = 17*65536;
    /* Unicode goes up to 0x10ffff */

extern char *AdobeStandardEncoding[256];
extern int32_t unicode_from_adobestd[256];

VISIBLE extern int default_fv_font_size;
VISIBLE extern int default_fv_antialias;
VISIBLE extern int default_fv_bbsized;
VISIBLE extern Encoding *default_encoding, custom;
VISIBLE extern int adjustwidth;
VISIBLE extern int adjustlbearing;
VISIBLE extern int autohint_before_generate;
extern int seperate_hint_controls;
extern int no_windowing_ui;
extern int running_script;
VISIBLE extern uint32_t default_background;
extern int use_utf8_in_script;

VISIBLE extern int new_em_size;
VISIBLE extern int new_fonts_are_order2;
VISIBLE extern int loaded_fonts_same_as_new;

VISIBLE extern char *BDFFoundry, *TTFFoundry;
VISIBLE extern char *xuid;

VISIBLE extern Encoding *enclist;

bool get_no_windowing_ui (void);
void set_no_windowing_ui (bool);

bool get_running_script (void);
void set_running_script (bool);

extern jmp_buf exit_jmp_buf;

// FIXME: Eliminate this.
#define SCRIPT_MENU_MAX	10

#endif
