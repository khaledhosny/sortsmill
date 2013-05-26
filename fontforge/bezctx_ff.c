#include <config.h>

// Copyright (C) Raph Levien.
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

// Copyright (C) by George Williams.
//
//   Redistribution and use in source and binary forms, with or without
//   modification, are permitted provided that the following conditions are met:
//
//   Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
//
//   Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
//   The name of the author may not be used to endorse or promote products
//   derived from this software without specific prior written permission.
//
//   THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
//   WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
//   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
//   EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
//   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
//   OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
//   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
//   OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
//   ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
/* This file written by George Williams to provide a gateway to fontforge */
/* it it a modification of Raph's bezctx_ps.c */
#include <basics.h>
#include <stdio.h>

#include "bezctx_ff.h"
#include "fontforgevw.h"		/* For LogError, else splinefont.h */
#include <math.h>

typedef struct {
    bezctx base;
    int is_open;
    int gotnans;
    SplineSet *ss;
} bezctx_ff;

static void
nancheck(bezctx_ff *bc) {

    if ( !bc->gotnans ) {
	LogError(_("Spiros did not converge") );
	bc->gotnans = true;
    }
}

static void
bezctx_ff_moveto(bezctx *z, double x, double y, int is_open) {
    bezctx_ff *bc = (bezctx_ff *)z;

    if ( !finite(x) || !finite(y)) {
	nancheck(bc);
	x = y = 0;
    }
    if (!bc->is_open) {
	SplineSet *ss = xzalloc(sizeof (SplineSet));
	ss->next = bc->ss;
	bc->ss = ss;
    }
    bc->ss->first = bc->ss->last = SplinePointCreate(x,y);
    bc->is_open = is_open;
}

static void
bezctx_ff_lineto(bezctx *z, double x, double y) {
    bezctx_ff *bc = (bezctx_ff *)z;
    SplinePoint *sp;

    if ( !finite(x) || !finite(y)) {
	nancheck(bc);
	x = y = 0;
    }
    sp = SplinePointCreate(x,y);
    SplineMake3(bc->ss->last,sp);
    bc->ss->last = sp;
}

static void
bezctx_ff_quadto(bezctx *z, double xm, double ym, double x3, double y3)
{
    bezctx_ff *bc = (bezctx_ff *)z;
    double x0, y0;
    double x1, y1;
    double x2, y2;
    SplinePoint *sp;

    if ( !finite(xm) || !finite(ym) || !finite(x3) || !finite(y3)) {
	nancheck(bc);
	xm = ym = x3 = y3 = 0;
    }
    sp = SplinePointCreate(x3,y3);
    x0 = bc->ss->last->me.x;
    y0 = bc->ss->last->me.y;
    x1 = xm + (1./3) * (x0 - xm);
    y1 = ym + (1./3) * (y0 - ym);
    x2 = xm + (1./3) * (x3 - xm);
    y2 = ym + (1./3) * (y3 - ym);
    bc->ss->last->nextcp.x = x1;
    bc->ss->last->nextcp.y = y1;
    bc->ss->last->nonextcp = false;
    sp->prevcp.x = x2;
    sp->prevcp.y = y2;
    sp->noprevcp = false;
    SplineMake3(bc->ss->last,sp);
    bc->ss->last = sp;
}

static void
bezctx_ff_curveto(bezctx *z, double x1, double y1, double x2, double y2,
		  double x3, double y3)
{
    bezctx_ff *bc = (bezctx_ff *)z;
    SplinePoint *sp;

    if ( !finite(x1) || !finite(y1) || !finite(x2) || !finite(y2) || !finite(x3) || !finite(y3)) {
	nancheck(bc);
	x1 = y1 = x2 = y2 = x3 = y3 = 0;
    }
    sp = SplinePointCreate(x3,y3);
    bc->ss->last->nextcp.x = x1;
    bc->ss->last->nextcp.y = y1;
    bc->ss->last->nonextcp = false;
    sp->prevcp.x = x2;
    sp->prevcp.y = y2;
    sp->noprevcp = false;
    SplineMake3(bc->ss->last,sp);
    bc->ss->last = sp;
}

bezctx *
new_bezctx_ff(void) {
    bezctx_ff *result = xzalloc(sizeof (bezctx_ff));

    result->base.moveto = bezctx_ff_moveto;
    result->base.lineto = bezctx_ff_lineto;
    result->base.quadto = bezctx_ff_quadto;
    result->base.curveto = bezctx_ff_curveto;
    result->base.mark_knot = NULL;
    result->is_open = 0;
    result->gotnans = 0;
    result->ss = NULL;
    return &result->base;
}

struct splinepointlist *
bezctx_ff_close(bezctx *z)
{
    bezctx_ff *bc = (bezctx_ff *)z;
    SplineSet *ss = bc->ss;

    if (!bc->is_open && ss!=NULL ) {
	if ( ss->first!=ss->last &&
		RealNear(ss->first->me.x,ss->last->me.x) &&
		RealNear(ss->first->me.y,ss->last->me.y)) {
	    ss->first->prevcp = ss->last->prevcp;
	    ss->first->noprevcp = ss->last->noprevcp;
	    ss->first->prev = ss->last->prev;
	    ss->first->prev->to = ss->first;
	    SplinePointFree(ss->last);
	    ss->last = ss->first;
	} else {
	    SplineMake3(ss->last,ss->first);
	    ss->last = ss->first;
	}
    }
    free(bc);
return( ss );
}
