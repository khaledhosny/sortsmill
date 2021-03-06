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
#include <stdlib.h>

#include "gdrawP.h"
#include "fontP.h"
#include "ustring.h"

FontInstance *GDrawSetFont(GWindow gw, FontInstance *fi) {
    FontInstance *old = gw->ggc->fi;
    gw->ggc->fi = fi;
return( old );
}

FontInstance *GDrawInstanciateFont(GWindow gw, FontRequest *rq) {
    struct font_instance *fi;

    if (gw == NULL)
	gw = GDrawGetRoot(NULL);

    if ( rq->point_size<0 )	/* It's in pixels, not points, convert to points */
	rq->point_size = PixelToPoint(-rq->point_size, ((GXWindow) gw)->display->res);

    fi = xcalloc(1,sizeof(struct font_instance));
    fi->rq = *rq;
    fi->rq.family_name = x_u32_strdup_or_null( fi->rq.family_name );
    fi->rq.utf8_family_name = xstrdup_or_null( fi->rq.utf8_family_name );

return( fi );
}

GFont *GDrawNewFont(GWindow gw, char *family_name, int point_size, int weight, enum font_style style) {
    FontRequest rq;

    memset(&rq,0,sizeof(rq));
    rq.utf8_family_name = family_name;
    rq.point_size = point_size;
    rq.weight = weight;
    rq.style = style;

    return (GDrawInstanciateFont(gw, &rq));
}

FontRequest *GDrawDecomposeFont(FontInstance *fi, FontRequest *rq) {
    *rq = fi->rq;
return( rq );
}
