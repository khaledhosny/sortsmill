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


#ifndef  _FF_INTERNAL_SPIROENTRYPOINTS_H
#define _FF_INTERNAL_SPIROENTRYPOINTS_H

#include "bezctx_intf.h"
#include "spiro.h"

    /* Possible values of the "ty" field. */
#define SPIRO_CORNER		'v'
#define SPIRO_G4		'o'
#define SPIRO_G2		'c'
#define SPIRO_LEFT		'['
#define SPIRO_RIGHT		']'

    /* For a closed contour add an extra cp with a ty set to */
#define SPIRO_END		'z'
    /* For an open contour the first cp must have a ty set to */
#define SPIRO_OPEN_CONTOUR	'{'
    /* For an open contour the last cp must have a ty set to */
#define SPIRO_END_OPEN_CONTOUR	'}'

/* The spiros array should indicate it's own end... So              */
/* Open contours must have the ty field of the first cp set to '{'  */
/*               and have the ty field of the last cp set to '}'    */
/* Closed contours must have an extra cp at the end whose ty is 'z' */
/*               the x&y values of this extra cp are ignored        */
extern void TaggedSpiroCPsToBezier (spiro_cp *spiros, bezctx *bc);

/* The first argument is an array of spiro control points.          */
/* Open contours do not need to start with '{', nor to end with '}' */
/* Close contours do not need to end with 'z'                       */
extern void SpiroCPsToBezier (spiro_cp *spiros, int n, int isclosed, bezctx *bc);

#endif // _FF_INTERNAL_SPIROENTRYPOINTS_H
