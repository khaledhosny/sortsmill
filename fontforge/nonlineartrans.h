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

/* Copyright (C) 2003-2012 by George Williams */
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
#include "fontforgeui.h"
#include <utype.h>
#include <ustring.h>
#include <math.h>
#ifdef HAVE_IEEEFP_H
# include <ieeefp.h>		/* Solaris defines isnan in ieeefp rather than math.h */
#endif

#ifndef _NONLINEARTRANS_H
#define _NONLINEARTRANS_H

enum operator {
    op_base = 0x100,			/* Bigger than any character */

    op_x, op_y,				/* Returns current x & y values, no operands */
    op_value,				/* Returns a constant value */
    op_negate, op_not,			/* Unary operators: op1 */
    op_log, op_exp, op_sqrt, op_sin, op_cos, op_tan,
    op_abs, op_rint, op_floor, op_ceil,
    op_pow,				/* Binary operators: op1, op2 */
    op_atan2,
    op_times, op_div, op_mod,
    op_add, op_sub,
    op_eq, op_ne, op_le, op_lt, op_gt, op_ge,
    op_and, op_or,
    op_if				/* Trinary operator: op1 ? op2 : op3 */
};

struct expr {
    enum operator operator;
    struct expr *op1, *op2, *op3;
    real value;
};

struct context {
    char *start, *cur;
    bool had_error;
    enum operator backed_token;
    real backed_val;

    real x, y;
    struct expr *x_expr, *y_expr;
    SplineChar *sc;
    void *pov;
    void (*pov_func)(BasePoint *me,void *);
};

VISIBLE extern void _SFNLTrans(FontViewBase *fv,struct context *c);
VISIBLE extern struct expr *nlt_parseexpr(struct context *c,char *str);
VISIBLE extern void nlt_exprfree(struct expr *e);
VISIBLE extern void CVNLTrans(CharViewBase *cv,struct context *c);
VISIBLE extern void SPLPoV(SplineSet *spl,struct pov_data *pov, int only_selected);
#endif
