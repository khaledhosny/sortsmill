/* Copyright (C) 2000-2012 by George Williams */
/* Copyright (C) 2012 by Barry Schwartz */
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

#ifndef _INTERNAL_CONTOUR_INTERFACE_H
#define _INTERNAL_CONTOUR_INTERFACE_H

#include <config.h>

#include <fontforge.h>

enum
{
  CONTOUR_INTERFACE_SUCCESS = 0,
  CONTOUR_INTERFACE_EMPTY_CONTOUR = 1,
  CONTOUR_INTERFACE_BAD_CUBIC = 2       /* In cubic splines there must be
                                           exactly two control points
                                           between on curve points. */
};

int SSFromContourData (SplineSet **result, double *x_vals, double *y_vals,
                       int *on_curve_vals, int *selected_vals, int pt_cnt,
                       int is_closed, int is_quadratic, char *name,
                       int *tt_start);
int ContourDataSizeFromSS (SplineSet *ss);
void ContourDataFromSS (SplineSet *ss, double *x_vals, double *y_vals,
                        int *on_curve_vals, int *selected_vals);

#endif // _INTERNAL_CONTOUR_INTERFACE_H
