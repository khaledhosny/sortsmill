#include <config.h>             // -*- coding: utf-8 -*- (contains mathematical comments)

// Copyright (C) 2015 Khaled Hosny and Barry Schwartz
//
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

// Copyright (C) 2000-2012 by George Williams */
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// Redistributions of source code must retain the above copyright notice, this
// list of conditions and the following disclaimer.
//
// Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
//
// The name of the author may not be used to endorse or promote products
// derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
// EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#include "fontforge.h"
#include "splinefont.h"
#include <sortsmill/guile.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

void
SplineRefigure3 (Spline *spline)
{
  SplinePoint *from = spline->from;
  SplinePoint *to = spline->to;
  Spline1D *xsp = &spline->splines[0];
  Spline1D *ysp = &spline->splines[1];
  Spline old;

  spline->isquadratic = false;

  if (spline->acceptableextrema)
    old = *spline;

  xsp->d = from->me.x;
  ysp->d = from->me.y;

  if (from->nonextcp)
    from->nextcp = from->me;
  else if (from->nextcp.x == from->me.x && from->nextcp.y == from->me.y)
    from->nonextcp = true;

  if (to->noprevcp)
    to->prevcp = to->me;
  else if (to->prevcp.x == to->me.x && to->prevcp.y == to->me.y)
    to->noprevcp = true;

  if (from->nonextcp && to->noprevcp)
    {
      spline->islinear = true;
      xsp->c = to->me.x - from->me.x;
      ysp->c = to->me.y - from->me.y;
      xsp->a = xsp->b = 0;
      ysp->a = ysp->b = 0;
    }
  else
    {
      // From p. 393 (Operator Details, curveto), PostScript Lang. Ref. Man. (Red book).
      //
      // Note by Barry Schwartz, 2013.03.01. What is described there
      // is the conversion of coefficients of a cubic polynomial from
      // monomial basis to Bernstein basis:
      //
      //     x(t) = x₀ + ct + bt² + at³        monomial basis
      //
      //     x(t) = x₀β₀ + x₁β₁ + x₂β₂ + x₃β₃     Bernstein basis
      //
      // where
      //
      //     β₀ = (1 − t)³
      //     β₁ = 3t(1 − t)²
      //     β₂ = 3t²(1 − t)
      //     β₃ = t³
      //
      // are the Bernstein polynomials of degree three. The
      // coefficients xₖ correspond exactly to (one coordinate of) the
      // bezier control points.
      //
      // Notice the following:
      //
      //     β₀ = 1
      //     β₁ = 0
      //     β₂ = 0
      //     β₃ = 0       if t = 0
      //
      //     β₀ = 0
      //     β₁ = 0
      //     β₂ = 0
      //     β₃ = 1       if t = 1
      //
      // Thus, in the Bernstein basis, both endpoints of the bezier
      // spline are calculated exactly, without roundoff. In the
      // monomial basis, on the other hand, the value calculated at
      // the t = 1 endpoint can be way off, even if a, b, and c are
      // only slightly perturbed. (Thus, if you use monomial basis,
      // there are gaps in the piecewise contour!)
      //
      // The transformation between the two bases also is ill
      // conditioned.
      //
      // See also http://en.wikipedia.org/wiki/Bernstein_polynomial
      //

      double bx[4] = { from->me.x, from->nextcp.x, to->prevcp.x, to->me.x };
      double mx[4];
      dpoly_bern_to_mono (3, 1, bx, 1, mx);

      xsp->c = mx[1];
      xsp->b = mx[2];
      xsp->a = mx[3];

      double by[4] = { from->me.y, from->nextcp.y, to->prevcp.y, to->me.y };
      double my[4];
      dpoly_bern_to_mono (3, 1, by, 1, my);

      ysp->c = my[1];
      ysp->b = my[2];
      ysp->a = my[3];

      if (RealNear (xsp->c, 0))
        xsp->c = 0;
      if (RealNear (ysp->c, 0))
        ysp->c = 0;
      if (RealNear (xsp->b, 0))
        xsp->b = 0;
      if (RealNear (ysp->b, 0))
        ysp->b = 0;
      if (RealNear (xsp->a, 0))
        xsp->a = 0;
      if (RealNear (ysp->a, 0))
        ysp->a = 0;
      if (xsp->a != 0
          && (Within16RoundingErrors (xsp->a + from->me.x, from->me.x)
              || Within16RoundingErrors (xsp->a + to->me.x, to->me.x)))
        xsp->a = 0;
      if (ysp->a != 0
          && (Within16RoundingErrors (ysp->a + from->me.y, from->me.y)
              || Within16RoundingErrors (ysp->a + to->me.y, to->me.y)))
        ysp->a = 0;

      // Note by Barry Schwartz, 2013.03.04: I am not completely sure
      // what is going on here, but estimating the minimum degree of a
      // spline is better done by conversion to the Sánchez-Reyes
      // (s-power) basis than to the monomial basis.
      SplineIsLinear (spline);
      spline->islinear = false;
      if (ysp->a == 0 && xsp->a == 0)
        {
          if (ysp->b == 0 && xsp->b == 0)
            spline->islinear = true;    // This seems extremely unlikely...
          else
            spline->isquadratic = true; // Only likely if we read in a TTF.
        }
    }

  if (!finite (ysp->a) || !finite (xsp->a) || !finite (ysp->c)
      || !finite (xsp->c) || !finite (ysp->d) || !finite (xsp->d))
    // Note by Barry Schwartz, 2013.03.04: How this could happen
    // except from roundoff or non-number inputs I do not know.
    IError ("NaN value in spline creation");

  LinearApproxFree (spline->approx);
  spline->approx = NULL;
  spline->knowncurved = false;
  spline->knownlinear = spline->islinear;
  SplineIsLinear (spline);
  spline->order2 = false;

  if (spline->acceptableextrema)
    {
      // I don't check "d", because changes to that reflect simple
      //  translations which will not affect the shape of the spline.
      if (!RealNear (old.splines[0].a, spline->splines[0].a) ||
          !RealNear (old.splines[0].b, spline->splines[0].b) ||
          !RealNear (old.splines[0].c, spline->splines[0].c) ||
          !RealNear (old.splines[1].a, spline->splines[1].a) ||
          !RealNear (old.splines[1].b, spline->splines[1].b) ||
          !RealNear (old.splines[1].c, spline->splines[1].c))
        spline->acceptableextrema = false;
    }
}
