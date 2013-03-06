#include <config.h>             // -*- coding: utf-8 -*- (contains mathematical comments)

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
#include <sortsmill/gmp_matrix.h>
#include <sortsmill/gmp_constants.h>
#include <sortsmill/guile.h>
#include <sortsmill/polyspline/bases.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>

static void
bernstein_to_monomial (const double b[4], double m[4])
{
  // In exact arithmetic, because the transformation is unstable,
  // multiply
  //
  //   (b[0] b[1] b[2] b[3]) T = (d c b a)
  //
  // where
  //
  //        ⎛1 -3  3 -1⎞
  //   T =  ⎜0  3 -6  3⎟
  //        ⎜0  0  3 -3⎟
  //        ⎝0  0  0  1⎠
  //
  // Here T = T₁T₂ where
  //
  //   T₁ =
  //         ⎛1  0  0  0⎞
  //         ⎜0  3  0  0⎟
  //         ⎜0  0  3  0⎟
  //         ⎝0  0  0  1⎠
  //
  // (row-wise multiplication by binomial coefficients) is the
  // coefficient transformation from the Bernstein basis to ‘scaled
  // Bernstein’ basis, and
  //
  //   T₂ =
  //         ⎛1 -3  3 -1⎞
  //         ⎜0  1 -2  1⎟
  //         ⎜0  0  1 -1⎟
  //         ⎝0  0  0  1⎠
  //
  // is the coefficient transformation from scaled Bernstein basis to
  // the ordinary monomial (power series) basis.
  //
  // The scaled Bernstein basis is very useful in itself, but that is
  // another topic.

  mpq_t x[1][4];
  mpq_matrix_init (1, 4, x);

  for (int i = 0; i < 4; i++)
    mpq_set_d (x[0][i], b[i]);

  mpq_matrix_trmm (CblasRight, CblasUpper, CblasNoTrans, CblasNonUnit,
                   1, 4, mpq_one (),
                   MPQMAT_REF (coefficients_bern_to_mono (3)), x);

  for (int i = 0; i < 4; i++)
    m[i] = mpq_get_d (x[0][i]);

  mpq_matrix_clear (1, 4, x);
}

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

#if 0                           // The old code.

      xsp->c = 3 * (from->nextcp.x - from->me.x);
      ysp->c = 3 * (from->nextcp.y - from->me.y);
      xsp->b = 3 * (to->prevcp.x - from->nextcp.x) - xsp->c;
      ysp->b = 3 * (to->prevcp.y - from->nextcp.y) - ysp->c;
      xsp->a = to->me.x - from->me.x - xsp->c - xsp->b;
      ysp->a = to->me.y - from->me.y - ysp->c - ysp->b;

#else // The new code. Please report if there are problems. (FIXME)

      double bx[4] = { from->me.x, from->nextcp.x, to->prevcp.x, to->me.x };
      double mx[4];
      bernstein_to_monomial (bx, mx);

      xsp->c = mx[1];
      xsp->b = mx[2];
      xsp->a = mx[3];

      double by[4] = { from->me.y, from->nextcp.y, to->prevcp.y, to->me.y };
      double my[4];
      bernstein_to_monomial (by, my);

      ysp->c = my[1];
      ysp->b = my[2];
      ysp->a = my[3];

#if 0                           // FIXME: A temporary double-check.
      double _xc = 3 * (from->nextcp.x - from->me.x);
      double _yc = 3 * (from->nextcp.y - from->me.y);
      double _xb = 3 * (to->prevcp.x - from->nextcp.x) - xsp->c;
      double _yb = 3 * (to->prevcp.y - from->nextcp.y) - ysp->c;
      double _xa = to->me.x - from->me.x - xsp->c - xsp->b;
      double _ya = to->me.y - from->me.y - ysp->c - ysp->b;
      double my_eps = 1e-6;
      assert (fabs (xsp->c - _xc) < my_eps);
      assert (fabs (ysp->c - _yc) < my_eps);
      assert (fabs (xsp->b - _xb) < my_eps);
      assert (fabs (ysp->b - _yb) < my_eps);
      assert (fabs (xsp->a - _xa) < my_eps);
      assert (fabs (ysp->a - _ya) < my_eps);
#endif

#endif

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
