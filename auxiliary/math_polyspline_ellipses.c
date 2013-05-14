#include <config.h>             // -*- coding: utf-8 -*-

// Approximation of elliptic arcs by polynomial splines. Based on Java
// code by Luc Maisonobe. See
// http://www.spaceroots.org/documents/ellipse/elliptical-arc.html

// Copyright (C) 2013 by Barry Schwartz
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

// Copyright (c) 2003-2004, Luc Maisonobe
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with
// or without modification, are permitted provided that
// the following conditions are met:
// 
//    Redistributions of source code must retain the
//    above copyright notice, this list of conditions and
//    the following disclaimer. 
//    Redistributions in binary form must reproduce the
//    above copyright notice, this list of conditions and
//    the following disclaimer in the documentation
//    and/or other materials provided with the
//    distribution. 
//    Neither the names of spaceroots.org, spaceroots.com
//    nor the names of their contributors may be used to
//    endorse or promote products derived from this
//    software without specific prior written permission. 
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL
// THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
// USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
// IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
// USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#include <sortsmill/math/polyspline/ellipses.h>
#include <sortsmill/guile.h>
#include <sortsmill/xgc.h>
#include <sortsmill/initialized_global_constants.h>
#include <intl.h>
#include <math.h>
#include <assert.h>

#define TWO_PI (2 * M_PI)

//-------------------------------------------------------------------------

#define _ELLIPSES_MODULE_NAME "sortsmill math polyspline ellipses"

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _pointer_to_elliptic_arc,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _ELLIPSES_MODULE_NAME ") pointer->elliptic-arc)");

INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, _elliptic_arc_to_pointer,
                      scm_c_initialize_from_eval_string,
                      "(@ (" _ELLIPSES_MODULE_NAME ") elliptic-arc->pointer)");

VISIBLE SCM
scm_from_elliptic_arc_t (elliptic_arc_t _arc)
{
  return scm_call_1 (_pointer_to_elliptic_arc (),
                     scm_from_pointer (_arc, NULL));
}

VISIBLE elliptic_arc_t
scm_to_elliptic_arc_t (SCM arc)
{
  return (elliptic_arc_t)
    scm_to_pointer (scm_call_1 (_elliptic_arc_to_pointer (), arc));
}

//-------------------------------------------------------------------------

// *INDENT-OFF*

// coefficients for error estimation
// while using quadratic Bézier curves for approximation
// 0 < b/a < 1/4
static double coeffs2Low[2][4][4] = {
  {
    {  3.92478,   -13.5822,     -0.233377,    0.0128206   },
    { -1.08814,     0.859987,    0.000362265, 0.000229036 },
    { -0.942512,    0.390456,    0.0080909,   0.00723895  },
    { -0.736228,    0.20998,     0.0129867,   0.0103456   }
  },
  {
    { -0.395018,    6.82464,     0.0995293,   0.0122198   },
    { -0.545608,    0.0774863,   0.0267327,   0.0132482   },
    {  0.0534754,  -0.0884167,   0.012595,    0.0343396   },
    {  0.209052,   -0.0599987,  -0.00723897,  0.00789976  }
  }
};

// coefficients for error estimation
// while using quadratic Bézier curves for approximation
// 1/4 <= b/a <= 1
static double coeffs2High[2][4][4] = {
  {
    {  0.0863805, -11.5595,     -2.68765,     0.181224    },
    {  0.242856,   -1.81073,     1.56876,     1.68544     },
    {  0.233337,   -0.455621,    0.222856,    0.403469    },
    {  0.0612978,  -0.104879,    0.0446799,   0.00867312  }
  },
  {
    {  0.028973,    6.68407,     0.171472,    0.0211706   },
    {  0.0307674,  -0.0517815,   0.0216803,  -0.0749348   },
    { -0.0471179,   0.1288,     -0.0781702,   2.0         },
    { -0.0309683,   0.0531557,  -0.0227191,   0.0434511   }
  }
};

// safety factor to convert the "best" error approximation
// into a "max bound" error
static double safety2[4] = {
  0.02, 2.83, 0.125, 0.01
};

// coefficients for error estimation
// while using cubic Bézier curves for approximation
// 0 < b/a < 1/4
static double coeffs3Low[2][4][4] = {
  {
    {  3.85268,   -21.229,      -0.330434,    0.0127842  },
    { -1.61486,     0.706564,    0.225945,    0.263682   },
    { -0.910164,    0.388383,    0.00551445,  0.00671814 },
    { -0.630184,    0.192402,    0.0098871,   0.0102527  }
  },
  {
    { -0.162211,    9.94329,     0.13723,     0.0124084  },
    { -0.253135,    0.00187735,  0.0230286,   0.01264    },
    { -0.0695069,  -0.0437594,   0.0120636,   0.0163087  },
    { -0.0328856,  -0.00926032, -0.00173573,  0.00527385 }
  }
};

// coefficients for error estimation
// while using cubic Bézier curves for approximation
// 1/4 <= b/a <= 1
static double coeffs3High[2][4][4] = {
  {
    {  0.0899116, -19.2349,     -4.11711,     0.183362   },
    {  0.138148,   -1.45804,     1.32044,     1.38474    },
    {  0.230903,   -0.450262,    0.219963,    0.414038   },
    {  0.0590565,  -0.101062,    0.0430592,   0.0204699  }
  },
  {
    {  0.0164649,   9.89394,     0.0919496,   0.00760802 },
    {  0.0191603,  -0.0322058,   0.0134667,  -0.0825018  },
    {  0.0156192,  -0.017535,    0.00326508, -0.228157   },
    { -0.0236752,   0.0405821,  -0.0173086,   0.176187   }
  }
};

// safety factor to convert the "best" error approximation
// into a "max bound" error
static double safety3[4] = {
  0.001, 4.98, 0.207, 0.0067
};

// *INDENT-ON*

// Compute the value at x of the rational function
//
//    (c[0]x² + c[1]x + c[2]) / (x + c[3])
//
static double
rational_function (double x, const double c[4])
{
  return (x * (x * c[0] + c[1]) + c[2]) / (x + c[3]);
}

static inline const double *
rational_coefs (double coef_array[][4][4], size_t i, size_t j)
{
  return ((double (*)[4][4]) coef_array)[i][j];
}

static inline const double *
coefs_2_low (size_t i, size_t j)
{
  return rational_coefs (coeffs2Low, i, j);
}

static inline const double *
coefs_2_high (size_t i, size_t j)
{
  return rational_coefs (coeffs2High, i, j);
}

static inline const double *
coefs_3_low (size_t i, size_t j)
{
  return rational_coefs (coeffs3Low, i, j);
}

static inline const double *
coefs_3_high (size_t i, size_t j)
{
  return rational_coefs (coeffs3High, i, j);
}

typedef const double *_rational_coefs_func_t (size_t i, size_t j);

static _rational_coefs_func_t *
rational_coefs_func (int degree, double semimajor, double semiminor)
{
  assert (degree == 2 || degree == 3);
  assert (0 < semimajor);
  assert (0 < semiminor);

  _rational_coefs_func_t *func;

  switch (degree)
    {
    case 2:
      func = (4 * semiminor < semimajor) ? coefs_2_low : coefs_2_high;
      break;
    case 3:
      func = (4 * semiminor < semimajor) ? coefs_3_low : coefs_3_high;
      break;
    }

  return func;
}

static const double *
safety_coefs (int degree)
{
  assert (degree == 2 || degree == 3);

  return (degree == 2) ? safety2 : safety3;
}

//-------------------------------------------------------------------------

typedef struct elliptic_arc_struct_t
{

  /** Abscissa of the center of the ellipse. */
  double cx;

  /** Ordinate of the center of the ellipse. */
  double cy;

  /** Semi-major axis. */
  double a;

  /** Semi-minor axis. */
  double b;

  /** Orientation of the major axis with respect to the x axis. */
  double theta;
  double cosTheta;
  double sinTheta;

  /** Start angle of the arc. */
  double eta1;

  /** End angle of the arc. */
  double eta2;

  /** Abscissa of the start point. */
  double x1;

  /** Ordinate of the start point. */
  double y1;

  /** Abscissa of the end point. */
  double x2;

  /** Ordinate of the end point. */
  double y2;

  /** Abscissa of the first focus. */
  double xF1;

  /** Ordinate of the first focus. */
  double yF1;

  /** Abscissa of the second focus. */
  double xF2;

  /** Ordinate of the second focus. */
  double yF2;

  /** Abscissa of the leftmost point of the arc. */
  double xLeft;

  /** Ordinate of the highest point of the arc. */
  double yUp;

  /** Horizontal width of the arc. */
  double width;

  /** Vertical height of the arc. */
  double height;

  /** Indicator for center to endpoints line inclusion. */
  bool isPieSlice;

  /** Maximal degree for Bézier curve approximation. */
  int maxDegree;

  /** Default flatness for Bézier curve approximation. */
  double defaultFlatness;

  double f;
  double e2;
  double g;
  double g2;

} elliptic_arc_struct_t;

/** Compute the locations of the focii. */
static void
computeFocii (elliptic_arc_t arc)
{
  double d = sqrt (arc->a * arc->a - arc->b * arc->b);
  double dx = d * arc->cosTheta;
  double dy = d * arc->sinTheta;

  arc->xF1 = arc->cx - dx;
  arc->yF1 = arc->cy - dy;
  arc->xF2 = arc->cx + dx;
  arc->yF2 = arc->cy + dy;
}

/** Compute the locations of the endpoints. */
static void
computeEndPoints (elliptic_arc_t arc)
{
  // start point
  double aCosEta1 = arc->a * cos (arc->eta1);
  double bSinEta1 = arc->b * sin (arc->eta1);
  arc->x1 = arc->cx + aCosEta1 * arc->cosTheta - bSinEta1 * arc->sinTheta;
  arc->y1 = arc->cy + aCosEta1 * arc->sinTheta + bSinEta1 * arc->cosTheta;

  // end point
  double aCosEta2 = arc->a * cos (arc->eta2);
  double bSinEta2 = arc->b * sin (arc->eta2);
  arc->x2 = arc->cx + aCosEta2 * arc->cosTheta - bSinEta2 * arc->sinTheta;
  arc->y2 = arc->cy + aCosEta2 * arc->sinTheta + bSinEta2 * arc->cosTheta;
}

/** Compute the bounding box. */
static void
computeBounds (elliptic_arc_t arc)
{
  double etaXMin, etaXMax, etaYMin, etaYMax;

  double bOnA = arc->b / arc->a;
  if (abs (arc->sinTheta) < 0.1)
    {
      double tanTheta = arc->sinTheta / arc->cosTheta;
      if (arc->cosTheta < 0)
        {
          etaXMin = -atan (tanTheta * bOnA);
          etaXMax = etaXMin + M_PI;
          etaYMin = M_PI_2 - atan (tanTheta / bOnA);
          etaYMax = etaYMin + M_PI;
        }
      else
        {
          etaXMax = -atan (tanTheta * bOnA);
          etaXMin = etaXMax - M_PI;
          etaYMax = M_PI_2 - atan (tanTheta / bOnA);
          etaYMin = etaYMax - M_PI;
        }
    }
  else
    {
      double cotTheta = arc->cosTheta / arc->sinTheta;
      if (arc->sinTheta < 0)
        {
          etaXMax = M_PI_2 + atan (cotTheta / bOnA);
          etaXMin = etaXMax - M_PI;
          etaYMin = atan (cotTheta * bOnA);
          etaYMax = etaYMin + M_PI;
        }
      else
        {
          etaXMin = M_PI_2 + atan (cotTheta / bOnA);
          etaXMax = etaXMin + M_PI;
          etaYMax = atan (cotTheta * bOnA);
          etaYMin = etaYMax - M_PI;
        }
    }

  etaXMin -= TWO_PI * floor ((etaXMin - arc->eta1) / TWO_PI);
  etaYMin -= TWO_PI * floor ((etaYMin - arc->eta1) / TWO_PI);
  etaXMax -= TWO_PI * floor ((etaXMax - arc->eta1) / TWO_PI);
  etaYMax -= TWO_PI * floor ((etaYMax - arc->eta1) / TWO_PI);

  arc->xLeft =
    (etaXMin <= arc->eta2)
    ? (arc->cx + arc->a * cos (etaXMin) * arc->cosTheta -
       arc->b * sin (etaXMin) * arc->sinTheta) : fmin (arc->x1, arc->x2);
  arc->yUp =
    (etaYMin <= arc->eta2)
    ? (arc->cy + arc->a * cos (etaYMin) * arc->sinTheta +
       arc->b * sin (etaYMin) * arc->cosTheta) : fmin (arc->y1, arc->y2);
  arc->width =
    ((etaXMax <= arc->eta2)
     ? (arc->cx + arc->a * cos (etaXMax) * arc->cosTheta -
        arc->b * sin (etaXMax) * arc->sinTheta) :
     fmax (arc->x1, arc->x2)) - arc->xLeft;
  arc->height =
    ((etaYMax <= arc->eta2)
     ? (arc->cy + arc->a * cos (etaYMax) * arc->sinTheta +
        arc->b * sin (etaYMax) * arc->cosTheta) :
     fmax (arc->y1, arc->y2)) - arc->yUp;
}

static void
computeDerivedFlatnessParameters (elliptic_arc_t arc)
{
  arc->f = (arc->a - arc->b) / arc->a;
  arc->e2 = arc->f * (2.0 - arc->f);
  arc->g = 1.0 - arc->f;
  arc->g2 = arc->g * arc->g;
}

/** Simple constructor.
 * Build an elliptical arc composed of the full unit circle centered
 * on origin
 */
VISIBLE elliptic_arc_t
make_unit_circle_at_origin (void)
{
  elliptic_arc_t arc = x_gc_malloc_atomic (sizeof (elliptic_arc_struct_t));

  // FIXME: Consider making this go clockwise starting at 180 degrees,
  // which is more of a FontForge way of doing things. OTOH maybe we
  // should just get rid of it. Let’s think about it.
  arc->cx = 0;
  arc->cy = 0;
  arc->a = 1;
  arc->b = 1;
  arc->theta = 0;
  arc->eta1 = 0;
  arc->eta2 = TWO_PI;
  arc->cosTheta = 1;
  arc->sinTheta = 0;
  arc->isPieSlice = false;
  arc->maxDegree = 3;
  arc->defaultFlatness = 0.5;   // half a pixel

  computeFocii (arc);
  computeEndPoints (arc);
  computeBounds (arc);
  computeDerivedFlatnessParameters (arc);

  return arc;
}

  /** Build an elliptical arc from its canonical geometrical elements.
   * @param cx abscissa of the center of the ellipse
   * @param cy ordinate of the center of the ellipse
   * @param a semi-major axis
   * @param b semi-minor axis
   * @param theta orientation of the major axis with respect to the x axis
   * @param lambda1 start angle of the arc
   * @param lambda2 end angle of the arc
   * @param isPieSlice if true, the lines between the center of the ellipse
   * and the endpoints are part of the shape (it is pie slice like)
   */
VISIBLE elliptic_arc_t
make_elliptic_arc (double cx, double cy, double a, double b,
                   double theta, double lambda1, double lambda2,
                   bool isPieSlice)
{
  elliptic_arc_t arc = x_gc_malloc_atomic (sizeof (elliptic_arc_struct_t));

  arc->cx = cx;
  arc->cy = cy;
  arc->a = a;
  arc->b = b;
  arc->theta = theta;
  arc->isPieSlice = isPieSlice;

  arc->eta1 = atan2 (sin (lambda1) / arc->b, cos (lambda1) / arc->a);
  arc->eta2 = atan2 (sin (lambda2) / arc->b, cos (lambda2) / arc->a);
  arc->cosTheta = cos (arc->theta);
  arc->sinTheta = sin (arc->theta);
  arc->maxDegree = 3;
  arc->defaultFlatness = 0.5;   // half a pixel

  // make sure we have eta1 <= eta2 <= eta1 + 2 PI
  arc->eta2 -= TWO_PI * floor ((arc->eta2 - arc->eta1) / TWO_PI);

  // the preceding correction fails if we have exactly et2 - eta1 = 2 PI
  // it reduces the interval to zero length
  if ((lambda2 - lambda1 > M_PI) && (arc->eta2 - arc->eta1 < M_PI))
    arc->eta2 += TWO_PI;

  computeFocii (arc);
  computeEndPoints (arc);
  computeBounds (arc);
  computeDerivedFlatnessParameters (arc);

  return arc;
}

/** Build a full ellipse from its canonical geometrical elements.
 * @param cx abscissa of the center of the ellipse
 * @param cy ordinate of the center of the ellipse
 * @param a semi-major axis
 * @param b semi-minor axis
 * @param theta orientation of the major axis with respect to the x axis
 */
VISIBLE elliptic_arc_t
make_ellipse (double cx, double cy, double a, double b, double theta)
{
  elliptic_arc_t arc = x_gc_malloc_atomic (sizeof (elliptic_arc_struct_t));

  arc->cx = cx;
  arc->cy = cy;
  arc->a = a;
  arc->b = b;
  arc->theta = theta;
  arc->isPieSlice = false;

  // FIXME: Consider making this go clockwise starting at 180 degrees,
  // which is more of a FontForge way of doing things. OTOH maybe we
  // should just get rid of it. Let’s think about it.
  arc->eta1 = 0;
  arc->eta2 = TWO_PI;
  arc->cosTheta = cos (theta);
  arc->sinTheta = sin (theta);
  arc->maxDegree = 3;
  arc->defaultFlatness = 0.5;   // half a pixel

  computeFocii (arc);
  computeEndPoints (arc);
  computeBounds (arc);
  computeDerivedFlatnessParameters (arc);

  return arc;
}

static double
error_estimate_for_linear (double semimajor, double semiminor,
                           double xcenter, double ycenter,
                           double cos_theta, double sin_theta,
                           double etaA, double etaB)
{
  // start point
  const double aCosEtaA = semimajor * cos (etaA);
  const double bSinEtaA = semiminor * sin (etaA);
  const double xA = xcenter + aCosEtaA * cos_theta - bSinEtaA * sin_theta;
  const double yA = ycenter + aCosEtaA * sin_theta + bSinEtaA * cos_theta;

  // end point
  const double aCosEtaB = semimajor * cos (etaB);
  const double bSinEtaB = semiminor * sin (etaB);
  const double xB = xcenter + aCosEtaB * cos_theta - bSinEtaB * sin_theta;
  const double yB = ycenter + aCosEtaB * sin_theta + bSinEtaB * cos_theta;

  // maximal error point
  const double eta = 0.5 * (etaA + etaB);
  const double aCosEta = semimajor * cos (eta);
  const double bSinEta = semiminor * sin (eta);
  const double x = xcenter + aCosEta * cos_theta - bSinEta * sin_theta;
  const double y = ycenter + aCosEta * sin_theta + bSinEta * cos_theta;

  const double dx = xB - xA;
  const double dy = yB - yA;

  return fabs (x * dy - y * dx + xB * yA - xA * yB) / sqrt (dx * dx + dy * dy);
}

static double
error_estimate_for_quadratic_or_cubic (int degree,
                                       double semimajor, double semiminor,
                                       double etaA, double etaB)
{
  assert (degree == 2 || degree == 3);

  _rational_coefs_func_t *coefs =
    rational_coefs_func (degree, semimajor, semiminor);

  const double x = semiminor / semimajor;

  const double eta = 0.5 * (etaA + etaB);
  const double cos2 = cos (2 * eta);
  const double cos4 = cos (4 * eta);
  const double cos6 = cos (6 * eta);

  double c0 = rational_function (x, coefs (0, 0))
    + cos2 * rational_function (x, coefs (0, 1))
    + cos4 * rational_function (x, coefs (0, 2))
    + cos6 * rational_function (x, coefs (0, 3));

  double c1 = rational_function (x, coefs (1, 0))
    + cos2 * rational_function (x, coefs (1, 1))
    + cos4 * rational_function (x, coefs (1, 2))
    + cos6 * rational_function (x, coefs (1, 3));

  const double *safety = safety_coefs (degree);

  return
    rational_function (x, safety) * semimajor * exp (c0 + c1 * (etaB - etaA));
}

// Estimate the approximation error for a sub-arc of the instance.
//
//    @var{etaA} = start angle of the sub-arc.
//    @var{etaB} = end angle of the sub-arc.
//
// Returns an upper bound of the approximation error between the
// Bézier curve and the actual ellipse.
static double
error_estimate (int degree,
                double semimajor, double semiminor,
                double xcenter, double ycenter,
                double cos_theta, double sin_theta, double etaA, double etaB)
{
  assert (degree == 1 || degree == 2 || degree == 3);

  return (degree == 1) ?
    error_estimate_for_linear (semimajor, semiminor, xcenter, ycenter,
                               cos_theta, sin_theta, etaA, etaB) :
    error_estimate_for_quadratic_or_cubic (degree, semimajor, semiminor,
                                           etaA, etaB);
}

/** Get the elliptical arc point for a given angular parameter.
 * @param lambda angular parameter for which point is desired
 * @param p placeholder where to put the point.
 */
VISIBLE void
elliptic_arc_point (elliptic_arc_t arc, double lambda, double *x, double *y)
{
  double eta = atan2 (sin (lambda) / arc->b, cos (lambda) / arc->a);
  double aCosEta = arc->a * cos (eta);
  double bSinEta = arc->b * sin (eta);

  *x = arc->cx + aCosEta * arc->cosTheta - bSinEta * arc->sinTheta;
  *y = arc->cy + aCosEta * arc->sinTheta + bSinEta * arc->cosTheta;
}

/** Tests if the specified coordinates are inside the boundary of the Shape.
 * @param x abscissa of the test point
 * @param y ordinate of the test point
 * @return true if the specified coordinates are inside the Shape
 * boundary; false otherwise
 */
VISIBLE bool
elliptic_arc_contains_point (elliptic_arc_t arc, double x, double y)
{
  // position relative to the foci
  double dx1 = x - arc->xF1;
  double dy1 = y - arc->yF1;
  double dx2 = x - arc->xF2;
  double dy2 = y - arc->yF2;

  bool result;

  if ((dx1 * dx1 + dy1 * dy1 + dx2 * dx2 + dy2 * dy2) > (4 * arc->a * arc->a))
    {
      // the point is outside of the ellipse
      result = false;
    }
  else if (arc->isPieSlice)
    {
      // check the location of the test point with respect to the
      // angular sector counted from the center of the ellipse
      double dxC = x - arc->cx;
      double dyC = y - arc->cy;
      double u = dxC * arc->cosTheta + dyC * arc->sinTheta;
      double v = dyC * arc->cosTheta - dxC * arc->sinTheta;
      double eta = atan2 (v / arc->b, u / arc->a);
      eta -= TWO_PI * floor ((eta - arc->eta1) / TWO_PI);
      result = (eta <= arc->eta2);
    }
  else
    {
      // check the location of the test point with respect to the
      // line joining the start and end points
      double dx = arc->x2 - arc->x1;
      double dy = arc->y2 - arc->y1;
      result = ((x * dy - y * dx + arc->x2 * arc->y1 - arc->x1 * arc->y2) >= 0);
    }

  return result;
}

/** Tests if a line segment intersects the arc.
 * @param xA abscissa of the first point of the line segment
 * @param yA ordinate of the first point of the line segment
 * @param xB abscissa of the second point of the line segment
 * @param yB ordinate of the second point of the line segment
 * @return true if the two line segments intersect
 */
static bool
intersectArc (elliptic_arc_t arc, double xA, double yA, double xB, double yB)
{
  double dx = xA - xB;
  double dy = yA - yB;
  double l = sqrt (dx * dx + dy * dy);

  bool result = false;

  // if too small line segment, we consider it doesn't intersect anything
  if ((1.0e-10 * arc->a) <= l)
    {
      double cz = (dx * arc->cosTheta + dy * arc->sinTheta) / l;
      double sz = (dy * arc->cosTheta - dx * arc->sinTheta) / l;

      // express position of the first point in canonical frame
      dx = xA - arc->cx;
      dy = yA - arc->cy;
      double u = dx * arc->cosTheta + dy * arc->sinTheta;
      double v = dy * arc->cosTheta - dx * arc->sinTheta;

      double u2 = u * u;
      double v2 = v * v;
      double g2u2ma2 = arc->g2 * (u2 - arc->a * arc->a);
      //double g2u2ma2mv2 = g2u2ma2 - v2;
      double g2u2ma2pv2 = g2u2ma2 + v2;

      // compute intersections with the ellipse along the line
      // as the roots of a 2nd degree polynom : c0 k^2 - 2 c1 k + c2 = 0
      double c0 = 1.0 - arc->e2 * cz * cz;
      double c1 = arc->g2 * u * cz + v * sz;
      double c2 = g2u2ma2pv2;
      double c12 = c1 * c1;
      double c0c2 = c0 * c2;

      // if false, the line does not intersect the ellipse at all
      if (c0c2 <= c12)
        {
          double k = (c1 >= 0)
            ? (c1 + sqrt (c12 - c0c2)) / c0 : c2 / (c1 - sqrt (c12 - c0c2));
          if ((k >= 0) && (k <= l))
            {
              double uIntersect = u - k * cz;
              double vIntersect = v - k * sz;
              double eta = atan2 (vIntersect / arc->b, uIntersect / arc->a);
              eta -= TWO_PI * floor ((eta - arc->eta1) / TWO_PI);
              result = (eta <= arc->eta2);
            }

          if (!result)
            {
              k = c2 / (k * c0);
              if ((k >= 0) && (k <= l))
                {
                  double uIntersect = u - k * cz;
                  double vIntersect = v - k * sz;
                  double eta = atan2 (vIntersect / arc->b, uIntersect / arc->a);
                  eta -= TWO_PI * floor ((eta - arc->eta1) / TWO_PI);
                  result = (eta <= arc->eta2);
                }
            }
        }
    }
  return result;
}

/** Tests if two line segments intersect.
 * @param x1 abscissa of the first point of the first line segment
 * @param y1 ordinate of the first point of the first line segment
 * @param x2 abscissa of the second point of the first line segment
 * @param y2 ordinate of the second point of the first line segment
 * @param xA abscissa of the first point of the second line segment
 * @param yA ordinate of the first point of the second line segment
 * @param xB abscissa of the second point of the second line segment
 * @param yB ordinate of the second point of the second line segment
 * @return true if the two line segments intersect
 */
static bool
intersect (double x1, double y1, double x2, double y2,
           double xA, double yA, double xB, double yB)
{
  // elements of the equation of the (1, 2) line segment
  double dx12 = x2 - x1;
  double dy12 = y2 - y1;
  double k12 = x2 * y1 - x1 * y2;

  // elements of the equation of the (A, B) line segment
  double dxAB = xB - xA;
  double dyAB = yB - yA;
  double kAB = xB * yA - xA * yB;

  // compute relative positions of endpoints versus line segments
  double pAvs12 = xA * dy12 - yA * dx12 + k12;
  double pBvs12 = xB * dy12 - yB * dx12 + k12;
  double p1vsAB = x1 * dyAB - y1 * dxAB + kAB;
  double p2vsAB = x2 * dyAB - y2 * dxAB + kAB;

  return (pAvs12 * pBvs12 <= 0) && (p1vsAB * p2vsAB <= 0);
}

/** Tests if a line segment intersects the outline.
 * @param xA abscissa of the first point of the line segment
 * @param yA ordinate of the first point of the line segment
 * @param xB abscissa of the second point of the line segment
 * @param yB ordinate of the second point of the line segment
 * @return true if the two line segments intersect
 */
static bool
intersectOutline (elliptic_arc_t arc, double xA, double yA, double xB,
                  double yB)
{
  bool result;
  if (intersectArc (arc, xA, yA, xB, yB))
    result = true;
  else if (arc->isPieSlice)
    result =
      (intersect (arc->cx, arc->cy, arc->x1, arc->y1, xA, yA, xB, yB)
       || intersect (arc->cx, arc->cy, arc->x2, arc->y2, xA, yA, xB, yB));
  else
    result = intersect (arc->x1, arc->y1, arc->x2, arc->y2, xA, yA, xB, yB);
  return result;
}

/** Tests if the interior of the Shape entirely contains the
 * specified rectangular area.
 * @param x abscissa of the upper-left corner of the test rectangle
 * @param y ordinate of the upper-left corner of the test rectangle
 * @param w width of the test rectangle
 * @param h height of the test rectangle
 * @return true if the interior of the Shape entirely contains the
 * specified rectangular area; false otherwise
 */
VISIBLE bool
elliptic_arc_contains_rectangle (elliptic_arc_t arc, double x, double y,
                                 double w, double h)
{
  double xPlusW = x + w;
  double yPlusH = y + h;
  return (elliptic_arc_contains_point (arc, x, y)
          && elliptic_arc_contains_point (arc, xPlusW, y)
          && elliptic_arc_contains_point (arc, x, yPlusH)
          && elliptic_arc_contains_point (arc, xPlusW, yPlusH)
          && (!intersectOutline (arc, x, y, xPlusW, y))
          && (!intersectOutline (arc, xPlusW, y, xPlusW, yPlusH))
          && (!intersectOutline (arc, xPlusW, yPlusH, x, yPlusH))
          && (!intersectOutline (arc, x, yPlusH, x, y)));
}

/** Returns an integer Rectangle that completely encloses the Shape.
 */
VISIBLE void
elliptic_arc_get_int_bounds (elliptic_arc_t arc, int *x, int *y, int *w, int *h)
{
  *x = (int) rint (arc->xLeft - 0.5);
  *y = (int) rint (arc->yUp - 0.5);
  *w = (int) rint (arc->xLeft + arc->width + 0.5) - *x;
  *h = (int) rint (arc->yUp + arc->height + 0.5) - *y;
}

/** Returns a high precision and more accurate bounding box of the
 * Shape than the elliptic_arc_get_int_bounds function.
 */
VISIBLE void
elliptic_arc_get_bounds (elliptic_arc_t arc, double *x, double *y,
                         double *w, double *h)
{
  *x = arc->xLeft;
  *y = arc->yUp;
  *w = arc->width;
  *h = arc->height;
}

VISIBLE size_t
elliptic_arc_spline_count (int degree, size_t max_count, double threshold,
                           double semimajor, double semiminor,
                           double xcenter, double ycenter,
                           double cos_theta, double sin_theta,
                           double eta1, double eta2)
{
  bool found = false;
  size_t spline_count = 1;
  while (!found && spline_count < max_count)
    {
      double dEta = (eta2 - eta1) / spline_count;
      if (dEta <= M_PI_2)
        {
          double etaB = eta1;
          found = true;
          for (size_t i = 0; found && i < spline_count; i++)
            {
              const double etaA = etaB;
              etaB += dEta;
              const double error = error_estimate (degree, semimajor, semiminor,
                                                   xcenter, ycenter, cos_theta,
                                                   sin_theta,
                                                   etaA, etaB);
              found = (error <= threshold);
            }
        }
      spline_count *= 2;
    }
  return spline_count;
}

// find the number of Bézier curves needed
static size_t
spline_count (elliptic_arc_t arc, int degree, double threshold)
{
  return elliptic_arc_spline_count (degree, 1024, threshold,
                                    arc->a, arc->b, arc->cx, arc->cy,
                                    arc->cosTheta, arc->sinTheta,
                                    arc->eta1, arc->eta2);
}

/** Build an approximation of the instance outline.
 * @param degree degree of the Bézier curve to use
 * @param threshold acceptable error
 * @return a piecewise bézier path in SVG-path-data-like S-expressions
 */
VISIBLE SCM
elliptic_arc_bezier_path (elliptic_arc_t arc, int degree, double threshold)
{
  SCM moveto = scm_integer_to_char (scm_from_uint ((unsigned int) 'M'));
  SCM lineto = scm_integer_to_char (scm_from_uint ((unsigned int) 'L'));
  SCM quadto = scm_integer_to_char (scm_from_uint ((unsigned int) 'Q'));
  SCM cubicto = scm_integer_to_char (scm_from_uint ((unsigned int) 'C'));

  size_t n = spline_count (arc, degree, threshold);

  double dEta = (arc->eta2 - arc->eta1) / n;
  double etaB = arc->eta1;

  double cosEtaB = cos (etaB);
  double sinEtaB = sin (etaB);
  double aCosEtaB = arc->a * cosEtaB;
  double bSinEtaB = arc->b * sinEtaB;
  double aSinEtaB = arc->a * sinEtaB;
  double bCosEtaB = arc->b * cosEtaB;
  double xB = arc->cx + aCosEtaB * arc->cosTheta - bSinEtaB * arc->sinTheta;
  double yB = arc->cy + aCosEtaB * arc->sinTheta + bSinEtaB * arc->cosTheta;
  double xBDot = -aSinEtaB * arc->cosTheta - bCosEtaB * arc->sinTheta;
  double yBDot = -aSinEtaB * arc->sinTheta + bCosEtaB * arc->cosTheta;

  SCM path = SCM_EOL;

  if (arc->isPieSlice)
    {
      path = scm_acons (moveto,
                        scm_list_2 (scm_from_double (arc->cx),
                                    scm_from_double (arc->cy)), path);
      path = scm_acons (lineto,
                        scm_list_2 (scm_from_double (xB),
                                    scm_from_double (yB)), path);
    }
  else
    path = scm_acons (moveto,
                      scm_list_2 (scm_from_double (xB),
                                  scm_from_double (yB)), path);

  double t = tan (0.5 * dEta);
  double alpha = sin (dEta) * (sqrt (4 + 3 * t * t) - 1) / 3;

  for (size_t i = 0; i < n; ++i)
    {

      //double etaA  = etaB;
      double xA = xB;
      double yA = yB;
      double xADot = xBDot;
      double yADot = yBDot;

      etaB += dEta;
      cosEtaB = cos (etaB);
      sinEtaB = sin (etaB);
      aCosEtaB = arc->a * cosEtaB;
      bSinEtaB = arc->b * sinEtaB;
      aSinEtaB = arc->a * sinEtaB;
      bCosEtaB = arc->b * cosEtaB;
      xB = arc->cx + aCosEtaB * arc->cosTheta - bSinEtaB * arc->sinTheta;
      yB = arc->cy + aCosEtaB * arc->sinTheta + bSinEtaB * arc->cosTheta;
      xBDot = -aSinEtaB * arc->cosTheta - bCosEtaB * arc->sinTheta;
      yBDot = -aSinEtaB * arc->sinTheta + bCosEtaB * arc->cosTheta;

      switch (degree)
        {
        case 1:
          path = scm_acons (lineto,
                            scm_list_2 (scm_from_double (xB),
                                        scm_from_double (yB)), path);
          break;

        case 2:
          {
            double k =
              (yBDot * (xB - xA) - xBDot * (yB - yA)) / (xADot * yBDot -
                                                         yADot * xBDot);
            SCM quad = scm_list_2 (scm_list_2 (scm_from_double (xA + k * xADot),
                                               scm_from_double (yA +
                                                                k * yADot)),
                                   scm_list_2 (scm_from_double (xB),
                                               scm_from_double (yB)));
            path = scm_acons (quadto, quad, path);
          }
          break;

        default:
          {
            SCM cubic =
              scm_list_3 (scm_list_2 (scm_from_double (xA + alpha * xADot),
                                      scm_from_double (yA + alpha * yADot)),
                          scm_list_2 (scm_from_double (xB - alpha * xBDot),
                                      scm_from_double (yB - alpha * yBDot)),
                          scm_list_2 (scm_from_double (xB),
                                      scm_from_double (yB)));
            path = scm_acons (cubicto, cubic, path);
          }
        }

    }

  if (arc->isPieSlice)
    path = scm_acons (scm_from_latin1_symbol ("Z"), SCM_EOL, path);

  return scm_reverse (path);
}

VISIBLE void
elliptic_arc_piecewise_bezier (int degree, size_t spline_count,
                               double semimajor, double semiminor,
                               double xcenter, double ycenter,
                               double cos_theta, double sin_theta,
                               double eta1, double eta2,
                               double xsplines[spline_count][degree + 1],
                               double ysplines[spline_count][degree + 1])
{
  assert (degree == 1 || degree == 2 || degree == 3);

  double etaB = eta1;
  double cosEtaB = cos (etaB);
  double sinEtaB = sin (etaB);
  double aCosEtaB = semimajor * cosEtaB;
  double bSinEtaB = semiminor * sinEtaB;
  double aSinEtaB = semimajor * sinEtaB;
  double bCosEtaB = semiminor * cosEtaB;
  double xB = xcenter + aCosEtaB * cos_theta - bSinEtaB * sin_theta;
  double yB = ycenter + aCosEtaB * sin_theta + bSinEtaB * cos_theta;
  double xBDot = -aSinEtaB * cos_theta - bCosEtaB * sin_theta;
  double yBDot = -aSinEtaB * sin_theta + bCosEtaB * cos_theta;

  for (size_t i = 0; i < spline_count; i++)
    {
      double xA = xB;
      double yA = yB;
      double xADot = xBDot;
      double yADot = yBDot;

      etaB = eta1 + (i + 1) * (eta2 - eta1) / spline_count;
      cosEtaB = cos (etaB);
      sinEtaB = sin (etaB);
      aCosEtaB = semimajor * cosEtaB;
      bSinEtaB = semiminor * sinEtaB;
      aSinEtaB = semimajor * sinEtaB;
      bCosEtaB = semiminor * cosEtaB;
      xB = xcenter + aCosEtaB * cos_theta - bSinEtaB * sin_theta;
      yB = ycenter + aCosEtaB * sin_theta + bSinEtaB * cos_theta;
      xBDot = -aSinEtaB * cos_theta - bCosEtaB * sin_theta;
      yBDot = -aSinEtaB * sin_theta + bCosEtaB * cos_theta;

      switch (degree)
        {
        case 1:
          xsplines[i][0] = xA;
          xsplines[i][1] = xB;

          ysplines[i][0] = yA;
          ysplines[i][1] = yB;
          break;

        case 2:
          {
            double k =
              (yBDot * (xB - xA) - xBDot * (yB - yA)) / (xADot * yBDot -
                                                         yADot * xBDot);

            xsplines[i][0] = xA;
            xsplines[i][1] = xA + k * xADot;
            xsplines[i][2] = xB;

            ysplines[i][0] = yA;
            ysplines[i][1] = yA + k * yADot;
            ysplines[i][2] = yB;
          }
          break;

        case 3:
          {
            const double t = tan (0.5 * (eta2 - eta1) / spline_count);
            const double alpha =
              sin ((eta2 - eta1) / spline_count) *
              (sqrt (4 + 3 * t * t) - 1) / 3;

            xsplines[i][0] = xA;
            xsplines[i][1] = xA + alpha * xADot;
            xsplines[i][2] = xB - alpha * xBDot;
            xsplines[i][3] = xB;

            ysplines[i][0] = yA;
            ysplines[i][1] = yA + alpha * yADot;
            ysplines[i][2] = yB - alpha * yBDot;
            ysplines[i][3] = yB;
          }
        }
    }
}

// FIXME: This could be made reusable.
INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE static, SCM, something_by_n_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (n) (lambda (i j) `[,(+ (* i n) j)]))");

VISIBLE SCM
scm_c_elliptic_arc_piecewise_bezier (int degree, size_t max_count,
                                     double threshold,
                                     double semimajor, double semiminor,
                                     double xcenter, double ycenter,
                                     double cos_theta, double sin_theta,
                                     double eta1, double eta2)
{
  const char *who = "scm_c_elliptic_arc_piecewise_bezier";

  if (degree < 1 || 3 < degree)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("degree 1, 2, or 3 expected")),
        rnrs_make_irritants_condition (scm_list_1 (scm_from_int (degree)))));

  if (semimajor < semiminor || semiminor <= 0)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected semimajor >= "
                                         "semiminor > 0")),
        rnrs_make_irritants_condition (scm_list_2
                                       (scm_from_double (semimajor),
                                        scm_from_double (semiminor)))));

  if (eta2 < eta1)
    rnrs_raise_condition
      (scm_list_4
       (rnrs_make_assertion_violation (),
        rnrs_c_make_who_condition (who),
        rnrs_c_make_message_condition (_("expected eta1 <= eta2")),
        rnrs_make_irritants_condition (scm_list_2 (scm_from_double (eta1),
                                                   scm_from_double (eta2)))));

  size_t spline_count = elliptic_arc_spline_count (degree, max_count, threshold,
                                                   semimajor, semiminor,
                                                   xcenter, ycenter,
                                                   cos_theta, sin_theta,
                                                   eta1, eta2);

  double *xsplines =
    (double *) scm_malloc (spline_count * (degree + 1) * sizeof (double));
  double *ysplines =
    (double *) scm_malloc (spline_count * (degree + 1) * sizeof (double));

  elliptic_arc_piecewise_bezier (degree, spline_count, semimajor, semiminor,
                                 xcenter, ycenter, cos_theta, sin_theta,
                                 eta1, eta2,
                                 (double (*)[degree + 1]) xsplines,
                                 (double (*)[degree + 1]) ysplines);

  SCM xvec = scm_take_f64vector (xsplines, spline_count * (degree + 1));
  SCM yvec = scm_take_f64vector (ysplines, spline_count * (degree + 1));
  SCM mapfunc = scm_call_1 (something_by_n_mapfunc (),
                            scm_from_int (degree + 1));
  SCM bounds = scm_list_2 (scm_from_size_t (spline_count),
                           scm_from_int (degree + 1));
  SCM xmat = scm_make_shared_array (xvec, mapfunc, bounds);
  SCM ymat = scm_make_shared_array (yvec, mapfunc, bounds);

  SCM results[2] = {
    [0] = xmat,
    [1] = ymat
  };
  return scm_c_values (results, 2);
}

VISIBLE SCM
scm_elliptic_arc_piecewise_bezier (SCM degree, SCM max_count, SCM threshold,
                                   SCM semimajor, SCM semiminor,
                                   SCM xcenter, SCM ycenter, SCM theta,
                                   SCM eta1, SCM eta2)
{
  const int _degree = scm_to_int (degree);
  const size_t _max_count = scm_to_size_t (max_count);
  const double _threshold = scm_to_double (threshold);
  const double _semimajor = scm_to_double (semimajor);
  const double _semiminor = scm_to_double (semiminor);
  const double _xcenter = scm_to_double (xcenter);
  const double _ycenter = scm_to_double (ycenter);
  const double _theta = scm_to_double (theta);
  const double _eta1 = scm_to_double (eta1);
  const double _eta2 = scm_to_double (eta2);

  const double cos_theta = cos (_theta);
  const double sin_theta = sin (_theta);

  return scm_c_elliptic_arc_piecewise_bezier (_degree, _max_count, _threshold,
                                              _semimajor, _semiminor,
                                              _xcenter, _ycenter,
                                              cos_theta, sin_theta,
                                              _eta1, _eta2);
}

/** Tests if the interior of the Shape intersects the interior of a
 * specified rectangular area.
 */
VISIBLE bool
elliptic_arc_and_rectangle_interiors_intersect (elliptic_arc_t arc, double x,
                                                double y, double w, double h)
{
  double xPlusW = x + w;
  double yPlusH = y + h;
  return elliptic_arc_contains_point (arc, x, y)
    || elliptic_arc_contains_point (arc, xPlusW, y)
    || elliptic_arc_contains_point (arc, x, yPlusH)
    || elliptic_arc_contains_point (arc, xPlusW, yPlusH)
    || intersectOutline (arc, x, y, xPlusW, y)
    || intersectOutline (arc, xPlusW, y, xPlusW, yPlusH)
    || intersectOutline (arc, xPlusW, yPlusH, x, yPlusH)
    || intersectOutline (arc, x, yPlusH, x, y);
}

//-------------------------------------------------------------------------

VISIBLE SCM
scm_make_unit_circle_at_origin (void)
{
  return scm_from_elliptic_arc_t (make_unit_circle_at_origin ());
}

VISIBLE SCM
scm_make_elliptic_arc (SCM cx, SCM cy, SCM a, SCM b,
                       SCM theta, SCM lambda1, SCM lambda2, SCM isPieSlice)
{
  isPieSlice = (SCM_UNBNDP (isPieSlice)) ? SCM_BOOL_F : isPieSlice;
  elliptic_arc_t _arc =
    make_elliptic_arc (scm_to_double (cx), scm_to_double (cy),
                       scm_to_double (a), scm_to_double (b),
                       scm_to_double (theta), scm_to_double (lambda1),
                       scm_to_double (lambda2), scm_is_true (isPieSlice));
  return scm_from_elliptic_arc_t (_arc);
}

VISIBLE SCM
scm_make_ellipse (SCM cx, SCM cy, SCM a, SCM b, SCM theta)
{
  elliptic_arc_t _arc = make_ellipse (scm_to_double (cx), scm_to_double (cy),
                                      scm_to_double (a), scm_to_double (b),
                                      scm_to_double (theta));
  return scm_from_elliptic_arc_t (_arc);
}

VISIBLE SCM
scm_elliptic_arc_bezier_path (SCM arc, SCM degree, SCM threshold)
{
  const char *who = "scm_elliptic_arc_bezier_path";

  elliptic_arc_t _arc = scm_to_elliptic_arc_t (arc);
  int _degree = scm_to_int (degree);
  double _threshold = scm_to_double (threshold);

  switch (_degree)
    {
    case 1:
    case 2:
    case 3:
      break;

    default:
      rnrs_raise_condition
        (scm_list_4
         (rnrs_make_assertion_violation (),
          rnrs_c_make_who_condition (who),
          rnrs_c_make_message_condition (_("degree 1, 2, or 3 expected")),
          rnrs_make_irritants_condition (scm_list_1 (degree))));
    }

  return elliptic_arc_bezier_path (_arc, _degree, _threshold);
}

//-------------------------------------------------------------------------

void init_math_polyspline_ellipses (void);

VISIBLE void
init_math_polyspline_ellipses (void)
{
  //?????????????????????????????????????????????????????????????????????????????????????????????????????????
  scm_c_define_gsubr ("make-unit-circle-at-origin", 0, 0, 0,
                      scm_make_unit_circle_at_origin);
  scm_c_define_gsubr ("make-elliptic-arc", 7, 1, 0, scm_make_elliptic_arc);
  scm_c_define_gsubr ("make-ellipse", 5, 0, 0, scm_make_ellipse);
  scm_c_define_gsubr ("elliptic-arc-bezier-path", 3, 0, 0,
                      scm_elliptic_arc_bezier_path);
  //?????????????????????????????????????????????????????????????????????????????????????????????????????????

  scm_c_define_gsubr ("elliptic-arc-piecewise-bezier", 10, 0, 0,
                      scm_elliptic_arc_piecewise_bezier);
}

//-------------------------------------------------------------------------
