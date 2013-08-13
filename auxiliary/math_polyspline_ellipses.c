#include <config.h>             // -*- coding: utf-8 -*-

// Approximation of elliptic arcs by polynomial splines. Based on Java
// code by Luc Maisonobe. See
// http://www.spaceroots.org/documents/ellipse/elliptical-arc.html

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

// FIXME: This could be made reusable, but first it needs a better
// name.
INITIALIZED_CONSTANT (_STM_ATTRIBUTE_PURE static, SCM, something_by_n_mapfunc,
                      scm_c_initialize_from_eval_string,
                      "(lambda (n) (lambda (i j) `[,(+ (* i n) j)]))");

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

VISIBLE size_t
elliptic_arc_spline_count (int degree, size_t max_count, double threshold,
                           double semimajor, double semiminor,
                           double xcenter, double ycenter,
                           double cos_theta, double sin_theta,
                           double eta1, double eta2)
{
  const double eta_lo = fmin (eta1, eta2);
  const double eta_hi = fmax (eta1, eta2);

  bool found = false;
  size_t spline_count = 1;
  while (!found && spline_count < max_count)
    {
      double dEta = (eta_hi - eta_lo) / spline_count;
      if (dEta <= M_PI_2)
        {
          double etaB = eta_lo;
          found = true;
          for (size_t i = 0; found && i < spline_count; i++)
            {
              const double etaA = etaB;
              etaB += dEta;
              const double error = error_estimate (degree, semimajor, semiminor,
                                                   xcenter, ycenter, cos_theta,
                                                   sin_theta, etaA, etaB);
              found = (error <= threshold);
            }
        }
      spline_count *= 2;
    }
  return spline_count;
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

  const double eta_lo = fmin (eta1, eta2);
  const double eta_hi = fmax (eta1, eta2);

  double etaB = eta_lo;
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
      const double xA = xB;
      const double yA = yB;
      const double xADot = xBDot;
      const double yADot = yBDot;

      etaB = eta_lo + (i + 1) * (eta_hi - eta_lo) / spline_count;
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

      switch ((eta1 <= eta2) ? degree : -degree)
        {
        case 1:
          xsplines[i][0] = xA;
          xsplines[i][1] = xB;

          ysplines[i][0] = yA;
          ysplines[i][1] = yB;
          break;

        case -1:
          xsplines[spline_count - 1 - i][1] = xA;
          xsplines[spline_count - 1 - i][0] = xB;

          ysplines[spline_count - 1 - i][1] = yA;
          ysplines[spline_count - 1 - i][0] = yB;
          break;

        case 2:
          {
            const double k =
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

        case -2:
          {
            const double k =
              (yBDot * (xB - xA) - xBDot * (yB - yA)) / (xADot * yBDot -
                                                         yADot * xBDot);

            xsplines[spline_count - 1 - i][2] = xA;
            xsplines[spline_count - 1 - i][1] = xA + k * xADot;
            xsplines[spline_count - 1 - i][0] = xB;

            ysplines[spline_count - 1 - i][2] = yA;
            ysplines[spline_count - 1 - i][1] = yA + k * yADot;
            ysplines[spline_count - 1 - i][0] = yB;
          }
          break;

        case 3:
          {
            const double t = tan (0.5 * (eta_hi - eta_lo) / spline_count);
            const double alpha =
              sin ((eta_hi - eta_lo) / spline_count) *
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
          break;

        case -3:
          {
            const double t = tan (0.5 * (eta_hi - eta_lo) / spline_count);
            const double alpha =
              sin ((eta_hi - eta_lo) / spline_count) *
              (sqrt (4 + 3 * t * t) - 1) / 3;

            xsplines[spline_count - 1 - i][3] = xA;
            xsplines[spline_count - 1 - i][2] = xA + alpha * xADot;
            xsplines[spline_count - 1 - i][1] = xB - alpha * xBDot;
            xsplines[spline_count - 1 - i][0] = xB;

            ysplines[spline_count - 1 - i][3] = yA;
            ysplines[spline_count - 1 - i][2] = yA + alpha * yADot;
            ysplines[spline_count - 1 - i][1] = yB - alpha * yBDot;
            ysplines[spline_count - 1 - i][0] = yB;
          }
        }
    }
}

// FIXME: Currently, behavior for sweeps of more than approximately
// 2*pi radians should be considered subject to change.
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

//-------------------------------------------------------------------------

void init_math_polyspline_ellipses (void);

VISIBLE void
init_math_polyspline_ellipses (void)
{
  scm_c_define_gsubr ("elliptic-arc-piecewise-bezier", 10, 0, 0,
                      scm_elliptic_arc_piecewise_bezier);
}

//-------------------------------------------------------------------------
