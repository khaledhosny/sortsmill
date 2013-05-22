#include <config.h>

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

// Copyright (C) 2000-2012 by George Williams
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

#include <sortsmill/nearness.h>
#include <sortsmill/guile/nearness.h>
#include <real_types.h>
#include <math.h>

//-------------------------------------------------------------------------

static inline bool
nondecreasing3 (double a, double b, double c)
{
  return (a <= b && b <= c);
}

#if defined( FONTFORGE_CONFIG_USE_DOUBLE )
#define RE_NearZero	.00000001
#define RE_Factor	(1024.0*1024.0*1024.0*1024.0*1024.0*2.0)        /* 52 bits => divide by 2^51 */
#else
#define RE_NearZero	.00001
#define RE_Factor	(1024.0*1024.0*4.0)     /* 23 bits => divide by 2^22 */
#endif

static bool
WithinSomeRoundingErrors (double v1, double v2, double adjusted_factor)
{
  bool result;

  if (v1 * v2 < 0)
    result = false;             // Opposite signs.
  else
    {
      const double abs_v1 = fabs (v1);
      const double abs_v2 = fabs (v2);

      if (abs_v1 == 0)
        result = (abs_v2 < RE_NearZero);
      else if (abs_v2 == 0)
        result = (abs_v1 < RE_NearZero);
      else if (abs_v2 < abs_v1)
        result = (abs_v1 - abs_v2 < abs_v1 / adjusted_factor);
      else
        result = (abs_v2 - abs_v1 < abs_v2 / adjusted_factor);
    }
  return result;
}

VISIBLE bool
Within4RoundingErrors (double v1, double v2)
{
  return WithinSomeRoundingErrors (v1, v2, RE_Factor / 4);
}

VISIBLE bool
Within16RoundingErrors (double v1, double v2)
{
  return WithinSomeRoundingErrors (v1, v2, RE_Factor / 16);
}

VISIBLE bool
Within64RoundingErrors (double v1, double v2)
{
  return WithinSomeRoundingErrors (v1, v2, RE_Factor / 64);
}

// RealNear() takes doubles so callers do not need to know what the
// ‘real’ type is.
VISIBLE bool
RealNear (double a, double b)
{
#ifdef FONTFORGE_CONFIG_USE_DOUBLE
  const real eps = 1e-8;
  const real denom = 1024.0 * 1024.0;
#else
  const real eps = 1e-5;
  const real denom = 1024.0 * 64.0;
#endif

  bool result;

  if (a == 0)
    result = (fabs (b) < eps);
  else if (b == 0)
    result = (fabs (a) < eps);
  else
    result = (fabs (a - b) < fabs (a / denom));
  return result;
}

VISIBLE bool
RealNearish (double a, double b)
{
  return (fabs (a - b) < .001);
}

VISIBLE bool
RealApprox (double a, double b)
{
  bool result = false;
  if (a == 0)
    result = (fabs (b) < .0001);
  else if (b == 0)
    result = (fabs (a) < .0001);
  else
    result = nondecreasing3 (0.95, a / b, 1.05);
  return result;
}

VISIBLE bool
RealWithin (double a, double b, double fudge)
{
  return (nondecreasing3 (a - fudge, b, a + fudge));
}

VISIBLE bool
RealRatio (double a, double b, double fudge)
{
  return (b == 0) ? RealWithin (a, b, fudge) : RealWithin (a / b, 1.0, fudge);
}

//-------------------------------------------------------------------------

VISIBLE bool
scm_are_Within4RoundingErrors (SCM v1, SCM v2)
{
  return Within4RoundingErrors (scm_to_double (v1), scm_to_double (v2));
}

VISIBLE SCM
scm_Within4RoundingErrors_p (SCM v1, SCM v2)
{
  return scm_from_bool (scm_are_Within4RoundingErrors (v1, v2));
}

VISIBLE bool
scm_are_Within16RoundingErrors (SCM v1, SCM v2)
{
  return Within16RoundingErrors (scm_to_double (v1), scm_to_double (v2));
}

VISIBLE SCM
scm_Within16RoundingErrors_p (SCM v1, SCM v2)
{
  return scm_from_bool (scm_are_Within16RoundingErrors (v1, v2));
}

VISIBLE bool
scm_are_Within64RoundingErrors (SCM v1, SCM v2)
{
  return Within64RoundingErrors (scm_to_double (v1), scm_to_double (v2));
}

VISIBLE SCM
scm_Within64RoundingErrors_p (SCM v1, SCM v2)
{
  return scm_from_bool (scm_are_Within64RoundingErrors (v1, v2));
}

VISIBLE bool
scm_are_RealNear (SCM a, SCM b)
{
  return RealNear (scm_to_double (a), scm_to_double (b));
}

VISIBLE SCM
scm_RealNear_p (SCM a, SCM b)
{
  return scm_from_bool (scm_are_RealNear (a, b));
}

VISIBLE bool
scm_are_RealNearish (SCM a, SCM b)
{
  return RealNearish (scm_to_double (a), scm_to_double (b));
}

VISIBLE SCM
scm_RealNearish_p (SCM a, SCM b)
{
  return scm_from_bool (scm_are_RealNearish (a, b));
}

VISIBLE bool
scm_are_RealApprox (SCM a, SCM b)
{
  return RealApprox (scm_to_double (a), scm_to_double (b));
}

VISIBLE SCM
scm_RealApprox_p (SCM a, SCM b)
{
  return scm_from_bool (scm_are_RealApprox (a, b));
}

VISIBLE bool
scm_are_RealWithin (SCM a, SCM b, SCM fudge)
{
  return RealWithin (scm_to_double (a), scm_to_double (b),
                     scm_to_double (fudge));
}

VISIBLE SCM
scm_RealWithin_p (SCM a, SCM b, SCM fudge)
{
  return scm_from_bool (scm_are_RealWithin (a, b, fudge));
}

VISIBLE bool
scm_are_RealRatio (SCM a, SCM b, SCM fudge)
{
  return RealRatio (scm_to_double (a), scm_to_double (b),
                    scm_to_double (fudge));
}

VISIBLE SCM
scm_RealRatio_p (SCM a, SCM b, SCM fudge)
{
  return scm_from_bool (scm_are_RealRatio (a, b, fudge));
}

//-------------------------------------------------------------------------

void init_guile_sortsmill_nearness (void);

VISIBLE void
init_guile_sortsmill_nearness (void)
{
  scm_c_define_gsubr ("Within4RoundingErrors?", 2, 0, 0,
                      scm_Within4RoundingErrors_p);
  scm_c_define_gsubr ("Within16RoundingErrors?", 2, 0, 0,
                      scm_Within16RoundingErrors_p);
  scm_c_define_gsubr ("Within64RoundingErrors?", 2, 0, 0,
                      scm_Within64RoundingErrors_p);

  scm_c_define_gsubr ("RealNear?", 2, 0, 0, scm_RealNear_p);
  scm_c_define_gsubr ("RealNearish?", 2, 0, 0, scm_RealNearish_p);
  scm_c_define_gsubr ("RealApprox?", 2, 0, 0, scm_RealApprox_p);
  scm_c_define_gsubr ("RealWithin?", 3, 0, 0, scm_RealWithin_p);
}

//-------------------------------------------------------------------------
