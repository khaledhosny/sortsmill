// Copyright (C) 2012 Barry Schwartz
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

#ifndef _POLYSPLINE_H
#define _POLYSPLINE_H

#include <config.h>

#include <stdlib.h>

// Use floating-point multiply-and-add if there is hardware support
// for it.
#ifndef MY_FAST_FMA
#ifdef FP_FAST_FMA
#define MY_FAST_FMA fma
#else
#define MY_FAST_FMA(x, y, z) ((x) * (y) + (z))
#endif
#endif

// vis--
// vis-- @deftypefun {void} sbern_to_bern_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from scaled Bernstein to Bernstein basis.
// vis--
// vis-- @end deftypefun
// vis--
void sbern_to_bern_double (unsigned int deg, const double *sbern,
			   double *bern, size_t num_splines);

// vis--
// vis-- @deftypefun {void} bern_to_sbern_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from Bernstein to scaled Bernstein basis.
// vis--
// vis-- @end deftypefun
// vis--
void bern_to_sbern_double (unsigned int deg, const double *bern,
			   double *sbern, size_t num_splines);

// vis--
// vis-- @deftypefun {void} sbern_to_mono_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from scaled Bernstein to monomial basis.
// vis--
// vis-- @end deftypefun
// vis--
void sbern_to_mono_double (unsigned int deg, const double *sbern,
			   double *mono, size_t num_splines);

// vis--
// vis-- @deftypefun {void} mono_to_sbern_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from monomial basis to scaled Bernstein.
// vis--
// vis-- @end deftypefun
// vis--
void mono_to_sbern_double (unsigned int deg, const double *mono,
			   double *sbern, size_t num_splines);

// vis--
// vis-- @deftypefun {void} bern_to_sbern_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from Bernstein to monomial basis.
// vis--
// vis-- @end deftypefun
// vis--
void bern_to_mono_double (unsigned int deg, const double *bern,
			  double *mono, size_t num_splines);

// vis--
// vis-- @deftypefun {void} mono_to_bern_double (unsigned int @var{deg}, const double *@var{from}, double *@var{to}, size_t @var{num_splines})
// vis--
// vis-- Convert a spline from monomial basis to Bernstein.
// vis--
// vis-- @end deftypefun
// vis--
void mono_to_bern_double (unsigned int deg, const double *mono,
			  double *bern, size_t num_splines);

// vis--
// vis-- @deftypefun {double} eval_sbern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
// vis--
// vis-- Evaluate a spline in scaled Bernstein basis, using the
// vis-- algorithm of Schumaker and Volk.
// vis--
// vis-- @end deftypefun
// vis--
_GL_ATTRIBUTE_PURE double
eval_sbern_double (unsigned int deg, const double *spline, double t);

// vis--
// vis-- @deftypefun {double} eval_bern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
// vis--
// vis-- Evaluate a spline in Bernstein basis, using the
// vis-- algorithm of Schumaker and Volk.
// vis--
// vis-- @end deftypefun
// vis--
_GL_ATTRIBUTE_PURE double
eval_bern_double (unsigned int deg, const double *spline, double t);

// vis--
// vis-- @deftypefun {double} eval_sbern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
// vis--
// vis-- Evaluate a spline in scaled Bernstein basis, using the
// vis-- algorithm of De~Casteljau.
// vis--
// vis-- (De~Casteljau’s generally is the most stable algorithm, but
// vis-- can be expensive.)
// vis--
// vis-- @end deftypefun
// vis--
_GL_ATTRIBUTE_PURE double
evaldc_sbern_double (unsigned int deg, const double *spline, double t);

// vis--
// vis-- @deftypefun {double} eval_bern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
// vis--
// vis-- Evaluate a spline in Bernstein basis, using the
// vis-- algorithm of De~Casteljau.
// vis--
// vis-- (De~Casteljau’s generally is the most stable algorithm, but
// vis-- can be expensive.)
// vis--
// vis-- @end deftypefun
// vis--
_GL_ATTRIBUTE_PURE double
evaldc_bern_double (unsigned int deg, const double *spline, double t);

// vis--
// vis-- @deftypefun {double} eval_mono_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t});
// vis--
// vis-- Evaluate a spline in monomial basis.
// vis--
// vis-- @end deftypefun
// vis--
_GL_ATTRIBUTE_PURE double
eval_mono_double (unsigned int deg, const double *spline, double t);

// vis--
// vis-- @deftypefun {void} subdiv_sbern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t}, double *@var{a}, double *@var{b});
// vis--
// vis-- Subdivide a spline in scaled Bernstein basis, using the
// vis-- algorithm of De~Casteljau.
// vis--
// vis-- @end deftypefun
// vis--
void subdiv_sbern_double (unsigned int deg, const double *spline,
                                  double t, double *a, double *b);
// vis--
// vis-- @deftypefun {void} subdiv_bern_double (unsigned int @var{deg}, const double *@var{spline}, double @var{t}, double *@var{a}, double *@var{b});
// vis--
// vis-- Subdivide a spline in Bernstein basis, using the
// vis-- algorithm of De~Casteljau.
// vis--
// vis-- @end deftypefun
// vis--
void subdiv_bern_double (unsigned int deg, const double *spline,
			 double t, double *a, double *b);

#endif // _POLYSPLINE_H
