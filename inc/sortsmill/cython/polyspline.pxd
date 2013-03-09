# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Barry Schwartz
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

cdef extern from "sortsmill/math/polyspline.h":
  # FIXME: These are obsolescent.
  void f64_sbern_to_bern (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void f64_bern_to_sbern (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void f64_mono_to_sbern (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void f64_sbern_to_mono (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void f64_mono_to_bern (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void f64_bern_to_mono (unsigned int deg, double *_from, double *_to, size_t num_splines)

  # FIXME: Some of these are obsolescent.
  double f64_eval_sbern (unsigned int deg, double *spline, double t)
  double f64_eval_bern (unsigned int deg, double *spline, double t)
  double f64_evaldc_sbern (unsigned int deg, double *spline, double t)
  double f64_evaldc_bern (unsigned int deg, double *spline, double t)
  double eval_f64_mono (unsigned int deg, int stride, double *spline, double t)

  # FIXME: These are obsolescent.
  void f64_subdiv_sbern (unsigned int deg, double *spline, double t, double *a, double *b)
  void f64_subdiv_bern (unsigned int deg, double *spline, double t, double *a, double *b)
