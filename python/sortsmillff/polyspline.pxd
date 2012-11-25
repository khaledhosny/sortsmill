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

cdef extern from "sortsmillff/polyspline.h":
  void sbern_to_bern_double (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void bern_to_sbern_double (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void mono_to_sbern_double (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void sbern_to_mono_double (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void mono_to_bern_double (unsigned int deg, double *_from, double *_to, size_t num_splines)
  void bern_to_mono_double (unsigned int deg, double *_from, double *_to, size_t num_splines)

  double eval_sbern_double (unsigned int deg, double *spline, double t)
  double eval_bern_double (unsigned int deg, double *spline, double t)
  double evaldc_sbern_double (unsigned int deg, double *spline, double t)
  double evaldc_bern_double (unsigned int deg, double *spline, double t)
  double eval_mono_double (unsigned int deg, double *spline, double t)

  void subdiv_sbern_double (unsigned int deg, double *spline, double t, double *a, double *b)
  void subdiv_bern_double (unsigned int deg, double *spline, double t, double *a, double *b)
