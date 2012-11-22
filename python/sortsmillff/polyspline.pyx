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

import cython

import numpy as np
cimport numpy as np

cdef extern from "polyspline.h":
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

cdef extern from "libguile.h":
  void *scm_with_guile (void *(*func)(void *), void *data)

ctypedef void (*_basis_changer) (unsigned int deg, double *_from, double *_to, size_t num_splines)
ctypedef double (*_evaluator) (unsigned int deg, double *spline, double t)
ctypedef void (*_subdivider) (unsigned int deg, double *spline, double t, double *a, double *b)

cdef struct _basis_changer_params:
  _basis_changer change_basis
  unsigned int deg
  double *_from
  double *_to
  size_t num_splines

cdef struct _evaluator_params:
  _evaluator evaluate
  unsigned int deg
  double *spline
  double t
  double result

cdef struct _subdivider_params:
  _subdivider subdivide
  unsigned int deg
  double *spline
  double t
  double *a
  double *b

cdef void *_call_basis_changer (void *params):
  cdef _basis_changer_params *p = <_basis_changer_params *> params
  p.change_basis (p.deg, p._from, p._to, p.num_splines)

cdef void *_call_evaluator (void *params):
  cdef _evaluator_params *p = <_evaluator_params *> params
  p.result = p.evaluate (p.deg, p.spline, p.t)

cdef void *_call_subdivider (void *params):
  cdef _subdivider_params *p = <_subdivider_params *> params
  p.subdivide (p.deg, p.spline, p.t, p.a, p.b)

cdef void _change_basis (_basis_changer change_basis,
                         unsigned int deg, double *_from, double *_to,
                         size_t num_splines):
  cdef _basis_changer_params p
  p.change_basis = change_basis
  p.deg = deg
  p._from = _from
  p._to = _to
  p.num_splines = num_splines
  scm_with_guile (_call_basis_changer, &p)

cdef double _evaluate (_evaluator evaluate, unsigned int deg, double *spline, double t):
  cdef _evaluator_params p
  p.evaluate = evaluate
  p.deg = deg
  p.spline = spline
  p.t = t
  scm_with_guile (_call_evaluator, &p)
  return p.result

cdef void _subdivide (_subdivider subdivide, unsigned int deg, double *spline, double t, double *a, double *b):
  cdef _subdivider_params p
  p.subdivide = subdivide
  p.deg = deg
  p.spline = spline
  p.t = t
  p.a = a
  p.b = b
  scm_with_guile (_call_subdivider, &p)

@cython.boundscheck(False)
@cython.wraparound(False)
def sbern_to_bern (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> sbern_to_bern_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def bern_to_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> bern_to_sbern_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def sbern_to_mono (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> sbern_to_mono_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def mono_to_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> mono_to_sbern_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def bern_to_mono (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> bern_to_mono_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def mono_to_bern (np.ndarray[double, ndim=1, mode="c"] spline not None):
  cdef int deg = spline.shape[0] - 1
  result = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
  _change_basis (<_basis_changer> mono_to_bern_double, deg, &spline[0], &cresult[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def eval_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  return eval_sbern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def eval_bern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  return _evaluate (<_evaluator> eval_bern_double, deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def evaldc_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  return _evaluate (<_evaluator> evaldc_sbern_double, deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def evaldc_bern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  return evaldc_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def eval_mono (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  return eval_mono_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def subdiv_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  a = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] ca = a
  b = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cb = b
  _subdivide (<_subdivider> subdiv_sbern_double, deg, &spline[0], t, &ca[0], &cb[0])
  return (a, b)

@cython.boundscheck(False)
@cython.wraparound(False)
def subdiv_bern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
  cdef int deg = spline.shape[0] - 1
  a = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] ca = a
  b = np.empty_like (spline)
  cdef np.ndarray[double, ndim=1, mode="c"] cb = b
  subdiv_bern_double (deg, &spline[0], t, &ca[0], &cb[0])
  return (a, b)
