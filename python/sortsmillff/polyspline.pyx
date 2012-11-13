# -*- coding: utf-8 -*-

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
    void sbern_to_bern_double (unsigned int deg, double *sbern, double *bern)
    void bern_to_sbern_double (unsigned int deg, double *bern, double *sbern)
    void mono_to_sbern_double (unsigned int deg, double *mono, double *sbern)
    void sbern_to_mono_double (unsigned int deg, double *sbern, double *mono)
    void mono_to_bern_double (unsigned int deg, double *mono, double *bern)
    void bern_to_mono_double (unsigned int deg, double *bern, double *mono)

    double eval_sbern_double (unsigned int deg, double *spline, double t)
    double eval_bern_double (unsigned int deg, double *spline, double t)
    double evaldc_sbern_double (unsigned int deg, double *spline, double t)
    double evaldc_bern_double (unsigned int deg, double *spline, double t)
    double eval_mono_double (unsigned int deg, double *spline, double t)

    void subdiv_sbern_double (unsigned int deg, double *spline, double t, double *a, double *b)
    void subdiv_bern_double (unsigned int deg, double *spline, double t, double *a, double *b)

@cython.boundscheck(False)
@cython.wraparound(False)
def sbern_to_bern (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    sbern_to_bern_double (deg, &spline[0], &cresult[0])
    return result

@cython.boundscheck(False)
@cython.wraparound(False)
def bern_to_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    bern_to_sbern_double (deg, &spline[0], &cresult[0])
    return result

@cython.boundscheck(False)
@cython.wraparound(False)
def sbern_to_mono (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    sbern_to_mono_double (deg, &spline[0], &cresult[0])
    return result

@cython.boundscheck(False)
@cython.wraparound(False)
def mono_to_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    mono_to_sbern_double (deg, &spline[0], &cresult[0])
    return result

@cython.boundscheck(False)
@cython.wraparound(False)
def bern_to_mono (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    bern_to_mono_double (deg, &spline[0], &cresult[0])
    return result

@cython.boundscheck(False)
@cython.wraparound(False)
def mono_to_bern (np.ndarray[double, ndim=1, mode="c"] spline not None):
    cdef int deg = spline.shape[0] - 1
    result = np.empty_like (spline)
    cdef np.ndarray[double, ndim=1, mode="c"] cresult = result
    mono_to_bern_double (deg, &spline[0], &cresult[0])
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
    return eval_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def evaldc_sbern (np.ndarray[double, ndim=1, mode="c"] spline not None, double t):
    cdef int deg = spline.shape[0] - 1
    return evaldc_sbern_double (deg, &spline[0], t)

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
    subdiv_sbern_double (deg, &spline[0], t, &ca[0], &cb[0])
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
