# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
# This file is part of the Sorts Mill Tools.
# 
# Sorts Mill Tools is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
# 
# Sorts Mill Tools is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

import cython
import array
from cpython cimport array
from cython.view cimport contiguous
from cython.view cimport array as cvarray
cimport sortsmill.cython.polyspline as ps

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_bern_to_sbern (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_bern_to_sbern (deg, 1, &spline[0], 1, &result[0])
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_sbern_to_bern (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_sbern_to_bern (deg, 1, &spline[0], 1, &result[0])
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_mono_to_sbern (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_mono_to_sbern (deg, 1, &spline[0], 1, &result[0])
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_sbern_to_mono (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_sbern_to_mono (deg, 1, &spline[0], 1, &result[0])
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_mono_to_bern (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_mono_to_bern (deg, 1, &spline[0], 1, &result[0])
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double[::contiguous] c_f64_bern_to_mono (double[::contiguous] spline):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.change_basis_f64_bern_to_mono (deg, 1, &spline[0], 1, &result[0])
  return result

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_f64_eval_sbern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  return ps.eval_f64_sbern_schumaker_volk (deg, 1, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_f64_eval_bern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  return ps.eval_f64_bern_schumaker_volk (deg, 1, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_f64_evaldc_sbern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  return ps.eval_f64_sbern_de_casteljau (deg, 1, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_f64_evaldc_bern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  return ps.eval_f64_bern_de_casteljau (deg, 1, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_f64_eval_mono (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  return ps.eval_f64_mono (deg, 1, &spline[0], t)

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object c_f64_subdiv_sbern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result1 = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  cdef double[::contiguous] result2 = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.subdiv_f64_sbern (deg, 1, &spline[0], t, 1, &result1[0], 1, &result2[0])
  return (result1, result2)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object c_f64_subdiv_bern (double[::contiguous] spline, double t):
  cdef unsigned int deg = len (spline) - 1
  cdef double[::contiguous] result1 = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  cdef double[::contiguous] result2 = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  ps.subdiv_f64_bern (deg, 1, &spline[0], t, 1, &result1[0], 1, &result2[0])
  return (result1, result2)

#--------------------------------------------------------------------------

cdef object _f64_change_basis (object spline,
                              double[::contiguous]
                              (*changer) (double[::contiguous] spline)):
  cdef double[::contiguous] a = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  cdef size_t i
  for i from 0 <= i < len (spline):
    a[i] = spline[i]
  cdef double[::contiguous] b = changer (a)
  if isinstance (spline, array.array):
    result = array.array ('d', b)     # Arrays give arrays of doubles.
  elif isinstance (spline, tuple):
    result = tuple (b)                # Tuples give tuples.
  else:
    result = list (b)           # The default output format is a list.
  return result

cdef object _f64_evaluate (object spline, double t,
                          double (*evaluator) (double[::contiguous] spline,
                                               double t)):
  cdef double[::contiguous] a = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  cdef size_t i
  for i from 0 <= i < len (spline):
    a[i] = spline[i]
  return evaluator (a, t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object _f64_subdivide (object spline, double t,
                           object (*subdivider)
                           (double[::contiguous] spline, double t)):
  cdef double[::contiguous] a = \
       cvarray (shape = (len (spline),), itemsize = sizeof(double), format = 'd')
  cdef size_t i
  for i from 0 <= i < len (spline):
    a[i] = spline[i]
  cdef double[::contiguous] b1
  cdef double[::contiguous] b2
  (b1, b2) = subdivider (a, t)
  if isinstance (spline, array.array):
    result = (array.array ('d', b1),
              array.array ('d', b2))  # Arrays give arrays of doubles.
  elif isinstance (spline, tuple):
    result = (tuple (b1), tuple (b2)) # Tuples give tuples.
  else:
    result = (list (b1), list (b2))   # The default output format is a list.
  return result

#--------------------------------------------------------------------------

def f64_bern_to_sbern (spline not None):
  return _f64_change_basis (spline, c_f64_bern_to_sbern)

def f64_sbern_to_bern (spline not None):
  return _f64_change_basis (spline, c_f64_sbern_to_bern)

def f64_sbern_to_mono (spline not None):
  return _f64_change_basis (spline, c_f64_sbern_to_mono)

def f64_mono_to_sbern (spline not None):
  return _f64_change_basis (spline, c_f64_mono_to_sbern)

def f64_bern_to_mono (spline not None):
  return _f64_change_basis (spline, c_f64_bern_to_mono)

def f64_mono_to_bern (spline not None):
  return _f64_change_basis (spline, c_f64_mono_to_bern)

def f64_eval_sbern (spline not None, double t):
  return _f64_evaluate (spline, t, c_f64_eval_sbern)

def f64_eval_bern (spline not None, double t):
  return _f64_evaluate (spline, t, c_f64_eval_bern)

def f64_evaldc_sbern (spline not None, double t):
  return _f64_evaluate (spline, t, c_f64_evaldc_sbern)

def f64_evaldc_bern (spline not None, double t):
  return _f64_evaluate (spline, t, c_f64_evaldc_bern)

def f64_eval_mono (spline not None, double t):
  return _f64_evaluate (spline, t, c_f64_eval_mono)

def f64_subdiv_sbern (spline not None, double t):
  return _f64_subdivide (spline, t, c_f64_subdiv_sbern)

def f64_subdiv_bern (spline not None, double t):
  return _f64_subdivide (spline, t, c_f64_subdiv_bern)

#--------------------------------------------------------------------------
