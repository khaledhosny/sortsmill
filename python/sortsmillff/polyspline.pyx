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
import array
from cpython cimport array

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_bern_to_sbern (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  bern_to_sbern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_sbern_to_bern (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  sbern_to_bern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_sbern_to_mono (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  sbern_to_mono_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_mono_to_sbern (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  mono_to_sbern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_bern_to_mono (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  bern_to_mono_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_mono_to_bern (array.array[double] spline not None):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  mono_to_bern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_eval_sbern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  return eval_sbern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_eval_bern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  return eval_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_evaldc_sbern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  return evaldc_sbern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_evaldc_bern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  return evaldc_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_eval_mono (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  return eval_mono_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_subdiv_sbern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result1 = array.copy (spline)
  cdef array.array[double] result2 = array.copy (spline)
  subdiv_sbern_double (deg, &result1[0], t, &result1[0], &result2[0])
  return (result1, result2)

@cython.boundscheck(False)
@cython.wraparound(False)
def fl_subdiv_bern (array.array[double] spline not None, double t):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result1 = array.copy (spline)
  cdef array.array[double] result2 = array.copy (spline)
  subdiv_bern_double (deg, &result1[0], t, &result1[0], &result2[0])
  return (result1, result2)

#--------------------------------------------------------------------------
