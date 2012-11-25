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
cdef array.array[double] c_fl_bern_to_sbern (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  bern_to_sbern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef array.array[double] c_fl_sbern_to_bern (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  sbern_to_bern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef array.array[double] c_fl_sbern_to_mono (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  sbern_to_mono_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef array.array[double] c_fl_mono_to_sbern (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  mono_to_sbern_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef array.array[double] c_fl_bern_to_mono (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  bern_to_mono_double (deg, &result[0], &result[0], 1)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef array.array[double] c_fl_mono_to_bern (array.array[double] spline):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result = array.copy (spline)
  mono_to_bern_double (deg, &result[0], &result[0], 1)
  return result

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_fl_eval_sbern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  return eval_sbern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_fl_eval_bern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  return eval_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_fl_evaldc_sbern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  return evaldc_sbern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_fl_evaldc_bern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  return evaldc_bern_double (deg, &spline[0], t)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef double c_fl_eval_mono (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  return eval_mono_double (deg, &spline[0], t)

#--------------------------------------------------------------------------

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object c_fl_subdiv_sbern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result1 = array.copy (spline)
  cdef array.array[double] result2 = array.copy (spline)
  subdiv_sbern_double (deg, &result1[0], t, &result1[0], &result2[0])
  return (result1, result2)

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object c_fl_subdiv_bern (array.array[double] spline, double t):
  cdef int deg = len (spline) - 1
  cdef array.array[double] result1 = array.copy (spline)
  cdef array.array[double] result2 = array.copy (spline)
  subdiv_bern_double (deg, &result1[0], t, &result1[0], &result2[0])
  return (result1, result2)

#--------------------------------------------------------------------------

_fl_typeerror_msg = "expected array.array('d'), tuple, or list"

cdef object _fl_change_basis (object spline,
                              array.array[double] (*changer) (array.array[double] spline)):
  if isinstance (spline, array.array) and spline.typecode == 'd':
    result = changer (spline)
  elif isinstance (spline, tuple):
    result = tuple (changer (array.array ('d', spline)))
  elif isinstance (spline, list):
    result = list (changer (array.array ('d', spline)))
  else:
    result = None              # Needed to get correct error messages.
    raise TypeError (_fl_typeerror_msg)
  return result

cdef object _fl_evaluate (object spline, double t,
                          double (*evaluator) (array.array[double] spline,
                                               double t)):
  if isinstance (spline, array.array) and spline.typecode == 'd':
    result = evaluator (spline, t)
  elif isinstance (spline, tuple) or isinstance (spline, list):
    result = evaluator (array.array ('d', spline), t)
  else:
    result = None              # Needed to get correct error messages.
    raise TypeError (_fl_typeerror_msg)
  return result

@cython.boundscheck(False)
@cython.wraparound(False)
cdef object _fl_subdivide (object spline, double t,
                           object (*subdivider) (array.array[double] spline,
                                                 double t)):
  if isinstance (spline, array.array) and spline.typecode == 'd':
    result = subdivider (spline, t)
  elif isinstance (spline, tuple):
    (a1, a2) = subdivider (array.array ('d', spline), t)
    result = (tuple (a1), tuple (a2))
  elif isinstance (spline, list):
    (a1, a2) = subdivider (array.array ('d', spline), t)
    result = (list (a1), list (a2))
  else:
    result = None              # Needed to get correct error messages.
    raise TypeError (_fl_typeerror_msg)
  return result

#--------------------------------------------------------------------------

def fl_bern_to_sbern (spline not None):
  return _fl_change_basis (spline, c_fl_bern_to_sbern)

def fl_sbern_to_bern (spline not None):
  return _fl_change_basis (spline, c_fl_sbern_to_bern)

def fl_sbern_to_mono (spline not None):
  return _fl_change_basis (spline, c_fl_sbern_to_mono)

def fl_mono_to_sbern (spline not None):
  return _fl_change_basis (spline, c_fl_mono_to_sbern)

def fl_bern_to_mono (spline not None):
  return _fl_change_basis (spline, c_fl_bern_to_mono)

def fl_mono_to_bern (spline not None):
  return _fl_change_basis (spline, c_fl_mono_to_bern)

def fl_eval_sbern (spline not None, double t):
  return _fl_evaluate (spline, t, c_fl_eval_sbern)

def fl_eval_bern (spline not None, double t):
  return _fl_evaluate (spline, t, c_fl_eval_bern)

def fl_evaldc_sbern (spline not None, double t):
  return _fl_evaluate (spline, t, c_fl_evaldc_sbern)

def fl_evaldc_bern (spline not None, double t):
  return _fl_evaluate (spline, t, c_fl_evaldc_bern)

def fl_eval_mono (spline not None, double t):
  return _fl_evaluate (spline, t, c_fl_eval_mono)

def fl_subdiv_sbern (spline not None, double t):
  return _fl_subdivide (spline, t, c_fl_subdiv_sbern)

def fl_subdiv_bern (spline not None, double t):
  return _fl_subdivide (spline, t, c_fl_subdiv_bern)

#--------------------------------------------------------------------------
