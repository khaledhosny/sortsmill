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
import sys
import gmpy

cimport sortsmillff.cython.brentroot as brentroot_c
from cpython.ref cimport PyObject, Py_INCREF, Py_DECREF

#--------------------------------------------------------------------------

cdef extern from "Python.h":
  PyObject *PyObject_CallObject (PyObject *callable_object,
                                 PyObject *args)
  double PyFloat_AsDouble (PyObject *)

#--------------------------------------------------------------------------

cdef double _call_func (double x, void *func_p):
  args = (x,)
  result = PyObject_CallObject (<PyObject *> func_p,
                                <PyObject *> args)
  return PyFloat_AsDouble (result)

def brentroot (double t1, double t2, func not None,
               int max_iters = -1, double tol = -1):
  Py_INCREF (func)
  cdef double root
  cdef int err
  cdef unsigned int iter_no
  brentroot_c.brentroot (max_iters, tol, t1, t2,
                         _call_func, <PyObject *> func,
                         &root, &err, &iter_no)
  Py_DECREF (func)
  return (root if err == 0 else None)

def brentroot_values (double t1, double t2, func not None,
                      int max_iters = -1, double tol = -1):
  Py_INCREF (func)
  cdef double root
  cdef int err
  cdef unsigned int iter_no
  brentroot_c.brentroot (max_iters, tol, t1, t2,
                         _call_func, <PyObject *> func,
                         &root, &err, &iter_no)
  Py_DECREF (func)
  return ((root if err == 0 else None), err, iter_no)

#--------------------------------------------------------------------------

qbrentroot_default_max_iters = 1000000
qbrentroot_default_tol = gmpy.mpq (sys.float_info.epsilon) # FIXME: Is
                                                           # this
                                                           # value
                                                           # appropriate?

def qbrentroot (t1, t2, func not None,
                max_iters = -1, tol = -1,
                epsilon = sys.float_info.epsilon):
  return (qbrentroot_values (t1, t2, func, max_iters, tol, epsilon))[0]

def actual_max_iterations (max_iters):
  return max_iters if 0 <= max_iters else qbrentroot_default_max_iters

def actual_tolerance (tol):
  return gmpy.mpq (tol) if 0 <= tol else qbrentroot_default_tol

def bracketed (f1, f2):
  return (f1 <= 0 and 0 <= f2) or (f2 <= 0 and 0 <= f1)

def bisection (a, b):
  return gmpy.qdiv(a - b, 2)

def linear (s, fa, fb):
  fba = gmpy.qdiv (fb, fa)
  p = fba * 2 * s
  q = 1 - fba
  return (p, q)

def inverse_quadratic (s, a, fa, b, fb, fc):
  fbc = gmpy.qdiv (fb, fc)
  fba = gmpy.qdiv (fb, fa)
  fac = gmpy.qdiv (fa, fc)
  p = fba * (2 * s * fac * (fac - fbc) - (b - a) * (fbc - 1))
  q = (fac - 1) * (fba - 1) * (fbc - 1)
  return (p, q)

def interpolate (a, fa, b, fb, fb1, step, step1, tolerance):

  s = bisection (a, b)

  if fb1 == fa or fb1 == fb:
    (p, q) = linear (s, fa, fb)
  else:
    (p, q) = inverse_quadratic (s, a, fa, b, fb, fb1)
  if 0 < p:
    q = -q
  else:
    p = -p

  if 2 * p < min (3 * s * q - abs (tolerance * q), abs (step1 * q)):
    new_step = gmpy.qdiv (p, q)
    new_step1 = gmpy.qdiv (step);
  else:
    new_step = s
    new_step1 = s

  return (new_step, new_step1)

def max_iterations_exceeded (max_iterations, iter_no):
  return (max_iterations <= iter_no)

def within_tolerance (tolerance, step, fb):
  return (abs (step) <= tolerance or fb == 0)

def we_are_done (max_iterations, iter_no, tolerance, step, fb):
  return (max_iterations_exceeded (max_iterations, iter_no)
          or within_tolerance (tolerance, step, fb))

def step_by_at_least_tolerance (tolerance, new_step, b):
  if tolerance < abs (new_step):
    guess = b + new_step
  elif new_step < 0:
    guess = b - tolerance
  else:
    guess = b + tolerance
  return guess

def qbrentroot_values (t1, t2, func not None,
                       max_iters = -1, tol = -1,
                       epsilon = sys.float_info.epsilon):
  t1 = gmpy.mpq (t1)
  t2 = gmpy.mpq (t2)
  epsilon = gmpy.mpq (epsilon)

  max_iterations = actual_max_iterations (max_iters)
  toler = actual_tolerance (tol)

  err = 0                       # err == 0 means 'no error'.
  iter_no = 0
  root = None

  a = t1
  b = t2
  fa = gmpy.mpq (func (a))
  fb = gmpy.mpq (func (b))

  if not bracketed (fa, fb):
    err = 1                     # err == 1 means 'root not bracketed'.
  else:
    if abs (fa) < abs (fb):
      # Swap a and b.
      step = b - a
      step1 = b - a
      b1 = b
      fb1 = fb
      b = a
      fb = fa
      a = b1
      fa = fb1
    else:
      step = a - b
      step1 = a - b
      b1 = a
      fb1 = fa
    tolerance = 2 * epsilon * abs (b) + gmpy.qdiv (toler, 2)
    while not we_are_done (max_iterations, iter_no, tolerance, step, fb):
      if abs (step1) < tolerance or abs (fa) <= abs (fb):
        # Interpolation is stepping too slowly.
        new_step = bisection (a, b)
        old_step = new_step
      else:
        (new_step, old_step) = interpolate (a, fa, b, fb, fb1,
                                            step, step1, tolerance)

      guess = step_by_at_least_tolerance (tolerance, new_step, b)
      fguess = gmpy.mpq (func (guess))

      iter_no += 1
      if bracketed (fb, fguess):
        if abs (fguess) < abs (fb):
          aa = b
          faa = fb
          bb = guess
          fbb = fguess
          step = new_step
          step1 = old_step
        else:
          aa = guess
          faa = fguess
          bb = b
          fbb = fb
          step = new_step
          step1 = old_step
      else:
        if abs (fguess) < abs (fa):
          aa = a
          faa = fa
          bb = guess
          fbb = fguess
          step = guess - a
          step1 = guess - a
        else:
          aa = guess
          faa = fguess
          bb = a
          fbb = fa
          step = guess - a
          step1 = guess - a
      b1 = b
      fb1 = fb
      a = aa
      fa = faa
      b = bb
      fb = fbb

      tolerance = 2 * epsilon * abs (b) + gmpy.qdiv (toler, 2)

    if max_iterations_exceeded (max_iterations, iter_no):
      err = 2            # err == 2 means maximum iterations exceeded.
    else:
      root = b

  return (root, err, iter_no)

#--------------------------------------------------------------------------
#
# qbrentroot_values_c and qbrentroot_c: An implementation of
# qbrentroot using callbacks from C.
#
# qbrentroot_values and qbrentroot, above, written in Python, may be
# faster. The C versions are here mainly to document how they were
# implemented. Also, someone might figure out how to speed them up, so
# they can replace the Python version.
#
# Until that happens, however, use of qbrentroot_values_c or
# qbrentroot_c is discouraged.
#

from sortsmillff.cython.gmpy cimport *
include "sortsmillff/cython/gmpy.pxi"

cdef void _call_qfunc (__mpq_struct *result, __mpq_struct *x, void *func_p):
  py_x = py_from_mpq (x)
  args = (py_x,)
  py_result = PyObject_CallObject (<PyObject *> func_p, <PyObject *> args)
  py_obj = gmpy.mpq (<object> py_result)
  py_to_mpq (py_obj, result)

def qbrentroot_values_c (t1 not None, t2 not None, func not None,
                         int max_iters = -1,
                         tol not None = gmpy.mpq (-1),
                         epsilon not None = gmpy.mpq (-1)):
  Py_INCREF (func)

  cdef int err
  cdef unsigned int iter_no

  t1 = gmpy.mpq (t1)
  t2 = gmpy.mpq (t2)
  tol = gmpy.mpq (tol)
  epsilon = gmpy.mpq (epsilon)

  cdef mpq_t c_t1
  cdef mpq_t c_t2
  cdef mpq_t c_tol
  cdef mpq_t c_epsilon
  cdef mpq_t c_root

  mpq_init (c_t1)
  mpq_init (c_t2)
  mpq_init (c_tol)
  mpq_init (c_epsilon)
  mpq_init (c_root)

  py_to_mpq (t1, c_t1)
  py_to_mpq (t2, c_t2)
  py_to_mpq (tol, c_tol)
  py_to_mpq (epsilon, c_epsilon)
  
  cdef void (*qbrent) (int, __mpq_struct *, __mpq_struct *,
                       __mpq_struct *, __mpq_struct *,
                       void (*) (__mpq_struct *, __mpq_struct *, void *),
                       void *, __mpq_struct *, int *, unsigned int *)
  qbrent = <void (*) (int, __mpq_struct *, __mpq_struct *,
                      __mpq_struct *, __mpq_struct *,
                      void (*) (__mpq_struct *, __mpq_struct *, void *),
                      void *, __mpq_struct *, int *,
                      unsigned int *)> &brentroot_c.qbrentroot  
  qbrent (max_iters, c_tol, c_epsilon, c_t1, c_t2,
          _call_qfunc, <PyObject *> func,
          c_root, &err, &iter_no)

  root = py_from_mpq (c_root)

  mpq_clear (c_t1)
  mpq_clear (c_t2)
  mpq_clear (c_tol)
  mpq_clear (c_epsilon)
  mpq_clear (c_root)

  Py_DECREF (func)
  return ((root if err == 0 else None), err, iter_no)

def qbrentroot_c (t1, t2, func not None,
                  max_iters = -1, tol = -1, epsilon = -1):
  return (qbrentroot_values_c (t1, t2, func, max_iters, tol, epsilon))[0]

#--------------------------------------------------------------------------
