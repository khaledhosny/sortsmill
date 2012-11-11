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
cimport brentroot_c
from cpython.ref cimport PyObject, Py_INCREF, Py_DECREF

cdef extern from "Python.h":
    PyObject *PyObject_CallObject (PyObject *callable_object,
                                   PyObject *args)
    double PyFloat_AsDouble (PyObject *)

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
