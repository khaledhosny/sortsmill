# -*- coding: utf-8; python-indent: 2 -*-

# Copyright (C) 2013 Barry Schwartz
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

##
## Some internals of the Guile-Python interface.
##

cdef extern from 'config.h':
  pass

cimport sortsmillff.cython.xgc as xgc

import sys
import gmpy

cdef public object __c_eval_python (char *python_code):
  py_code = python_code
  return eval (py_code.decode ('UTF-8'))

cdef public char *__python_string_to_c_string (object s):
  py_s = s.encode ('UTF-8') if isinstance (s, unicode) else s
  cdef char *c_s = py_s
  return xgc.x_gc_strdup (c_s)

cdef public object __c_string_to_python_string (char *s):
  py_s = s
  return py_s.decode ('UTF-8')

cdef public object __current_python_module ():
  return sys.modules[__name__]

cdef public object __pylong_to_pympz (object obj):
  return gmpy.mpz (obj)

cdef public object __pympz_to_pylong (object obj):
  return long (obj)

###cdef public object __is_pympz (object obj):
###  return isinstance (obj, gmpy.mpz)

cdef public object __py_repr (object obj):
  return repr (obj)

cdef public object __py_str (object obj):
  return str (obj)

cdef public object __py_name (object obj):
  return obj.__name__

cdef public object __py_dict (object obj):
  return obj.__dict__

cdef public object __py_dict_ref (object obj, object i):
  return obj[i]

cdef public object __py_dict_set (object obj, object i, object v):
  obj[i] = v
