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

cdef public char *__c_repr (object obj):
  return __python_string_to_c_string (repr (obj))

cdef public char *__c_str (object obj):
  return __python_string_to_c_string (str (obj))
