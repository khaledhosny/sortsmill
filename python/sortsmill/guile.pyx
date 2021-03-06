# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

from sortsmill.cython.const_pointers cimport const_char_ptr
from cpython.ref cimport PyObject
from libc.stdint cimport uintptr_t

from __sortsmill__.__pyguile__ import pyguile
import gmpy
from . import (conditions)
from conditions import result_type_is

# Start Guile mode if it is not already started.
scm.scm_init_guile ()

#--------------------------------------------------------------------------

def __arg_string_is_string (string):
  return True if isinstance (string, unicode) or isinstance (string, bytes) \
      else "expected a string: {!r}".format (string)

def __arg_pyg_obj_is_pyguile (pyg_obj):
  return True if isinstance (pyg_obj, pyguile) \
      else "expected a pyguile: {!r}".format (pyg_obj)

def __arg_value_is_bool (value):
  return True if isinstance (value, bool) \
      else "expected a bool: {!r}".format (value)

def __arg_value_is_guile_compatible_number (value):
  return True if number_is_guile_compatible (value) \
      else "expected a Guile-compatible number: {!r}".format (value)

#--------------------------------------------------------------------------

def init_guile ():
  scm.scm_init_guile ()

@conditions.post (result_type_is (pyguile))
def resolve_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  cdef SCM module = scm.scm_c_resolve_module (name)
  return pyguile (<uintptr_t> module)

@conditions.post (result_type_is (pyguile))
def current_module ():
  cdef SCM module = scm.scm_current_module ()
  return pyguile (<uintptr_t> module)

@conditions.pre (lambda module: isinstance (module, pyguile))
@conditions.post (result_type_is (pyguile))
def set_current_module (module):
  cdef SCM module_pointer = scm.scm_from_pyguile_object (module)
  cdef SCM old_module = scm.scm_set_current_module (module_pointer)
  return pyguile (<uintptr_t> old_module)

def use_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  scm.scm_c_use_module (name)

@conditions.post (result_type_is (pyguile))
def interaction_environment ():
  cdef SCM env = scm.scm_interaction_environment ()
  return pyguile (<uintptr_t> env)

@conditions.pre (lambda expression, module: True if isinstance (expression, pyguile) \
                   else "expected a pyguile for ‘expression’: {!r}".format (expression),
                 lambda expression, module: True if isinstance (module, pyguile) \
                   else "expected a pyguile for ‘module’: {!r}".format (module))
@conditions.post (result_type_is (pyguile))
def guile_eval (expression, module):
  cdef SCM expression_pointer = scm.scm_from_pyguile_object (expression)
  cdef SCM module_pointer = scm.scm_from_pyguile_object (module)
  cdef SCM result = scm.scm_eval (expression_pointer, module_pointer)
  return pyguile (<uintptr_t> result)

@conditions.pre (lambda string, module = None: True if isinstance (string, unicode) or isinstance (string, bytes) \
                   else "expected a string for ‘string’: {!r}".format (string),
                 lambda string, module = None: True if module is None or isinstance (module, pyguile) \
                   else "expected a pyguile for ‘module’: {!r}".format (module))
@conditions.post (result_type_is (pyguile))
def guile_eval_string (string not None, module = None):
  cdef SCM result
  cdef SCM module_pointer
  cdef SCM scm_string = scm.scm_from_string_object (string)
  if module is None:
    result = scm.scm_eval_string (scm_string)
  else:
    module_pointer = scm.scm_from_pyguile_object (module)
    result = scm.scm_eval_string_in_module (scm_string, module_pointer)
  return pyguile (<uintptr_t> result)

@conditions.post (result_type_is (pyguile))
def pyobject (obj):
  cdef SCM pyobj = scm.scm_from_borrowed_object (obj)
  return pyguile (<uintptr_t> pyobj)

@conditions.pre (__arg_string_is_string)
@conditions.post (result_type_is (pyguile))
def string_to_pyguile (string):
  cdef SCM scm_string = scm.scm_from_string_object (string)
  return pyguile (<uintptr_t> scm_string)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (unicode, bytes))
def pyguile_to_string (pyg_obj):
  cdef SCM scm_string = scm.scm_from_pyguile_object (pyg_obj)
  cdef SCM py_string = scm.scm_string_to_pystring (scm_string)
  py_s = scm.scm_to_object (py_string)
  return py_s

@conditions.pre (__arg_string_is_string)
@conditions.post (result_type_is (pyguile))
def string_to_guile_symbol (string):
  cdef SCM scm_string = scm.scm_from_string_object (string)
  cdef SCM scm_symbol = scm.scm_string_to_symbol (scm_string)
  return pyguile (<uintptr_t> scm_symbol)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (unicode, bytes))
def guile_symbol_to_string (pyg_obj):
  cdef SCM scm_symbol = scm.scm_from_pyguile_object (pyg_obj)
  cdef SCM scm_string = scm.scm_symbol_to_string (scm_symbol)
  cdef SCM py_string = scm.scm_string_to_pystring (scm_string)
  py_s = scm.scm_to_object (py_string)
  return py_s

@conditions.pre (__arg_string_is_string)
@conditions.post (result_type_is (pyguile))
def string_to_guile_keyword (string):
  cdef SCM scm_string = scm.scm_from_string_object (string)
  cdef SCM scm_symbol = scm.scm_string_to_symbol (scm_string)
  cdef SCM scm_keyword = scm.scm_symbol_to_keyword (scm_symbol)
  return pyguile (<uintptr_t> scm_keyword)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (unicode, bytes))
def guile_keyword_to_string (pyg_obj):
  cdef SCM scm_keyword = scm.scm_from_pyguile_object (pyg_obj)
  cdef SCM scm_symbol = scm.scm_keyword_to_symbol (scm_keyword)
  cdef SCM scm_string = scm.scm_symbol_to_string (scm_symbol)
  cdef SCM py_string = scm.scm_string_to_pystring (scm_string)
  py_s = scm.scm_to_object (py_string)
  return py_s

@conditions.pre (__arg_value_is_bool)
@conditions.post (result_type_is (pyguile))
def bool_to_pyguile (value):
  cdef SCM b = scm.scm_pybool_to_boolean (scm.scm_from_object (value))
  return pyguile (<uintptr_t> b)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (bool))
def pyguile_to_bool (pyg_obj):
  cdef bint b = scm.scm_is_true (scm.scm_from_pyguile_object (pyg_obj))
  return b

__acceptable_number_types = (type (1),
                             type (111111111111111111111111111111111111111111111111L),
                             type (1.1),
                             type (gmpy.mpz (1)),
                             type (gmpy.mpq (1,11)),
                             type (1+1j))

@conditions.post (result_type_is (bool))
def number_is_guile_compatible (n):
  return (type (n) in __acceptable_number_types)

@conditions.pre (__arg_value_is_guile_compatible_number)
@conditions.post (result_type_is (pyguile))
def number_to_pyguile (value):
  cdef SCM n = scm.scm_pyobject_to_number (scm.scm_from_object (value))
  return pyguile (<uintptr_t> n)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (*__acceptable_number_types))
def pyguile_to_number (pyg_obj):
  cdef SCM py_n = scm.scm_number_to_pyobject (scm.scm_from_pyguile_object (pyg_obj))
  result = scm.scm_to_object (py_n)
  return result

@conditions.pre (lambda value: <bint> scm.scm_is_pysequence (scm.scm_from_object (value)))
@conditions.post (result_type_is (pyguile))
def sequence_to_pyguile (value):
  cdef SCM py_obj = scm.scm_from_object (value)
  cdef SCM lst = scm.scm_pysequence_to_list (py_obj)
  return pyguile (<uintptr_t> lst)

@conditions.pre (__arg_pyg_obj_is_pyguile)
@conditions.post (result_type_is (list))
def pyguile_to_list (pyg_obj):
  cdef SCM py_lst = scm.scm_list_to_pylist (scm.scm_from_pyguile_object (pyg_obj))
  result = scm.scm_to_object (py_lst)
  return result

def public_ref (module_name not None, name not None):
  assert isinstance (module_name, unicode) or isinstance (module_name, bytes)
  assert isinstance (name, unicode) or isinstance (name, bytes)
  if isinstance (module_name, unicode):
    module_name = module_name.encode ('UTF-8')
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  cdef char *modname = module_name
  cdef char *varname = name
  cdef SCM value = scm.scm_c_public_ref (modname, varname)
  return pyguile (<uintptr_t> value)

def private_ref (module_name not None, name not None):
  assert isinstance (module_name, unicode) or isinstance (module_name, bytes)
  assert isinstance (name, unicode) or isinstance (name, bytes)
  if isinstance (module_name, unicode):
    module_name = module_name.encode ('UTF-8')
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  cdef char *modname = module_name
  cdef char *varname = name
  cdef SCM value = scm.scm_c_private_ref (modname, varname)
  return pyguile (<uintptr_t> value)

def call (procedure, *args):
  cdef SCM proc = scm.scm_from_pyguile_object (procedure)
  cdef SCM arglist = scm.scm_eol ()
  cdef ssize_t i
  cdef SCM arg
  for i in range (len (args) - 1, -1, -1):
    arg = scm.scm_from_pyguile_object (args[i])
    arglist = scm.scm_cons (arg, arglist)
  cdef SCM result = scm.scm_apply_0 (proc, arglist)
  return pyguile (<uintptr_t> result)

#--------------------------------------------------------------------------
