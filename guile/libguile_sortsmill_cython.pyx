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

include 'sortsmill/cython/config.pxi'

cdef extern from 'config.h':
  pass

cdef extern from 'sortsmill/xdie_on_null.h':
  pass

from cpython.ref cimport PyObject
from libc.stdint cimport uintptr_t, uintmax_t
cimport sortsmill.cython.xgc as xgc

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

ctypedef object (*stringifier_t) (void *)
ctypedef object (*tuple_func_t) (object)

import sys
import gmpy
import inspect
import traceback
from __sortsmill__.__pyguile__ import pyguile

#--------------------------------------------------------------------------

cdef object wrap_exception_and_throw_to_guile (object who, object exc_info):
  cdef SCM who_symbol = scm.scm_string_to_symbol (scm.scm_from_string_object (who))
  cdef SCM info = scm.scm_from_object (exc_info)
  scm.scm_throw (scm.scm_from_latin1_symbol ('python-exception'),
                 scm.scm_list_2 (who_symbol, info))

cdef inline object exec_python (object who, object python_code, object glob, object locl):
  try:
    exec python_code in glob, locl
  except:
    wrap_exception_and_throw_to_guile (who, sys.exc_info ())

cdef inline object exec_python_file_name (object who, object file_name, object glob, object locl):
  try:
    execfile (file_name, glob, locl)
  except:
    wrap_exception_and_throw_to_guile (who, sys.exc_info ())

cdef inline object eval_python (object who, object python_code, object glob, object locl):
  try:
    retval = eval (python_code, glob, locl)
  except:
    wrap_exception_and_throw_to_guile (who, sys.exc_info ())
    retval = None
  return retval

#--------------------------------------------------------------------------

cdef public object __c_pyguile_make (void *pointer):
  cdef uintptr_t address = <uintptr_t> pointer
  return pyguile (address)

cdef public void *__c_pyguile_address (object obj):
  cdef uintptr_t address = obj.address
  cdef void *pointer = <void *> address
  return pointer

cdef public object __pyguile_check (object obj):
  return isinstance (obj, pyguile)

#--------------------------------------------------------------------------

class guile_exception (Exception):

  def __init__ (self, key, args):
    self.key = key
    self.args = args

cdef public object __py_raise_guile_exception (object data):
  (key, args) = data
  raise guile_exception (key, args)

#--------------------------------------------------------------------------

cdef public object __py_exception_description (object exc_info):
  description = traceback.format_exception (*exc_info)
  return description

#--------------------------------------------------------------------------

cdef public object __exec_python (object python_code):
  main_dict = sys.modules['__main__'].__dict__
  return exec_python ('pyexec-in-main', python_code, main_dict, main_dict)

cdef public object __exec_python_file_name (object file_name):
  main_dict = sys.modules['__main__'].__dict__
  return exec_python_file_name ('pyexec-file-name-in-main', file_name, main_dict, main_dict)

cdef public object __eval_python (object python_code):
  main_dict = sys.modules['__main__'].__dict__
  return eval_python ('pyeval-in-main', python_code, main_dict, main_dict)

cdef public object __exec_python_in_module (object module_and_code):
  (module, python_code) = module_and_code
  if inspect.ismodule (module):
    the_dict = module.__dict__
  else:
    the_dict = sys.modules[module].__dict__
  return exec_python ('pyexec', python_code, the_dict, the_dict)

cdef public object __exec_python_file_name_in_module (object module_and_file_name):
  (module, file_name) = module_and_file_name
  if inspect.ismodule (module):
    the_dict = module.__dict__
  else:
    the_dict = sys.modules[module].__dict__
  return exec_python_file_name ('pyexec-file-name', file_name, the_dict, the_dict)

cdef public object __eval_python_in_module (object module_and_code):
  (module, python_code) = module_and_code
  if inspect.ismodule (module):
    the_dict = module.__dict__
  else:
    the_dict = sys.modules[module].__dict__
  return eval_python ('pyeval', python_code, the_dict, the_dict)

#--------------------------------------------------------------------------

def __py_wrap_function (uintptr_t func_address):
  def wrapped_func (*args):
    cdef object args_tuple = tuple (args)
    cdef void *func_pointer = <void *> func_address
    cdef tuple_func_t tuple_func = <tuple_func_t> func_pointer
    cdef object retval = tuple_func (args_tuple)
    return retval
  return wrapped_func

cdef public object __c_py_wrap_function (void *func):
  return __py_wrap_function (<uintptr_t> func)

cdef public object __apply_python_callable (object func, object args, object keyword_args):
  assert (func is not None)
  assert (args is not None)
  if keyword_args is None:
    retval = func (*args)
  else:
    retval = func (*args, **keyword_args)
  return retval

cdef public object __python_module (object module_name):
  cdef SCM scm_pymodule
  try:
    module = sys.modules[module_name]
  except:
    scm_pymodule = scm.scm_call_1 (scm.scm_c_public_ref ('sortsmill python', 'pyimport'),
                                   scm.scm_from_string_object (module_name))
    module = scm.scm_to_object (scm_pymodule)
  return module

cdef public object __python_import (object args):
  (name, glob, locl, fromlist, level) = args
  try:
    result = __import__ (name, glob, locl, fromlist, level)
  except:
    wrap_exception_and_throw_to_guile ('__python_import', sys.exc_info ())
    result = None
  return result

cdef public object __pylong_to_pympz (object obj):
  return gmpy.mpz (obj)

cdef public object __pympz_to_pylong (object obj):
  return long (obj)

cdef public object __py_repr (object obj):
  return repr (obj)

cdef public object __py_str (object obj):
  return str (obj)

cdef public object __py_name (object obj):
  return obj.__name__

cdef public object __py_dict (object obj):
  return obj.__dict__

cdef public object __pyindexed_ref (object args):
  (obj, i) = args
  return obj[i]

cdef public object __pyindexed_set (object args):
  (obj, i, v) = args
  obj[i] = v

cdef public object __python_alist_to_pydict (object alist):
  return { key: value for (key, value) in alist }

cdef public object __make_pyiterator (object iterable):
  return iter (iterable)

cdef public PyObject *__pyiterator_next (object iter):
  cdef PyObject *result
  try:
    py_result = iter.next ()
    result = <PyObject *> py_result
  except StopIteration:
    result = NULL
  return result

#--------------------------------------------------------------------------
