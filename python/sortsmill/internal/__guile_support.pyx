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

include "sortsmill/cython/config.pxi"

cdef extern from 'config.h':
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
import importlib
import traceback

from sortsmill.cython.internal.__exec cimport \
    exec_python, exec_python_file_name, eval_python
import sortsmill.internal.__exec as __exec

#--------------------------------------------------------------------------

# ‘pyguile’ objects stay in this container until they are destroyed,
# to help ensure that the Boehm GC does not collect them. FIXME: Is
# this container needed, and is it adequate?
__pyguile_objects = set ()

cdef class pyguile (object):
  """An opaque representation of Guile objects."""

  # This address should be kept in uintptr_t format rather than as a
  # Python long, so the Boehm GC can recognize it
  cdef public uintptr_t address # The Guile ‘object-address’.

  def __cinit__ (self):
    self.address = <uintptr_t> NULL

  def __init__ (self, uintptr_t address):
    global __pyguile_objects
    self.address = address
    __pyguile_objects.add (self)

  def __del__ (self):
    global __pyguile_objects
    __pyguile_objects.remove (self)

  def __repr__ (self):
    return "pyguile(0x{:x})".format (long (self.address))

  def __str__ (self):
    cdef SCM obj = <SCM> <void *> self.address
    cdef SCM port = scm.scm_open_output_string ()
    scm.scm_write (obj, port)
    cdef SCM scm_string = scm.scm_get_output_string (port)
    scm.scm_close_port (port)
    cdef char *string = xgc.x_gc_grabstr (scm.scm_to_utf8_stringn (scm_string, NULL))
    return "<pyguile {} 0x{:x}>".format (string, long (self.address))

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

cdef public char *__python_string_to_c_string (object s):
  py_s = s.encode ('UTF-8') if isinstance (s, unicode) else s
  cdef char *c_s = py_s
  return xgc.x_gc_strdup (c_s)

cdef public object __c_string_to_python_string (char *s):
  py_s = s
  return py_s.decode ('UTF-8')

cdef public object __python_module (object module_name):
  try:
    module = sys.modules[module_name]
  except:
    module = importlib.import_module (module_name)
  return module

cdef public object __current_python_module ():
  return sys.modules[__name__]

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

#--------------------------------------------------------------------------
