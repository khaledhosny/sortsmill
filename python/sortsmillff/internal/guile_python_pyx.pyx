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

include "sortsmillff/cython/config.pxi"

cdef extern from 'config.h':
  pass

from cpython.ref cimport PyObject
from libc.stdint cimport uintptr_t, uintmax_t
cimport sortsmillff.cython.xgc as xgc

from sortsmillff.cython.guile cimport SCM
cimport sortsmillff.cython.guile as scm

ctypedef object (*stringifier_t) (void *)
ctypedef object (*tuple_func_t) (object)

import sys
import gmpy

#--------------------------------------------------------------------------

# ‘pyguile’ objects stay in this container until they are destroyed,
# to help ensure that the Boehm GC does not collect them. FIXME: Is
# this container needed, and is it adequate?
__pyguile_objects = set ()

cdef class pyguile (object):
  """A opaque representation of Guile objects."""

  # These addresses should be kept in uintptr_t format rather than as
  # a Python long, so the Boehm GC can recognize them.
  cdef public uintptr_t address      # The Guile ‘object-address’.
  cdef uintptr_t stringifier_address # A function __str__ calls to
                                     # ‘stringify’ the object.

  def __cinit__ (self):
    self.stringifier_address = <uintptr_t> NULL
    self.address = <uintptr_t> NULL

  def __init__ (self, uintptr_t address, uintptr_t stringifier_address = 0):
    global __pyguile_objects
    self.address = address
    self.stringifier_address = stringifier_address
    __pyguile_objects.add (self)

  def __del__ (self):
    global __pyguile_objects
    __pyguile_objects.remove (self)

  def __repr__ (self):
    return ("pyguile(0x{:x},0x{:x})"
            .format (long (self.address), long (self.stringifier_address)))

  def __str__ (self):
    cdef void *stringifier_pointer = <void *> self.stringifier_address
    cdef stringifier_t stringifier = <stringifier_t> stringifier_pointer
    cdef void *pointer = <void *> self.address
    string = stringifier (pointer)
    return "<pyguile {} 0x{:x}>".format (string, long (self.address))

cdef public object __c_pyguile_make (void *pointer, void *stringifier_pointer):
  cdef uintptr_t address = <uintptr_t> pointer
  cdef uintptr_t stringifier_address = <uintptr_t> stringifier_pointer
  return pyguile (address, stringifier_address)

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

###  def __repr__ (self):
###    return "guile_exception({},{})".format (repr (self.key), repr (self.args))
###
###  def __str__ (self):
###    scm.scm_dynwind_begin (<scm.scm_t_dynwind_flags> 0)
###
###    cdef char *key = __python_string_to_c_string (self.key)
###
###    cdef SCM args = scm.scm_eol ()
###    cdef SCM obj
###    for arg in self.args:
###      obj = scm.scm_from_uintmax (<uintmax_t> arg.address)
###      args = scm.scm_cons (obj, args)
###    args = scm.scm_reverse (args)
###
###    cdef SCM guile_string = \
###        scm.scm_apply_1 (scm.scm_c_private_ref ("sortsmillff python",
###                                                "py-guile-exception-string"),
###                         scm.scm_from_utf8_symbol (key), args)
###
###    cdef char *s = scm.scm_to_utf8_stringn (guile_string, NULL)
###    scm.scm_dynwind_free (s)
###
###    python_string = __c_string_to_python_string (s)
###
###    scm.scm_dynwind_end ()
###
###    return python_string

cdef public object __py_raise_guile_exception (object key_and_args):
  (key, args) = key_and_args
  raise guile_exception (key, args)

#--------------------------------------------------------------------------

cdef public object __exec_python (object python_code):
  retval = None
  try:
    exec python_code in globals (), {}
  except (object, BaseException, Exception) as exc:
    retval = (exc, sys.exc_info ())
  return retval

cdef public object __exec_python_file_name (object file_name):
  retval = None
  try:
    execfile (file_name, globals (), {})
  except (object, BaseException, Exception) as exc:
    retval = (exc, sys.exc_info ())
  return retval

cdef public object __c_eval_python (char *python_code):
  py_code = python_code
  return eval (py_code.decode ('UTF-8'))

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
