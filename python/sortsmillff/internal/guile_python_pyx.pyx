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

from libc.stdint cimport uintptr_t
cimport sortsmillff.cython.xgc as xgc

import sys
import gmpy

ctypedef object (*stringifier_t) (void *)

# ‘pyguile’ objects stay in this container until they are destroyed,
# to help ensure that the Boehm GC does not collect them. FIXME: Is
# this container needed, and is it adequate?
__pyguile_objects = set ()

cdef class pyguile (object):
  """A opaque representation of Guile objects."""

  # These addresses should be kept in uintptr_t format rather than as
  # a Python long, so the Boehm GC can recognize them
  cdef public uintptr_t address      # The Guile ‘object-address’.
  cdef uintptr_t stringifier_address # A function __str__ calls to
                                     # ‘stringify’ the object.

  def __cinit__ (self):
    self.stringifier_address = <uintptr_t> NULL
    self.address = <uintptr_t> NULL

  def __init__ (self, uintptr_t address, uintptr_t stringifier_address):
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

cdef public object __exec_python (object python_code):
  retval = None
  try:
    exec (python_code)
  except (object, BaseException, Exception) as exc:
    retval = (exc, sys.exc_info ())
  return retval

cdef public object __exec_python_file_name (object file_name):
  retval = None
  try:
    execfile (file_name)
  except (object, BaseException, Exception) as exc:
    retval = (exc, sys.exc_info ())
  return retval

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
