# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012, 2013 Barry Schwartz
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

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

from sortsmill.cython.const_pointers cimport const_char_ptr
from cpython.ref cimport PyObject
from libc.stdint cimport uintptr_t

from __sortsmill__.__pyguile__ import pyguile
import gmpy

# Start Guile mode if it is not already started.
scm.scm_init_guile ()

#--------------------------------------------------------------------------

# FIXME: Make this available to other modules. What is the best way to
# do this in Cython?
cdef SCM scm_from_string_object (object string):
  assert isinstance (string, unicode) or isinstance (string, bytes)
  if isinstance (string, unicode):
    string = string.encode ('UTF-8')
  cdef char *s = string
  cdef SCM scm_string = scm.scm_from_utf8_string (s)
  return scm_string

cdef SCM scm_pyobject_from_object (object obj):
  return scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                               "pointer->pyobject"),
                         scm.scm_from_pointer (<PyObject *> obj, NULL))

cdef SCM scm_pyobject_from_borrowed_object (object obj):
  return scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                               "borrowed-pointer->pyobject"),
                         scm.scm_from_pointer (<PyObject *> obj, NULL))

#--------------------------------------------------------------------------

def init_guile ():
  scm.scm_init_guile ()

def resolve_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  cdef SCM module = scm.scm_c_resolve_module (name)
  return pyguile (<uintptr_t> module)

def current_module ():
  cdef SCM module = scm.scm_current_module ()
  return pyguile (<uintptr_t> module)

def set_current_module (module not None):
  assert isinstance (module, pyguile)
  cdef SCM module_pointer = scm.scm_from_pyguile_object (module)
  cdef SCM old_module = scm.scm_set_current_module (module_pointer)
  return pyguile (<uintptr_t> old_module)

def use_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  scm.scm_c_use_module (name)

def interaction_environment ():
  cdef SCM env = scm.scm_interaction_environment ()
  return pyguile (<uintptr_t> env)

def guile_eval (expression not None, module not None):
  assert isinstance (expression, pyguile)
  assert isinstance (module, pyguile)
  cdef SCM expression_pointer = scm.scm_from_pyguile_object (expression)
  cdef SCM module_pointer = scm.scm_from_pyguile_object (module)
  cdef SCM result = scm.scm_eval (expression_pointer, module_pointer)
  return pyguile (<uintptr_t> result)

def guile_eval_string (string not None, module = None):
  cdef SCM result
  cdef SCM module_pointer
  cdef SCM scm_string = scm_from_string_object (string)
  if module is None:
    result = scm.scm_eval_string (scm_string)
  else:
    assert isinstance (module, pyguile)
    module_pointer = scm.scm_from_pyguile_object (module)
    result = scm.scm_eval_string_in_module (scm_string, module_pointer)
  return pyguile (<uintptr_t> result)

def guile_string (string not None):
  cdef SCM scm_string = scm_from_string_object (string)
  return pyguile (<uintptr_t> scm_string)

def pyobject (obj):
  cdef SCM pyobj = scm_pyobject_from_borrowed_object (obj)
  return pyguile (<uintptr_t> pyobj)

def bool_to_pyguile (v):
  assert isinstance (v, bool)
  cdef SCM b = scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                                     "pybool->boolean"),
                               scm_pyobject_from_object (v))
  return pyguile (<uintptr_t> b)

def pyguile_to_bool (pyg_obj):
  assert isinstance (pyg_obj, pyguile)
  cdef bint b = scm.scm_is_true (scm.scm_from_pyguile_object (pyg_obj))
  return b

__example_number_types = (type (1),
                          type (1111111111111111111111111111111111111111111111111111111111111111111111111111111L),
                          type (1.1),
                          type (gmpy.mpq (1,11)),
                          type (1+1j))

def number_to_pyguile (v):
  assert type (v) in __example_number_types
  cdef SCM n = scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                                     "pyobject->number"),
                               scm_pyobject_from_object (v))
  return pyguile (<uintptr_t> n)

def pyguile_to_number (pyg_obj):
  assert isinstance (pyg_obj, pyguile)
  cdef SCM py_n = scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                                        "number->pyobject"),
                                  scm.scm_from_pyguile_object (pyg_obj))
  cdef SCM p = scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                                     "pyobject->pointer"),
                               py_n)
  result = <object> scm.scm_to_pointer (p)
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
