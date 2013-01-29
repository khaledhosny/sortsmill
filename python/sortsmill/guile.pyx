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

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

from sortsmill.cython.const_pointers cimport const_char_ptr

from sortsmill.internal.__guile_support import pyguile
from libc.stdint cimport uintptr_t

#--------------------------------------------------------------------------

# FIXME: Make this available to other modules.
cdef SCM scm_from_string_object (object string):
  assert isinstance (string, unicode) or isinstance (string, bytes)
  if isinstance (string, unicode):
    string = string.encode ('UTF-8')
  cdef char *s = string
  cdef SCM scm_string = scm.scm_from_utf8_string (s)
  return scm_string

#--------------------------------------------------------------------------

def init_guile ():
  scm.scm_init_guile ()

def resolve_guile_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  cdef SCM module = scm.scm_c_resolve_module (name)
  return pyguile (<uintptr_t> module)

def current_guile_module ():
  cdef SCM module = scm.scm_current_module ()
  return pyguile (<uintptr_t> module)

def set_current_guile_module (module not None):
  assert isinstance (module, pyguile)
  cdef SCM module_pointer = scm.scm_from_pyguile_object (module)
  cdef SCM old_module = scm.scm_set_current_module (module_pointer)
  return pyguile (<uintptr_t> old_module)

def use_guile_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  scm.scm_c_use_module (name)

def guile_interaction_environment ():
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

#--------------------------------------------------------------------------
