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
  cdef SCM old_module = scm.scm_set_current_module (<SCM> module.address)
  return pyguile (<uintptr_t> old_module)

def use_guile_module (name not None):
  if isinstance (name, unicode):
    name = name.encode ('UTF-8')
  scm.scm_c_use_module (name)

#--------------------------------------------------------------------------
