# -*- coding: utf-8; python-indent: 2 -*-

# Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

cdef extern from 'config.h':
  pass

from libc.stdint cimport uintptr_t
cimport sortsmill.cython.xgc as xgc

from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm


# ‘pyguile’ objects stay in this container until they are destroyed,
# to help ensure that the Boehm GC does not collect them.
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
