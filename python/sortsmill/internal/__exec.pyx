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

include "sortsmill/cython/config.pxi"

cdef extern from 'config.h':
  pass

from cpython.ref cimport PyObject
from sortsmill.cython.guile cimport SCM
cimport sortsmill.cython.guile as scm

import sys
import sortsmill

cpdef object wrap_exception_and_throw_to_guile (object who, object exc_info):
  who_bytes = who.encode ("UTF-8")
  cdef char *c_who = who_bytes
  cdef SCM scm_info = scm.scm_call_1 (scm.scm_c_public_ref ("sortsmill python",
                                                            "pointer->pyobject"),
                                      scm.scm_from_pointer (<PyObject *> exc_info, NULL))
  scm.scm_throw (scm.scm_from_utf8_symbol ("python-exception"),
                 scm.scm_list_2 (scm.scm_from_utf8_symbol (c_who), scm_info))
