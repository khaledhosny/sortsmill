# -*- coding: utf-8; python-indent: 2; -*-

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

import sys
import sortsmill.internal.__exec as __exec

cpdef extern object wrap_exception_and_throw_to_guile (object who, object exc_info)

cdef inline object exec_python (object who, object python_code, object glob, object locl):
  try:
    exec python_code in glob, locl
  except:
    __exec.wrap_exception_and_throw_to_guile (who, sys.exc_info ())

cdef inline object exec_python_file_name (object who, object file_name, object glob, object locl):
  try:
    execfile (file_name, glob, locl)
  except:
    __exec.wrap_exception_and_throw_to_guile (who, sys.exc_info ())

cdef inline object eval_python (object who, object python_code, object glob, object locl):
  try:
    retval = eval (python_code, glob, locl)
  except:
    __exec.wrap_exception_and_throw_to_guile (who, sys.exc_info ())
    retval = None
  return retval

