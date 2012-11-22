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

import cython
import gmpy

#--------------------------------------------------------------------------

def mpz_matrix_set_all (*args):
  cdef int i, j
  cdef int m, n
  if len (args) == 3:
    (m, n, x) = args
    v = gmpy.mpz (x)
    result = [[v for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 2:
    (A, x) = args
    v = gmpy.mpz (x)
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = v
    result = A
  else:
    assert False, "mpz_matrix_set_all: wrong number of arguments"
  return result

def mpq_matrix_set_all (*args):
  cdef int i, j
  cdef int m, n
  if len (args) == 3:
    (m, n, x) = args
    v = gmpy.mpq (x)
    result = [[v for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 2:
    (A, x) = args
    v = gmpy.mpq (x)
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = v
    result = A
  else:
    assert False, "mpq_matrix_set_all: wrong number of arguments"
  return result

#--------------------------------------------------------------------------

def mpz_matrix_set_zero (*args):
  cdef int i, j
  cdef int m, n
  v = gmpy.mpz (0)
  if len (args) == 2:
    (m, n) = args
    result = [[v for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 1:
    (A,) = args
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = v
    result = A
  else:
    assert False, "mpz_matrix_set_zero: wrong number of arguments"
  return result

def mpq_matrix_set_zero (*args):
  cdef int i, j
  cdef int m, n
  v = gmpy.mpq (0)
  if len (args) == 2:
    (m, n) = args
    result = [[v for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 1:
    (A,) = args
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = v
    result = A
  else:
    assert False, "mpq_matrix_set_zero: wrong number of arguments"
  return result

#--------------------------------------------------------------------------

def mpz_matrix_set_identity (*args):
  cdef int i, j
  cdef int m, n
  v = gmpy.mpz (0)
  w = gmpy.mpz (1)
  if len (args) == 2:
    (m, n) = args
    result = [[(w if i == j else v) for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 1:
    (A,) = args
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = (w if i == j else v)
    result = A
  else:
    assert False, "mpz_matrix_set_identity: wrong number of arguments"
  return result

def mpq_matrix_set_identity (*args):
  cdef int i, j
  cdef int m, n
  v = gmpy.mpq (0)
  w = gmpy.mpq (1)
  if len (args) == 2:
    (m, n) = args
    result = [[(w if i == j else v) for j in range (0, n)] for i in range (0, m)]
  elif len (args) == 1:
    (A,) = args
    for i in range (0, len (A)):
      for j in range (0, len (A[i])):
        A[i][j] = (w if i == j else v)
    result = A
  else:
    assert False, "mpq_matrix_set_identity: wrong number of arguments"
  return result

#--------------------------------------------------------------------------
