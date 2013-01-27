# -*- mode: cython; coding: utf-8; python-indent: 2; -*-

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

import gmpy

cdef inline void py_to_mpz (object x, __mpz_struct *result):
  cdef PympzObject *obj = <PympzObject *> x
  mpz_set (result, obj.z)

cdef inline object py_from_mpz (__mpz_struct *z):
  result = gmpy.mpz (0)  
  cdef PympzObject *obj = <PympzObject *> result
  mpz_set (obj.z, z)
  return result

cdef inline void py_to_mpq (object x, __mpq_struct *result):
  cdef PympqObject *obj = <PympqObject *> x
  mpq_set (result, obj.q)

cdef inline object py_from_mpq (__mpq_struct *q):
  result = gmpy.mpq (0)  
  cdef PympqObject *obj = <PympqObject *> result
  mpq_set (obj.q, q)
  return result
