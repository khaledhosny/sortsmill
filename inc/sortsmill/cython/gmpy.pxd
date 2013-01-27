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

cdef extern from "gmp.h":

  ctypedef struct __mpz_struct:
    pass

  ctypedef struct __mpq_struct:
    pass

  ctypedef __mpz_struct mpz_t[1]
  ctypedef __mpq_struct mpq_t[1]

  void mpz_init (__mpz_struct *)
  void mpz_clear (__mpz_struct *)
  void mpz_set (__mpz_struct *, __mpz_struct *)

  void mpq_init (__mpq_struct *)
  void mpq_clear (__mpq_struct *)
  void mpq_set (__mpq_struct *, __mpq_struct *)

cdef extern from "gmpy.h":

  ctypedef struct PympzObject:
    __mpz_struct z[1]

  ctypedef struct PympqObject:
    __mpq_struct q[1]
