# -*- coding: utf-8; python-indent: 2; -*-

# Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

cimport sortsmill.cython.gmpy

cdef extern from "sortsmill/linalg.h":
  pass

#  void mpz_matrix_init (unsigned int m, unsigned int n, mpz_t A[m][n]);
#  void mpz_matrix_clear (unsigned int m, unsigned int n, mpz_t A[m][n]);

#  void mpq_matrix_init (unsigned int m, unsigned int n, mpq_t A[m][n]);
#  void mpq_matrix_clear (unsigned int m, unsigned int n, mpq_t A[m][n]);

