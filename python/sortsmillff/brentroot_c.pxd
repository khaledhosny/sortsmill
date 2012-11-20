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

from gmpy cimport *

cdef extern from "brentroot.h":
  ctypedef double (*brentroot_func_t) (double, void *)

  void brentroot (int max_iters, double tol,
                  double t1, double t2,
                  brentroot_func_t func, void *data,
                  double *root, int *err,
                  unsigned int *iter_no)

cdef extern from "qbrentroot.h":
  ctypedef void (*qbrentroot_func_t) (__mpq_struct *, __mpq_struct *, void *)

  void qbrentroot (int max_iters, __mpq_struct *tol,
                   __mpq_struct *epsilon,
                   __mpq_struct *t1, __mpq_struct *t2,
                   qbrentroot_func_t func, void *data,
                   __mpq_struct *root, int *err,
                   unsigned int *iter_no)
