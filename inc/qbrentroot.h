// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#ifndef _QBRENTROOT_H
#define _QBRENTROOT_H

#include <config.h>

#include <gmp.h>

typedef void (*qbrentroot_func_t) (mpq_t, const mpq_t, void *);

// vis--
// vis-- @deftypefun void qbrentroot (int @var{max_iters}, const mpq_t @var{tol}, const mpq_t @var{epsilon}, const mpq_t @var{t1}, const mpq_t @var{t2}, void (*@var{func}) (mpq_t, const mpq_t, const void *), const void *@var{data}, mpq_t @var{root}, int *@var{err}, unsigned int *@var{iter_no})
// vis--
// vis-- Brent's method for root-finding.
// vis--
// vis-- FIXME: Document the parameters, including
// vis-- (a)~that max_iters < 0 and tol < 0 mean to use defaults,
// vis-- and (b)~what the error codes are.
// vis--
// vis-- @end deftypefun
// vis--
VISIBLE void qbrentroot (int max_iters, const mpq_t tol, const mpq_t epsilon,
                         const mpq_t t1, const mpq_t t2,
                         qbrentroot_func_t func, void *data, mpq_t root,
                         int *err, unsigned int *iter_no);

#endif // _QBRENTROOT_H
