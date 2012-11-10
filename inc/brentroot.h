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

#ifndef _BRENTROOT_H
#define _BRENTROOT_H

#include <config.h>

// vis--
// vis-- @deftypefun void brentroot (int @var{max_iters}, double @var{tol}, double @var{t1}, double @var{t2}, double (*@var{func}) (double, const void *), const void *@var{data}, double *@var{root}, int *@var{err}, unsigned int *@var{iter_no})
// vis--
// vis-- Brent's method for root-finding.
// vis--
// vis-- FIXME: Document the parameters, including
// vis-- (a)~that max_iters < 0 and tol < 0 mean to use defaults,
// vis-- and (b)~what the error codes are.
// vis--
// vis-- @end deftypefun
// vis--
VISIBLE void
brentroot (int max_iters,
           double tol,
           double t1,
           double t2,
           double (*func) (double, const void *),
           const void *data, double *root, int *err, unsigned int *iter_no);

#endif // _BRENTROOT_H
