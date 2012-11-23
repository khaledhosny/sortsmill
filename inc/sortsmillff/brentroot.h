/*
 * Copyright (C) 2012 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILLFF_BRENTROOT_H
#define _SORTSMILLFF_BRENTROOT_H

/* *INDENT-OFF* */
#ifdef __cplusplus
extern "C" {
#endif
/* *INDENT-ON* */

typedef double (*brentroot_func_t) (double, void *);

/* FIXME: Add an @var{epsilon} parameter to brentroot so it is in
   parallel with qbrentroot; assert that epsilon is at least
   DBL_EPSILON. */
/*
 * vis--
 * vis-- @deftypefun void brentroot (int @var{max_iters}, double @var{tol}, double @var{t1}, double @var{t2}, double (*@var{func}) (double, const void *), const void *@var{data}, double *@var{root}, int *@var{err}, unsigned int *@var{iter_no})
 * vis--
 * vis-- Brent's method for root-finding.
 * vis--
 * vis-- FIXME: Document the parameters, including
 * vis-- (a)~that max_iters < 0 and tol < 0 mean to use defaults,
 * vis-- and (b)~what the error codes are.
 * vis--
 * vis-- @end deftypefun
 * vis--
 */
void brentroot (int max_iters, double tol, double t1, double t2,
                brentroot_func_t func, void *data, double *root,
                int *err, unsigned int *iter_no);

/* *INDENT-OFF* */
#ifdef __cplusplus
}
#endif
/* *INDENT-ON* */

#endif /* _SORTSMILLFF_BRENTROOT_H */
