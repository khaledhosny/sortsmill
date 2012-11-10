#include <config.h>

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

#include <sortsmillff/guile/brentroot.h>
#include <brentroot.h>
#include <math.h>

void init_guile_sortsmillff_brentroot (void);

static double
call_func (double x, void *func_p)
{
  SCM func = *(SCM *) func_p;
  return scm_to_double (scm_call_1 (func, scm_from_double (x)));
}

VISIBLE SCM
scm_f64_brentroot (SCM max_iters, SCM tol, SCM t1, SCM t2, SCM func)
{
  double root;
  int err;
  unsigned int iter_no;
  SCM result[3];

  int c_max_iters = scm_to_int (max_iters);
  double c_tol = scm_to_double (tol);
  double c_t1 = scm_to_double (t1);
  double c_t2 = scm_to_double (t2);

  root = nan ("");
  brentroot (c_max_iters, c_tol, c_t1, c_t2, call_func, &func, &root,
             &err, &iter_no);

  result[0] = (err == 0) ? scm_from_double (root) : SCM_BOOL_F;
  result[1] = scm_from_int (err);
  result[2] = scm_from_uint (iter_no);

  return scm_c_values (result, 3);
}

VISIBLE void
init_guile_sortsmillff_brentroot (void)
{
  scm_c_define_gsubr ("f64-brentroot", 5, 0, 0, scm_f64_brentroot);
}
