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

#include <sortsmillff/guile/polyspline.h>

void init_guile_sortsmillff_polyspline (void);

VISIBLE void
init_guile_sortsmillff_polyspline (void)
{
  scm_c_define_gsubr ("f64vector-sbern->bern", 1, 0, 0,
                      scm_f64vector_sbern_to_bern);
  scm_c_define_gsubr ("f64vector-bern->sbern", 1, 0, 0,
                      scm_f64vector_bern_to_sbern);
  scm_c_define_gsubr ("f64vector-eval-sbern", 2, 0, 0,
                      scm_f64vector_eval_sbern);
  scm_c_define_gsubr ("f64vector-eval-bern", 2, 0, 0,
                      scm_f64vector_eval_bern);
  scm_c_define_gsubr ("f64vector-evaldc-sbern", 2, 0, 0,
                      scm_f64vector_evaldc_sbern);
  scm_c_define_gsubr ("f64vector-evaldc-bern", 2, 0, 0,
                      scm_f64vector_evaldc_bern);
  scm_c_define_gsubr ("f64vector-subdiv-sbern", 2, 0, 0,
                      scm_f64vector_subdiv_sbern);
  scm_c_define_gsubr ("f64vector-subdiv-bern", 2, 0, 0,
                      scm_f64vector_subdiv_bern);
}
