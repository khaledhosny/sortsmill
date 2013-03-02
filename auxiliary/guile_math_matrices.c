#include <config.h>

// Copyright (C) 2012, 2013 Barry Schwartz
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

#include <sortsmill/guile.h>
#include <sortsmill/gmp_matrix.h>

void init_guile_sortsmill_math_matrices (void);

static void
mpqmat_finalizer (void *p)
{
  mpqmat_t *mat = (mpqmat_t *) p;
  const unsigned int m = mat->size1;
  const unsigned int n = mat->size2;
  mpq_matrix_clear (m, n, (mpq_t (*)[(unsigned int) n]) mat->data);
  free (mat->data);
  free (mat);
}

VISIBLE SCM
scm_mpqmat_p (SCM obj)
{
  return scm_call_1 (scm_c_private_ref ("sortsmill math matrices",
                                        "private:mpqmat?"),
                     obj);
}

VISIBLE bool
scm_is_mpqmat (SCM obj)
{
  return scm_is_true (scm_mpqmat_p (obj));
}

VISIBLE SCM
scm_pointer_to_mpqmat (SCM pointer)
{
  return scm_call_1 (scm_c_private_ref ("sortsmill math matrices",
                                        "private:pointer->mpqmat"),
                     pointer);
}

VISIBLE SCM
scm_mpqmat_to_pointer (SCM mpqmat)
{
  return scm_call_1 (scm_c_private_ref ("sortsmill math matrices",
                                        "private:mpqmat->pointer"),
                     mpqmat);
}

VISIBLE SCM
scm_matrix_to_mpqmat (SCM A)
{
  const char *who = "scm_matrix_to_mpqmat";

  scm_t_array_handle handle_A;

  scm_dynwind_begin (0);

  scm_array_get_handle (A, &handle_A);
  scm_dynwind_array_handle_release (&handle_A);
  assert_c_rank_1_or_2_array (who, A, &handle_A);

  const unsigned int m = scm_matrix_dim1 (&handle_A);
  const unsigned int n = scm_matrix_dim2 (&handle_A);

  mpq_t *_A = scm_malloc (m * n * sizeof (mpq_t));
  mpq_matrix_init (m, n, (mpq_t (*)[n]) _A);

  scm_array_handle_to_mpq_matrix (A, &handle_A, m, n,
                                  (mpq_t (*)[(unsigned int) n]) _A);

  scm_dynwind_end ();

  mpqmat_t *mat = scm_malloc (sizeof (mpqmat_t));
  mat->size1 = m;
  mat->size2 = n;
  mat->data = _A;
  
  return scm_pointer_to_mpqmat (scm_from_pointer (mat, mpqmat_finalizer));
}

VISIBLE SCM
scm_mpqmat_to_matrix (SCM mpqmat)
{
  mpqmat_t *mat = (mpqmat_t *) scm_to_pointer (scm_mpqmat_to_pointer (mpqmat));
  const unsigned int m = mat->size1;
  const unsigned int n = mat->size2;
  return scm_from_mpq_matrix (m, n, (mpq_t (*)[(unsigned int) n]) mat->data);
}

VISIBLE void
init_guile_sortsmill_math_matrices (void)
{
  scm_c_define_gsubr ("matrix->mpqmat", 1, 0, 0, scm_matrix_to_mpqmat);
  scm_c_define_gsubr ("mpqmat->matrix", 1, 0, 0, scm_mpqmat_to_matrix);
}
