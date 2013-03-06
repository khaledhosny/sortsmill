#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/gmp_matrix.h>

#include <main_with_guile.x>

static CBLAS_SIDE_t
side_func (const char *a)
{
  return (a[0] == 'L') ? CblasLeft : CblasRight;
}

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  scm_dynwind_begin (0);

  CBLAS_SIDE_t Side = side_func (argv[1]);

  int m = atoi (argv[2]);
  int n = atoi (argv[3]);

  int k = (Side == CblasLeft) ? m : n;

  mpq_t A[m][n];
  mpq_matrix_init (m, n, A);
  scm_dynwind_mpq_matrix_clear (m, n, A);

  mpq_t x[k];
  mpq_vector_init (k, x);
  scm_dynwind_mpq_vector_clear (k, x);

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < k; i++)
    {
      mpq_set_str (x[i], argv[i_argv], 0);
      mpq_canonicalize (x[i]);
      i_argv++;
    }

  mpq_matrix_mul_diagonal (Side, m, n, A, x);

  for (unsigned int i = 0; i < m; i++)
    {
      gmp_printf ("| ");
      for (unsigned int j = 0; j < n; j++)
        gmp_printf ("%Qd ", A[i][j]);
    }
  gmp_printf ("|");

  scm_dynwind_end ();

  return 0;
}
