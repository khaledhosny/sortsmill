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
#include <gsl/gsl_blas.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  scm_dynwind_begin (0);

  setlocale (LC_ALL, "");

  const int uplo = atoi (argv[1]);
  const int trans = atoi (argv[2]);
  const int diag = atoi (argv[3]);

  const CBLAS_UPLO_t UL[] = { CblasLower, CblasUpper };
  const CBLAS_TRANSPOSE_t Tr[] = { CblasNoTrans, CblasTrans, CblasConjTrans };
  const CBLAS_DIAG_t Dg[] = { CblasNonUnit, CblasUnit };

  const int rows = atoi (argv[4]);
  const int cols = rows;

  mpq_t A[rows][cols];
  mpq_matrix_init (rows, cols, A);
  scm_dynwind_mpq_matrix_clear (rows, cols, A);
  double A1[rows][cols];

  mpq_t b[rows];
  mpq_vector_init (rows, b);
  scm_dynwind_mpq_vector_clear (rows, b);
  double b1[rows];

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], rows, cols);
  gsl_vector_view vb1 = gsl_vector_view_array (b1, rows);

  unsigned int i_argv = 5;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        A1[i][j] = mpq_get_d (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < rows; i++)
    {
      mpq_set_str (b[i], argv[i_argv], 0);
      mpq_canonicalize (b[i]);
      b1[i] = mpq_get_d (b[i]);
      i_argv++;
    }

  int exit_status = 0;

  bool singular;
  mpq_matrix_trsv (UL[uplo], Tr[trans], Dg[diag], rows, A, b, &singular);
  if (singular)
    printf ("singular");
  else
    {
      gsl_blas_dtrsv (UL[uplo], Tr[trans], Dg[diag], &mA1.matrix, &vb1.vector);

      // Check that our routine gives the same result as does GSL.
      for (unsigned int i = 0; i < rows; i++)
        if (100 * DBL_EPSILON < fabs (mpq_get_d (b[i]) - b1[i]))
          exit_status = 1;

      if (exit_status != 0)
        {
          for (unsigned int i = 0; i < rows; i++)
            printf ("%10lf ", b1[i]);
          printf ("\n");
          for (unsigned int i = 0; i < rows; i++)
            printf ("%10lf ", mpq_get_d (b[i]));
          printf ("\n");
        }
    }

  scm_dynwind_end ();

  return exit_status;
}
