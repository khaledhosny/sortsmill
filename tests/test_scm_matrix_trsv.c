#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/scm_matrix.h>
#include <gsl/gsl_blas.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  const int uplo = atoi (argv[1]);
  const int trans = atoi (argv[2]);
  const int diag = atoi (argv[3]);

  const CBLAS_UPLO_t UL[] = { CblasLower, CblasUpper };
  const CBLAS_TRANSPOSE_t Tr[] = { CblasNoTrans, CblasTrans, CblasConjTrans };
  const CBLAS_DIAG_t Dg[] = { CblasNonUnit, CblasUnit };

  const int rows = atoi (argv[4]);
  const int cols = rows;

  SCM A[rows][cols];
  double A1[rows][cols];

  SCM b[rows];
  double b1[rows];

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], rows, cols);
  gsl_vector_view vb1 = gsl_vector_view_array (b1, rows);

  unsigned int i_argv = 5;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        A1[i][j] = scm_to_double (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < rows; i++)
    {
      b[i] = scm_c_locale_stringn_to_number (argv[i_argv],
                                             strlen (argv[i_argv]), 10);
      b1[i] = scm_to_double (b[i]);
      i_argv++;
    }

  scm_matrix_trsv (UL[uplo], Tr[trans], Dg[diag], rows, A, b);
  gsl_blas_dtrsv (UL[uplo], Tr[trans], Dg[diag], &mA1.matrix, &vb1.vector);

  int exit_status = 0;

  // Check that our routine gives the same result as does GSL.
  for (unsigned int i = 0; i < rows; i++)
    if (100 * DBL_EPSILON < fabs (scm_to_double (b[i]) - b1[i]))
      exit_status = 1;

  if (exit_status != 0)
    {
      for (unsigned int i = 0; i < rows; i++)
        printf ("%10lf ", b1[i]);
      printf ("\n");
      for (unsigned int i = 0; i < rows; i++)
        printf ("%10lf ", scm_to_double (b[i]));
      printf ("\n");
    }

  return exit_status;
}
