#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/gmp_matrix.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  mpz_t alpha;
  mpz_t beta;

  mpz_inits (alpha, beta, NULL);

  int tA = atoi (argv[1]);
  int tB = atoi (argv[2]);
  int m = atoi (argv[3]);
  int n = atoi (argv[4]);
  int k = atoi (argv[5]);

  mpz_set_str (alpha, argv[6], 0);
  mpz_set_str (beta, argv[7], 0);

  CBLAS_TRANSPOSE_t TransA = (tA) ? CblasTrans : CblasNoTrans;
  CBLAS_TRANSPOSE_t TransB = (tB) ? CblasTrans : CblasNoTrans;

  int RowsA = (tA) ? k : m;
  int ColsA = (tA) ? m : k;
  int RowsB = (tB) ? n : k;
  int ColsB = (tB) ? k : n;

  mpz_t A[RowsA][ColsA];
  mpz_t B[RowsB][ColsB];
  mpz_t C[m][n];

  mpz_matrix_init (RowsA, ColsA, A);
  mpz_matrix_init (RowsB, ColsB, B);
  mpz_matrix_init (m, n, C);

  double A1[RowsA][ColsA];
  double B1[RowsB][ColsB];
  double C1[m][n];

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], RowsA, ColsA);
  gsl_matrix_view mB1 = gsl_matrix_view_array (&B1[0][0], RowsB, ColsB);
  gsl_matrix_view mC1 = gsl_matrix_view_array (&C1[0][0], m, n);

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < RowsA; i++)
    for (unsigned int j = 0; j < ColsA; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        A1[i][j] = mpz_get_d (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < RowsB; i++)
    for (unsigned int j = 0; j < ColsB; j++)
      {
        mpz_set_str (B[i][j], argv[i_argv], 0);
        B1[i][j] = mpz_get_d (B[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpz_set_str (C[i][j], argv[i_argv], 0);
        C1[i][j] = mpz_get_d (C[i][j]);
        i_argv++;
      }

  mpz_matrix_gemm (TransA, TransB, m, n, k, alpha, A, B, beta, C);
  gsl_blas_dgemm (TransA, TransB, mpz_get_d (alpha),
                  &mA1.matrix, &mB1.matrix, mpz_get_d (beta), &mC1.matrix);

  for (unsigned int i = 0; i < m; i++)
    {
      for (unsigned int j = 0; j < n; j++)
        gmp_printf (" %Zd", C[i][j]);
      printf (" |");
    }

  int exit_status = 0;

  // Check that we get the same results as gsl_blas_dgemm.
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      if (100 * DBL_EPSILON < fabs (mpz_get_d (C[i][j]) - C1[i][j]))
        exit_status = 1;

  mpz_matrix_clear (RowsA, ColsA, A);
  mpz_matrix_clear (RowsB, ColsB, B);
  mpz_matrix_clear (m, n, C);

  mpz_clears (alpha, beta, NULL);

  return exit_status;
}
