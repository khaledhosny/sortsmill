#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/gmp_matrix.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  mpq_t alpha;
  mpq_t beta;

  mpq_inits (alpha, beta, NULL);

  int tA = atoi (argv[1]);
  int tB = atoi (argv[2]);
  int m = atoi (argv[3]);
  int n = atoi (argv[4]);
  int k = atoi (argv[5]);

  mpq_set_str (alpha, argv[6], 0);
  mpq_canonicalize (alpha);

  mpq_set_str (beta, argv[7], 0);
  mpq_canonicalize (beta);

  CBLAS_TRANSPOSE_t TransA = (tA) ? CblasTrans : CblasNoTrans;
  CBLAS_TRANSPOSE_t TransB = (tB) ? CblasTrans : CblasNoTrans;

  int RowsA = (tA) ? k : m;
  int ColsA = (tA) ? m : k;
  int RowsB = (tB) ? n : k;
  int ColsB = (tB) ? k : n;

  mpq_t A[RowsA][ColsA];
  mpq_t B[RowsB][ColsB];
  mpq_t C[m][n];

  mpq_matrix_init (RowsA, ColsA, A);
  mpq_matrix_init (RowsB, ColsB, B);
  mpq_matrix_init (m, n, C);

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
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        A1[i][j] = mpq_get_d (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < RowsB; i++)
    for (unsigned int j = 0; j < ColsB; j++)
      {
        mpq_set_str (B[i][j], argv[i_argv], 0);
        mpq_canonicalize (B[i][j]);
        B1[i][j] = mpq_get_d (B[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpq_set_str (C[i][j], argv[i_argv], 0);
        mpq_canonicalize (C[i][j]);
        C1[i][j] = mpq_get_d (C[i][j]);
        i_argv++;
      }

  mpq_matrix_gemm (TransA, TransB, m, n, k, alpha, A, B, beta, C);
  gsl_blas_dgemm (TransA, TransB, mpq_get_d (alpha),
                  &mA1.matrix, &mB1.matrix, mpq_get_d (beta), &mC1.matrix);

  for (unsigned int i = 0; i < m; i++)
    {
      for (unsigned int j = 0; j < n; j++)
        gmp_printf (" %Qd", C[i][j]);
      printf (" |");
    }

  int exit_status = 0;

  // Check that we get the same results as gsl_blas_dgemm.
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      if (100 * DBL_EPSILON < fabs (mpq_get_d (C[i][j]) - C1[i][j]))
        exit_status = 1;

  mpq_matrix_clear (RowsA, ColsA, A);
  mpq_matrix_clear (RowsB, ColsB, B);
  mpq_matrix_clear (m, n, C);

  mpq_clears (alpha, beta, NULL);

  return exit_status;
}
