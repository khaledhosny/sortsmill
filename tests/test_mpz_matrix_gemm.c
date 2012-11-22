#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmillff/linalg.h>

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

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < RowsA; i++)
    for (unsigned int j = 0; j < ColsA; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  for (unsigned int i = 0; i < RowsB; i++)
    for (unsigned int j = 0; j < ColsB; j++)
      {
        mpz_set_str (B[i][j], argv[i_argv], 0);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpz_set_str (C[i][j], argv[i_argv], 0);
        i_argv++;
      }

  mpz_matrix_gemm (TransA, TransB, m, n, k, alpha, A, B, beta, C);

  for (unsigned int i = 0; i < m; i++)
    {
      for (unsigned int j = 0; j < n; j++)
        gmp_printf (" %Zd", C[i][j]);
      printf (" |");
    }

  mpz_matrix_clear (RowsA, ColsA, A);
  mpz_matrix_clear (RowsB, ColsB, B);
  mpz_matrix_clear (m, n, C);

  mpz_clears (alpha, beta, NULL);

  return 0;
}
