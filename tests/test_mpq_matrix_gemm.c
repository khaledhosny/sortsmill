#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <linalg.h>

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

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < RowsA; i++)
    for (unsigned int j = 0; j < ColsA; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < RowsB; i++)
    for (unsigned int j = 0; j < ColsB; j++)
      {
        mpq_set_str (B[i][j], argv[i_argv], 0);
        mpq_canonicalize (B[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpq_set_str (C[i][j], argv[i_argv], 0);
        mpq_canonicalize (C[i][j]);
        i_argv++;
      }

  mpq_matrix_gemm (TransA, TransB, m, n, k, alpha, A, B, beta, C);

  for (unsigned int i = 0; i < m; i++)
    {
      for (unsigned int j = 0; j < n; j++)
        gmp_printf (" %Qd", C[i][j]);
      printf (" |");
    }

  mpq_matrix_clear (RowsA, ColsA, A);
  mpq_matrix_clear (RowsB, ColsB, B);
  mpq_matrix_clear (m, n, C);

  mpq_clears (alpha, beta, NULL);

  return 0;
}
