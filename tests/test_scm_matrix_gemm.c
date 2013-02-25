#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/scm_matrix.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int tA = atoi (argv[1]);
  int tB = atoi (argv[2]);
  int m = atoi (argv[3]);
  int n = atoi (argv[4]);
  int k = atoi (argv[5]);

  SCM alpha = scm_c_locale_stringn_to_number (argv[6], strlen (argv[6]), 10);
  SCM beta = scm_c_locale_stringn_to_number (argv[7], strlen (argv[7]), 10);

  CBLAS_TRANSPOSE_t TransA = (tA) ? CblasTrans : CblasNoTrans;
  CBLAS_TRANSPOSE_t TransB = (tB) ? CblasTrans : CblasNoTrans;

  int RowsA = (tA) ? k : m;
  int ColsA = (tA) ? m : k;
  int RowsB = (tB) ? n : k;
  int ColsB = (tB) ? k : n;

  SCM A[RowsA][ColsA];
  SCM B[RowsB][ColsB];
  SCM C[m][n];

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < RowsA; i++)
    for (unsigned int j = 0; j < ColsA; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  for (unsigned int i = 0; i < RowsB; i++)
    for (unsigned int j = 0; j < ColsB; j++)
      {
        B[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        C[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  scm_matrix_gemm (TransA, TransB, m, n, k, alpha, A, B, beta, C);

  for (unsigned int i = 0; i < m; i++)
    {
      for (unsigned int j = 0; j < n; j++)
        scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" ~a"),
                           scm_list_1 (C[i][j]));
      scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" |"), SCM_EOL);
    }

  return 0;
}
