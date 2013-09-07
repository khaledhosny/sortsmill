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

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);

  mpq_t A[rows][cols];
  mpq_t B[rows][cols];

  mpq_matrix_init (rows, cols, A);
  mpq_matrix_init (rows, cols, B);

  unsigned int i_argv = 3;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpq_set_str (B[i][j], argv[i_argv], 0);
        mpq_canonicalize (B[i][j]);
        i_argv++;
      }

  mpq_matrix_swap (rows, cols, A, B);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Qd", A[i][j]);
      printf (" |");
    }

  printf (" <-> |");

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Qd", B[i][j]);
      printf (" |");
    }

  mpq_matrix_clear (rows, cols, A);
  mpq_matrix_clear (rows, cols, B);

  return 0;
}
