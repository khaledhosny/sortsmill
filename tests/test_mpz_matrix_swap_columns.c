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
  int j1 = atoi (argv[3]);
  int j2 = atoi (argv[4]);

  mpz_t A[rows][cols];

  mpz_matrix_init (rows, cols, A);

  unsigned int i_argv = 5;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  mpz_matrix_swap_columns (rows, cols, A, j1, j2);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Zd", A[i][j]);
      printf (" |");
    }

  mpz_matrix_clear (rows, cols, A);

  return 0;
}
