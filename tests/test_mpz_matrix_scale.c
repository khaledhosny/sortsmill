#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmillff/xgc.h>
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

  mpz_t x;
  mpz_init (x);

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);

  mpz_set_str (x, argv[3], 0);

  mpz_t A[rows][cols];
  mpz_matrix_init (rows, cols, A);

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  mpz_matrix_scale (rows, cols, A, x);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Zd", A[i][j]);
      printf (" |");
    }

  mpz_clear (x);
  mpz_matrix_clear (rows, cols, A);

  return 0;
}
