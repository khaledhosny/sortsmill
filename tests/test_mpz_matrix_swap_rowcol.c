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

  int rows = atoi (argv[1]);
  int i1 = atoi (argv[2]);
  int i2 = atoi (argv[3]);

  mpz_t A[rows][rows];

  mpz_matrix_init (rows, rows, A);

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < rows; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  mpz_matrix_swap_rowcol (rows, A, i1, i2);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < rows; j++)
        gmp_printf (" %Zd", A[i][j]);
      printf (" |");
    }

  mpz_matrix_clear (rows, rows, A);

  return 0;
}
