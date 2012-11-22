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

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);
  int i1 = atoi (argv[3]);
  int i2 = atoi (argv[4]);

  mpq_t A[rows][cols];

  mpq_matrix_init (rows, cols, A);

  unsigned int i_argv = 5;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  mpq_matrix_swap_rows (rows, cols, A, i1, i2);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Qd", A[i][j]);
      printf (" |");
    }

  mpq_matrix_clear (rows, cols, A);

  return 0;
}
