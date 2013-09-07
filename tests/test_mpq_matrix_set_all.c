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

  mpq_t x;
  mpq_init (x);

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);

  mpq_set_str (x, argv[3], 0);
  mpq_canonicalize (x);

  mpq_t A[rows][cols];
  mpq_matrix_init (rows, cols, A);

  mpq_matrix_set_all (rows, cols, A, x);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        gmp_printf (" %Qd", A[i][j]);
      printf (" |");
    }

  mpq_clear (x);
  mpq_matrix_clear (rows, cols, A);

  return 0;
}
