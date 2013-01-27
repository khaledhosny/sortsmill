#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/linalg.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int rows = atoi (argv[1]);
  int i1 = atoi (argv[2]);
  int i2 = atoi (argv[3]);

  mpq_t A[rows][rows];

  mpq_matrix_init (rows, rows, A);

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < rows; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  mpq_matrix_swap_rowcol (rows, A, i1, i2);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < rows; j++)
        gmp_printf (" %Qd", A[i][j]);
      printf (" |");
    }

  mpq_matrix_clear (rows, rows, A);

  return 0;
}
