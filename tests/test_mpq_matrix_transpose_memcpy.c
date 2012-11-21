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

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);

  mpq_t A[rows][cols];
  mpq_t B[cols][rows];

  mpq_matrix_init (rows, cols, A);
  mpq_matrix_init (cols, rows, B);

  unsigned int i_argv = 3;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        i_argv++;
      }

  mpq_matrix_transpose_memcpy (rows, cols, A, B);

  for (unsigned int i = 0; i < cols; i++)
    {
      for (unsigned int j = 0; j < rows; j++)
        gmp_printf (" %Qd", B[i][j]);
      printf (" |");
    }

  mpq_matrix_clear (rows, cols, A);
  mpq_matrix_clear (cols, rows, B);

  return 0;
}
