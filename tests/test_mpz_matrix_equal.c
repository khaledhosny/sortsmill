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

  mpz_t A[rows][cols];
  mpz_t B[rows][cols];

  mpz_matrix_init (rows, cols, A);
  mpz_matrix_init (rows, cols, B);

  unsigned int i_argv = 3;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        mpz_set_str (B[i][j], argv[i_argv], 0);
        i_argv++;
      }

  bool result = mpz_matrix_equal (rows, cols, A, B);

  printf ("%d", result);

  mpz_matrix_clear (rows, cols, A);
  mpz_matrix_clear (rows, cols, B);

  return 0;
}
