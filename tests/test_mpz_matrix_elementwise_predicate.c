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

typedef bool elementwise_predicate (size_t m, size_t n, mpz_t A[m][n]);

static elementwise_predicate *
predicate (const char *predicate_name)
{
  elementwise_predicate *pred = NULL;
  if (strcmp (predicate_name, "null") == 0)
    pred = mpz_matrix_isnull;
  else if (strcmp (predicate_name, "pos") == 0)
    pred = mpz_matrix_ispos;
  else if (strcmp (predicate_name, "neg") == 0)
    pred = mpz_matrix_isneg;
  else if (strcmp (predicate_name, "nonneg") == 0)
    pred = mpz_matrix_isnonneg;
  else
    assert (false);
  return pred;
}

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  elementwise_predicate *pred = predicate (argv[1]);
  int rows = atoi (argv[2]);
  int cols = atoi (argv[3]);

  mpz_t A[rows][cols];

  mpz_matrix_init (rows, cols, A);

  size_t i_argv = 4;

  for (size_t i = 0; i < rows; i++)
    for (size_t j = 0; j < cols; j++)
      {
        mpz_set_str (A[i][j], argv[i_argv], 0);
        i_argv++;
      }

  bool result = pred (rows, cols, A);

  printf ("%d", result);

  mpz_matrix_clear (rows, cols, A);

  return 0;
}
