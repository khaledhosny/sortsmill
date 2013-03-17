#include <assert.h>
#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/scm_matrix.h>

#include <main_with_guile.x>

typedef bool elementwise_predicate (size_t m, size_t n, SCM A[m][n]);

static elementwise_predicate *
predicate (const char *predicate_name)
{
  elementwise_predicate *pred = NULL;
  if (strcmp (predicate_name, "null") == 0)
    pred = scm_matrix_isnull;
  else if (strcmp (predicate_name, "pos") == 0)
    pred = scm_matrix_ispos;
  else if (strcmp (predicate_name, "neg") == 0)
    pred = scm_matrix_isneg;
  else if (strcmp (predicate_name, "nonneg") == 0)
    pred = scm_matrix_isnonneg;
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

  SCM A[rows][cols];

  size_t i_argv = 4;

  for (size_t i = 0; i < rows; i++)
    for (size_t j = 0; j < cols; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  bool result = pred (rows, cols, A);

  printf ("%d", result);

  return 0;
}
