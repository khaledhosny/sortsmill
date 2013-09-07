#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/f64_matrix.h>

#include <main_with_guile.x>

static CBLAS_SIDE_t
side_func (const char *a)
{
  return (a[0] == 'L') ? CblasLeft : CblasRight;
}

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  CBLAS_SIDE_t Side = side_func (argv[1]);

  int m = atoi (argv[2]);
  int n = atoi (argv[3]);

  int k = (Side == CblasLeft) ? m : n;

  double A[m][n];
  double x[k];

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        A[i][j] = atof (argv[i_argv]);
        i_argv++;
      }

  for (unsigned int i = 0; i < k; i++)
    {
      x[i] = atof (argv[i_argv]);
      i_argv++;
    }

  f64_matrix_mul_diagonal (Side, m, n, A, x);

  for (unsigned int i = 0; i < m; i++)
    {
      printf ("| ");
      for (unsigned int j = 0; j < n; j++)
        printf ("%lg ", A[i][j]);
    }
  printf ("|");

  return 0;
}
