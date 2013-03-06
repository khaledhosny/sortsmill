#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/scm_matrix.h>
#include <sortsmill/guile.h>

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

  SCM A[m][n];
  SCM x[k];

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  for (unsigned int i = 0; i < k; i++)
    {
      x[i] = scm_c_locale_stringn_to_number (argv[i_argv],
                                             strlen (argv[i_argv]), 10);
      i_argv++;
    }

  scm_matrix_mul_diagonal (Side, m, n, A, x);

  for (unsigned int i = 0; i < m; i++)
    {
      scm_c_utf8_format (SCM_BOOL_T, "| ", SCM_EOL);
      for (unsigned int j = 0; j < n; j++)
        scm_c_utf8_format (SCM_BOOL_T, "~a ", scm_list_1 (A[i][j]));
    }
  scm_c_utf8_format (SCM_BOOL_T, "|", SCM_EOL);

  return 0;
}
