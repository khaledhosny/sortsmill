#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/scm_matrix.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);
  SCM x = scm_c_locale_stringn_to_number (argv[3], strlen (argv[3]), 10);

  SCM A[rows][cols];

  scm_matrix_set_all (rows, cols, A, x);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" ~a"),
                           scm_list_1 (A[i][j]));
      scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" |"), SCM_EOL);
    }

  return 0;
}
