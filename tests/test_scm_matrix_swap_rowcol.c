#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/scm_matrix.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int rows = atoi (argv[1]);
  int i1 = atoi (argv[2]);
  int i2 = atoi (argv[3]);

  SCM A[rows][rows];

  unsigned int i_argv = 4;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < rows; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  scm_matrix_swap_rowcol (rows, A, i1, i2);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < rows; j++)
        scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" ~a"),
                           scm_list_1 (A[i][j]));
      scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" |"), SCM_EOL);
    }

  return 0;
}
