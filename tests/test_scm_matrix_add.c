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

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int rows = atoi (argv[1]);
  int cols = atoi (argv[2]);

  SCM A[rows][cols];
  SCM B[rows][cols];

  unsigned int i_argv = 3;

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  for (unsigned int i = 0; i < rows; i++)
    for (unsigned int j = 0; j < cols; j++)
      {
        B[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        i_argv++;
      }

  scm_matrix_add (rows, cols, A, B);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" ~a"),
                           scm_list_1 (A[i][j]));
      scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" |"), SCM_EOL);
    }

  scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" *** |"), SCM_EOL);

  for (unsigned int i = 0; i < rows; i++)
    {
      for (unsigned int j = 0; j < cols; j++)
        scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" ~a"),
                           scm_list_1 (B[i][j]));
      scm_simple_format (SCM_BOOL_T, scm_from_latin1_string (" |"), SCM_EOL);
    }

  return 0;
}
