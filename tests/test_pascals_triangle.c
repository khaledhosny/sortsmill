#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <pascals_triangle.h>

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  int bc[20][20];
  int bca[20][20];

  // Compute Pascal’s triangle (binomial coefficients) row-by-row, to
  // test against.
  bc[0][0] = 1;
  for (unsigned int i = 1; i < 20; i++)
    {
      bc[i][0] = 1;
      bc[i][i] = 1;
      for (unsigned int j = 1; j < i; j++)
        bc[i][j] = bc[i - 1][j - 1] + bc[i - 1][j];
    }

  int exit_status = 0;

  for (unsigned int i = 0; i < 20; i++)
    {
      const int *row = pascals_triangle_row (i);
      bool bad = false;
      for (unsigned int j = 0; j <= i; j++)
        bad = (row[j] != bc[i][j]);
      if (bad)
        {
          printf ("bad row %u\n", i);
          exit_status = 1;
        }
    }

  // Compute Pascal’s triangle with alternating signs, row-by-row, to
  // test against.
  bca[0][0] = 1;
  for (unsigned int i = 1; i < 20; i++)
    {
      bca[i][0] = 1;
      bca[i][i] = 1;
      for (unsigned int j = 1; j < i; j++)
        bca[i][j] = bca[i - 1][j - 1] + bca[i - 1][j];
    }
  for (unsigned int i = 1; i < 20; i += 2)
    for (unsigned int j = 0; j < 20; j++)
      bca[i][j] *= -1;

  for (unsigned int i = 0; i < 20; i++)
    {
      const int *row = pascals_triangle_row_altsigns (i);
      bool bad = false;
      for (unsigned int j = 0; j <= i; j++)
        bad = (row[j] != bca[i][j]);
      if (bad)
        {
          printf ("altsigns bad row %u\n", i);
          exit_status = 1;
        }
    }

  return exit_status;
}
