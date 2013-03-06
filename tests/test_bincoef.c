// -*- coding: utf-8 -*-

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/gmp_matrix.h>
#include <sortsmill/bincoef.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int exit_status = 0;

  const size_t m = 100;

  mpz_t P[m][m];
  mpq_t Q[m][m];

  mpz_matrix_init (m, m, P);
  mpq_matrix_init (m, m, Q);

  // Compute Pascal’s triangle.
  mpz_set_ui (P[0][0], 1);
  for (size_t i = 1; i < m; i++)
    {
      mpz_set_ui (P[i][0], 1);
      mpz_set_ui (P[i][i], 1);
      for (size_t j = 1; j < m; j++)
        mpz_add (P[i][j], P[i - 1][j - 1], P[i - 1][j]);
    }

  // Compare the results of bincoef() against Pascal’s triangle.
  for (size_t n = 0; n <= 34; n++)
    for (size_t k = 0; k <= 34; k++)
      if (bincoef (n, k) != mpz_get_ui (P[n][k]))
        exit_status = 10;

  if (exit_status == 0)
    // Check for n < k.
    for (size_t n = 0; n <= 34; n++)
      if (bincoef (n, n + 1) != 0)
        exit_status = 20;

  if (exit_status == 0)
    {
      mpz_t C;
      mpz_init (C);

      // Compare the results of mpz_bincoef_ui() against Pascal’s
      // triangle.
      for (size_t n = 0; n <= m - 1; n++)
        for (size_t k = 0; k <= m - 1; k++)
          {
            mpz_bincoef_ui (C, n, k);
            if (mpz_cmp (C, P[n][k]) != 0)
              exit_status = 110;
          }

      mpz_clear (C);
    }

  if (exit_status == 0)
    {
      mpz_t C;
      mpz_init (C);

      // Check for n < k.
      for (size_t n = 0; n <= m - 1; n++)
        {
          mpz_bincoef_ui (C, n, n + 1);
          if (mpz_sgn (C) != 0)
            exit_status = 120;
        }

      mpz_clear (C);
    }

  if (exit_status == 0)
    {
      mpq_t C;
      mpq_init (C);

      // Compare the results of mpq_bincoef_ui() against Pascal’s
      // triangle.
      for (size_t n = 0; n <= m - 1; n++)
        for (size_t k = 0; k <= m - 1; k++)
          {
            mpq_bincoef_ui (C, n, k);
            if (mpz_cmp (mpq_numref (C), P[n][k]) != 0
                || mpz_cmp_ui (mpq_denref (C), 1) != 0)
              exit_status = 210;
          }

      mpq_clear (C);
    }

  if (exit_status == 0)
    {
      mpq_t C;
      mpq_init (C);

      // Check for n < k.
      for (size_t n = 0; n <= m - 1; n++)
        {
          mpq_bincoef_ui (C, n, n + 1);
          if (mpq_sgn (C) != 0)
            exit_status = 220;
        }

      mpq_clear (C);
    }

  mpz_matrix_clear (m, m, P);
  mpq_matrix_clear (m, m, Q);

  return exit_status;
}
