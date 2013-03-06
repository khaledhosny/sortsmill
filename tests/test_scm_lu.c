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
#include <gsl/gsl_linalg.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  int n = atoi (argv[1]);

  SCM A[n][n];
  SCM LU[n][n];
  SCM b[n];
  SCM x[n];
  size_t p[n];
  int signum;
  SCM determinant;

  double A1[n][n];
  double LU1[n][n];
  double b1[n];
  double x1[n];
  gsl_permutation *p1 = gsl_permutation_alloc (n);
  int signum1;
  double determinant1;

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], n, n);
  gsl_matrix_view mLU1 = gsl_matrix_view_array (&LU1[0][0], n, n);
  gsl_vector_view vb1 = gsl_vector_view_array (&b1[0], n);
  gsl_vector_view vx1 = gsl_vector_view_array (&x1[0], n);

  unsigned int i_argv = 2;

  for (unsigned int i = 0; i < n; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        LU[i][j] = A[i][j];
        A1[i][j] = scm_to_double (A[i][j]);
        LU1[i][j] = A1[i][j];
        i_argv++;
      }

  for (unsigned int i = 0; i < n; i++)
    {
      b[i] = scm_c_locale_stringn_to_number (argv[i_argv],
                                             strlen (argv[i_argv]), 10);
      b1[i] = scm_to_double (b[i]);
      i_argv++;
    }

  int exit_status = 0;

  if (exit_status == 0)
    {
      scm_linalg_LU_decomp (n, LU, p, &signum);

      gsl_linalg_LU_decomp (&mLU1.matrix, p1, &signum1);

      // Check that we got the same LU decomposition as GSL.
      for (unsigned int i = 0; i < n; i++)
        for (unsigned int j = 0; j < n; j++)
          if (100 * DBL_EPSILON < fabs (scm_to_double (LU[i][j]) - LU1[i][j]))
            exit_status = 10;
      if (exit_status == 0 && signum != signum1)
        exit_status = 20;
    }

  if (exit_status == 0)
    {
      determinant = scm_linalg_LU_det (n, LU, signum);
      determinant1 = gsl_linalg_LU_det (&mLU1.matrix, signum1);

      // Check that we got the same determinant as GSL.
      if (1e6 * DBL_EPSILON < fabs (scm_to_double (determinant) - determinant1))
        {
          printf ("%lf %lf\n", scm_to_double (determinant), determinant1);
          exit_status = 100;
        }
    }

  if (exit_status == 0 && 100 * DBL_EPSILON < fabs (determinant1))
    {
      if (exit_status == 0)
        {
          int sgndet = scm_linalg_LU_sgndet (n, LU, signum);
          int sgndet1 = gsl_linalg_LU_sgndet (&mLU1.matrix, signum1);

          // Check that we got the same sgndet as GSL.
          if (sgndet != sgndet1)
            exit_status = 110;
        }

      if (exit_status == 0)
        {
          SCM lndet = scm_linalg_LU_lndet (n, LU);
          double lndet1 = gsl_linalg_LU_lndet (&mLU1.matrix);

          // Check that we got the same lndet as GSL.
          if (100 * DBL_EPSILON < fabs (scm_to_double (lndet) - lndet1))
            exit_status = 120;
        }

      if (exit_status == 0)
        {
          memcpy (x, b, n * sizeof (SCM));
          scm_linalg_LU_svx (n, LU, p, x);

          gsl_vector_memcpy (&vx1.vector, &vb1.vector);
          gsl_linalg_LU_svx (&mLU1.matrix, p1, &vx1.vector);

          // Check that we got the same linear system solution as GSL.
          for (unsigned int i = 0; i < n; i++)
            if (100 * DBL_EPSILON < fabs (scm_to_double (x[i]) - x1[i]))
              exit_status = 200;
        }

      if (exit_status == 0)
        {
          memset (x, 0, n * sizeof (SCM));
          scm_linalg_LU_solve (n, LU, p, b, x);

          gsl_vector_set_all (&vx1.vector, 0);
          gsl_linalg_LU_solve (&mLU1.matrix, p1, &vb1.vector, &vx1.vector);

          // Check that we got the same linear system solution as GSL.
          for (unsigned int i = 0; i < n; i++)
            if (100 * DBL_EPSILON < fabs (scm_to_double (x[i]) - x1[i]))
              exit_status = 210;

          if (exit_status == 0)
            // Check that b stayed the same as for GSL.
            for (unsigned int i = 0; i < n; i++)
              if (100 * DBL_EPSILON < fabs (scm_to_double (b[i]) - b1[i]))
                exit_status = 220;
        }

      if (exit_status == 0)
        {
          SCM inverse[n][n];

          double inverse1[n][n];
          gsl_matrix_view minverse1 =
            gsl_matrix_view_array (&inverse1[0][0], n, n);

          scm_linalg_LU_invert (n, LU, p, inverse);
          gsl_linalg_LU_invert (&mLU1.matrix, p1, &minverse1.matrix);

          // Check that we got the same inverse as GSL.
          for (unsigned int i = 0; i < n; i++)
            for (unsigned int j = 0; j < n; j++)
              if (100 * DBL_EPSILON <
                  fabs (scm_to_double (inverse[i][j]) - inverse1[i][j]))
                exit_status = 230;
        }

      if (exit_status == 0)
        {
          // Test that the residual refinement routine gives the same
          // results as GSL, if given heavily perturbed arguments.

          SCM c[n];
          SCM residual[n];

          double c1[n];
          double residual1[n];

          gsl_vector_view vc1 = gsl_vector_view_array (&c1[0], n);
          gsl_vector_view vresidual1 = gsl_vector_view_array (&residual1[0], n);

          for (unsigned int i = 0; i < n; i++)
            {
              c1[i] = x1[i] + 0.1;
              c[i] = scm_from_double (c1[i]);
            }

          scm_linalg_LU_refine (n, A, LU, p, b, c, residual);
          gsl_linalg_LU_refine (&mA1.matrix, &mLU1.matrix, p1, &vb1.vector,
                                &vc1.vector, &vresidual1.vector);

          for (unsigned int i = 0; i < n; i++)
            if (100 * DBL_EPSILON < fabs (scm_to_double (c[i]) - c1[i]))
              exit_status = 300;

          if (exit_status == 0)
            for (unsigned int i = 0; i < n; i++)
              if (100 * DBL_EPSILON <
                  fabs (scm_to_double (residual[i]) - residual1[i]))
                exit_status = 310;
        }
    }

  gsl_permutation_free (p1);

  return exit_status;
}
