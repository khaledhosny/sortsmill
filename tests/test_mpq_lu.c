#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/guile.h>
#include <sortsmill/math/gmp_matrix.h>
#include <sortsmill/math/gmp_constants.h>
#include <gsl/gsl_linalg.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  scm_dynwind_begin (0);

  int n = atoi (argv[1]);

  mpq_t A[n][n];
  mpq_t LU[n][n];
  mpq_t inverse[n][n];
  mpq_t b[n];
  mpq_t x[n];
  size_t p[n];
  int signum;
  mpq_t determinant;
  bool singular;

  mpq_t LU2[n][n];
  mpq_t inverse2[n][n];
  mpq_t x2[n];
  size_t p2[n];
  int signum2;
  mpq_t determinant2;
  bool singular2;

  double A1[n][n];
  double LU1[n][n];
  double inverse1[n][n];
  double b1[n];
  double x1[n];
  gsl_permutation *p1;
  int signum1;
  double determinant1;

  mpq_matrix_init (n, n, A);
  scm_dynwind_mpq_matrix_clear (n, n, A);

  mpq_matrix_init (n, n, LU);
  scm_dynwind_mpq_matrix_clear (n, n, LU);

  mpq_matrix_init (n, n, inverse);
  scm_dynwind_mpq_matrix_clear (n, n, inverse);

  mpq_vector_init (n, b);
  scm_dynwind_mpq_vector_clear (n, b);

  mpq_vector_init (n, x);
  scm_dynwind_mpq_vector_clear (n, x);

  mpq_init (determinant);
  scm_dynwind_mpq_clear (determinant);

  mpq_matrix_init (n, n, LU2);
  scm_dynwind_mpq_matrix_clear (n, n, LU2);

  mpq_matrix_init (n, n, inverse2);
  scm_dynwind_mpq_matrix_clear (n, n, inverse2);

  mpq_vector_init (n, x2);
  scm_dynwind_mpq_vector_clear (n, x2);

  mpq_init (determinant2);
  scm_dynwind_mpq_clear (determinant2);

  p1 = gsl_permutation_alloc (n);
  scm_dynwind_gsl_permutation_free (p1);

  gsl_matrix_view mLU1 = gsl_matrix_view_array (&LU1[0][0], n, n);
  gsl_vector_view vb1 = gsl_vector_view_array (&b1[0], n);
  gsl_vector_view vx1 = gsl_vector_view_array (&x1[0], n);

  unsigned int i_argv = 2;

  for (unsigned int i = 0; i < n; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        mpq_set (LU[i][j], A[i][j]);
        mpq_set (LU2[i][j], A[i][j]);
        A1[i][j] = mpq_get_d (A[i][j]);
        LU1[i][j] = A1[i][j];
        i_argv++;
      }

  for (unsigned int i = 0; i < n; i++)
    {
      mpq_set_str (b[i], argv[i_argv], 0);
      mpq_canonicalize (b[i]);
      b1[i] = mpq_get_d (b[i]);
      i_argv++;
    }

  int exit_status = 0;

  if (exit_status == 0)
    {
      mpq_linalg_LU_decomp (n, LU, p, &signum);
      mpq_linalg_LU_decomp_fast_pivot (n, LU2, p2, &signum2);
      gsl_linalg_LU_decomp (&mLU1.matrix, p1, &signum1);

      // Check that with ordinary pivoting we got the same LU
      // decomposition as GSL.
      for (unsigned int i = 0; i < n; i++)
        for (unsigned int j = 0; j < n; j++)
          if (100 * DBL_EPSILON < fabs (mpq_get_d (LU[i][j]) - LU1[i][j]))
            exit_status = 10;
      if (exit_status == 0 && signum != signum1)
        exit_status = 20;
    }

  if (exit_status == 0)
    {
      mpq_linalg_LU_det (n, LU, signum, determinant);
      mpq_linalg_LU_det (n, LU2, signum2, determinant2);
      determinant1 = gsl_linalg_LU_det (&mLU1.matrix, signum1);

      // Check that we got the same determinant as GSL.
      if (1e6 * DBL_EPSILON < fabs (mpq_get_d (determinant) - determinant1))
        {
          printf ("%lf %lf\n", mpq_get_d (determinant), determinant1);
          exit_status = 100;
        }

      if (exit_status == 0)
        // Check that fast pivoting gives the same determinant.
        if (!mpq_equal (determinant, determinant2))
          {
            gmp_printf ("%Qd %Qd\n", determinant, determinant2);
            exit_status = 107;
          }
    }

  if (exit_status == 0 && 100 * DBL_EPSILON < fabs (determinant1))
    {
      if (exit_status == 0)
        {
          int sgndet = mpq_linalg_LU_sgndet (n, LU, signum);
          int sgndet2 = mpq_linalg_LU_sgndet (n, LU2, signum2);
          int sgndet1 = gsl_linalg_LU_sgndet (&mLU1.matrix, signum1);

          // Check that we got the same sgndet as GSL.
          if (sgndet != sgndet1)
            exit_status = 110;

          if (exit_status == 0)
            // Check that fast pivoting gives the same sgndet.
            if (sgndet != sgndet2)
              exit_status = 117;
        }

      if (exit_status == 0)
        {
          mpq_matrix_memcpy (n, 1, (mpq_t (*)[1]) x, (mpq_t (*)[1]) b);
          mpq_linalg_LU_svx (n, LU, p, x, &singular);
          if (singular)
            exit_status = 205;

          if (exit_status == 0)
            {
              mpq_matrix_memcpy (n, 1, (mpq_t (*)[1]) x2, (mpq_t (*)[1]) b);
              mpq_linalg_LU_svx (n, LU2, p2, x2, &singular2);
              if (singular2)
                exit_status = 206;
            }

          if (exit_status == 0)
            {
              gsl_vector_memcpy (&vx1.vector, &vb1.vector);
              gsl_linalg_LU_svx (&mLU1.matrix, p1, &vx1.vector);

              // Check that we got the same linear system solution as GSL.
              for (unsigned int i = 0; i < n; i++)
                if (100 * DBL_EPSILON < fabs (mpq_get_d (x[i]) - x1[i]))
                  exit_status = 200;

              if (exit_status == 0)
                // Check that fast pivoting gives the same solution.
                for (unsigned int i = 0; i < n; i++)
                  if (!mpq_equal (x[i], x2[i]))
                    exit_status = 207;
            }
        }

      if (exit_status == 0)
        {
          mpq_matrix_set_all (n, 1, (mpq_t (*)[1]) x, mpq_zero ());
          mpq_linalg_LU_solve (n, LU, p, b, x, &singular);
          if (singular)
            exit_status = 215;

          if (exit_status == 0)
            {
              gsl_vector_set_all (&vx1.vector, 0);
              gsl_linalg_LU_solve (&mLU1.matrix, p1, &vb1.vector, &vx1.vector);

              // Check that we got the same linear system solution as GSL.
              for (unsigned int i = 0; i < n; i++)
                if (100 * DBL_EPSILON < fabs (mpq_get_d (x[i]) - x1[i]))
                  exit_status = 210;

              if (exit_status == 0)
                // Check that b stayed the same as for GSL.
                for (unsigned int i = 0; i < n; i++)
                  if (100 * DBL_EPSILON < fabs (mpq_get_d (b[i]) - b1[i]))
                    exit_status = 220;

              if (exit_status == 0)
                {
                  // Redo the computation using the fast-pivot LU
                  // decomposition.
                  mpq_matrix_set_all (n, 1, (mpq_t (*)[1]) x2, mpq_zero ());
                  mpq_linalg_LU_solve (n, LU2, p2, b, x2, &singular2);
                  if (singular2)
                    exit_status = 216;
                }

              if (exit_status == 0)
                // Check that we got the same solution with fast pivoting.
                for (unsigned int i = 0; i < n; i++)
                  if (!mpq_equal (x[i], x2[i]))
                    exit_status = 217;

              if (exit_status == 0)
                // Check that b again stayed the same as for GSL.
                for (unsigned int i = 0; i < n; i++)
                  if (100 * DBL_EPSILON < fabs (mpq_get_d (b[i]) - b1[i]))
                    exit_status = 221;
            }
        }

      if (exit_status == 0)
        {
          gsl_matrix_view minverse1 =
            gsl_matrix_view_array (&inverse1[0][0], n, n);

          mpq_linalg_LU_invert (n, LU, p, inverse, &singular);
          if (singular)
            exit_status = 235;

          if (exit_status == 0)
            {
              mpq_linalg_LU_invert (n, LU2, p2, inverse2, &singular2);
              if (singular2)
                exit_status = 236;
            }

          if (exit_status == 0)
            {
              gsl_linalg_LU_invert (&mLU1.matrix, p1, &minverse1.matrix);

              // Check that we got the same inverse as GSL.
              for (unsigned int i = 0; i < n; i++)
                for (unsigned int j = 0; j < n; j++)
                  if (100 * DBL_EPSILON <
                      fabs (mpq_get_d (inverse[i][j]) - inverse1[i][j]))
                    exit_status = 230;
            }

          if (exit_status == 0)
            // Check that fast pivoting gives the same inverse.
            for (unsigned int i = 0; i < n; i++)
              for (unsigned int j = 0; j < n; j++)
                if (!mpq_equal (inverse[i][j], inverse2[i][j]))
                  exit_status = 237;
        }
    }

  scm_dynwind_end ();

  return exit_status;
}
