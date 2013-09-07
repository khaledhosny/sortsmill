#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/gmp_matrix.h>
#include <sortsmill/guile.h>

#include <main_with_guile.x>

static CBLAS_SIDE_t
side_func (const char *a)
{
  return (a[0] == 'L') ? CblasLeft : CblasRight;
}

static CBLAS_UPLO_t
uplo_func (const char *a)
{
  return (a[0] == 'U') ? CblasUpper : CblasLower;
}

static CBLAS_TRANSPOSE_t
trans_func (const char *a)
{
  return (a[0] == 'N') ? CblasNoTrans : CblasTrans;
}

static CBLAS_DIAG_t
diag_func (const char *a)
{
  return (a[0] == 'N') ? CblasNonUnit : CblasUnit;
}

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  scm_dynwind_begin (0);

  CBLAS_SIDE_t Side = side_func (argv[1]);
  CBLAS_UPLO_t Uplo = uplo_func (argv[2]);
  CBLAS_TRANSPOSE_t TransA = trans_func (argv[3]);
  CBLAS_DIAG_t Diag = diag_func (argv[4]);

  int m = atoi (argv[5]);
  int n = atoi (argv[6]);

  mpq_t alpha;
  mpq_init (alpha);
  scm_dynwind_mpq_clear (alpha);
  mpq_set_str (alpha, argv[7], 0);
  mpq_canonicalize (alpha);

  int k = (Side == CblasLeft) ? m : n;

  mpq_t A[k][k];
  mpq_matrix_init (k, k, A);
  scm_dynwind_mpq_matrix_clear (k, k, A);

  mpq_t B[m][n];
  mpq_matrix_init (m, n, B);
  scm_dynwind_mpq_matrix_clear (m, n, B);

  double A1[k][k];
  double B1[m][n];

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], k, k);
  gsl_matrix_view mB1 = gsl_matrix_view_array (&B1[0][0], m, n);

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < k; i++)
    for (unsigned int j = 0; j < k; j++)
      {
        mpq_set_str (A[i][j], argv[i_argv], 0);
        mpq_canonicalize (A[i][j]);
        A1[i][j] = mpq_get_d (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        mpq_set_str (B[i][j], argv[i_argv], 0);
        mpq_canonicalize (B[i][j]);
        B1[i][j] = mpq_get_d (B[i][j]);
        i_argv++;
      }

  mpq_matrix_trmm (Side, Uplo, TransA, Diag, m, n, alpha, A, B);
  gsl_blas_dtrmm (Side, Uplo, TransA, Diag, mpq_get_d (alpha), &mA1.matrix,
                  &mB1.matrix);

  int exit_status = 0;

  // Check that we get the same results as gsl_blas_dtrmm.
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        gmp_printf ("B[%u][%u] = %lf\t%Qd\n", i, j, B1[i][j], B[i][j]);
        if (10000 * DBL_EPSILON < fabs (mpq_get_d (B[i][j]) - B1[i][j]))
          exit_status = 1;
      }

  scm_dynwind_end ();

  return exit_status;
}
