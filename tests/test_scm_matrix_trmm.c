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

  CBLAS_SIDE_t Side = side_func (argv[1]);
  CBLAS_UPLO_t Uplo = uplo_func (argv[2]);
  CBLAS_TRANSPOSE_t TransA = trans_func (argv[3]);
  CBLAS_DIAG_t Diag = diag_func (argv[4]);

  int m = atoi (argv[5]);
  int n = atoi (argv[6]);

  SCM alpha = scm_c_locale_stringn_to_number (argv[7], strlen (argv[7]), 10);

  int k = (Side == CblasLeft) ? m : n;

  SCM A[k][k];
  SCM B[m][n];

  double A1[k][k];
  double B1[m][n];

  gsl_matrix_view mA1 = gsl_matrix_view_array (&A1[0][0], k, k);
  gsl_matrix_view mB1 = gsl_matrix_view_array (&B1[0][0], m, n);

  unsigned int i_argv = 8;

  for (unsigned int i = 0; i < k; i++)
    for (unsigned int j = 0; j < k; j++)
      {
        A[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        A1[i][j] = scm_to_double (A[i][j]);
        i_argv++;
      }

  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        B[i][j] = scm_c_locale_stringn_to_number (argv[i_argv],
                                                  strlen (argv[i_argv]), 10);
        B1[i][j] = scm_to_double (B[i][j]);
        i_argv++;
      }

  scm_matrix_trmm (Side, Uplo, TransA, Diag, m, n, alpha, A, B);
  gsl_blas_dtrmm (Side, Uplo, TransA, Diag, scm_to_double (alpha), &mA1.matrix,
                  &mB1.matrix);

  int exit_status = 0;

  // Check that we get the same results as gsl_blas_dtrmm.
  for (unsigned int i = 0; i < m; i++)
    for (unsigned int j = 0; j < n; j++)
      {
        scm_simple_format (SCM_BOOL_T,
                           scm_from_utf8_string ("B[~a][~a] = ~a\t~a\n"),
                           scm_list_4 (scm_from_uint (i),
                                       scm_from_uint (j),
                                       scm_from_double (B1[i][j]), B[i][j]));
        if (10000 * DBL_EPSILON < fabs (scm_to_double (B[i][j]) - B1[i][j]))
          exit_status = 1;
      }

  return exit_status;
}
