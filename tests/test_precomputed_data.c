#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <xgc.h>
#include <xalloc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <precomputed_data.h>

const char *which;
int deg;

static void *
my_main (void *UNUSED (p))
{
  const double *data;
  int length;

  const int veclength = deg + 1;
  const int matlength = veclength * veclength;

  if (strcmp (which, "binomial_coefficients") == 0)
    {
      data = get_binomial_coefficients (deg);
      length = veclength;
    }
  else if (strcmp (which, "altsigns") == 0)
    {
      data = get_altsigns (deg);
      length = veclength;
    }
  else if (strcmp (which, "binomial_coefficients_altsigns") == 0)
    {
      data = get_binomial_coefficients_altsigns (deg);
      length = veclength;
    }
  else if (strcmp (which, "sbern_basis_in_mono") == 0)
    {
      data = get_sbern_basis_in_mono (deg);
      length = matlength;
    }
  else if (strcmp (which, "mono_basis_in_sbern") == 0)
    {
      data = get_mono_basis_in_sbern (deg);
      length = matlength;
    }
  else
    abort ();

  for (size_t i = 0; i < length; i++)
    printf ("%lf ", data[i]);

  return NULL;
}

struct _my_args
{
  int argc;
  char **argv;
};

int
main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  which = argv[1];
  deg = atoi (argv[2]);

  (void) scm_with_guile (my_main, NULL);
  return 0;
}
