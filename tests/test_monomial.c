#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmill/core.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmill/math/polyspline.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  unsigned int deg = argc - 2;

  double mono[deg + 1];
  for (unsigned int i = 0; i <= deg; i++)
    mono[i] = atof (argv[i + 1]);

  double sbern[deg + 1];
  f64_mono_to_sbern (deg, mono, sbern, 1);

  double mono2[deg + 1];
  f64_sbern_to_mono (deg, sbern, mono2, 1);
  for (unsigned int i = 0; i <= deg; i++)
    if (10 * DBL_EPSILON < fabs (mono[i] - mono2[i]))
      exit (10);

  double bern[deg + 1];
  f64_mono_to_bern (deg, mono, bern, 1);

  f64_bern_to_mono (deg, bern, mono2, 1);
  for (unsigned int i = 0; i <= deg; i++)
    if (10 * DBL_EPSILON < fabs (mono[i] - mono2[i]))
      exit (20);

  for (unsigned int i = 0; i <= 100; i++)
    {
      double t = i / 100.0;
      double x1 = eval_f64_mono (deg, 1, mono, t);
      double x2 = f64_evaldc_sbern (deg, sbern, t);
      double x3 = f64_evaldc_bern (deg, bern, t);
      if (10 * DBL_EPSILON < fabs (x1 - x2))
        exit (30);
      if (10 * DBL_EPSILON < fabs (x1 - x3))
        exit (40);
    }

  return 0;
}
