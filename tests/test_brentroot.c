#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <brentroot.h>

//
// FIXME: This test is far from adequate.
//
// (Anyway, it may be more practical to do the more extensive tests
// with a wrapper in a language having closures and run-time
// compilation.)
//

static double
my_sine (double x, void *UNUSED(data))
{
  return sin (x);
}

static double (*func) (double, void *) = my_sine;

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  double root;
  int err;
  unsigned int iter_no;

  // argv[1] is reserved to specify the function.
  double t1 = atof (argv[2]);
  double t2 = atof (argv[3]);

  brentroot (-1, -1, t1, t2, func, NULL, &root, &err, &iter_no);
  printf ("err = %d", err);
  if (err == 0)
    printf (", root = %lf, iter_no = %d", root, iter_no);

  return 0;
}
