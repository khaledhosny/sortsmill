#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <gmp.h>
#include <qbrentroot.h>

//
// FIXME: This test is far from adequate.
//
// (Anyway, it may be more practical to do the more extensive tests
// with a wrapper in a language having closures and run-time
// compilation.)
//

static void
my_sine (mpq_t result, const mpq_t x, void *UNUSED (data))
{
  return mpq_set_d (result, sin (mpq_get_d (x)));
}

static void (*func) (mpq_t, const mpq_t, void *) = my_sine;

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  mpq_t root, t1, t2, tol, epsilon;
  int err;
  unsigned int iter_no;

  mpq_inits (root, t1, t2, tol, epsilon, NULL);

  // argv[1] is reserved to specify the function.
  mpq_set_d (t1, atof (argv[2]));
  mpq_set_d (t2, atof (argv[3]));

  int max_iters = (5 <= argc) ? atoi (argv[4]) : -1;
  mpq_set_d (tol, (6 <= argc) ? atof (argv[5]) : -1.0);
  mpq_set_d (epsilon, (7 <= argc) ? atof (argv[6]) : -1.0);

  qbrentroot (max_iters, tol, epsilon, t1, t2, func, NULL, root, &err,
              &iter_no);
  printf ("err = %d", err);
  if (err == 0)
    printf (", root = %lf, iter_no = %d", mpq_get_d (root), iter_no);

  mpq_clears (root, t1, t2, tol, epsilon, NULL);

  return 0;
}
