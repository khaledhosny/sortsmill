#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <sortsmillff/xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <sortsmillff/polyspline.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  double spline1[] = { 5, 4, -3, 2, 1, 0, 1, -2, 3, 4, 5 };
  double times[] = { 0, 0.25, 0.5, 0.75, 1, -1 };

  printf ("eval_sbern_double\n");
  for (size_t deg = 0; deg < 11; deg++)
    {
      for (size_t i = 0; 0 <= times[i]; i++)
        printf ("%lf|", eval_sbern_double (deg, spline1, times[i]));
      printf ("\n");
    }

  printf ("eval_bern_double\n");
  for (size_t deg = 0; deg < 11; deg++)
    {
      for (size_t i = 0; 0 <= times[i]; i++)
        printf ("%lf|", eval_bern_double (deg, spline1, times[i]));
      printf ("\n");
    }

  printf ("evaldc_sbern_double\n");
  for (size_t deg = 0; deg < 11; deg++)
    {
      for (size_t i = 0; 0 <= times[i]; i++)
        printf ("%lf|", evaldc_sbern_double (deg, spline1, times[i]));
      printf ("\n");
    }

  printf ("evaldc_bern_double\n");
  for (size_t deg = 0; deg < 11; deg++)
    {
      for (size_t i = 0; 0 <= times[i]; i++)
        printf ("%lf|", evaldc_bern_double (deg, spline1, times[i]));
      printf ("\n");
    }

  printf ("subdiv_sbern_double\n");
  for (size_t deg = 0; deg < 5; deg++)
    {
      double spline1a[deg + 1];
      double spline1b[deg + 1];

      for (size_t i = 0; 0 <= times[i]; i++)
        {
          printf ("t=%lf|", times[i]);
          subdiv_sbern_double (deg, spline1, times[i], spline1a, spline1b);
          for (size_t j = 0; j <= deg; j++)
            printf ("%lf|", spline1a[j]);
          for (size_t j = 0; j <= deg; j++)
            printf ("%lf|", spline1b[j]);
          printf ("\n");
        }
    }

  printf ("subdiv_bern_double\n");
  for (size_t deg = 0; deg < 5; deg++)
    {
      double spline1a[deg + 1];
      double spline1b[deg + 1];

      for (size_t i = 0; 0 <= times[i]; i++)
        {
          printf ("t=%lf|", times[i]);
          subdiv_bern_double (deg, spline1, times[i], spline1a, spline1b);
          for (size_t j = 0; j <= deg; j++)
            printf ("%lf|", spline1a[j]);
          for (size_t j = 0; j <= deg; j++)
            printf ("%lf|", spline1b[j]);

          // Check that subdivision gives the same result as
          // evaluation.
          double v = eval_bern_double (deg, spline1, times[i]);
          double difference = fabs (spline1b[0] - v);
          bool close_enough = (difference <= 10 * DBL_EPSILON);
          printf ("%d", close_enough);

          printf ("\n");
        }
    }

  return 0;
}
