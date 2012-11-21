#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <xgc.h>
#include <locale.h>
#include <float.h>
#include <math.h>
#include <libguile.h>
#include <gmp_constants.h>

#include <main_with_guile.x>

static int
my_main (int argc, char **argv)
{
  setlocale (LC_ALL, "");

  gmp_printf ("%Qd\n", mpq_zero ());
  gmp_printf ("%Qd\n", mpq_one_half ());
  gmp_printf ("%Qd\n", mpq_neg_one_half ());
  gmp_printf ("%Qd\n", mpq_one ());
  gmp_printf ("%Qd\n", mpq_neg_one ());
  gmp_printf ("%Qd\n", mpq_two ());
  gmp_printf ("%Qd\n", mpq_neg_two ());
  gmp_printf ("%Qd\n", mpq_three ());
  gmp_printf ("%Qd\n", mpq_neg_three ());

  return 0;
}
