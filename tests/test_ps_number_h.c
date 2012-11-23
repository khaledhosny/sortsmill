#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <gc.h>
#include <locale.h>
#include <sortsmillff/ps_number.h>

static const char *
truth_val (bool b)
{
  return (b ? "T" : "F");
}

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *s = argv[1];

  printf ("%s", truth_val (is_postscript_integer (s)));
  printf ("%s", truth_val (is_postscript_real (s)));
  printf ("%s", truth_val (is_postscript_radix_number (s)));
  printf ("%s", truth_val (is_postscript_number (s)));

  return 0;
}
