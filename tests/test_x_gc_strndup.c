#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <xgc.h>
#include <locale.h>

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  const char *s = argv[1];
  size_t n = (unsigned int) atoi (argv[2]);

  printf ("%s", x_gc_strndup (s, n));

  return 0;
}
