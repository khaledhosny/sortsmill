#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <gfile.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  printf ("%s", GFileBaseName (argv[1]));
  return 0;
}
