#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <gfile.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  printf ("%s", GFileAppendFile (argv[1], argv[2], atoi (argv[3])));
  return 0;
}
