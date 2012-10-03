#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <gfile.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  printf ("%s", GFileBuildName (argv[1], argv[2]));
  return 0;
}
