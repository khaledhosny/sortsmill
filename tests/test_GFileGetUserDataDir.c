#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <gfile.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  printf ("%s", GFileGetUserDataDir ());
  return 0;
}
