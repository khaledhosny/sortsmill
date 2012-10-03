#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <locale.h>
#include <gfile.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  setlocale(LC_ALL, "");
  printf ("%s", GFileBaseName (argv[1]));
  return 0;
}
