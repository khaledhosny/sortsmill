#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <locale.h>
#include <gfile.h>
#include <unistdio.h>

int
main (int argc, char **argv)
{
  GC_INIT ();
  setlocale (LC_ALL, "");
  const char *arg1 = ((argc < 2) ? "" : argv[1]);
  const char *arg2 = ((argc < 3) ? "" : argv[2]);
  printf ("%s|", GFileBuildName (arg1, arg2));
  ulc_fprintf (stdout, "%U|",
               u8_GFileBuildName ((uint8_t *) arg1, (uint8_t *) arg2));
  return 0;
}
