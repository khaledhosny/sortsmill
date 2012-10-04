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
  printf ("%s|", GFileBuildName (argv[1], argv[2]));
  ulc_fprintf (stdout, "%U|",
               u8_GFileBuildName ((uint8_t *) argv[1], (uint8_t *) argv[2]));
  return 0;
}
