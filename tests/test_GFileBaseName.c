#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <locale.h>
#include <gfile.h>
#include <unistdio.h>
#include <xunistring.h>

int
main (int argc, char **argv)
{
  GC_INIT ();
  setlocale (LC_ALL, "");
  const char *arg1 = ((argc < 2) ? "" : argv[1]);
  printf ("%s|", GFileBaseName (arg1));
  ulc_fprintf (stdout, "%U|", u8_GFileBaseName ((uint8_t *) arg1));
  ulc_fprintf (stdout, "%llU|",
               u32_GFileBaseName (x_gc_u8_to_u32 ((uint8_t *) arg1)));
  return 0;
}
