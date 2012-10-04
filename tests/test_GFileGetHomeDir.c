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
  printf ("%s|", GFileGetHomeDir ());
  ulc_fprintf (stdout, "%U|", u8_GFileGetHomeDir ());
  return 0;
}
