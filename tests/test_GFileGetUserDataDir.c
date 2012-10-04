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
  printf ("%s|", GFileGetUserDataDir ());
  ulc_fprintf (stdout, "%U|", u8_GFileGetUserDataDir ());
  return 0;
}
