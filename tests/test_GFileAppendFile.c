#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <locale.h>
#include <gfile.h>
#include <xunistring.h>
#include <unistdio.h>

int
main (int argc, char **argv)
{
  GC_INIT ();
  setlocale (LC_ALL, "");
  printf ("%s|", GFileAppendFile (argv[1], argv[2], atoi (argv[3])));
  ulc_fprintf (stdout, "%U|",
               u8_GFileAppendFile ((uint8_t *) argv[1], (uint8_t *) argv[2],
                                   atoi (argv[3])));
  ulc_fprintf (stdout, "%llU|",
               u32_GFileAppendFile (x_gc_u8_to_u32 ((uint8_t *) argv[1]),
                                    x_gc_u8_to_u32 ((uint8_t *) argv[2]),
                                    atoi (argv[3])));
  return 0;
}
