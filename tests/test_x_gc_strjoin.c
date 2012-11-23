#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <sortsmillff/xgc.h>
#include <locale.h>

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *s = NULL;

  switch (argc)
    {
    case 1:
      s = x_gc_strjoin (NULL);
      break;

    case 2:
      s = x_gc_strjoin (argv[1], NULL);
      break;

    case 3:
      s = x_gc_strjoin (argv[1], argv[2], NULL);
      break;

    case 4:
      s = x_gc_strjoin (argv[1], argv[2], argv[3], NULL);
      break;

    case 5:
      s = x_gc_strjoin (argv[1], argv[2], argv[3], argv[4], NULL);
      break;

    case 6:
      s = x_gc_strjoin (argv[1], argv[2], argv[3], argv[4], argv[5], NULL);
      break;

    case 7:
      s =
        x_gc_strjoin (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
                      NULL);
      break;

    case 8:
      s =
        x_gc_strjoin (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
                      argv[7], NULL);
      break;

    case 9:
      s =
        x_gc_strjoin (argv[1], argv[2], argv[3], argv[4], argv[5], argv[6],
                      argv[7], argv[8], NULL);
      break;
    }

  printf ("%s", s);

  return 0;
}
