#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>
#include <rexp.h>

typedef rexp_match_t (*matcher) (rexp_t re, const char *s);
typedef rexp_t (*compiler) (const char *s);
typedef rexp_t (*filter) (rexp_t re);

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *pattern = argv[1];
  char *string = argv[2];
  char *operation = argv[3];
  char *study = argv[4];

  matcher my_matcher = NULL;
  if (strcmp (operation, "match") == 0)
    my_matcher = rexp_match;
  else if (strcmp (operation, "search") == 0)
    my_matcher = rexp_search;
  else
    abort ();

  filter my_filter = NULL;
  compiler my_compiler = NULL;
  if (strcmp (study, "study") == 0)
    {
      my_compiler = rexp_compile;
      my_filter = rexp_study;
    }
  else if (strcmp (study, "jit") == 0)
    {
      my_compiler = rexp_compile;
      my_filter = rexp_jit;
    }
  else if (strcmp (study, "identity") == 0)
    {
      my_compiler = rexp_compile;
      my_filter = rexp_identity;
    }
  else if (strcmp (study, "compile_study") == 0)
    {
      my_compiler = rexp_compile_study;
      my_filter = rexp_identity;
    }

  else if (strcmp (study, "compile_jit") == 0)
    {
      my_compiler = rexp_compile_jit;
      my_filter = rexp_identity;
    }
  else
    abort ();

  int exit_status = 0;

  rexp_t re = my_filter (my_compiler (pattern));
  if (re == NULL)
    exit_status = 10;
  else
    {
      rexp_match_t m = my_matcher (re, string);
      if (m == NULL)
        exit_status = 20;
      else
        {
          for (size_t i = 0; i < rexp_num_subexpr (m) + 1; i++)
            {
              rexp_interval_t interv = rexp_interval (m, i);
              char *substr = rexp_substr (m, string, i);
              ulc_fprintf (stdout, "%zu: %d %d |%s|\n", i, interv.i_start,
                           interv.i_end, substr);
            }
        }
    }

  if (exit_status == 0)
    {
      // Check that null PCRE pattern buffer gives a null match
      // object.
      rexp_match_t m = my_matcher (NULL, string);
      if (m)
        exit_status = 30;
    }

  GC_gcollect ();

  return exit_status;
}
