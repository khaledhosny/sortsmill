#include <config.h>

#include <assert.h>
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
typedef rexp_t (*compiler_once) (rexp_buffer_t *buf, const char *s);
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
  compiler_once my_compiler_once = NULL;
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
  else if (strcmp (study, "redo_study") == 0)
    {
      my_compiler = rexp_compile_jit;
      my_filter = rexp_study;
    }
  else if (strcmp (study, "redo_jit") == 0)
    {
      my_compiler = rexp_compile_study;
      my_filter = rexp_jit;
    }
  else if (strcmp (study, "once_study") == 0)
    {
      my_compiler_once = rexp_compile_once;
      my_filter = rexp_study;
    }
  else if (strcmp (study, "once_jit") == 0)
    {
      my_compiler_once = rexp_compile_once;
      my_filter = rexp_jit;
    }
  else if (strcmp (study, "once_identity") == 0)
    {
      my_compiler_once = rexp_compile_once;
      my_filter = rexp_identity;
    }
  else if (strcmp (study, "compile_once_study") == 0)
    {
      my_compiler_once = rexp_compile_once_study;
      my_filter = rexp_identity;
    }
  else if (strcmp (study, "compile_once_jit") == 0)
    {
      my_compiler_once = rexp_compile_once_jit;
      my_filter = rexp_identity;
    }
  else if (strcmp (study, "redo_once_study") == 0)
    {
      my_compiler_once = rexp_compile_once_jit;
      my_filter = rexp_study;
    }
  else if (strcmp (study, "redo_once_jit") == 0)
    {
      my_compiler_once = rexp_compile_once_study;
      my_filter = rexp_jit;
    }
  else
    abort ();

  int exit_status = 0;

  static rexp_buffer_t re_buf = REXP_BUFFER_T_INITIALIZER;

  rexp_t re;
  if (my_compiler != NULL)
    re = my_filter (my_compiler (pattern));
  else
    {
      re = my_filter (my_compiler_once (&re_buf, pattern));
      assert (re == NULL || (re == &re_buf && re_buf.is_initialized));
      if (re != NULL)
        {
          // Verify that the regex is not re-compiled on a second
          // call.
          rexp_t re2 = my_filter (my_compiler_once (&re_buf, pattern));
          assert (memcmp (re2, re, sizeof (rexp_buffer_t)) == 0);
        }
    }

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

  if (exit_status == 0)
    {
      ulc_fprintf (stderr, "study: %s\n", (re->extra != NULL ? "yes" : "no"));
      int jit;
      pcre_fullinfo (re->pcre_ptr, re->extra, PCRE_INFO_JIT, &jit);
      ulc_fprintf (stderr, "jit:   %s\n", (jit ? "yes" : "no"));
    }

  GC_gcollect ();

  return exit_status;
}
