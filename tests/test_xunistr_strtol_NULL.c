#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>

static int
test (const char *filename, int base, int encoding)
{
  FILE *tests = fopen (filename, "r");
  if (tests == NULL)
    abort ();

  int exit_status = 0;
  char *line = NULL;
  size_t n = 0;
  ssize_t num_read = getline (&line, &n, tests);
  while (exit_status == 0 && 0 <= num_read)
    {
      long int val1 = strtol (line, NULL, base);
      long int val2;

      uint8_t *s8 = (uint8_t *) line;

      if (encoding == 16)
        {
          uint16_t *s16 = x_gc_u8_to_u16 (s8);
          val2 = u16_strtol (s16, NULL, base);
          ulc_fprintf (stdout, "|%lU| %ld %ld", s16, val1, val2);
        }
      else if (encoding == 32)
        {
          uint32_t *s32 = x_gc_u8_to_u32 (s8);
          val2 = u32_strtol (s32, NULL, base);
          ulc_fprintf (stdout, "|%llU| %ld %ld", s32, val1, val2);
        }
      else
        {
          val2 = u8_strtol (s8, NULL, base);
          ulc_fprintf (stdout, "|%U| %ld %ld", s8, val1, val2);
        }
      exit_status = (val1 == val2) ? 0 : 1;
      num_read = getline (&line, &n, tests);
    }

  free (line);
  fclose (tests);
  return exit_status;
}

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  int encoding[] = { 8, 16, 32, -1 };

  int i = 0;
  while (encoding[i] != -1)
    {
      test (argv[1], 0, encoding[i]);
      for (int base = 2; base <= 36; base++)
        test (argv[1], base, encoding[i]);
      i++;
    }
}
