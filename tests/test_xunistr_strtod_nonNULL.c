#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>

static int
test (const char *filename, int encoding)
{
  printf ("%s %d\n", filename, encoding);

  FILE *tests = fopen (filename, "r");
  if (tests == NULL)
    abort ();

  int exit_status = 0;
  char *line = NULL;
  size_t n = 0;
  ssize_t num_read = getline (&line, &n, tests);
  while (exit_status == 0 && 0 <= num_read)
    {
      char *endp;
      long int val1 = strtod (line, &endp);
      long int val2;

      size_t char_count = endp - line;
      size_t uchar_count;

      uint8_t *s8 = (uint8_t *) line;

      if (encoding == 16)
        {
          uint16_t *s16 = x_gc_u8_to_u16 (s8);
          uint16_t *end16;
          val2 = u16_strtod (s16, &end16);
          uchar_count = u16_mbsnlen (s16, end16 - s16);
          ulc_fprintf (stdout, "|%lU| %ld %ld %zu %zu\n", s16, val1, val2,
                       char_count, uchar_count);
        }
      else if (encoding == 32)
        {
          uint32_t *s32 = x_gc_u8_to_u32 (s8);
          uint32_t *end32;
          val2 = u32_strtod (s32, &end32);
          uchar_count = u32_mbsnlen (s32, end32 - s32);
          ulc_fprintf (stdout, "|%llU| %ld %ld %zu %zu\n", s32, val1, val2,
                       char_count, uchar_count);
        }
      else
        {
          uint8_t *end8;
          val2 = u8_strtod (s8, &end8);
          uchar_count = u8_mbsnlen (s8, end8 - s8);
          ulc_fprintf (stdout, "|%U| %ld %ld %zu %zu\n", s8, val1, val2,
                       char_count, uchar_count);
        }
      if (val1 != val2)
        exit_status += 0x01;
      if (char_count != uchar_count)
        exit_status += 0x02;
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
      test (argv[1], encoding[i]);
      i++;
    }
}