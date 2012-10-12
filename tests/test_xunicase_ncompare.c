#include <config.h>
#include <stdio.h>
#include <string.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>

static int
abs1 (int i)
{
  int j;
  if (i < 0)
    j = -1;
  else if (i == 0)
    j = 0;
  else
    j = 1;
  return j;
}

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *s1 = argv[1];
  char *s2 = argv[2];
  int n = atoi (argv[3]);
  int encoding = atoi (argv[4]);

  if (encoding == 32)
    {
      uint32_t *s1_32 = x_gc_u32_strconv_from_locale (s1);
      uint32_t *s2_32 = x_gc_u32_strconv_from_locale (s2);
      fprintf (stdout, "%d", abs1 (u32_ncasecompare (s1_32, s2_32, n)));

    }
  else if (encoding == 16)
    {
      uint16_t *s1_16 = x_gc_u16_strconv_from_locale (s1);
      uint16_t *s2_16 = x_gc_u16_strconv_from_locale (s2);
      fprintf (stdout, "%d", abs1 (u16_ncasecompare (s1_16, s2_16, n)));
    }
  else
    {
      uint8_t *s1_8 = x_gc_u8_strconv_from_locale (s1);
      uint8_t *s2_8 = x_gc_u8_strconv_from_locale (s2);
      fprintf (stdout, "%d", abs1 (u8_ncasecompare (s1_8, s2_8, n)));
    }  

  return 0;
}
