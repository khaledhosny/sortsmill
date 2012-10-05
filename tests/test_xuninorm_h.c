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

  uint8_t *s1_8 = x_gc_u8_strconv_from_locale (s1);
  uint16_t *s1_16 = x_gc_u16_strconv_from_locale (s1);
  uint32_t *s1_32 = x_gc_u32_strconv_from_locale (s1);

  uint8_t *s2_8 = x_gc_u8_strconv_from_locale (s2);
  uint16_t *s2_16 = x_gc_u16_strconv_from_locale (s2);
  uint32_t *s2_32 = x_gc_u32_strconv_from_locale (s2);

  fprintf (stdout, "%s|%s,", s1, s2);
  ulc_fprintf (stdout, "%U|%U,", s1_8, s2_8);
  ulc_fprintf (stdout, "%lU|%lU,", s1_16, s2_16);
  ulc_fprintf (stdout, "%llU|%llU,", s1_32, s2_32);

  fprintf (stdout, "%d|%d|%d,",
           abs1 (u8_compare (s1_8, s2_8)), abs1 (u16_compare (s1_16, s2_16)),
           abs1 (u32_compare (s1_32, s2_32)));

  return 0;
}
