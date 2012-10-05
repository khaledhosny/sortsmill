#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>
#include <unistdio.h>

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *s = argv[1];
  uint8_t *s_8 = x_gc_u8_strconv_from_locale (s);
  uint16_t *s_16 = x_gc_u16_strconv_from_locale (s);
  uint32_t *s_32 = x_gc_u32_strconv_from_locale (s);

  fprintf (stdout, "%s\n", s);
  ulc_fprintf (stdout, "%U\n", s_8);
  ulc_fprintf (stdout, "%lU\n", s_16);
  ulc_fprintf (stdout, "%llU\n", s_32);
  fprintf (stdout, "%s\n", x_gc_u8_strconv_to_locale (s_8));
  fprintf (stdout, "%s\n", x_gc_u16_strconv_to_locale (s_16));
  fprintf (stdout, "%s\n", x_gc_u32_strconv_to_locale (s_32));

  return 0;
}
