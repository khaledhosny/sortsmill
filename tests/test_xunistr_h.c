#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <locale.h>
#include <xunistr.h>
#include <unistdio.h>

int
main (int argc, char **argv)
{
  GC_INIT();

  setlocale(LC_ALL, "");

  uint8_t *s8 = (uint8_t *) argv[1];
  uint16_t *s8_16 = x_gc_u8_to_u16 (s8);
  uint32_t *s8_32 = x_gc_u8_to_u32 (s8);
  uint8_t *s8_16_8 = x_gc_u16_to_u8 (s8_16);
  uint8_t *s8_32_8 = x_gc_u32_to_u8 (s8_32);
  uint32_t *s8_16_32 = x_gc_u16_to_u32 (s8_16);
  uint32_t *s8_32_16 = x_gc_u32_to_u16 (s8_32);

  ulc_fprintf (stdout, "%U\n", s8);
  ulc_fprintf (stdout, "%lU\n", s8_16);
  ulc_fprintf (stdout, "%llU\n", s8_32);
  ulc_fprintf (stdout, "%U\n", s8_16_8);
  ulc_fprintf (stdout, "%U\n", s8_32_8);
  ulc_fprintf (stdout, "%llU\n", s8_16_32);
  ulc_fprintf (stdout, "%lU\n", s8_32_16);

  return 0;
}
