#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <gc.h>
#include <locale.h>
#include <xunistring.h>
#include <byteswap.h>

#if WORDS_BIGENDIAN

static void
big_endiannize16 (uint16_t *UNUSED (s))
{
}

static void
big_endiannize32 (uint32_t *UNUSED (s))
{
}

#else // little endian

static void
big_endiannize16 (uint16_t *s)
{
  size_t i = 0;
  while (s[i] != 0)
    {
      s[i] = bswap_16 (s[i]);
      i++;
    }
}

static void
big_endiannize32 (uint32_t *s)
{
  size_t i = 0;
  while (s[i] != 0)
    {
      s[i] = bswap_32 (s[i]);
      i++;
    }
}

#endif // little endian

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  char *s = argv[1];
  size_t i = 2;
  size_t j = 0;
  while (s[j] != '\0')
    {
      if (s[j] == '#')
        {
          s[j] = (char) (atol (argv[i]) & 0xFF);
          i++;
        }
      j++;
    }

  int encoding = atoi (argv[i]);


  if (encoding == 32)
    {
      uint32_t *s32 = (uint32_t *) s;
      big_endiannize32 (s32);
      ulc_fprintf (stdout, "%llU|", x_u32_valid_prefix (s32));
      ulc_fprintf (stdout, "%llU|", x_gc_u32_valid_prefix (s32));
      u32_trim_invalid_suffix (s32);
      ulc_fprintf (stdout, "%llU|", s32);
    }
  else if (encoding == 16)
    {
      uint16_t *s16 = (uint16_t *) s;
      big_endiannize16 (s16);
      ulc_fprintf (stdout, "%lU|", x_u16_valid_prefix (s16));
      ulc_fprintf (stdout, "%lU|", x_gc_u16_valid_prefix (s16));
      u16_trim_invalid_suffix (s16);
      ulc_fprintf (stdout, "%lU|", s16);
    }
  else
    {
      uint8_t *s8 = (uint8_t *) s;
      ulc_fprintf (stdout, "%U|", x_u8_valid_prefix (s8));
      ulc_fprintf (stdout, "%U|", x_gc_u8_valid_prefix (s8));
      u8_trim_invalid_suffix (s8);
      ulc_fprintf (stdout, "%U|", s8);
    }

  return 0;
}
