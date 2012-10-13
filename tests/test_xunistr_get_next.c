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

  char *str = (char *) argv[1];
  size_t i = 2;
  size_t j = 0;
  while (str[j] != '\0')
    {
      if (str[j] == '#')
        {
          str[j] = (char) (atol (argv[i]) & 0xFF);
          i++;
        }
      j++;
    }

  int encoding = atoi (argv[i]);

  uint32_t ch[2];
  ch[1] = 0;

  if (encoding == 32)
    {
      big_endiannize32 ((uint32_t *) str);
      const uint32_t *s = (uint32_t *) str;
      int c = u32_get_next (&s);
      while (0 < c)
	{
	  ch[0] = c;
	  ulc_fprintf (stdout, "%llU", ch);
	  c = u32_get_next (&s);
	}
      ulc_fprintf (stdout, "|%d", c);
    }
  else if (encoding == 16)
    {
      big_endiannize16 ((uint16_t *) str);
      const uint16_t *s = (uint16_t *) str;
      int c = u16_get_next (&s);
      while (0 < c)
	{
	  ch[0] = c;
	  ulc_fprintf (stdout, "%llU", ch);
	  c = u16_get_next (&s);
	}
      ulc_fprintf (stdout, "|%d", c);
    }
  else
    {
      const uint8_t *s = (uint8_t *) str;
      int c = u8_get_next (&s);
      while (0 < c)
	{
	  ch[0] = c;
	  ulc_fprintf (stdout, "%llU", ch);
	  c = u8_get_next (&s);
	}
      ulc_fprintf (stdout, "|%d", c);
    }

  return 0;
}
