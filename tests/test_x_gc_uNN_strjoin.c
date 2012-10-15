#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <xunistring.h>
#include <locale.h>

#define TEST_FUNC(NAME, SIZE)						\
  static void								\
  NAME (int argc, uint##SIZE##_t **argv)				\
  {									\
    uint##SIZE##_t *s = NULL;						\
									\
    switch (argc)							\
      {									\
      case 1:								\
	s = x_gc_u##SIZE##_strjoin (NULL);				\
	break;								\
									\
      case 2:								\
	s = x_gc_u##SIZE##_strjoin (argv[1], NULL);			\
	break;								\
									\
      case 3:								\
	s = x_gc_u##SIZE##_strjoin (argv[1], argv[2], NULL);		\
	break;								\
									\
      case 4:								\
	s = x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], NULL);	\
	break;								\
									\
      case 5:								\
	s = x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], argv[4],	\
				    NULL);				\
	break;								\
									\
      case 6:								\
	s = x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], argv[4],	\
				    argv[5], NULL);			\
	break;								\
									\
      case 7:								\
	s =								\
	  x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], argv[4],	\
				  argv[5], argv[6], NULL);		\
	break;								\
									\
      case 8:								\
	s =								\
	  x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], argv[4],	\
				  argv[5], argv[6],argv[7], NULL);	\
	break;								\
									\
      case 9:								\
	s =								\
	  x_gc_u##SIZE##_strjoin (argv[1], argv[2], argv[3], argv[4],	\
				  argv[5], argv[6], argv[7], argv[8],	\
				  NULL);				\
	break;								\
      }									\
    ulc_fprintf (stdout, "%U", x_gc_u##SIZE##_to_u8 (s));		\
  }

TEST_FUNC (test_u8, 8);
TEST_FUNC (test_u16, 16);
TEST_FUNC (test_u32, 32);

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  int encoding = atoi (argv[argc - 1]);

  if (encoding == 32)
    {
      uint32_t *argv32[argc];
      for (size_t i = 0; i < argc; i++)
        argv32[i] = x_gc_u8_to_u32 ((uint8_t *) argv[i]);
      test_u32 (argc - 1, argv32);
    }
  else if (encoding == 16)
    {
      uint16_t *argv16[argc];
      for (size_t i = 0; i < argc; i++)
        argv16[i] = x_gc_u8_to_u16 ((uint8_t *) argv[i]);
      test_u16 (argc - 1, argv16);
    }
  else
    {
      uint8_t *argv8[argc];
      for (size_t i = 0; i < argc; i++)
        argv8[i] = x_gc_u8_strdup ((uint8_t *) argv[i]);
      test_u8 (argc - 1, argv8);
    }

  return 0;
}
