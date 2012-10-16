#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <xunistring.h>
#include <locale.h>

static int
test_x (const uint32_t *s32)
{
  uint8_t *t8 = x_u32_to_u8 (s32);
  uint16_t *t16 = x_u8_to_u16 (t8);
  free (t8);
  uint32_t *t32 = x_u16_to_u32 (t16);
  free (t16);
  t16 = x_u32_to_u16 (t32);
  free (t32);
  uint16_t *u16 = x_u16_to_u16 (t16);
  free (t16);
  t8 = x_u16_to_u8 (u16);
  free (u16);
  uint8_t *u8 = x_u8_to_u8 (t8);
  free (t8);
  t32 = x_u8_to_u32 (u8);
  free (u8);
  uint32_t *u32 = x_u32_to_u32 (t32);
  free (t32);
  int return_value = (u32_compare (s32, u32) == 0) ? 0 : 1;
  free (u32);
  return return_value;
}

static int
test_x_gc (const uint32_t *s32)
{
  uint8_t *t8 = x_gc_u32_to_u8 (s32);
  uint16_t *t16 = x_gc_u8_to_u16 (t8);
  uint32_t *t32 = x_gc_u16_to_u32 (t16);
  t16 = x_gc_u32_to_u16 (t32);
  t16 = x_gc_u16_to_u16 (t16);
  t8 = x_gc_u16_to_u8 (t16);
  t8 = x_gc_u8_to_u8 (t8);
  t32 = x_gc_u8_to_u32 (t8);
  t32 = x_gc_u32_to_u32 (t32);
  return (u32_compare (s32, t32) == 0) ? 0 : 1;
}

int
main (int argc, char **argv)
{
  GC_INIT ();

  setlocale (LC_ALL, "");

  int exit_status = 0;

  uint32_t s32[] = {
    0x0001,
    0x007F,
    0x0080,
    0xD7FF,
    0xE000,
    0xFFFF,
    0x10000,
    0x10FFFF,
    0
  };

  exit_status = test_x (s32);
  if (exit_status == 0)
    exit_status = test_x_gc (s32);

  size_t s32_length = u32_strlen (s32);

  const size_t big_size = atoi (argv[1]);
  uint32_t big_s[big_size + 1];

  size_t i = 0;
  while (exit_status == 0 && i < atoi (argv[2]))
    {
      for (size_t j = 0; j < big_size; j++)
        big_s[j] = s32[rand () % s32_length];
      big_s[big_size] = 0;
      exit_status = test_x (big_s);
      if (exit_status == 0)
        exit_status = test_x_gc (big_s);
      i++;
    }

  return exit_status;
}
