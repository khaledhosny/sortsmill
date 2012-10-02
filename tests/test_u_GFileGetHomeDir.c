#include <config.h>
#include <stdio.h>
#include <gc.h>
#include <gfile.h>
#include <uniconv.h>

int
main (int argc, char **argv)
{
  GC_INIT();
  printf ("%s", u32_strconv_to_locale (u_GFileGetHomeDir ()));
  return 0;
}
