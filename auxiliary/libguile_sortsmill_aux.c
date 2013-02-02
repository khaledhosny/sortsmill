#include <config.h>

/*
  An essentially empty source file, just in case the linker wants at
  least one .o file.

  FIXME: Are there any platforms where this actually is necessary?
*/

#include <stdbool.h>

extern bool __libguile_sortsmill_aux_c_has_something_in_it;
bool __libguile_sortsmill_aux_c_has_something_in_it = true;
