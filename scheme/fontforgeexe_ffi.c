#include <config.h>

// Copyright (C) 2012 Barry Schwartz
// 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

#include <stdio.h>
#include <xalloc.h>
#include <fontforge.h>
#include <libguile.h>

static SCM
fontforge_main_wrapper (SCM args)
{
  // Convert the 'args' list to C-style argc and argv.
  int argc = scm_to_int (scm_length (args));
  char *argv[argc];
  for (int i = 0; i < argc; i++) 
    {
      argv[i] = scm_to_locale_string (scm_list_ref (args, scm_from_int (i)));
      if (argv[i] == NULL)
	xalloc_die ();
    }

  // Run FontForge.
  int return_value = fontforge_main (argc, argv);

  // Free the argv strings.
  for (int i = 0; i < argc; i++)
    free (argv[i]);

  return scm_from_int (return_value);
}

void sortsmillff_init_fontforge_main (void *UNUSED(unused));

void
sortsmillff_init_fontforge_main (void *UNUSED(unused))
{
  scm_c_define_gsubr ("fontforge-main", 1, 0, 0, fontforge_main_wrapper);
  scm_c_export ("fontforge-main", NULL);
}
