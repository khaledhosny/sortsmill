#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <libguile.h>
#include <glib.h>

int main (int, char**);

typedef struct
{
  int argc;
  char **argv;
  int exit_status;
} _my_args_t;

static void *
run_main (void *my_args_ptr)
{
  // The editor can be started by importing (sortsmill editor main)
  // and then evaluating ‘(editor-main (command-line))’. That is the
  // method used below (via Guile’s C API).
  //
  // The editor also can be started by a call to the C function ‘int
  // fontforge_main (int argc, char **argv)’. Which method is more
  // ‘direct’ depends on what happens to be the current
  // implementation.

  _my_args_t args = *(_my_args_t *) my_args_ptr;

  scm_set_program_arguments (args.argc, args.argv, NULL);
  SCM exit_status =
    scm_call_1 (scm_c_public_ref ("sortsmill editor main", "editor-main"),
                scm_program_arguments ());
  args.exit_status = scm_to_int (exit_status);

  *(_my_args_t *) my_args_ptr = args;
  return NULL;
}

int
main (int argc, char **argv)
{
  g_set_prgname (argv[0]);

  _my_args_t args = { argc, argv, -99 };
  (void) scm_with_guile (run_main, &args);
  return args.exit_status;
}
