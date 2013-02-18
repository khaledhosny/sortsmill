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
#include <sortsmill/xgc.h>

void init_guile_sortsmill_options (void);

static GOptionEntry
make_GOptionEntry (SCM long_name, SCM short_name, SCM flags, SCM arg,
                   SCM arg_data, SCM description, SCM arg_description)
{
  GOptionEntry e;
  e.long_name = x_gc_grabstr (scm_to_utf8_stringn (long_name, NULL));
  e.short_name = scm_to_int (scm_char_to_integer (short_name));
  e.flags = scm_to_int (flags);
  e.arg = scm_to_int (arg);
  e.arg_data = scm_to_pointer (arg_data);
  e.description =
    scm_is_true (description) ?
    x_gc_grabstr (scm_to_utf8_stringn (description, NULL)) : NULL;
  e.arg_description =
    scm_is_true (arg_description) ?
    x_gc_grabstr (scm_to_utf8_stringn (arg_description, NULL)) : NULL;
  return e;
}

static GOptionEntry null_option = { NULL };

static SCM
scm_list_to_GOptionEntry_array (SCM lst)
{
  size_t n = scm_to_size_t (scm_length (lst));
  SCM bv = scm_c_make_bytevector ((n + 1) * sizeof (GOptionEntry));
  GOptionEntry *array =
    scm_to_pointer (scm_bytevector_to_pointer (bv, scm_from_int (0)));
  SCM p = lst;
  for (size_t i = 0; i < n; i++)
    {
      SCM e = SCM_CAR (p);
      array[i] = make_GOptionEntry (scm_list_ref (e, scm_from_int (0)),
                                    scm_list_ref (e, scm_from_int (1)),
                                    scm_list_ref (e, scm_from_int (2)),
                                    scm_list_ref (e, scm_from_int (3)),
                                    scm_list_ref (e, scm_from_int (4)),
                                    scm_list_ref (e, scm_from_int (5)),
                                    scm_list_ref (e, scm_from_int (6)));
      p = SCM_CDR (p);
    }
  array[n] = null_option;
  return bv;
}

VISIBLE void
init_guile_sortsmill_options (void)
{
  scm_c_define ("option-arg-none", scm_from_int (G_OPTION_ARG_NONE));
  scm_c_define ("option-arg-string", scm_from_int (G_OPTION_ARG_STRING));
  scm_c_define ("option-arg-int", scm_from_int (G_OPTION_ARG_INT));
  scm_c_define ("option-arg-callback", scm_from_int (G_OPTION_ARG_CALLBACK));
  scm_c_define ("option-arg-filename", scm_from_int (G_OPTION_ARG_FILENAME));
  scm_c_define ("option-arg-string-array",
                scm_from_int (G_OPTION_ARG_STRING_ARRAY));
  scm_c_define ("option-arg-filename-array",
                scm_from_int (G_OPTION_ARG_FILENAME_ARRAY));
  scm_c_define ("option-arg-double", scm_from_int (G_OPTION_ARG_DOUBLE));
  scm_c_define ("option-arg-int64", scm_from_int (G_OPTION_ARG_INT64));

  scm_c_define ("option-flag-hidden", scm_from_int (G_OPTION_FLAG_HIDDEN));
  scm_c_define ("option-flag-in-main", scm_from_int (G_OPTION_FLAG_IN_MAIN));
  scm_c_define ("option-flag-reverse", scm_from_int (G_OPTION_FLAG_REVERSE));
  scm_c_define ("option-flag-no-arg", scm_from_int (G_OPTION_FLAG_NO_ARG));
  scm_c_define ("option-flag-filename", scm_from_int (G_OPTION_FLAG_FILENAME));
  scm_c_define ("option-flag-optional-arg",
                scm_from_int (G_OPTION_FLAG_OPTIONAL_ARG));
  scm_c_define ("option-flag-noalias", scm_from_int (G_OPTION_FLAG_NOALIAS));

  scm_c_define ("option-error-unknown-option", scm_from_int (G_OPTION_ERROR_UNKNOWN_OPTION));
  scm_c_define ("option-error-bad-value", scm_from_int (G_OPTION_ERROR_BAD_VALUE));
  scm_c_define ("option-error-failed", scm_from_int (G_OPTION_ERROR_FAILED));

  scm_c_define_gsubr ("list->GOptionEntry-array", 1, 0, 0,
                      scm_list_to_GOptionEntry_array);
}
