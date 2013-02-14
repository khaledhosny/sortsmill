#include <config.h>

/*
 * Copyright (C) 2000-2012 by George Williams
 * Copyright (C) 2012 by Barry Schwartz

 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.

 * The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef _NO_PYTHON
#include <Python.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <progname.h>
//#include <sortsmill/xgc.h>
#include <libguile.h>
#include <glib.h>
#define GMenuItem GMenuItem_GTK // FIXME
#include <gio/gio.h>
#undef GMenuItem

#include <fontforge.h>
#include <compare.h>
#include <basics.h>

#ifndef _NO_PYTHON
#include <ffpython.h>
#endif

static void initialize (void);

//-------------------------------------------------------------------------

static int
combine_flags (bool ignorehints, bool ignorenames, bool ignoregpos,
               bool ignoregsub, bool ignorebitmaps, bool exact, bool warn, bool merge)
{
  int flags = 0x789;
  if (ignorehints)
    flags &= ~fcf_hinting;
  if (ignorenames)
    flags &= ~fcf_names;
  if (ignoregpos)
    flags &= ~fcf_gpos;
  if (ignoregsub)
    flags &= ~fcf_gsub;
  if (ignorebitmaps)
    flags &= ~fcf_bitmaps;
  if (exact)
    flags |= fcf_exact;
  if (warn)
    flags |= (fcf_warn_not_exact | fcf_warn_not_ref_exact);
  if (merge)
    flags |= (fcf_addmissing | fcf_adddiff2sf1);
  return flags;
}

static void
compare (char *font1, char *font2, int flags, char *merged_output)
{
  SplineFont *sf1 = LoadSplineFont (font1, of_fstypepermitted);
  if (sf1 == NULL)
    {
      printf ("%s: failed to open the font file %s\n", program_name, font1);
      exit (1);
    }
  FontViewBase *fv1 = FVAppend (_FontViewCreate (sf1));
  SplineFont *sf2 = LoadSplineFont (font2, of_fstypepermitted);
  if (sf2 == NULL)
    {
      printf ("%s: failed to open the font file %s\n", program_name, font2);
      exit (1);
    }
  CompareFonts (sf1, fv1->map, sf2, stdout, flags);
  if (merged_output != NULL)
    {
      char *p = strrchr (merged_output, '.');
      bool sfdir = (p != NULL && strcasecmp (p, ".sfdir") == 0);
      SFDWrite (merged_output, sf1, fv1->map, fv1->normal, sfdir);
    }
}

static int
my_main (int argc, char **argv)
{
  set_program_name (argv[0]);
  char progname[(strlen (program_name) + 1) * sizeof (char)];
  strcpy (progname, program_name);
  argv[0] = progname;

  bool show_version = false;
  bool ignorehints = false;
  bool ignorenames = false;
  bool ignoregpos = false;
  bool ignoregsub = false;
  bool ignorebitmaps = false;
  bool exact = false;
  bool warn = false;
  bool merge = false;
  char *merged_output = NULL;
  char **remaining_args = NULL;
  GError *error = NULL;
  GOptionContext *context;

  initialize ();

  // *INDENT-OFF*
  GOptionEntry entries[] = {
    { "version", 'V', 0, G_OPTION_ARG_NONE, &show_version, N_("Show version information and exit"), NULL },
    { "ignore-hints", '\0', 0, G_OPTION_ARG_NONE, &ignorehints, N_("Do not compare PostScript hints or TrueType instructions."), NULL },
    { "ignore-names", '\0', 0, G_OPTION_ARG_NONE, &ignorenames, N_("Do not compare font names."), NULL },
    { "ignore-gpos", '\0', 0, G_OPTION_ARG_NONE, &ignoregpos, N_("Do not compare kerning, etc."), NULL },
    { "ignore-gsub", '\0', 0, G_OPTION_ARG_NONE, &ignoregsub, N_("Do not compare ligatures, etc."), NULL },
    { "ignore-bitmaps", '\0', 0, G_OPTION_ARG_NONE, &ignorebitmaps, N_("Do not compare bitmap strikes."), NULL },
    { "exact", '\0', 0, G_OPTION_ARG_NONE, &exact, N_("Require outlines to match exactly."), NULL },
    { "warn", '\0', 0, G_OPTION_ARG_NONE, &warn, N_("Provide a warning when an exact match is not found."), NULL },
    { "merge", '\0', 0, G_OPTION_ARG_FILENAME, &merged_output, N_("Put any outline differences in the backgrounds of appropriate glyphs."), N_("FILE") },
    { G_OPTION_REMAINING, '\0', 0, G_OPTION_ARG_FILENAME_ARRAY, &remaining_args, NULL, N_("[FILE...]") },
    { NULL }
  };
  // *INDENT-ON*

  context = g_option_context_new (_("- Compare two font files."));
  g_option_context_add_main_entries (context, entries, FF_TEXTDOMAIN);

  if (!g_option_context_parse (context, &argc, &argv, &error))
    {
      printf ("%s: %s\n", program_name, error->message);
      exit (1);
    }

  if (show_version)
    {
      printf ("%s: %s\n", program_name, VERSION);
	  exit (0);
    }

  if (remaining_args == NULL || remaining_args[1] == NULL)
    {
      printf (_("%s: you must specify exactly two font file\n"), program_name);
      exit (1);
    }

  GFile *file1 = g_file_new_for_commandline_arg (remaining_args[0]);
  GFile *file2 = g_file_new_for_commandline_arg (remaining_args[1]);
  char *font1 = g_file_get_path (file1);
  char *font2 = g_file_get_path (file2);
  g_object_unref (file1);
  g_object_unref (file2);

  if (merged_output != NULL)
    {
      merge = true;
      GFile *file3 = g_file_new_for_commandline_arg (merged_output);
      merged_output = g_file_get_path (file3);
      g_object_unref (file3);
    }

  int flags = combine_flags (ignorehints, ignorenames, ignoregpos, ignoregsub,
                             ignorebitmaps, exact, warn, merge);
  compare (font1, font2, flags, merged_output);

  g_option_context_free (context);

  return 0;
}

//-------------------------------------------------------------------------

static void
initialize (void)
{
  InitSimpleStuff ();

  if (default_encoding == NULL)
    default_encoding = FindOrMakeEncoding ("ISO8859-1");
  if (default_encoding == NULL)
    default_encoding = &custom; /* In case iconv is broken. */

  no_windowing_ui = true;
  running_script = false;

  g_type_init ();               // for glib

#ifndef _NO_PYTHON
  /* This ugly hack initializes the SFD unpickler. */
  Py_Initialize ();
  initPyFontForge ();
#endif
}

//-------------------------------------------------------------------------

struct _my_args
{
  int argc;
  char **argv;
  int exit_status;
};

static void *
call_my_main (void *args)
{
  struct _my_args *a = (struct _my_args *) args;
  a->exit_status = my_main (a->argc, a->argv);
  return NULL;
}

int
main (int argc, char **argv)
{
  // This looks complicated only because of the need to pass data
  // around through void pointers.

  struct _my_args args = { argc, argv, 0 };
  (void) scm_with_guile (call_my_main, (void *) &args);
  return args.exit_status;
}

//-------------------------------------------------------------------------
