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
//#include <sortsmillff/xgc.h>
#include <libguile.h>

#include <fontforge.h>
#include <scripting.h>
#include <basics.h>
#include "sfddiff_opts.h"

#ifndef _NO_PYTHON
#include <ffpython.h>
#endif

static void initialize (void);

//-------------------------------------------------------------------------

static int
combine_flags (bool ignorehints, bool ignorenames, bool ignoregpos,
               bool ignoregsub, bool ignorebitmaps, bool exact, bool warn,
               bool merge)
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

  struct gengetopt_args_info args_info;
  int errval = cmdline_parser (argc, argv, &args_info);
  if (errval != 0)
    exit (1);

  if (args_info.inputs_num != 2)
    {
      printf ("%s: you must specify exactly two font files\n", program_name);
      exit (1);
    }

  bool ignorehints = args_info.ignorehints_given;
  bool ignorenames = args_info.ignorenames_given;
  bool ignoregpos = args_info.ignoregpos_given;
  bool ignoregsub = args_info.ignoregsub_given;
  bool ignorebitmaps = args_info.ignorebitmaps_given;
  bool exact = args_info.exact_given;
  bool warn = args_info.warn_given;
  bool merge = args_info.merge_given;
  char *merged_output = (merge ? args_info.merge_arg : NULL);
  char *font1 = args_info.inputs[0];
  char *font2 = args_info.inputs[1];

  initialize ();

  int flags = combine_flags (ignorehints, ignorenames, ignoregpos, ignoregsub,
                             ignorebitmaps, exact, warn, merge);
  compare (font1, font2, flags, merged_output);

  cmdline_parser_free (&args_info);

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

#ifndef _NO_PYTHON
  /* This ugly hack initializes the SFD unpickler. */
  Py_SetProgramName ((char *) program_name);
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
