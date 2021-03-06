#include <config.h>

// Copyright (C) 2012 Khaled Hosny and Barry Schwartz
// This file is part of the Sorts Mill Tools.
// 
// Sorts Mill Tools is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
// 
// Sorts Mill Tools is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program; if not, see <http://www.gnu.org/licenses/>.

/*
 * Copyright (C) 2000-2012 by George Williams

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

#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <progname.h>
#include <libguile.h>
#include <glib.h>
#define GMenuItem GMenuItem_GTK // FIXME
#include <gio/gio.h>
#undef GMenuItem

#include <fontforge.h>
#include <compare.h>

#ifndef _NO_PYTHON
#include <ffpython.h>
#endif

static void initialize (void);

//-------------------------------------------------------------------------

static void
failures_of_mask (int mask)
{
  if (mask & 0x2)
    printf ("  Open Contour\n");
  if (mask & 0x4)
    printf ("  Self Intersecting Glyph\n");
  if (mask & 0x8)
    printf ("  Wrong Direction\n");
  if (mask & 0x10)
    printf ("  Flipped Reference\n");
  if (mask & 0x20)
    printf ("  Missing Points at Extrema\n");
  if (mask & 0x40)
    printf ("  Unknown glyph referenced in GSUB/GPOS/MATH\n");
  if (mask & 0x80)
    printf ("  More points in a glyph than PostScript allows\n");
  if (mask & 0x100)
    printf ("  Too Many Hints\n");
  if (mask & 0x200)
    printf ("  Bad Glyph Name\n");
  if (mask & 0x400)
    printf ("  More points in a glyph than specified in 'maxp'\n");
  if (mask & 0x800)
    printf ("  More paths in a glyph than specified in 'maxp'\n");
  if (mask & 0x1000)
    printf ("  More points in a composite glyph than specified in 'maxp'\n");
  if (mask & 0x2000)
    printf ("  More paths in a composite glyph than specified in 'maxp'\n");
  if (mask & 0x4000)
    printf ("  Instructions longer than allowed in 'maxp'\n");
  if (mask & 0x8000)
    printf ("  More references in a glyph than specified in 'maxp'\n");
  if (mask & 0x10000)
    printf ("  References nested more deeply than specified in 'maxp'\n");
  if (mask & 0x20000)
    //printf( "  'prep' or 'fpgm' tables are longer than specified in 'maxp'" );
    // I no longer think this is an error
    ;
  if (mask & 0x40000)
    printf ("  Adjacent points too far apart in a glyph\n");
  if (mask & 0x80000)
    printf ("  Non integral coordinates in a glyph\n");
  if (mask & 0x100000)
    {
      printf
        ("  A glyph uses at least one, but not all, anchor classes in a subtable\n");
      printf ("   (I'm not absolutely sure this is an error)\n");
    }
  if (mask & 0x200000)
    printf ("  Two glyphs have the same name.\n");
  if (mask & 0x400000)
    printf ("  Two glyphs have the unicode.\n");
  if (mask & 0x800000)
    printf ("  Overlapping hints in a glyph.\n");
}

static void
failures_of_load_state (int load_state)
{
  if (load_state & 0x01)
    printf ("  Bad PostScript fontname entry in the 'name' table\n");
  if (load_state & 0x02)
    printf ("  Bad 'glyf' or 'loca' table\n");
  if (load_state & 0x04)
    printf ("  Bad 'CFF ' table\n");
  if (load_state & 0x08)
    printf ("  Bad 'hhea', 'hmtx', 'vhea' or 'vmtx' table\n");
  if (load_state & 0x10)
    printf ("  Bad 'cmap' table\n");
  if (load_state & 0x20)
    printf ("  Bad 'EBDT', 'bdat', 'EBLC' or 'bloc' (embedded bitmap) table\n");
  if (load_state & 0x40)
    printf ("  Bad Apple GX advanced typography table\n");
  if (load_state & 0x80)
    printf ("  Bad OpenType advanced typography table\n");
  if (load_state & 0x100)
    printf
      ("  Bad version number in OS/2 table (must be >0, and must be >1 for OT-CFF fonts)\n");
  if (load_state & 0x200)
    printf ("  Bad sfnt file header\n");
}

static void
failures_of_blues (int blues)
{
  if (blues & 0x010000)
    printf ("  Missing BlueValues entry in PostScript Private dictionary\n");
  if (blues & 0x000001)
    printf
      ("  Odd number of elements in either the BlueValues or OtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000002)
    printf
      ("  Disordered elements in either the BlueValues or OtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000004)
    printf
      ("  Too many elements in either the BlueValues or OtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000008)
    printf
      ("  Elements too close in either the BlueValues or OtherBlues entries in the PostScript Private dictionary (must be at least 2*BlueFuzz+1 apart)\n");
  if (blues & 0x000010)
    printf
      ("  Non-integral elements in either the BlueValues or OtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000020)
    printf
      ("  Alignment zone height in either the BlueValues or OtherBlues is too big for the BlueScale in the PostScript Private dictionary\n");
  if (blues & 0x000100)
    printf
      ("  Odd number of elements in either the FamilyBlues or FamilyOtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000200)
    printf
      ("  Disordered elements in either the FamilyBlues or FamilyOtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000400)
    printf
      ("  Too many elements in either the FamilyBlues or FamilyOtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x000800)
    printf
      ("  Elements too close in either the FamilyBlues or FamilyOtherBlues entries in the PostScript Private dictionary (must be at least 2*BlueFuzz+1 apart)\n");
  if (blues & 0x001000)
    printf
      ("  Non-integral elements in either the FamilyBlues or FamilyOtherBlues entries in the PostScript Private dictionary\n");
  if (blues & 0x002000)
    printf
      ("  Alignment zone height in either the FamilyBlues or FamilyOtherBlues is too big for the BlueScale in the PostScript Private dictionary\n");
  if (blues & 0x020000)
    printf ("  Bad BlueFuzz entry in PostScript Private dictionary\n");
  if (blues & 0x040000)
    printf ("  Bad BlueScale entry in PostScript Private dictionary\n");
  if (blues & 0x080000)
    printf ("  Bad StdHW entry in PostScript Private dictionary\n");
  if (blues & 0x100000)
    printf ("  Bad StdVW entry in PostScript Private dictionary\n");
  if (blues & 0x200000)
    printf ("  Bad StemSnapH entry in PostScript Private dictionary\n");
  if (blues & 0x400000)
    printf ("  Bad StemSnapV entry in PostScript Private dictionary\n");
  if (blues & 0x800000)
    printf
      ("  StemSnapH does not contain StdHW value in PostScript Private dictionary\n");
  if (blues & 0x1000000)
    printf
      ("  StemSnapV does not contain StdVW value in PostScript Private dictionary\n");
  if (blues & 0x2000000)
    printf ("  Bad BlueShift entry in PostScript Private dictionary\n");
}

static bool
validate (char *filename)
{
  bool passed = false;
  const enum openflags flags = of_fstypepermitted | of_fontlint;
  SplineFont *sf = LoadSplineFont (filename, flags);
  if (sf != NULL)
    {
      passed = true;

      FontViewBase *fv = FVAppend (_FontViewCreate (sf));

      int mask = SFValidate (sf, fv->active_layer, true);
      int blues = ValidatePrivate (sf);
      bool is_quadratic = sf->layers[fv->active_layer].order2;
      int load_state = sf->loadvalidation_state;

      if (is_quadratic)
        blues &= ~0x010000;
      else
        mask &= ~0x80000;

      if (mask == 0 && load_state == 0 && blues == 0)
        printf ("Validation %s ...Passed\n", sf->fontname);
      else
        {
          passed = false;
          printf ("Validation %s ...Failed\n", sf->fontname);
          failures_of_mask (mask);
          failures_of_load_state (load_state);
          failures_of_blues (blues);
        }

      FontViewClose (fv);
    }
  return passed;
}

static int
my_main (int argc, char **argv)
{
  set_program_name (argv[0]);
  char progname[(strlen (program_name) + 1) * sizeof (char)];
  strcpy (progname, program_name);
  argv[0] = progname;

  bool show_version = false;
  char **remaining_args = NULL;
  GError *error = NULL;
  GOptionContext *context;

  initialize ();

  // *INDENT-OFF*
  GOptionEntry entries[] = {
    { "version", 'V', 0, G_OPTION_ARG_NONE, &show_version, N_("Show version information and exit"), NULL },
    { G_OPTION_REMAINING, '\0', 0, G_OPTION_ARG_FILENAME_ARRAY, &remaining_args, NULL, N_("[FILE...]") },
    { NULL }
  };
  // *INDENT-ON*

  context = g_option_context_new (_("- Validate the listed font files."));
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

  if (remaining_args == NULL)
    {
      printf (_("%s: you must specify at least one font file\n"), program_name);
      exit (1);
    }

  bool all_passed = true;
  for (size_t i = 0; remaining_args[i] != NULL; i++)
    {
      GFile *file = g_file_new_for_commandline_arg (remaining_args[0]);
      char *font = g_file_get_path (file);

      bool passed = validate (font);
      all_passed = (all_passed && passed);

      g_object_unref (file);
    }

  g_option_context_free (context);

  return (all_passed ? 0 : 1);
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

#if !defined(GLIB_VERSION_2_36)
  g_type_init ();               // for glib
#endif

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
  setlocale (LC_ALL, "");

  // This looks complicated only because of the need to pass data
  // around through void pointers.

  struct _my_args args = { argc, argv, 0 };
  (void) scm_with_guile (call_my_main, (void *) &args);
  return args.exit_status;
}

//-------------------------------------------------------------------------
