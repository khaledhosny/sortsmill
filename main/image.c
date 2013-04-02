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
#include <sortsmill/xgc.h>
#include <libguile.h>
#include <glib.h>
#define GMenuItem GMenuItem_GTK // FIXME
#include <gio/gio.h>
#undef GMenuItem

#include <fontforge.h>
#include <fontimage.h>
#include <basics.h>

#ifndef _NO_PYTHON
#include <ffpython.h>
#endif

static void initialize (void);

char *default_text = "<fontname>";

//-------------------------------------------------------------------------

static void
make_default_image (char *filename, char *font, int width, int height,
                    int pixelsize)
{
  SplineFont *sf = LoadSplineFont (font, of_fstypepermitted);
  if (sf == NULL)
    {
      printf ("%s: failed to open the font file %s\n", program_name, font);
      exit (1);
    }
  if (filename[0] == '\0')
    filename = x_gc_strjoin (sf->fontname, ".png", NULL);
  Val v[1] = {
    {.type = v_int,.u.ival = pixelsize}
  };
  Array arr = {
    .argc = 1,
    .vals = v
  };
  FontImage (sf, filename, &arr, width, height);
}

static void
make_image (char *filename, char *font, int width, int height, int pixelsize,
            char **lines)
{
  size_t line_count;
  SplineFont *sf = LoadSplineFont (font, of_fstypepermitted);
  if (sf == NULL)
    {
      printf ("%s: failed to open the font file %s\n", program_name, font);
      exit (1);
    }
  if (filename[0] == '\0')
    filename = x_gc_strjoin (sf->fontname, ".png", NULL);

  for (line_count = 0; lines[line_count] != NULL;)
    line_count++;

  Val v[2 * line_count];
  for (size_t i = 0; i < line_count; i++)
    {
      v[2 * i].type = v_int;
      v[2 * i].u.ival = pixelsize;
      v[2 * i + 1].type = v_str;
      if (strcmp (lines[i], default_text) == 0)
        v[2 * i + 1].u.sval = sf->fontname;
      else
        v[2 * i + 1].u.sval = lines[i];
    }
  Array arr = {
    .argc = 2 * line_count,
    .vals = v
  };
  FontImage (sf, filename, &arr, width, height);
}

static int
my_main (int argc, char **argv)
{
  set_program_name (argv[0]);
  char progname[(strlen (program_name) + 1) * sizeof (char)];
  strcpy (progname, program_name);
  argv[0] = progname;

  bool show_version = false;
  int width = -1;
  int height = -1;
  int pixelsize = 24;
  char *output = "";
  char **text = NULL;
  char **remaining_args = NULL;
  GError *error = NULL;
  GOptionContext *context;

  initialize ();

  // *INDENT-OFF*
  GOptionEntry entries[] = {
    { "version", 'V', 0, G_OPTION_ARG_NONE, &show_version, N_("Show version information and exit"), NULL },
    { "width", 'W', 0, G_OPTION_ARG_INT, &width, N_("Set the width of output image in pixels. If omitted (or -1) the image will be as wide as needed."), "INT" },
    { "height", 'H', 0, G_OPTION_ARG_INT, &height, N_("Set the height of output image in pixels. If omitted (or -1) the image will be as high as needed."), "INT" },
    { "pixel-size", 'P', 0, G_OPTION_ARG_INT, &pixelsize, N_("Set the size of text in pixels."), "INT" },
    { "text", 't', 0, G_OPTION_ARG_STRING_ARRAY, &text, N_("A comma-separated lines of text to be used. Use '\\,' for literal comma.\n\t\t\t   If STRING is `<fontname>', the fontname will be used instead. May be specified multiple times."), N_("STRING") },
    { "output", 'o', 0, G_OPTION_ARG_STRING, &output, N_("Set the name of the output file. If omitted, font name will be used."), N_("STRING") },
    { G_OPTION_REMAINING, '\0', 0, G_OPTION_ARG_FILENAME_ARRAY, &remaining_args, NULL, N_("[FILE...]") },
    { NULL }
  };
  // *INDENT-ON*

  context = g_option_context_new
    (_("- Produce an image containing representative glyphs of the font"));
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

  if (remaining_args == NULL || remaining_args[1] != NULL)
    {
      printf (_("%s: you must specify exactly one font file\n"), program_name);
      exit (1);
    }

  GFile *file = g_file_new_for_commandline_arg (remaining_args[0]);
  char *font = g_file_get_path (file);
  g_object_unref (file);

  if (output[0] != '\0')
    {
      file = g_file_new_for_commandline_arg (output);
      output = g_file_get_path (file);
      g_object_unref (file);
    }

  width = imax (width, -1);
  height = imax (height, -1);
  pixelsize = imax (pixelsize, 0);

  if (text == NULL)
    make_default_image (output, font, width, height, pixelsize);
  else
    make_image (output, font, width, height, pixelsize, text);

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
