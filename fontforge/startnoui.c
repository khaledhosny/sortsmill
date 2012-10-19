#include <config.h>

/* Copyright (C) 2007-2012 by George Williams */
/*
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

#include "fontforgevw.h"
#include "annotations.h"
#include <gfile.h>
#include <ustring.h>
#include <time.h>
#include <sys/time.h>
#include <locale.h>
#include <unistd.h>
#include <stdlib.h>
#include <libguile.h>

#ifndef LOCALEDIR
#error You must set LOCALEDIR.
#endif

#ifndef SHAREDIR
#error You must set SHAREDIR.
#endif

static char *
getLocaleDir (void)
{
  return LOCALEDIR;
}

static void
_doscriptusage (void)
{
  printf ("fontforge [options]\n");
  printf ("\t-usage\t\t\t (displays this message, and exits)\n");
  printf
    ("\t-help\t\t\t (displays this message, invokes a browser)\n\t\t\t\t  (Using the BROWSER environment variable)\n");
  printf ("\t-version\t\t (prints the version of fontforge and exits)\n");
  printf ("\t-lang=py\t\t use python to execute scripts\n");
  printf ("\t-lang=ff\t\t use fontforge's old language to execute scripts\n");
  printf ("\t-script scriptfile\t (executes scriptfile)\n");
  printf ("\t-c script-string\t (executes the argument as scripting cmds)\n");
  printf ("\n");
  printf
    ("If no scriptfile/string is given (or if it's \"-\") FontForge will read stdin\n");
  printf
    ("FontForge will read postscript (pfa, pfb, ps, cid), opentype (otf),\n");
  printf
    ("\ttruetype (ttf,ttc), macintosh resource fonts (dfont,bin,hqx),\n");
  printf ("\tand bdf and pcf fonts. It will also read its own format --\n");
  printf ("\tsfd files.\n");
  printf ("Any arguments after the script file will be passed to it.\n");
  printf
    ("If the first argument is an executable filename, and that file's first\n");
  printf
    ("\tline contains \"fontforge\" then it will be treated as a scriptfile.\n\n");
  printf ("For more information see:\n\t%s\n", PACKAGE_URL);
  printf ("Submit bug reports at:\t%s\n", PACKAGE_BUGREPORT);
}

static void
doscriptusage (void)
{
  _doscriptusage ();
  exit (0);
}

static void
doscripthelp (void)
{
  _doscriptusage ();
  /*help("overview.html"); */
  exit (0);
}

//-------------------------------------------------------------------------

static int
fontforge_main_in_guile_mode (int argc, char **argv)
{
  fprintf (stderr,
           "Copyright (c) 2000-2012 by George Williams and others.\n%s"
#ifdef FREETYPE_HAS_DEBUGGER
           "-TtfDb"
#endif
#ifdef _NO_PYTHON
           "-NoPython"
#endif
#ifdef FONTFORGE_CONFIG_USE_DOUBLE
           "-D"
#endif
           ".\n", PACKAGE_STRING);

  /* I don't bother to check that the exe's exectations of the library are */
  /*  valid. The exe only consists of this file, and so it doesn't care. */
  /*  as long as the library is self consistant, all should be well */
  /* check_library_version(&exe_library_version_configuration,true,false); */

  InitSimpleStuff ();

  bind_textdomain_codeset (ff_textdomain (), "UTF-8");
  bindtextdomain (ff_textdomain (), getLocaleDir ());
  textdomain (ff_textdomain ());

  if (default_encoding == NULL)
    default_encoding = FindOrMakeEncoding ("ISO8859-1");
  if (default_encoding == NULL)
    default_encoding = &custom; /* In case iconv is broken */
  CheckIsScript (argc, argv);   /* Will run the script and exit if it is a script */
  if (argc == 2)
    {
      char *pt = argv[1];
      if (*pt == '-' && pt[1] == '-')
        ++pt;
      if (strcmp (pt, "-usage") == 0)
        doscriptusage ();
      else if (strcmp (pt, "-help") == 0)
        doscripthelp ();
      else if (strcmp (pt, "-version") == 0)
        {
          printf ("%s\n", PACKAGE_STRING);
          exit (0);
        }
    }
#if defined(_NO_PYTHON)
  ProcessNativeScript (argc, argv, stdin);
#else
  PyFF_Stdin ();
#endif

  uninm_names_db_close (names_db);
  return 0;
}

//-------------------------------------------------------------------------

struct _my_args
{
  int argc;
  char **argv;
};

static void *
call_fontforge (void *args)
{
  struct _my_args a = *(struct _my_args *) args;
  int *exit_status = xmalloc (sizeof (int));
  *exit_status = fontforge_main_in_guile_mode (a.argc, a.argv);
  return (void *) exit_status;
}

int
fontforge_main (int argc, char **argv)
{
  // This looks complicated only because of the need to pass data
  // around through void pointers.

  struct _my_args args = { argc, argv };
  int *exit_status = (int *) scm_with_guile (call_fontforge, (void *) &args);
  int status = *exit_status;
  free (exit_status);
  return status;
}

//-------------------------------------------------------------------------
