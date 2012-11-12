#include <config.h>

/* Copyright (C) 2000-2012 by George Williams */
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

#include "fontforgeui.h"
#include "annotations.h"
#include <xalloc.h>
#include <gfile.h>
#include <gresource.h>
#include <ustring.h>
#include <time.h>
#include <sys/time.h>
#include <locale.h>
#include <unistd.h>
#include <stdlib.h>
#include <canonicalize.h>
#include <libguile.h>
#include <xdie_on_null.h>

extern int AutoSaveFrequency;

static void
_dousage (void)
{
  printf ("fontforge [options] [fontfiles]\n");
  printf ("\t-new\t\t\t (creates a new font)\n");
  printf ("\t-last\t\t\t (loads the last sfd file closed)\n");
#if HANYANG
  printf ("\t-newkorean\t\t (creates a new korean font)\n");
#endif
  printf ("\t-recover none|auto|inquire|clean (control error recovery)\n");
  printf
    ("\t-allglyphs\t\t (load all glyphs in the 'glyf' table\n\t\t\t of a truetype collection)\n");
  printf ("\t-display display-name\t (sets the X display)\n");
  printf ("\t-depth val\t\t (sets the display depth if possible)\n");
  printf ("\t-vc val\t\t\t (sets the visual class if possible)\n");
  printf ("\t-cmap current|copy|private\t (sets the type of colormap)\n");
  printf ("\t-dontopenxdevices\t (in case that fails)\n");
  printf ("\t-sync\t\t\t (syncs the display, debugging)\n");
  printf
    ("\t-keyboard ibm|mac|sun|ppc  (generates appropriate hotkeys in menus)\n");
  printf ("\t-help\t\t\t (displays this message, and exits)\n");
  printf
    ("\t-docs\t\t\t (displays this message, invokes a browser)\n\t\t\t\t (Using the BROWSER environment variable)\n");
  printf ("\t-version\t\t (prints the version of fontforge and exits)\n");
#ifndef _NO_PYTHON
  printf ("\t-lang=py\t\t use python for scripts (may precede -script)\n");
#endif
#ifndef _NO_FFSCRIPT
  printf ("\t-lang=ff\t\t use fontforge's legacy scripting language\n");
#endif
  printf ("\t-script scriptfile\t (executes scriptfile)\n");
  printf ("\t\tmust be the first option (or follow -lang).\n");
  printf ("\t\tAll others passed to scriptfile.\n");
  printf ("\t-dry scriptfile\t\t (syntax checks scriptfile)\n");
  printf ("\t\tmust be the first option. All others passed to scriptfile.\n");
  printf ("\t\tOnly for fontforge's own scripting language, not python.\n");
  printf ("\t-c script-string\t (executes argument as scripting cmds)\n");
  printf ("\t\tmust be the first option. All others passed to the script.\n");
  printf ("\n");
  printf
    ("FontForge will read postscript (pfa, pfb, ps, cid), opentype (otf),\n");
  printf
    ("\ttruetype (ttf,ttc), macintosh resource fonts (dfont,bin,hqx),\n");
  printf ("\tand bdf and pcf fonts. It will also read its own format --\n");
  printf ("\tsfd files.\n");
  printf
    ("If no fontfiles are specified (and -new is not either and there's nothing\n");
  printf ("\tto recover) then fontforge will produce an open font dlg.\n");
  printf
    ("If a scriptfile is specified then FontForge will not open the X display\n");
  printf
    ("\tnor will it process any additional arguments. It will execute the\n");
  printf ("\tscriptfile and give it any remaining arguments\n");
  printf
    ("If the first argument is an executable filename, and that file's first\n");
  printf
    ("\tline contains \"fontforge\" then it will be treated as a scriptfile.\n\n");
  printf ("For more information see:\n\t%s\n", PACKAGE_URL);
  printf ("Submit bug reports at:\t%s\n", PACKAGE_BUGREPORT);
}

static void
dousage (void)
{
  _dousage ();
  exit (0);
}

static void
dohelp (void)
{
  _dousage ();
  help ("overview.html");
  exit (0);
}

struct delayed_event
{
  void *data;
  void (*func) (void *);
};

static GWindow eventw;
static GTimer *autosave_timer;

void
DelayEvent (void (*func) (void *), void *data)
{
  struct delayed_event *info = xcalloc (1, sizeof (struct delayed_event));

  info->data = data;
  info->func = func;
  GDrawRequestTimer (eventw, 100, 0, info);
}

static void
DoDelayedEvents (GEvent * event)
{
  GTimer *t = event->u.timer.timer;
  struct delayed_event *info =
    (struct delayed_event *) (event->u.timer.userdata);

  if (info != NULL)
    {
      (info->func) (info->data);
      free (info);
    }
  GDrawCancelTimer (t);
}

static int
event_e_h (GWindow gw, GEvent * event)
{
  switch (event->type)
    {
    case et_create:
      GDrawGrabSelection (gw, sn_user1);
      break;
    case et_timer:
      if (event->u.timer.timer == autosave_timer)
        {
          DoAutoSaves ();
        }
      else
        {
          DoDelayedEvents (event);
        }
      break;
    case et_selclear:
      /* If this happens, it means someone wants to send us a message with a */
      /*  filename to open. So we need to ask for it, process it, and then  */
      /*  take the selection back again */
      if (event->u.selclear.sel == sn_user1)
        {
          int len;
          char *arg;
          arg = GDrawRequestSelection (eventw, sn_user1, "STRING", &len);
          if (arg == NULL)
            return (true);
          if (strcmp (arg, "-new") == 0 || strcmp (arg, "--new") == 0)
            FontNew ();
          else if (strcmp (arg, "-open") == 0 || strcmp (arg, "--open") == 0)
            MenuOpen (NULL, NULL, NULL);
          else
            ViewPostScriptFont (arg, 0);
          free (arg);
          GDrawGrabSelection (eventw, sn_user1);
        }
      break;
    case et_destroy:
      IError ("Who killed the event window?");
      break;
    }
  return (true);
}

static void
AddR (char *name, char *val)
{
  /* Add this command line value to this GUI resource. */
  char *full = xmalloc (strlen (name) + strlen (val) + 4);
  strcpy (full, name);
  strcat (full, ": ");
  strcat (full, val);
  GResourceAddResourceString (full);
  free (full);
}

static int
ReopenLastFonts (void)
{
  char buffer[1024];
  char *ffdir = getUserCacheDir ();
  FILE *old;
  int any = 0;

  if (ffdir == NULL)
    return (false);
  sprintf (buffer, "%s/FontsOpenAtLastQuit", ffdir);
  old = fopen (buffer, "r");
  if (old == NULL)
    return (false);
  while (fgets (buffer, sizeof (buffer), old) != NULL)
    {
      int len = strlen (buffer);
      if (buffer[len - 1] == '\n')
        buffer[--len] = '\0';
      if (buffer[len - 1] == '\r')
        buffer[--len] = '\0';
      if (ViewPostScriptFont (buffer, 0) != 0)
        any = 1;
    }
  fclose (old);
  return (any);
}

//-------------------------------------------------------------------------

static int
fontforge_main_in_guile_mode (int argc, char **argv)
{
  const char *load_prefs = getenv ("FONTFORGE_LOADPREFS");
  int i;
  int recover = 2;
  int any;
  int next_recent = 0;
  GRect pos;
  GWindowAttrs wattrs;
  char *display = NULL;
  int openflags = 0;
  int doopen = 0;
  extern int navigation_mask;

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

  /* Must be done before we cache the current directory */
  /* Change to HOME dir if specified on the commandline */
  for (i = 1; i < argc; ++i)
    {
      if (strcmp (argv[i], "-home") == 0)
        {
          chdir (GFileGetHomeDir ());
          break;
        }
    }

  FF_SetUiInterface (&gdraw_ui_interface);
  FF_SetPrefsInterface (&gdraw_prefs_interface);
  FF_SetSCInterface (&gdraw_sc_interface);
  FF_SetCVInterface (&gdraw_cv_interface);
  FF_SetBCInterface (&gdraw_bc_interface);
  FF_SetFVInterface (&gdraw_fv_interface);
  FF_SetFIInterface (&gdraw_fi_interface);
  FF_SetMVInterface (&gdraw_mv_interface);
  FF_SetClipInterface (&gdraw_clip_interface);
#ifndef _NO_PYTHON
  PythonUI_Init ();
#endif

  InitSimpleStuff ();

  GMenuSetShortcutDomain (FF_SHORTCUTSDOMAIN);
  bind_textdomain_codeset (FF_SHORTCUTSDOMAIN, "UTF-8");
  bindtextdomain (FF_SHORTCUTSDOMAIN, LOCALEDIR);

  bind_textdomain_codeset (FF_TEXTDOMAIN, "UTF-8");
  bindtextdomain (FF_TEXTDOMAIN, LOCALEDIR);
  textdomain (FF_TEXTDOMAIN);

  GGadgetSetImageDir (SHAREDIR "/pixmaps");
  GResourceAddResourceFile (SHAREDIR "/resources/fontforge.resource", false);

  if (load_prefs != NULL && strcasecmp (load_prefs, "Always") == 0)
    LoadPrefs ();
  if (default_encoding == NULL)
    default_encoding = FindOrMakeEncoding ("ISO8859-1");
  if (default_encoding == NULL)
    default_encoding = &custom; /* In case iconv is broken */

  CheckIsScript (argc, argv);   /* Will run the script and exit if it is a script */

  /* If there is no UI, there is always a script */
  /*  and we will never return from the above */
  if (load_prefs == NULL || (strcasecmp (load_prefs, "Always") != 0 &&  /* Already loaded */
                             strcasecmp (load_prefs, "Never") != 0))
    LoadPrefs ();
  navigation_mask = GMenuItemParseMask (H_ ("NavigationMask|None"));
  for (i = 1; i < argc; ++i)
    {
      char *pt = argv[i];
      if (pt[0] == '-' && pt[1] == '-')
        ++pt;
      if (strcmp (pt, "-sync") == 0)
        GResourceAddResourceString ("Gdraw.Synchronize: true");
      else if (strcmp (pt, "-depth") == 0 && i < argc - 1)
        AddR ("Gdraw.Depth", argv[++i]);
      else if (strcmp (pt, "-vc") == 0 && i < argc - 1)
        AddR ("Gdraw.VisualClass", argv[++i]);
      else if ((strcmp (pt, "-cmap") == 0 || strcmp (pt, "-colormap") == 0)
               && i < argc - 1)
        AddR ("Gdraw.Colormap", argv[++i]);
      else if ((strcmp (pt, "-dontopenxdevices") == 0))
        AddR ("Gdraw.DontOpenXDevices", "true");
      else if (strcmp (pt, "-keyboard") == 0 && i < argc - 1)
        AddR ("Gdraw.Keyboard", argv[++i]);
      else if (strcmp (pt, "-display") == 0 && i < argc - 1)
        display = argv[++i];
      else if (strcmp (pt, "-recover") == 0 && i < argc - 1)
        {
          ++i;
          if (strcmp (argv[i], "none") == 0)
            recover = 0;
          else if (strcmp (argv[i], "clean") == 0)
            recover = -1;
          else if (strcmp (argv[i], "auto") == 0)
            recover = 1;
          else if (strcmp (argv[i], "inquire") == 0)
            recover = 2;
          else
            {
              fprintf (stderr,
                       "Invalid argument to -recover, must be none, auto, inquire or clean\n");
              dousage ();
            }
        }
      else if (strcmp (pt, "-recover=none") == 0)
        recover = 0;
      else if (strcmp (pt, "-recover=clean") == 0)
        recover = -1;
      else if (strcmp (pt, "-recover=auto") == 0)
        recover = 1;
      else if (strcmp (pt, "-recover=inquire") == 0)
        recover = 2;
      else if (strcmp (pt, "-docs") == 0)
        dohelp ();
      else if (strcmp (pt, "-help") == 0)
        dousage ();
      else if (strcmp (pt, "-version") == 0)
        {
          printf ("%s\n", PACKAGE_STRING);
          exit (0);
        }
      else if (strcmp (pt, "-home") == 0)
        /* already did a chdir earlier, don't need to do it again */ ;
    }

  GDrawCreateDisplays (display, argv[0]);
  default_background = GDrawGetDefaultBackground (screen_display);
  InitCursors ();

#ifndef _NO_PYTHON
  PyFF_ProcessInitFiles ();
#endif

  /* This is an invisible window to catch some global events */
  wattrs.mask = wam_events | wam_isdlg;
  wattrs.is_dlg = true;
  pos.x = pos.y = 0;
  pos.width = pos.height = 1;
  GDrawBindSelection (NULL, sn_user1, "FontForge");
  eventw = GDrawCreateTopWindow (NULL, &pos, event_e_h, NULL, &wattrs);

  if (AutoSaveFrequency > 0)
    autosave_timer = GDrawRequestTimer (eventw,
                                        2 * AutoSaveFrequency * 1000,
                                        AutoSaveFrequency * 1000, NULL);
  GDrawProcessPendingEvents (NULL);

  any = 0;
  if (recover == -1)
    CleanAutoRecovery ();
  else if (recover)
    any = DoAutoRecovery (recover - 1);

  openflags = 0;
  for (i = 1; i < argc; ++i)
    {
      char *pt = argv[i];

      GDrawProcessPendingEvents (NULL);
      if (pt[0] == '-' && pt[1] == '-')
        ++pt;
      if (strcmp (pt, "-new") == 0)
        {
          FontNew ();
          any = 1;
#if HANYANG
        }
      else if (strcmp (pt, "-newkorean") == 0)
        {
          MenuNewComposition (NULL, NULL, NULL);
          any = 1;
#endif
        }
      else if (strcmp (pt, "-last") == 0)
        {
          if (next_recent < RECENT_MAX && RecentFiles[next_recent] != NULL)
            if (ViewPostScriptFont (RecentFiles[next_recent++], openflags))
              any = 1;
        }
      else if (strcmp (pt, "-sync") == 0 || strcmp (pt, "-memory") == 0
               || strcmp (pt, "-recover=none") == 0
               || strcmp (pt, "-recover=clean") == 0
               || strcmp (pt, "-recover=auto") == 0
               || strcmp (pt, "-dontopenxdevices") == 0
               || strcmp (pt, "-home") == 0)
        /* Already done, needed to be before display opened */ ;
      else if (strncmp (pt, "-psn_", 5) == 0)
        /* Already done */ ;
      else if ((strcmp (pt, "-depth") == 0 || strcmp (pt, "-vc") == 0 ||
                strcmp (pt, "-cmap") == 0 || strcmp (pt, "-colormap") == 0 ||
                strcmp (pt, "-keyboard") == 0 ||
                strcmp (pt, "-display") == 0 || strcmp (pt, "-recover") == 0)
               && i < argc - 1)
        ++i;                    /* Already done, needed to be before display opened */
      else if (strcmp (pt, "-allglyphs") == 0)
        openflags |= of_all_glyphs_in_ttc;
      else if (strcmp (pt, "-open") == 0)
        doopen = true;
      else
        {
          char *buffer;

          if (strstr (argv[i], "://") != NULL)  /* FIXME: This is
                                                   broken. There is
                                                   regular expression
                                                   code elsewhere to
                                                   re-use here. */
            /* Assume an absolute URL */
            buffer = xstrdup (argv[i]);
          else
            buffer =
              XDIE_ON_NULL (canonicalize_filename_mode
                            (argv[i], CAN_MISSING));

          if (GFileIsDir (buffer) || (strstr (buffer, "://") != NULL    /* FIXME: This is
                                                                           broken. There is
                                                                           regular expression
                                                                           code elsewhere to
                                                                           re-use here. */
                                      && buffer[strlen (buffer) - 1] == '/'))
            {
              char *fname = xmalloc (strlen (buffer) +
                                     strlen ("/glyphs/contents.plist") + 1);
              strcpy (fname, buffer);
              strcat (fname, "/glyphs/contents.plist");
              if (GFileExists (fname))
                {
                  /* It's probably a Unified Font Object directory */
                  free (fname);
                  if (ViewPostScriptFont (buffer, openflags))
                    any = 1;
                }
              else
                {
                  strcpy (fname, buffer);
                  strcat (fname, "/font.props");
                  if (GFileExists (fname))
                    {
                      /* It's probably a sf dir collection */
                      free (fname);
                      if (ViewPostScriptFont (buffer, openflags))
                        any = 1;
                    }
                  else
                    {
                      free (fname);
                      if (buffer[strlen (buffer) - 1] != '/')
                        {
                          /* If dirname doesn't end in "/" we'll be looking in parent dir */
                          buffer[strlen (buffer) + 1] = '\0';
                          buffer[strlen (buffer)] = '/';
                        }
                      fname = GetPostScriptFontName (buffer, false);
                      if (fname != NULL)
                        ViewPostScriptFont (fname, openflags);
                      any = 1;  /* Even if we didn't get a font, don't bring up dlg again */
                      free (fname);
                    }
                }
            }
          else if (ViewPostScriptFont (buffer, openflags) != 0)
            any = 1;
          free (buffer);
        }
    }
  if (!any && !doopen)
    any = ReopenLastFonts ();
  if (doopen || !any)
    MenuOpen (NULL, NULL, NULL);
  GDrawEventLoop (NULL);

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
