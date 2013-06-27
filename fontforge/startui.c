#include <config.h>

// Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#ifndef _NO_PYTHON
#include <Python.h>
#endif

#include <sortsmill/fontforge_main.h>
#include <sortsmill/xdie_on_null.h>
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
#include <stdbool.h>
#include <canonicalize.h>
#include <libguile.h>
#include <gsl/gsl_errno.h>
#include <sortsmill/guile.h>
#include <glib.h>
#define GMenuItem GMenuItem_GTK // FIXME
#include <gio/gio.h>
#undef GMenuItem

extern int AutoSaveFrequency;

static void
dousage (GOptionContext *context)
{
  char *help;
  help = g_option_context_get_help (context, true, NULL);
  printf ("%s", help);
  free (help);
  exit (0);
}

struct delayed_event
{
  void *data;
  void (*func) (void *);
};

static GWindow eventw;
static GDTimer *autosave_timer;

void
DelayEvent (void (*func) (void *), void *data)
{
  struct delayed_event *info = xcalloc (1, sizeof (struct delayed_event));

  info->data = data;
  info->func = func;
  GDrawRequestTimer (eventw, 100, 0, info);
}

static void
DoDelayedEvents (GEvent *event)
{
  GDTimer *t = event->u.timer.timer;
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
event_e_h (GWindow gw, GEvent *event)
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
            return true;
          if (strcmp (arg, "-new") == 0 || strcmp (arg, "--new") == 0)
            FontNew ();
          else if (strcmp (arg, "-open") == 0 || strcmp (arg, "--open") == 0)
            MenuOpen (NULL, NULL, NULL);
          else
            ViewFont (arg, 0);
          free (arg);
          GDrawGrabSelection (eventw, sn_user1);
        }
      break;
    case et_destroy:
      IError ("Who killed the event window?");
      break;
    }
  return true;
}

//-------------------------------------------------------------------------

static void
restore_gsl_error_handler (void *handler)
{
  gsl_set_error_handler ((gsl_error_handler_t *) handler);
}

static void
g_option_context_free_unwind_handler (void *context)
{
  if (context != NULL)
    g_option_context_free ((GOptionContext *) context);
}

static void
g_error_free_unwind_handler (void *error)
{
  if (error != NULL)
    g_error_free ((GError *) error);
}

static void
uninm_names_db_close_unwind_handler (void *UNUSED (_p))
{
  if (names_db != NULL)
    uninm_names_db_close (names_db);
}

static int
fontforge_main_in_guile_mode (int argc, char **argv)
{
  scm_dynwind_begin (0);

  int exit_status = 0;

  // Install the do-nothing GSL error handler, and use a dynwind to
  // restore the original handler when we leave this function.
  gsl_error_handler_t *old_gsl_error_handler = gsl_set_error_handler_off ();
  scm_dynwind_unwind_handler (restore_gsl_error_handler, old_gsl_error_handler,
                              SCM_F_WIND_EXPLICITLY);

  const char *load_prefs = getenv ("FONTFORGE_LOADPREFS");
  int recovery_mode = 2;
  int openflags = 0;
  bool no_font_loaded = true;

  bool open_last = false;
  bool all_glyphs = false;
  bool sync = false;
  bool show_version = false;
  char *recover = NULL;
  char *init_scm = NULL;
  char *display = NULL;
  char **remaining_args = NULL;
  GError *error = NULL;
  GOptionContext *context;

  GRect pos;
  GWindowAttrs wattrs;
  extern int navigation_mask;

  FF_SetUiInterface (&gdraw_ui_interface);
  FF_SetPrefsInterface (&gdraw_prefs_interface);
  FF_SetSCInterface (&gdraw_sc_interface);
  FF_SetCVInterface (&gdraw_cv_interface);
  FF_SetBCInterface (&gdraw_bc_interface);
  FF_SetFVInterface (&gdraw_fv_interface);
  FF_SetFIInterface (&gdraw_fi_interface);
  FF_SetMVInterface (&gdraw_mv_interface);
  FF_SetClipInterface (&gdraw_clip_interface);

  InitSimpleStuff ();

  scm_dynwind_unwind_handler (uninm_names_db_close_unwind_handler,
                              NULL, SCM_F_WIND_EXPLICITLY);

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

  if (load_prefs == NULL || (strcasecmp (load_prefs, "Always") != 0 &&  /* Already loaded */
                             strcasecmp (load_prefs, "Never") != 0))
    LoadPrefs ();

  navigation_mask = GMenuItemParseMask (H_ ("NavigationMask|None"));

  ///////////////////////////////////////////////////////////////////////
  // FIXME: If we have automatic help processing, we do _not_ want     //
  // it. The automatic help processing in glib calls exit(0), which we //
  // _do not_ want. Sorts Mill Editor should be usable as a plug-in or //
  // library.                                                          //
  ///////////////////////////////////////////////////////////////////////

  // *INDENT-OFF*
  GOptionEntry entries[] = {
    { "version", 'V', 0, G_OPTION_ARG_NONE, &show_version, N_("Show version information and exit"), NULL },
    { "all-glyphs", 'a', 0, G_OPTION_ARG_NONE, &all_glyphs, N_("Load all glyphs in the 'glyf' table of a truetype collection"), NULL },
    { "last", 'l', 0, G_OPTION_ARG_NONE, &open_last, N_("Load the last font closed"), NULL },
    { "recover", 'r', 0, G_OPTION_ARG_STRING, &recover, N_("Control error recovery"), "none|auto|inquire|clean" },
    { "init-scm", '\0', 0, G_OPTION_ARG_FILENAME, &init_scm, N_("Initialize with this file instead of the default site-init.scm"), N_("FILE") },
    { "display", '\0', 0, G_OPTION_ARG_STRING, &display, N_("X display to use"), N_("DISPLAY") },
    { "sync", '\0', 0, G_OPTION_ARG_NONE, &sync, N_("Make X calls synchronous"), NULL },
    { G_OPTION_REMAINING, '\0', 0, G_OPTION_ARG_FILENAME_ARRAY, &remaining_args, NULL, N_("[FILE...]") },
    { NULL }
  };
  // *INDENT-ON*

  const char *summary =
    _("Sorts Mill Editor will read PostScript (PFA, PFB, PS, CID),\n"
      "OpenType (OTF), TrueType (TTF, TTC), Macintosh resource fonts\n"
      "(dfont, bin, hqx), and BDF and PCF fonts. It will also read\n"
      "FontForge SFD files.");

  const char *description =
    x_gc_strjoin (_("For more information see: "), PACKAGE_URL, "\n",
                  _("Submit bug reports at: "), PACKAGE_BUGREPORT, NULL);

  context = g_option_context_new ("- Create and edit font files");
  scm_dynwind_unwind_handler (g_option_context_free_unwind_handler,
                              context, SCM_F_WIND_EXPLICITLY);
  g_option_context_add_main_entries (context, entries, FF_TEXTDOMAIN);
  g_option_context_set_summary (context, summary);
  g_option_context_set_description (context, description);

  const bool parsed_successfully =
    g_option_context_parse (context, &argc, &argv, &error);
  scm_dynwind_unwind_handler (g_error_free_unwind_handler, error,
                              SCM_F_WIND_EXPLICITLY);
  if (!parsed_successfully)
    {
      fprintf (stderr, "Option parsing failed: %s\n", error->message);
      exit_status = 1;

      // FIXME: Get rid of this goto through serious rewriting.
      goto exit;                // You did not see this.
    }

  if (show_version)
    {
      printf ("%s\n", PACKAGE_STRING);
      exit_status = 0;

      // FIXME: Get rid of this goto through serious rewriting.
      goto exit;                // You did not see this.
    }

  if (recover != NULL)
    {
      if (strcmp (recover, "none") == 0)
        recovery_mode = 0;
      else if (strcmp (recover, "clean") == 0)
        recovery_mode = -1;
      else if (strcmp (recover, "auto") == 0)
        recovery_mode = 1;
      else if (strcmp (recover, "inquire") == 0)
        recovery_mode = 2;
      else
        {
          fprintf (stderr,
                   "Invalid argument to --recover, must be none, auto, inquire or clean\n");
          dousage (context);
        }
    }

  if (all_glyphs)
    openflags |= of_all_glyphs_in_ttc;

  if (sync)
    GResourceAddResourceString ("Gdraw.Synchronize: true");

  fprintf (stderr, "Copyright (c) 2000-2013 by George Williams and others.\n%s"
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

  GDrawCreateDisplays (display, argv[0]);
  default_background = GDrawGetDefaultBackground (screen_display);
  InitCursors ();

  if (init_scm == NULL)
    init_scm = x_gc_strjoin (SHAREDIR, "/guile/site-init.scm", NULL);

  scm_call_1 (scm_c_public_ref ("sortsmill editor main-loop", "load-site-init"),
              scm_from_locale_string (init_scm));

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

  if (recovery_mode == -1)
    CleanAutoRecovery ();
  else if (recovery_mode)
    no_font_loaded = !DoAutoRecovery (recovery_mode - 1);

  if (open_last)
    {
      if (RecentFiles[0] != NULL
          && ViewFont (RecentFiles[0], openflags))
        no_font_loaded = false;
    }

  if (remaining_args != NULL)
    {
      for (int i = 0; remaining_args[i] != NULL; i++)
        {
          GFile *file;
          char *path;

          GDrawProcessPendingEvents (NULL);

          file = g_file_new_for_commandline_arg (remaining_args[i]);
          path = g_file_get_path (file);

          // if no local path, check if it has a URI scheme and assume it is
          // valid URI
          if (!path && g_uri_parse_scheme (remaining_args[i]) != NULL)
            path = strdup (remaining_args[i]);

          if (GFileIsDir (path))
            {
              GFile *sfdir, *ufo;
              sfdir = g_file_get_child (file, "font.props");
              ufo = g_file_get_child (file, "fontinfo.plist");
              if (g_file_query_exists (sfdir, NULL)
                  || g_file_query_exists (ufo, NULL))
                {
                  /* It's probably a Unified Font Object directory or sf dir collection */
                  if (ViewFont (path, openflags))
                    no_font_loaded = false;

                  g_object_unref (sfdir);
                  g_object_unref (ufo);
                }
              else
                {
                  /* bring open file dialog */
                  if (path[strlen (path) - 1] != '/')
                    {
                      /* If dirname doesn't end in "/" we'll be looking in parent dir */
                      path[strlen (path) + 1] = '\0';
                      path[strlen (path)] = '/';
                    }

                  char *fname = GetFontNameDialog (path, false);
                  if (fname != NULL)
                    ViewFont (fname, openflags);

                  no_font_loaded = false;       /* Even if we didn't get a font, don't bring up dlg again */
                  free (fname);
                }
            }
          else if (ViewFont (path, openflags) != 0)
            no_font_loaded = false;

          free (path);
          g_object_unref (file);
        }
    }

  if (no_font_loaded)
    FontNew ();

  SCM alist = scm_call_0 (scm_c_public_ref ("sortsmill editor main-loop",
                                            "editor-main-loop"));

  SCM exit_stat = scm_assoc_ref (alist, scm_from_utf8_symbol ("exit-status"));
  if (scm_is_true (exit_stat))
    exit_status = scm_to_int (exit_stat);

exit:
  scm_dynwind_end ();

  return exit_status;
}

//-------------------------------------------------------------------------

struct _my_args
{
  int argc;
  char **argv;
  int exit_status;
};

static void *
call_fontforge (void *args)
{
  struct _my_args *a = (struct _my_args *) args;
  a->exit_status = fontforge_main_in_guile_mode (a->argc, a->argv);

  return NULL;
}

VISIBLE int
fontforge_main (int argc, char **argv)
{
  // This looks complicated only because of the need to pass data
  // around through void pointers.

  struct _my_args args = { argc, argv, 0 };
  (void) scm_with_guile (call_fontforge, (void *) &args);
  return args.exit_status;
}

//-------------------------------------------------------------------------
