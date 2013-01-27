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

#include <sortsmill/guile/views.h>
#include <fontforgeui.h>
#include <usermenu.h>
#include <stdio.h>

VISIBLE GMenuItem *cv_menu = NULL;
VISIBLE GMenuItem *fv_menu = NULL;

static gmenuitem_moveto_t
moveto_func (void)
{
  return (gmenuitem_moveto_t)
    scm_to_pointer (scm_c_private_ref ("sortsmill usermenu", "moveto-proc-ptr"));
}

static gmenuitem_invoke_t
invoke_func (void)
{
  return (gmenuitem_moveto_t)
    scm_to_pointer (scm_c_private_ref ("sortsmill usermenu", "invoke-proc-ptr"));
}

// FIXME: Get rid of this. It is needed now only because the menus are
// initialized at compile time, in C.
VISIBLE void
cv_tools_list_check (GWindow gw, GMenuItem *mi, GEvent *e)
{
  moveto_func () (gw, mi, e);
}

// FIXME: Get rid of this. It is needed now only because the menus are
// initialized at compile time, in C.
VISIBLE void
fv_tools_list_check (GWindow gw, GMenuItem *mi, GEvent *e)
{
  moveto_func () (gw, mi, e);
}

// FIXME: Get rid of this. It is needed now only because the menus are
// initialized at compile time, in C.
VISIBLE void
cv_do_action (GWindow gw, GMenuItem *mi, GEvent *e)
{
  invoke_func () (gw, mi, e);
}

// FIXME: Get rid of this. It is needed now only because the menus are
// initialized at compile time, in C.
VISIBLE void
fv_do_action (GWindow gw, GMenuItem *mi, GEvent *e)
{
  invoke_func () (gw, mi, e);
}

static SCM
make_string_list (const char **strings)
{
  SCM list = SCM_EOL;
  for (unsigned int i = 0; strings[i] != NULL; i++)
    list = scm_cons (scm_from_utf8_string (strings[i]), list);
  return scm_reverse (list);
}

VISIBLE void
register_fontforge_menu_entry (int window, const char **menu_path, SCM action,
                               SCM enabled, const char *shortcut)
{
  SCM window_symbol = SCM_UNSPECIFIED;
  switch (window)
    {
    case FF_GLYPH_WINDOW:
      window_symbol = scm_from_latin1_symbol ("glyph");
      break;
    case FF_FONT_WINDOW:
      window_symbol = scm_from_latin1_symbol ("font");
      break;
    }

  SCM shortcut_object = (shortcut != NULL) ? scm_from_utf8_string (shortcut) : SCM_BOOL_F;

  scm_call_5 (scm_c_private_ref ("sortsmill usermenu",
                                 "register-fontforge-menu-entry-from-c-code"),
              window_symbol, make_string_list (menu_path), action, enabled, shortcut_object);
}
