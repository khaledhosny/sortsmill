#include <config.h>

// Copyright (C) 2012 Barry Schwartz
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

#include <sortsmillff/guile/notices.h>
#include <uiinterface.h>
#include <stdlib.h>

void init_guile_sortsmillff_notices (void);

//-------------------------------------------------------------------------

SCM
scm_log_fontforge_warning (SCM message)
{
  char *msg = scm_to_utf8_stringn (message, NULL);
  ui_interface->logwarning (msg);
  free (msg);
  return SCM_UNSPECIFIED;
}

SCM
scm_post_fontforge_notice (SCM title, SCM message)
{
  char *titl = scm_to_utf8_stringn (title, NULL);
  char *msg = scm_to_utf8_stringn (message, NULL);
  ui_interface->post_warning (titl, msg);
  free (msg);
  free (titl);
  return SCM_UNSPECIFIED;
}

SCM
scm_post_fontforge_error (SCM title, SCM message)
{
  char *titl = scm_to_utf8_stringn (title, NULL);
  char *msg = scm_to_utf8_stringn (message, NULL);
  ui_interface->post_error (titl, msg);
  free (msg);
  free (titl);
  return SCM_UNSPECIFIED;
}

//-------------------------------------------------------------------------

VISIBLE void
init_guile_sortsmillff_notices (void)
{
  scm_c_define_gsubr ("log-fontforge-warning", 1, 0, 0,
                      scm_log_fontforge_warning);
  scm_c_define_gsubr ("post-fontforge-notice", 2, 0, 0,
                      scm_post_fontforge_notice);
  scm_c_define_gsubr ("post-fontforge-error", 2, 0, 0,
                      scm_post_fontforge_error);
}

//-------------------------------------------------------------------------
