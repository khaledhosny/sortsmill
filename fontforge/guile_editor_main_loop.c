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

#include <sortsmill/guile/editor/main_loop.h>

VISIBLE SCM
scm_exit_editor_main_loop (SCM exit_status, SCM alist)
{
  return scm_call_4 (scm_c_public_ref ("sortsmill editor main-loop",
                                       "exit-editor-main-loop"),
                     scm_from_utf8_keyword ("exit-status"), exit_status,
                     scm_from_utf8_keyword ("alist"), alist);
}

VISIBLE SCM
scm_c_exit_editor_main_loop (int exit_status)
{
  return scm_exit_editor_main_loop (scm_from_int (exit_status), SCM_EOL);
}
