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

#include <libguile.h>

void init_guile_sortsmill_directory_layout (void);

#define _FF_SCM_DIR_LAYOUT_VAR(VAR, DEF)                        \
  scm_c_define ("pkg-info:" #VAR, scm_from_locale_string (DEF))

VISIBLE void
init_guile_sortsmill_directory_layout (void)
{
  _FF_SCM_DIR_LAYOUT_VAR (pixmapsdir, D_pixmapsdir);
  _FF_SCM_DIR_LAYOUT_VAR (cursorsdir, D_cursorsdir);
  _FF_SCM_DIR_LAYOUT_VAR (htdocsdir, D_htdocsdir);
  _FF_SCM_DIR_LAYOUT_VAR (pkgconfigdir, D_pkgconfigdir);
  _FF_SCM_DIR_LAYOUT_VAR (pkgguiledatadir, D_pkgguiledatadir);
  _FF_SCM_DIR_LAYOUT_VAR (guilemoduledir, D_guilemoduledir);
  _FF_SCM_DIR_LAYOUT_VAR (guileobjmoduledir, D_guileobjmoduledir);
  _FF_SCM_DIR_LAYOUT_VAR (cythonincludedir, D_cythonincludedir);
  _FF_SCM_DIR_LAYOUT_VAR (fcmoduleincludedir, D_fcmoduleincludedir);
  _FF_SCM_DIR_LAYOUT_VAR (pkgdatadir, D_pkgdatadir);
  _FF_SCM_DIR_LAYOUT_VAR (localedir, D_localedir);
  _FF_SCM_DIR_LAYOUT_VAR (sysconfdir, D_sysconfdir);
}
