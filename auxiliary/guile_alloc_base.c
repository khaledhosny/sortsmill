#include <config.h>

// Copyright (C) 2013 Barry Schwartz
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

#include <libguile.h>
#include <xalloc.h>
#include <sortsmill/xgc.h>

static SCM
scm_czalloc (SCM size)
{
  return scm_from_pointer (xzalloc (scm_to_size_t (size)), NULL);
}

static SCM
scm_cfree (SCM p)
{
  free (scm_to_pointer (p));
  return SCM_UNSPECIFIED;
}

static SCM
scm_cgczalloc (SCM size)
{
  return scm_from_pointer (x_gc_malloc (scm_to_size_t (size)), NULL);
}

static SCM
scm_cgcfree (SCM p)
{
  GC_FREE (scm_to_pointer (p));
  return SCM_UNSPECIFIED;
}

VISIBLE void
init_sortsmill_guile_alloc_base (void)
{
  scm_c_define_gsubr ("c:zalloc", 1, 0, 0, scm_czalloc);
  scm_c_define_gsubr ("c:free", 1, 0, 0, scm_cfree);
  scm_c_define_gsubr ("c:gc-zalloc", 1, 0, 0, scm_cgczalloc);
  scm_c_define_gsubr ("c:gc-free", 1, 0, 0, scm_cgcfree);
}

//-------------------------------------------------------------------------
