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

#include <sortsmill/x_alloc.h>
#include <xalloc.h>

// Exported wrappers around otherwise hidden xalloc routines.

VISIBLE void
x_alloc_die (void)
{
  xalloc_die ();
}

VISIBLE void *
x_malloc (size_t s)
{
  return xmalloc (s);
}

VISIBLE void *
x_zalloc (size_t s)
{
  return xzalloc (s);
}

VISIBLE void *
x_calloc (size_t n, size_t s)
{
  return xcalloc (n, s);
}

VISIBLE void *
x_realloc (void *p, size_t s)
{
  return xrealloc (p, s);
}

VISIBLE void *
x_2realloc (void *p, size_t *pn)
{
  return x2realloc (p, pn);
}

VISIBLE void *
x_memdup (void const *p, size_t s)
{
  return xmemdup (p, s);
}

VISIBLE char *
x_strdup (char const *str)
{
  return xstrdup (str);
}

VISIBLE void *
x_nmalloc (size_t n, size_t s)
{
  return xnmalloc (n, s);
}

VISIBLE void *
x_nrealloc (void *p, size_t n, size_t s)
{
  return xnrealloc (p, n, s);
}

VISIBLE void *
x_2nrealloc (void *p, size_t *pn, size_t s)
{
  return x2nrealloc (p, pn, s);
}

VISIBLE char *
x_charalloc (size_t n)
{
  return xcharalloc (n);
}
