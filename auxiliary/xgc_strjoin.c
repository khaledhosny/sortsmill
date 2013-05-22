#include <config.h>

// Copyright (C) 2012 by Khaled Hosny and Barry Schwartz
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

#include <sortsmill/xgc.h>
#include <stdarg.h>
#include <string.h>

VISIBLE char *
x_gc_strjoin (const char *s1, ...)
{
  va_list ap;

  va_start (ap, s1);
  char *p = x_gc_vstrjoin (s1, ap);
  va_end (ap);
  return p;
}

VISIBLE char *
x_gc_vstrjoin (const char *s1, va_list ap)
{
  char *p;

  if (s1 == NULL)
    p = x_gc_strdup ("");
  else
    {
      va_list aq;

      size_t length = strlen (s1);

      size_t total_length = length;
      va_copy (aq, ap);
      char *s = va_arg (aq, char *);
      while (s != NULL)
        {
          total_length += strlen (s);
          s = va_arg (aq, char *);
        }
      va_end (aq);

      p = (char *) x_gc_malloc_atomic ((total_length + 1) * sizeof (char));

      memcpy (p, s1, length * sizeof (char));
      total_length = length;
      va_copy (aq, ap);
      s = va_arg (aq, char *);
      while (s != NULL)
        {
          length = strlen (s);
          memcpy (p + total_length, s, length * sizeof (char));
          total_length += length;
          s = va_arg (aq, char *);
        }
      p[total_length] = '\0';
      va_end (aq);
    }

  return p;
}
