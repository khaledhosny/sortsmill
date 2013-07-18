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

#include <sortsmill/x_printf.h>

// Exported wrappers around C locale versions of printf routines.

VISIBLE int
x_fprintf (FILE *file, const char *format, ...)
{
  int result;
  char *buffer;
  va_list ap;
  va_start (ap, format);
  result = c_vasprintf (&buffer, format, ap);
  if (result != -1)
    {
      fputs (buffer, file);
      free (buffer);
    }
  va_end (ap);

  return result;
}
