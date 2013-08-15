#include <config.h>

// Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <c-ctype.h>
#include <xstrndup.h>
#include <sortsmill/ps_number.h>

static const char digits[] = "0123456789";
static const char letters[] = "abcdefghijklmnopqrstuvwxyz";

//-------------------------------------------------------------------------

static bool
is_digit_in_radix (int c, int radix)
{
  bool is_valid = false;

  if (c_isdigit (c))
    {
      unsigned int digit_value = strchr (digits, c) - digits;
      is_valid = (digit_value < radix);
    }
  else if (c_isalpha (c))
    {
      unsigned int digit_value =
        10 + (strchr (letters, c_tolower (c)) - letters);
      is_valid = (digit_value < radix);
    }

  return is_valid;
}

static bool
is_exponent (const char *s)
{
  return ((s[0] == 'E' || s[0] == 'e') && is_postscript_integer (s + 1));
}

static bool
is_decimal_fraction (const char *s)
{
  bool is_dec_frac = false;

  char *decimal_point_ptr = strchr (s, '.');
  if (decimal_point_ptr != NULL)
    {
      char *prefix = xstrndup (s, decimal_point_ptr - s);
      if (decimal_point_ptr[1] == '\0')
        // Look for something like "+123.".
        is_dec_frac = is_postscript_integer (prefix);
      else if (c_isdigit (decimal_point_ptr[1])
               && is_postscript_integer (decimal_point_ptr + 1))
        // Look for something like ".012", "+.012", or "+123.012".
        is_dec_frac = (strcmp (prefix, "") == 0
                       || strcmp (prefix, "-") == 0
                       || strcmp (prefix, "+") == 0
                       || is_postscript_integer (prefix));
      free (prefix);
    }

  return is_dec_frac;
}

VISIBLE bool
is_postscript_integer (const char *s)
{
  assert (s != NULL);

  bool is_integer = false;

  size_t i = 0;
  if (s[i] == '-' || s[i] == '+')
    i++;
  if (c_isdigit (s[i]))
    {
      do
        i++;
      while (c_isdigit (s[i]));
      is_integer = (s[i] == '\0');
    }

  return is_integer;
}

VISIBLE bool
is_postscript_real (const char *s)
{
  assert (s != NULL);

  bool is_real = false;

  char *e_ptr = strchr (s, 'e');
  if (e_ptr == NULL)
    e_ptr = strchr (s, 'E');

  if (e_ptr == NULL)
    is_real = is_decimal_fraction (s);
  else
    {
      char *prefix = xstrndup (s, e_ptr - s);
      is_real = is_exponent (e_ptr) &&
        (is_postscript_integer (prefix) || is_decimal_fraction (prefix));
      free (prefix);
    }

  return is_real;
}

VISIBLE bool
is_postscript_radix_number (const char *s)
{
  assert (s != NULL);

  bool is_radix_number = false;

  size_t i = 0;
  if (c_isdigit (s[i]))
    {
      do
        i++;
      while (c_isdigit (s[i]));
      if (s[i] == '#')
        {
          long int radix = strtol (s, NULL, 10);
          if (2 <= radix && radix <= 36
              && is_digit_in_radix (s[i + 1], (int) radix))
            {
              do
                i++;
              while (is_digit_in_radix (s[i], (int) radix));
              is_radix_number = (s[i] == '\0');
            }
        }
    }

  return is_radix_number;
}

VISIBLE bool
is_postscript_number (const char *s)
{
  assert (s != NULL);
  return (is_postscript_integer (s) || is_postscript_real (s)
          || is_postscript_radix_number (s));
}

//-------------------------------------------------------------------------
