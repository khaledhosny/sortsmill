#include <config.h>

/* Copyright (C) 2012 by Barry Schwartz */
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

// intro--
// intro-- Header file: @file{ps_number.h}
// intro--

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <c-ctype.h>
#include <xstrndup.h>
#include <ps_number.h>

static const char digits[] = "0123456789";
static const char letters[] = "abcdefghijklmnopqrstuvwxyz";

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

// vis--
// vis-- @deftypefun {VISIBLE bool} is_postscript_integer (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, such as
// vis-- `@code{555}', `@code{+034}', or `@code{-1234}'.
// vis--
// vis-- @end deftypefun
// vis--
bool
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

// vis--
// vis-- @deftypefun {VISIBLE bool} is_postscript_real (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript real, such as
// vis-- `@code{555.}', `@code{+.034}', `@code{-01234E56}', or
// vis-- `@code{1.9e+23}'.
// vis--
// vis-- @end deftypefun
// vis--
bool
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

// vis--
// vis-- @deftypefun {VISIBLE bool} is_postscript_radix_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript radix number, such as
// vis-- `@code{2#01011}', `@code{008#555}', or `@code{16#9ABCdef0}'.
// vis--
// vis-- @end deftypefun
// vis--
bool
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
      if (s[i] == '#' && c_isdigit (s[i + 1]))
        {
          long int radix = strtol (s, NULL, 10);
          if (2 <= radix && radix <= 36)
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

// vis--
// vis-- @deftypefun {VISIBLE bool} is_postscript_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, real, or radix number.
// vis--
// vis-- @end deftypefun
// vis--
bool
is_postscript_number (const char *s)
{
  assert (s != NULL);
  return (is_postscript_integer (s) || is_postscript_real (s)
          || is_postscript_radix_number (s));
}
