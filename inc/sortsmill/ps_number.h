/*
 * Copyright (C) 2012, 2013 Khaled Hosny and Barry Schwartz
 * This file is part of the Sorts Mill Tools.
 * 
 * Sorts Mill Tools is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * Sorts Mill Tools is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */

#ifndef _SORTSMILL_PS_NUMBER_H
#define _SORTSMILL_PS_NUMBER_H

/*
 * PostScript numbers.
 */

#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

/*
// vis--
// vis-- The following items are declared in header file
// vis-- @file{sortsmill/ps_number.h}.
// vis--
*/

/*
// vis--
// vis-- @deftypefun {bool} is_postscript_integer (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, such as
// vis-- `@code{555}', `@code{+034}', or `@code{-1234}'.
// vis--
// vis-- @end deftypefun
// vis--
*/
bool is_postscript_integer (const char *s);

/*
// vis--
// vis-- @deftypefun {bool} is_postscript_real (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript real, such as
// vis-- `@code{555.}', `@code{+.034}', `@code{-01234E56}', or
// vis-- `@code{1.9e+23}'.
// vis--
// vis-- @end deftypefun
// vis--
*/
bool is_postscript_real (const char *s);

/*
// vis--
// vis-- @deftypefun {bool} is_postscript_radix_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript radix number, such as
// vis-- `@code{2#01011}', `@code{008#555}', or `@code{16#9ABCdef0}'.
// vis--
// vis-- @end deftypefun
// vis--
*/
bool is_postscript_radix_number (const char *s);

/*
// vis--
// vis-- @deftypefun {bool} is_postscript_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, real, or radix number.
// vis--
// vis-- @end deftypefun
// vis--
*/
bool is_postscript_number (const char *s);

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_PS_NUMBER_H */
