/*
 * Copyright (C) 2012 by Barry Schwartz
  
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

#ifndef _PS_NUMBER_H
#define _PS_NUMBER_H

//
// PostScript numbers.
//

#include <config.h>

#include <stdbool.h>

// vis--
// vis-- The following items are declared in header file
// vis-- @file{ps_number.h}.
// vis--

// vis--
// vis-- @deftypefun {bool} is_postscript_integer (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, such as
// vis-- `@code{555}', `@code{+034}', or `@code{-1234}'.
// vis--
// vis-- @end deftypefun
// vis--
VISIBLE bool is_postscript_integer (const char *s);

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
VISIBLE bool is_postscript_real (const char *s);

// vis--
// vis-- @deftypefun {bool} is_postscript_radix_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript radix number, such as
// vis-- `@code{2#01011}', `@code{008#555}', or `@code{16#9ABCdef0}'.
// vis--
// vis-- @end deftypefun
// vis--
VISIBLE bool is_postscript_radix_number (const char *s);

// vis--
// vis-- @deftypefun {bool} is_postscript_number (const char *@var{s})
// vis--
// vis-- Test if the @code{NULL}-terminated string @var{s} represents
// vis-- a PostScript integer, real, or radix number.
// vis--
// vis-- @end deftypefun
// vis--
VISIBLE bool is_postscript_number (const char *s);

#endif // _PS_NUMBER_H
