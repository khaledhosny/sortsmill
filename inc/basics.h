/* Copyright (C) 2000-2012 by George Williams */
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
#ifndef _BASICS_H
#define _BASICS_H

#include <config.h>

#ifndef LOCALEDIR
#error You must set LOCALEDIR.
#endif

#ifndef SHAREDIR
#error You must set SHAREDIR.
#endif

#ifndef FF_TEXTDOMAIN
#error You must define FF_TEXTDOMAIN.
#endif

#ifndef FF_SHORTCUTSDOMAIN
#error You must define FF_SHORTCUTSDOMAIN.
#endif

#ifndef FF_MACSHORTCUTSDOMAIN
#error You must define FF_MACSHORTCUTSDOMAIN.
#endif

#include <sortsmillff/xgc.h>
#include <xstrndup.h>
#include <xunistring.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <strings.h>
#include <math.h>
#include <limits.h>
#include <null_passthru.h>

static inline int
imin (int a, int b)
{
  return (a < b) ? a : b;
}

static inline int
imax (int a, int b)
{
  return (a < b) ? b : a;
}

static inline unsigned int
umin (unsigned int a, unsigned int b)
{
  return (a < b) ? a : b;
}

static inline unsigned int
umax (unsigned int a, unsigned int b)
{
  return (a < b) ? b : a;
}

static inline size_t
szmin (size_t a, size_t b)
{
  return (a < b) ? a : b;
}

static inline size_t
szmax (size_t a, size_t b)
{
  return (a < b) ? b : a;
}

static inline char *
xstrdup_or_null (const char *str)
{
  return NULL_PASSTHRU (str, xstrdup (str));
}

static inline char *
xstrndup_or_null (const char *str, size_t n)
{
  return NULL_PASSTHRU (str, xstrndup (str, n));
}

#endif
