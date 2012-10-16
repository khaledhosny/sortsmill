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

#ifndef _FONTFORGE_REXP_H
#define _FONTFORGE_REXP_H

//
// Regular expressions.
//

#include <config.h>

#include <stdio.h>
#include <pcre.h>

struct rexp
{
  pcre *pcre_ptr;
  size_t capture_count;
};
typedef struct rexp *rexp_t;

struct rexp_match
{
  int *ovector;
  size_t capture_count;
};
typedef struct rexp_match *rexp_match_t;

struct rexp_interval_t
{
  int i_start;
  int i_end;
};
typedef struct rexp_interval_t rexp_interval_t;

VISIBLE rexp_t rexp_compile_opt (const char *pattern, int options);
VISIBLE rexp_t rexp_compile (const char *pattern);
VISIBLE rexp_match_t rexp_search_opt (rexp_t re, const char *s, int options);
VISIBLE rexp_match_t rexp_match (rexp_t re, const char *s);
VISIBLE rexp_match_t rexp_search (rexp_t re, const char *s);
VISIBLE size_t rexp_num_subexpr (rexp_match_t m);
VISIBLE rexp_interval_t rexp_interval (rexp_match_t m, size_t subexpression);
VISIBLE char *rexp_substr (rexp_match_t m, const char *s,
                           size_t subexpression);

#endif // _FONTFORGE_REXP_H
