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

#ifndef _SORTSMILLFF_REXP_H
#define _SORTSMILLFF_REXP_H

/*
 * Regular expressions.
 */

#include <atomic_ops.h>
#include <sortsmillff/xgc.h>    /* Includes gc.h and pthreads.h in the
                                   right order. */

#include <stdio.h>
#include <stdbool.h>
#include <pcre.h>
#include <sortsmillff/xunistring.h>

/* *INDENT-OFF* */
#ifdef __cplusplus
extern "C" {
#endif
/* *INDENT-ON* */

struct rexp_buffer_t
{
  pcre *pcre_ptr;
  pcre_extra *extra;
  size_t capture_count;

  /* These fields are used by the 'compile_once' routines. */
  volatile AO_t is_initialized;
  pthread_mutex_t mutex;
};

typedef struct rexp_buffer_t rexp_buffer_t;
typedef struct rexp_buffer_t *rexp_t;

/* An initializer for 'compile_once' rexp_buffer_t objects. */
#define REXP_BUFFER_T_INITIALIZER { NULL, NULL, 0, false, PTHREAD_MUTEX_INITIALIZER }

struct rexp_match_buffer_t
{
  int *ovector;
  size_t capture_count;
};

typedef struct rexp_match_buffer_t rexp_match_buffer_t;
typedef struct rexp_match_buffer_t *rexp_match_t;

struct rexp_interval_t
{
  int i_start;
  int i_end;
};

typedef struct rexp_interval_t rexp_interval_t;

rexp_t rexp_compile_opt (const char *pattern, int options);
rexp_t rexp_compile (const char *pattern);
rexp_t rexp_compile_study (const char *pattern);
rexp_t rexp_compile_jit (const char *pattern);

rexp_t rexp_compile_once_opt (rexp_buffer_t *re_buf_ptr,
                              const char *pattern, int options);
rexp_t rexp_compile_once (rexp_buffer_t *re_buf_ptr, const char *pattern);
rexp_t rexp_compile_once_study (rexp_buffer_t *re_buf_ptr,
                                const char *pattern);
rexp_t rexp_compile_once_jit (rexp_buffer_t *re_buf_ptr, const char *pattern);

rexp_t u8_rexp_compile_opt (const uint8_t *pattern, int options);
rexp_t u8_rexp_compile (const uint8_t *pattern);
rexp_t u8_rexp_compile_study (const uint8_t *pattern);
rexp_t u8_rexp_compile_jit (const uint8_t *pattern);

rexp_t u8_rexp_compile_once_opt (rexp_buffer_t *re_buf_ptr,
                                 const uint8_t *pattern, int options);
rexp_t u8_rexp_compile_once (rexp_buffer_t *re_buf_ptr,
                             const uint8_t *pattern);
rexp_t u8_rexp_compile_once_study (rexp_buffer_t *re_buf_ptr,
                                   const uint8_t *pattern);
rexp_t u8_rexp_compile_once_jit (rexp_buffer_t *re_buf_ptr,
                                 const uint8_t *pattern);

/*
  The following functions return the same rexp_t, but altered (except
  for rexp_identity, which simply returns its argument). Thus they
  violate our usual preference to avoid this kind of side effect, but
  in this case it seems worth it, so one can write something like

     m = rexp_search (rexp_study (rexp_compile (pattern), s));

*/
rexp_t rexp_study_opt (rexp_t re, int options);
rexp_t rexp_study (rexp_t re);
rexp_t rexp_jit (rexp_t re);
rexp_t rexp_identity (rexp_t re);

rexp_match_t rexp_search_opt (rexp_t re, const char *s, int options);
rexp_match_t rexp_match (rexp_t re, const char *s);
rexp_match_t rexp_search (rexp_t re, const char *s);
size_t rexp_num_subexpr (rexp_match_t m);
rexp_interval_t rexp_interval (rexp_match_t m, size_t subexpression);
char *rexp_substr (rexp_match_t m, const char *s, size_t subexpression);

rexp_match_t u8_rexp_search_opt (rexp_t re, const uint8_t *s, int options);
rexp_match_t u8_rexp_match (rexp_t re, const uint8_t *s);
rexp_match_t u8_rexp_search (rexp_t re, const uint8_t *s);
uint8_t *u8_rexp_substr (rexp_match_t m, const uint8_t *s,
                         size_t subexpression);

/* *INDENT-OFF* */
#ifdef __cplusplus
}
#endif
/* *INDENT-ON* */

#endif /* _SORTSMILLFF_REXP_H */
