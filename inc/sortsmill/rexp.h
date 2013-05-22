/*
 * Copyright (C) 2012 Khaled Hosny and Barry Schwartz
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

#ifndef _SORTSMILL_REXP_H
#define _SORTSMILL_REXP_H

/*
 * Regular expressions.
 */

#include <atomic_ops.h>
#include <sortsmill/xgc.h>      /* Includes gc.h and pthreads.h in the
                                   right order. */

#include <stdio.h>
#include <stdbool.h>
#include <pcre.h>
#include <sortsmill/xunistring.h>

#ifdef __cplusplus
extern "C"
{
#endif
#if 0
}
#endif

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
rexp_t rexp_compile_once_study (rexp_buffer_t *re_buf_ptr, const char *pattern);
rexp_t rexp_compile_once_jit (rexp_buffer_t *re_buf_ptr, const char *pattern);

rexp_t u8_rexp_compile_opt (const uint8_t *pattern, int options);
rexp_t u8_rexp_compile (const uint8_t *pattern);
rexp_t u8_rexp_compile_study (const uint8_t *pattern);
rexp_t u8_rexp_compile_jit (const uint8_t *pattern);

rexp_t u8_rexp_compile_once_opt (rexp_buffer_t *re_buf_ptr,
                                 const uint8_t *pattern, int options);
rexp_t u8_rexp_compile_once (rexp_buffer_t *re_buf_ptr, const uint8_t *pattern);
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

#if 0
{
#endif
#ifdef __cplusplus
}
#endif

#endif /* _SORTSMILL_REXP_H */
