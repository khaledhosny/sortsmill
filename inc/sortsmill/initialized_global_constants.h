/* -*- coding: utf-8 -*- */
/*
 * Copyright (C) 2013 Khaled Hosny and Barry Schwartz
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

#ifndef _SORTSMILL_INITIALIZED_GLOBAL_CONSTANTS_H
#define _SORTSMILL_INITIALIZED_GLOBAL_CONSTANTS_H

#include <atomic_ops.h>
#include <sortsmill/xgc.h>      /* Includes pthread.h and gc.h in the
                                   correct order. */
#include <libguile.h>
#include <stdbool.h>
#include <sortsmill/c_version.h>

#if _FF_C99_OR_GREATER          /* For standard variadic macros support. */

/*
 * We use "double-checked locking".
 * See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
 *
 * Values defined with this macro are ‘lazy’; that is, they are not
 * initialized until the first time they are used.
 */

/* Unfortunately, in a C99-conforming program, the
   INITIALIZER_FUNCTION must take at least one extra argument, even if
   unused. GCC is more permissive. */
#define INITIALIZED_CONSTANT(MODIFIERS, TYPE, NAME,                     \
                             INITIALIZER_FUNCTION, ...)                 \
                                                                        \
  static struct __##NAME##__data_t__                                    \
  {                                                                     \
    volatile AO_t is_initialized;                                       \
    pthread_mutex_t mutex;                                              \
    TYPE value;                                                         \
  } __##NAME##__data__ = { false, PTHREAD_MUTEX_INITIALIZER };          \
                                                                        \
  /* Use dynamic wind, to unlock the mutex regardless of exceptions. */ \
  static void                                                           \
  __##NAME##__unwind_handler__ (void *p)                                \
  {                                                                     \
    struct __##NAME##__data_t__ *data_ptr =                             \
      ((struct __##NAME##__data_t__ *) p);                              \
    pthread_mutex_unlock (&data_ptr->mutex);                            \
  }                                                                     \
                                                                        \
  MODIFIERS TYPE                                                        \
  NAME (void)                                                           \
  {                                                                     \
    if (!AO_load_acquire_read (&__##NAME##__data__.is_initialized))     \
      {                                                                 \
        scm_dynwind_begin (0);                                          \
        pthread_mutex_lock (&__##NAME##__data__.mutex);                 \
        scm_dynwind_unwind_handler (__##NAME##__unwind_handler__,       \
                                    &__##NAME##__data__,                \
                                    SCM_F_WIND_EXPLICITLY);             \
        if (!__##NAME##__data__.is_initialized)                         \
          {                                                             \
            INITIALIZER_FUNCTION (&__##NAME##__data__.value,            \
                                  __VA_ARGS__);                         \
            AO_store_release_write (&__##NAME##__data__.is_initialized, \
                                    true);                              \
          }                                                             \
        scm_dynwind_end ();                                             \
      }                                                                 \
    return __##NAME##__data__.value;                                    \
  }

#endif /* _FF_C99_OR_GREATER */

/* An INITIALIZER function for Guile data specified as Scheme source
   code in a C string. */
void scm_c_initialize_from_eval_string (SCM *proc, const char *s);

#define SCM_SYMBOL_CONSTANT(MODIFIER, C_NAME, SCM_NAME)                 \
  INITIALIZED_CONSTANT (_FF_ATTRIBUTE_PURE MODIFIER, SCM, C_NAME,       \
                        scm_c_initialize_from_eval_string,              \
                        "(quote " SCM_NAME ")");

#endif /* _SORTSMILL_INITIALIZED_GLOBAL_CONSTANTS_H */
