/*
 * Copyright (C) 2013 Barry Schwartz
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
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

/*
 * We use "double-checked locking".
 *
 * See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
 */

#define INITIALIZED_CONSTANT(MODIFIERS, TYPE, NAME,                     \
                             INITIALIZER_FUNCTION)                      \
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
            INITIALIZER_FUNCTION (&__##NAME##__data__.value);           \
            AO_store_release_write (&__##NAME##__data__.is_initialized, \
                                    true);                              \
          }                                                             \
        scm_dynwind_end ();                                             \
      }                                                                 \
    return __##NAME##__data__.value;                                    \
  }

#endif /* _SORTSMILL_INITIALIZED_GLOBAL_CONSTANTS_H */
