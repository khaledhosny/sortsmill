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

#include <libguile.h>
#include <atomic_ops.h>
#include <sortsmill/xgc.h>      /* Includes pthread.h and gc.h in the
                                   correct order. */
#include <sortsmill/one_time_initialization.h>
#include <sortsmill/attributes.h>

void scm_dynwind_pthread_mutex_unlock (void *mutex_ptr);

#if STM_AT_LEAST_C99       /* For standard variadic macros support. */

/*
 * We use "double-checked locking".
 * See http://www.hpl.hp.com/research/linux/atomic_ops/example.php4
 *
 * Values defined with this macro are ‘lazy’; that is, they are not
 * initialized until the first time they are used.
 */

#define __INITIALIZED_CONSTANT_THREAD_LOCK(MUTEX)       \
  scm_dynwind_begin (0);                                \
  pthread_mutex_lock (&MUTEX);                          \
  scm_dynwind_pthread_mutex_unlock (&MUTEX)

#define __INITIALIZED_CONSTANT_THREAD_UNLOCK(MUTEX)     \
  scm_dynwind_end ()

/* Unfortunately, in a C99-conforming program, the
   INITIALIZER_FUNCTION must take at least one extra argument, even if
   unused. GCC is more permissive. */
#define INITIALIZED_CONSTANT(MODIFIERS, TYPE, NAME,                     \
                             INITIALIZER_FUNCTION, ...)                 \
                                                                        \
  pthread_mutex_t __##NAME##__mutex = PTHREAD_MUTEX_INITIALIZER;        \
                                                                        \
  STM_ONE_TIME_INITIALIZE(MODIFIERS, TYPE, NAME,                        \
                          __INITIALIZED_CONSTANT_THREAD_LOCK(__##NAME##__mutex), \
                          __INITIALIZED_CONSTANT_THREAD_UNLOCK(__##NAME##__mutex), \
                          INITIALIZER_FUNCTION (&NAME##__Value, __VA_ARGS__))

#endif /* STM_AT_LEAST_C99 */

#endif /* _SORTSMILL_INITIALIZED_GLOBAL_CONSTANTS_H */
