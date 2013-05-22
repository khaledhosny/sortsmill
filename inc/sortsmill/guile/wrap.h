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

#ifndef _SORTSMILL_GUILE_WRAP_H
#define _SORTSMILL_GUILE_WRAP_H

#include <libguile.h>
#include <sortsmill/c_version.h>

#define C_WRAP_SCM_CALL_0(C_NAME, SCM_MODULE, SCM_NAME)                 \
  SCM                                                                   \
  C_NAME (void)                                                         \
  {                                                                     \
    return scm_call_0 (scm_c_public_ref (SCM_MODULE, SCM_NAME));        \
  }

#define C_WRAP_SCM_CALL_1(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1)                                             \
  {                                                             \
    return scm_call_1 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1);                                   \
  }

#define C_WRAP_SCM_CALL_2(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2)                                   \
  {                                                             \
    return scm_call_2 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2);                             \
  }

#define C_WRAP_SCM_CALL_3(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3)                         \
  {                                                             \
    return scm_call_3 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3);                       \
  }

#define C_WRAP_SCM_CALL_4(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4)               \
  {                                                             \
    return scm_call_4 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4);                 \
  }

#define C_WRAP_SCM_CALL_5(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5)     \
  {                                                             \
    return scm_call_5 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4, arg5);           \
  }

#define C_WRAP_SCM_CALL_6(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,     \
          SCM arg6)                                             \
  {                                                             \
    return scm_call_6 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4, arg5, arg6);     \
  }

#define C_WRAP_SCM_CALL_7(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,     \
          SCM arg6, SCM arg7)                                   \
  {                                                             \
    return scm_call_7 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4, arg5, arg6,      \
                       arg7);                                   \
  }

#define C_WRAP_SCM_CALL_8(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,     \
          SCM arg6, SCM arg7, SCM arg8)                         \
  {                                                             \
    return scm_call_8 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4, arg5, arg6,      \
                       arg7, arg8);                             \
  }

#define C_WRAP_SCM_CALL_9(C_NAME, SCM_MODULE, SCM_NAME)         \
  SCM                                                           \
  C_NAME (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5,     \
          SCM arg6, SCM arg7, SCM arg8, SCM arg9)               \
  {                                                             \
    return scm_call_9 (scm_c_public_ref (SCM_MODULE, SCM_NAME), \
                       arg1, arg2, arg3, arg4, arg5, arg6,      \
                       arg7, arg8, arg9);                       \
  }

#define SCM_FF_API_CALL_0(PROC_NAME)                            \
  (scm_call_0 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME)))

#define SCM_FF_API_CALL_1(PROC_NAME, A1)                        \
  (scm_call_1 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1)))

#define SCM_FF_API_CALL_2(PROC_NAME, A1, A2)                    \
  (scm_call_2 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1), (A2)))

#define SCM_FF_API_CALL_3(PROC_NAME, A1, A2, A3)                \
  (scm_call_3 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME), (A1), (A2), (A3)))

#define SCM_FF_API_CALL_4(PROC_NAME, A1, A2, A3, A4)            \
  (scm_call_4 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4)))

#define SCM_FF_API_CALL_5(PROC_NAME, A1, A2, A3, A4, A5)        \
  (scm_call_5 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4), (A5)))

#define SCM_FF_API_CALL_6(PROC_NAME, A1, A2, A3, A4, A5, A6)    \
  (scm_call_6 (scm_c_public_ref ("sortsmill fontforge-api",     \
                                 PROC_NAME),                    \
               (A1), (A2), (A3), (A4), (A5), (A6)))

#endif /* _SORTSMILL_GUILE_WRAP_H */
